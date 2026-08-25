#!/usr/bin/env python
# coding: utf-8

import os
import re
import yaml
import chromadb
import openai
import bibtexparser
import click
from llama_index.core import VectorStoreIndex, SimpleDirectoryReader, StorageContext, Document
from llama_index.core.schema import TransformComponent
from llama_index.core.node_parser import TextSplitter, SentenceSplitter
from llama_index.embeddings.openai import OpenAIEmbedding
from llama_index.vector_stores.chroma import ChromaVectorStore
from llama_index.core.ingestion.pipeline import IngestionPipeline, DocstoreStrategy
from llama_index.core.storage.docstore import SimpleDocumentStore

from dotenv import load_dotenv

load_dotenv()


# Configuration
CHROMADB_PATH = os.getenv("CHROMADB_PATH", False)
CHROMADB_HOST = os.getenv("CHROMADB_HOST", "localhost")
CHROMADB_PORT = os.getenv("CHROMADB_PORT", "8000")
DOCSTORES_PATH = os.getenv("DOCSTORES_PATH")
CHROMA_TENANT = os.getenv("CHROMA_TENANT")
CHROMA_API_KEY = os.getenv("CHROMA_API_KEY")
CHROMA_DATABASE = os.getenv("CHROMA_DATABASE")

EXTENSIONS = [".markdown", ".md", ".txt", ".org", ".pl"]
TEXT_EMBEDDING_MODEL = "text-embedding-3-small"


# Set up OpenAI
openai.api_key = os.getenv("OPENAI_API_KEY")
embed_model = OpenAIEmbedding(embed_batch_size=10, model=TEXT_EMBEDDING_MODEL)

# Metadata fields
METADATA_FIELDS = ['id', 'title', 'author', 'date', 'citekey', 'loc', 'ref_tlg', 'section',
                   'line', 'uplink', 'proj', 'event', 'tags']


# Module-level YAML configuration to ensure pickling compatibility for multiprocessing
def date_as_string(loader, node):
    return loader.construct_scalar(node)

yaml.add_constructor("tag:yaml.org,2002:timestamp", date_as_string, Loader=yaml.SafeLoader)


class YAMLMetadataExtractor(TransformComponent):
    """Extracts YAML metadata from the header. Safe for multiprocessing."""

    def load_yaml(self, yaml_string):
        return yaml.safe_load(yaml_string)

    def convert_tags_to_yaml_str(self, line):
        key, tags_str = line.split(":", 1)
        tags = re.findall(r'#(\w+)', tags_str)
        return f"{key.strip()}: \"" + " ".join([f"#{tag}" for tag in tags]) + "\""

    def sanitize_field_value(self, input_string):
        if ":" not in input_string:
            return input_string
        field, value = input_string.split(":", 1)
        sanitized_value = value.replace(":", "-").replace("*", "_")
        return f"{field}: {sanitized_value}"

    def sanitize_yaml_header(self, yaml_header):
        sanitized_lines = []
        for line in yaml_header.splitlines():
            line = self.sanitize_field_value(line)
            line = re.sub(r':[\t ]+', ': ', line)
            line = re.sub(r'^\t+', '    ', line)
            line = re.sub(r'[ \t]+$', '', line)
            line = re.sub(r'[\']', '', line)
            line = re.sub(r'\[\[(\d+)\]\]', r'\g<1>', line)
            if line.startswith("tags:"):
                line = self.convert_tags_to_yaml_str(line)
                sanitized_lines.append(line)
        return '\n'.join(sanitized_lines)

    def extract_yaml_header(self, text):
        yaml_pattern = r'^(---|\.\.\.)(.*?)(---|\.\.\.)(\n|\r|$)'
        match = re.search(yaml_pattern, text, re.DOTALL)
        if match:
            yaml_content = match.group(2).strip()
            sanitized_yaml_content = self.sanitize_yaml_header(yaml_content)
            return self.load_yaml(sanitized_yaml_content)
        return None

    def filter_metadata(self, input_dict):
        if not isinstance(input_dict, dict) or not input_dict:
            return {}
        return {key: input_dict[key] for key in METADATA_FIELDS if key in input_dict}

    def transform(self, document: Document, **kwargs) -> Document:
        print(f"DEBUG: Processing headers from {document.doc_id}")
        metadata = self.filter_metadata(self.extract_yaml_header(document.text))
        if metadata:
            document.metadata.update(metadata)
            try:
                document.text = re.sub(r'^(---|\.\.\.)(.*?)(---|\.\.\.)(\r|\n|$)', '',
                                       document.text, flags=re.DOTALL).strip()
            except AttributeError:
                print(f"WARNING: Could not process YAML header in {document.doc_id}")
                return document
        return document

    def __call__(self, nodes, **kwargs):
        for node in nodes:
            self.transform(node)
        return nodes


class MarkdownTextSplitter(TextSplitter):
    """Splits markdown text by headers. Uses standard Pydantic annotation to ensure multiprocessing compatibility."""
    chunk_size: int = 1500

    def split_text(self, text: str) -> list:
        sections = re.split(r'(#{1,6} .*?\n)', text)
        chunks = []
        current_chunk = ""
        for section in sections:
            if section.startswith("#"):
                if current_chunk.strip():
                    chunks.append(current_chunk.strip())
                    current_chunk = ""
                current_chunk += section
            else:
                current_chunk += section
        if current_chunk.strip():
            chunks.append(current_chunk.strip())
        return chunks


class HeadingExtractor(TransformComponent):
    """Extracts first heading for metadata. Safe for multiprocessing."""

    def transform(self, document: Document) -> Document:
        heading_pattern = r'^(#{1,6})\s+(.*)'
        lines = document.text.split('\n')
        for line in lines:
            match = re.match(heading_pattern, line)
            if match:
                heading_text = match.group(2)
                title = document.metadata.get('title')
                if heading_text != title:
                    document.metadata['heading'] = heading_text
                if len(heading_text) > 512:
                    raise ValueError(f"doc title {title} has too big of a heading: {heading_text}")
                break
        return document

    def __call__(self, nodes, **kwargs):
        for node in nodes:
            self.transform(node)
        return nodes


# Main ingestion process

@click.command()
@click.argument('collection_name', required=True, type=str)
@click.option('--cloud', is_flag=True, help='Use ChromaDB from the Cloud')
@click.option('--source', required=True, help='Source path for note repository')
@click.option('--bibliography', required=True, help='Path for Bib(La)TeX .bib file')
@click.option('--num_workers', required=False, type=int, help='Number of simultaneous workers', default=os.cpu_count())
@click.option('--purge', is_flag=True, help='Flag to purge the database from documents corresponding to no longer existant files.')
def ingest(collection_name, cloud, source, bibliography, num_workers, purge):
    """CLI tool to create or update chromadb ingesting the phi-notes"""
    
    print(f"Updating '{collection_name}'...")
    
    if cloud:
        print("Connecting to Chroma Cloud...")
        db = chromadb.CloudClient(
            tenant=CHROMA_TENANT,
            database=CHROMA_DATABASE,
            api_key=CHROMA_API_KEY)
    elif CHROMADB_PATH:
        print(f"Connecting to persistent local database at {CHROMADB_PATH}...")
        db = chromadb.PersistentClient(path=CHROMADB_PATH)
    else:
        print(f"Connecting to local Chroma HTTP server at {CHROMADB_HOST}:{CHROMADB_PORT}...")
        db = chromadb.HttpClient(host=CHROMADB_HOST, port=CHROMADB_PORT)

    chroma_collection = db.get_or_create_collection(collection_name)
    vector_store = ChromaVectorStore(chroma_collection=chroma_collection)

    transformations = [
        YAMLMetadataExtractor(),
        MarkdownTextSplitter(),
        HeadingExtractor(),
        SentenceSplitter(chunk_size=512, chunk_overlap=100),
        OpenAIEmbedding(embed_batch_size=10, model=TEXT_EMBEDDING_MODEL),
    ]

    base_docstore_path = DOCSTORES_PATH if DOCSTORES_PATH else "./docstores"
    docstore_dir = f"{base_docstore_path}/{collection_name}"

    if not os.path.exists(docstore_dir):
        print(f"Creating new local docstore tracker in {docstore_dir}")
        os.makedirs(docstore_dir, exist_ok=True)
        docstore = SimpleDocumentStore()
        storage_context = StorageContext.from_defaults(
            docstore=docstore,
            vector_store=vector_store)
        storage_context.persist(persist_dir=docstore_dir)
    else:
        print(f"Loading existing local docstore tracker from {docstore_dir}")
        docstore = SimpleDocumentStore.from_persist_dir(persist_dir=docstore_dir)
        storage_context = StorageContext.from_defaults(
            docstore=docstore,
            vector_store=vector_store)
            
    pipeline = IngestionPipeline(transformations=transformations,
                                 docstore=docstore,
                                 docstore_strategy=DocstoreStrategy.UPSERTS)

    print(f"Ingesting from {source}")

    the_documents = SimpleDirectoryReader(input_dir=source,
                                          required_exts=EXTENSIONS,
                                          filename_as_id=True,
                                          num_files_limit=None,
                                          ).load_data()

    print(f"Processing the pipeline with {num_workers} workers...")
    nodes = pipeline.run(documents=the_documents, num_workers=num_workers)

    current_doc_ids = set()
    for doc in the_documents:
        current_doc_ids.add(doc.id_)

    BATCH_LIMIT = 300

    for i in range(0, len(nodes), BATCH_LIMIT):
        node_batch = nodes[i : i + BATCH_LIMIT]
        vector_store.add(node_batch)
        print(f"Uploaded node batch {i} to {i + len(node_batch)} of {len(nodes)}")

    if purge:
        all_stored_doc_ids = set(storage_context.docstore.docs.keys())
        ids_to_delete = all_stored_doc_ids - current_doc_ids

        if ids_to_delete:
            print(f"Cleaning up {len(ids_to_delete)} deleted documents from stores...")
            for doc_id in ids_to_delete:
                vector_store.delete(doc_id) 
                storage_context.docstore.delete_document(doc_id, raise_error=False)

    storage_context.persist(persist_dir=docstore_dir)
    print("Ingestion completed successfully.")
    
if __name__ == '__main__':
    ingest()
