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
from llama_index.core.ingestion import IngestionPipeline
from llama_index.core.storage.docstore import SimpleDocumentStore

# Configuration
CHROMADB_PATH = os.getenv("CHROMADB_PATH", False)
CHROMADB_HOST = os.getenv("CHROMADB_HOST", "localhost")
CHROMADB_PORT = os.getenv("CHROMADB_PORT", "8000")
DOCSTORES_PATH = os.getenv("DOCSTORES_PATH")

EXTENSIONS = [".markdown", ".md", ".txt", ".org", ".pl"]
TEXT_EMBEDDING_MODEL = "text-embedding-3-small"


# Set up OpenAI
openai.api_key = os.getenv("OPENAI_API_KEY")
embed_model = OpenAIEmbedding(embed_batch_size=10, model=TEXT_EMBEDDING_MODEL)

# Metadata fields
METADATA_FIELDS = ['id', 'title', 'author', 'date', 'citekey', 'loc', 'ref_tlg', 'section',
                   'line', 'uplink', 'proj', 'event', 'tags']

class YAMLMetadataExtractor(TransformComponent):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        yaml.add_constructor("tag:yaml.org,2002:timestamp", self.date_as_string,
                             Loader=yaml.SafeLoader)

    def date_as_string(self, loader, node):
        return loader.construct_scalar(node)

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
            except AttributeError: ## TODO: could do better when there's no YAML header
                print(f"WARNING: Could not process YAML header in {document.doc_id}")
                return document
        return document

    def __call__(self, nodes, **kwargs) -> Document:
        for node in nodes:
            node = self.transform(node)
        return nodes

class BibliographyQuoter(TransformComponent):
    bib_file_path: str = None
    bib_entries: list = []

    def __init__(self, bib_file_path: str, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.bib_entries = self.read_biblatex_file(bib_file_path)

    def read_biblatex_file(self, bib_file_path):
        with open(bib_file_path, 'r') as bib_file:
            bib_database = bibtexparser.load(bib_file)
        return bib_database.entries

    def format_bibliography(self, entry, location: str):
        author = entry.get('author', 'Unknown Author')
        title = entry.get('title', 'Unknown Title')
        date = entry.get('year', 'Unknown Date')
        return f'{author} ({date}). "{title}", p. {location}.' if location else f'{author} ({date}). "{title}".'

    def transform(self, document: Document) -> Document:
        citekey = document.metadata.get("citekey")
        loc = document.metadata.get("loc")
        if citekey:
            matching_entry = next((entry for entry in self.bib_entries if entry['ID'] == citekey), None)
            if matching_entry:
                bib_ref = self.format_bibliography(matching_entry, location=loc)
                document.metadata.update({'source': bib_ref, 'bib_file_path': self.bib_file_path})
                for x in ['bib_file_path', 'citekey', 'loc']:
                    document.excluded_embed_metadata_keys.append(x)
                document.excluded_llm_metadata_keys.append('bib_file_path')
        return document

    def __call__(self, nodes, **kwargs) -> Document:
        for node in nodes:
            node = self.transform(node)
        return nodes

class MarkdownTextSplitter(TextSplitter):
    chunk_size: int = None

    def __init__(self, chunk_size=512):
        super().__init__()
        self.chunk_size = chunk_size

    def split_text(self, text):
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
    def __init__(self):
        super().__init__()

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

    def __call__(self, nodes, **kwargs) -> Document:
        for node in nodes:
            node = self.transform(node)
        return nodes

# Main ingestion process

@click.command()
@click.argument('collection_name', required=True, type=str)
@click.option('--source', required=True, help='Source path for note repository')
@click.option('--bibliography', required=True, help='Path for Bib(La)TeX .bib file') # TODO: poderia aceitar False
@click.option('--num_workers', required=False, help='Number of simultaneous workers', default=os.cpu_count())
@click.option('--purge', is_flag=True, help='Flag to purge the database from documents ' +
              'corresponding to no longer existant files.') # TODO: implementar
def ingest(collection_name, source, bibliography, num_workers, purge):
    """CLI tool to create or update chromadb ingesting the phi-notes"""

    print(f"Updating \'{collection_name}\'...")
    
    if CHROMADB_PATH:
        db = chromadb.PersistentClient(path=CHROMADB_PATH)
    else:
        db = chromadb.HttpClient(host=CHROMADB_HOST, port=CHROMADB_PORT)

    chroma_collection = db.get_or_create_collection(collection_name)
    vector_store = ChromaVectorStore(chroma_collection=chroma_collection)

    transformations = [
        YAMLMetadataExtractor(),
        BibliographyQuoter(bib_file_path=bibliography),
        MarkdownTextSplitter(),
        HeadingExtractor(),
        SentenceSplitter(chunk_size=1024, chunk_overlap=200),
        OpenAIEmbedding(embed_batch_size=10, model=TEXT_EMBEDDING_MODEL),
    ]

    if not os.path.exists(DOCSTORES_PATH):
        new_storage_context = StorageContext.from_defaults(
            docstore=SimpleDocumentStore())
        new_storage_context.persist(persist_dir=DOCSTORES_PATH)
        docstore = new_storage_context.docstore
    else:
        docstore = SimpleDocumentStore.from_persist_dir(persist_dir=DOCSTORES_PATH)

    pipeline = IngestionPipeline(transformations=transformations,
                                 vector_store=vector_store,
                                 docstore=docstore)

    print(f"Ingesting from {source}")

    the_documents = SimpleDirectoryReader(input_dir=source,
                                          required_exts=EXTENSIONS,
                                          filename_as_id=True,
                                          # num_files_limit=1024,
                                          ).load_data()

    print(f"This is the first document: {the_documents[0]}")

    
    nodes = pipeline.run(documents=the_documents, num_workers=num_workers)
    docstore.persist(persist_path=f"{DOCSTORES_PATH}/docstore.json")

    print(f"Ingested {len(nodes)} Nodes")

if __name__ == '__main__':
    ingest()

