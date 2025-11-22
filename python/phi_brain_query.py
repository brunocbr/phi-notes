#!env python3

import os
import json
import sys
import click
import chromadb
import chromadb.utils.embedding_functions as embedding_functions

# Set up the OpenAI API key and chromadb path
API_KEY = os.getenv("OPENAI_API_KEY")

CHROMADB_PATH = os.getenv("CHROMADB_PATH", False)  # Use HTTP client by default
CHROMADB_HOST = os.getenv("CHROMADB_HOST", "localhost")
CHROMADB_PORT = os.getenv("CHROMADB_HOST", "8000")
EMBEDDING_MODEL="text-embedding-3-small"

# Initialize the embedding function
openai_ef = embedding_functions.OpenAIEmbeddingFunction(
    api_key=API_KEY,
    model_name=EMBEDDING_MODEL
)

@click.command()
@click.argument('collection_name', required=False, type=str)
@click.option('--n_results', default=5, help="Number of results to retrieve.")
@click.option('--embedding', is_flag=True, help="Flag to indicate if embedding vector is provided as input.")
@click.option('--list-collections', is_flag=True, help="List all available collections in the database.")
def query(collection_name, n_results, embedding, list_collections):
    """CLI tool to query a chromadb collection using input from STDIN."""

    # Initialize the chromadb client and get the specified collection
    if CHROMADB_PATH:
        client = chromadb.PersistentClient(path=CHROMADB_PATH)
    else:
        client = chromadb.HttpClient(host=CHROMADB_HOST, port=CHROMADB_PORT)

    # Check if user requested to list collections
    if list_collections:
        # List all available collections
        collections = client.list_collections()
        collection_names = [collection.name for collection in collections]
        print(json.dumps(collection_names, indent=2))
        return

    # If no collection name is provided and list-collections is not used, print usage
    if not collection_name:
        print("Error: Collection name is required unless using --list-collections.")
        sys.exit(1)

    # Read the input from STDIN
    input_data = sys.stdin.read().strip()
    if not input_data:
        print("Error: No input received from STDIN.")
        sys.exit(1)

    collection = client.get_or_create_collection(collection_name, embedding_function=openai_ef)

    # Determine whether to use embedding vector or query text
    if embedding:
        try:
            # Convert the input data to a list of floats for the embedding vector
            query_embedding = json.loads(input_data)
            if not isinstance(query_embedding, list) or not all(isinstance(x, (float, int)) for x in query_embedding):
                raise ValueError("Embedding vector must be a list of numbers.")

            # Query using the embedding vector
            response = collection.query(
                query_embeddings=[query_embedding],
                n_results=n_results,
            )
        except json.JSONDecodeError:
            print("Error: Embedding vector input must be valid JSON.")
            sys.exit(1)
    else:
        # Use the input as query text and generate the embedding
        query_text = input_data
        response = collection.query(
            query_texts=[query_text],
            n_results=n_results,
        )

    # Prepare the results as a list of dictionaries
    results = []
    for i in range(len(response["ids"][0])):
        # Parse _node_content JSON from metadata
        node_content = json.loads(response["metadatas"][0][i]["_node_content"])

        # Build the result object
        result = {
            "id": response["ids"][0][i],
            "document": response["documents"][0][i],
            "metadata": node_content["metadata"],
            "distance": response["distances"][0][i]
        }
        results.append(result)

    # Output the results as formatted JSON
    print(json.dumps(results, indent=2))

if __name__ == "__main__":
    query()
