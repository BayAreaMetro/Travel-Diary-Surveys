# Purpose Investigation Setup and Linker Explanation

## Environment Setup

1. **Install UV**
    ```bash
    # On macOS/Linux
    curl -LsSf https://astral.sh/uv/install.sh | sh
    
    # On Windows
    powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
    ```

2. **Create virtual environment**
    ```bash
    uv sync
    ```

3. **Activate virtual environment**
    Point VSCode to use the Python interpreter located at:
    ```
    .venv/bin/python  # macOS/Linux
    .venv\Scripts\python.exe  # Windows
    ```

    Open a python file, click the interpreter version in the bottom-right corner, and select the correct interpreter path.

## How the Linker Works

The linker in this context connects survey data across different tables or datasets. Here's the general process:

1. **Identify Key Fields**: Common identifiers (e.g., `respondent_id`, `trip_id`) that exist across datasets

2. **Data Matching**: Links records using these keys to combine information from multiple sources

3. **Relationship Mapping**: Establishes connections between:
    - Survey responses
    - Trip purposes
    - Demographics
    - Travel patterns

4. **Output**: Creates a unified dataset where related information is accessible in a single structure

**Example**: A trip record might be linked to respondent demographics using `respondent_id`, allowing analysis of how different demographic groups report different trip purposes.