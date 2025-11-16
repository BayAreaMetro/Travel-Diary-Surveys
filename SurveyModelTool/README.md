# Modeling_Warehouse

## Setup Instructions

### 0. Clone the GitHub Repository

First, clone the repository to your local machine:

```sh
git clone https://github.com/BayAreaMetro/travel-diary-surveys.git
cd Travel-Diary-Surveys/SurveyModelTool
```

### 1. Create and Activate Conda Environment

Create the environment using the provided `pipeline_env.yml` file:

```sh
conda env create -f pipeline_env.yml
conda activate pipeline
```

### 2. Install the Pipeline Tool

Navigate to the `SurveyTools/Tools` directory and install the pipeline tool in editable mode:

```sh
pip install -e .
```

### 3. Example Usage

see the Examples/Example.ipynb notebook for a simple example of using the survey reader and plotter.

For configuration for the BATS23 data or the configurations for the trip linkage code, please reach out to: hannah.carson@vta.org

