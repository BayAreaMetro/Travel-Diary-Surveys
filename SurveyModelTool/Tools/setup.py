from setuptools import setup, find_packages

setup(
    name='survey_model_tool',
    version='0.1.0',
    author='Your Name',
    author_email='hannah.carson@vta.org',
    description='A tool to read and summarize survey and model data, format for estimation, and create visualizations',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    url='https://github.com/vta/Modeling_Warehouse/tree/main/BATS%20Analysis/Pipeline/pipeline',
    packages=find_packages(),
    install_requires=[
        'pandas',
        'openmatrix',
        'scipy',
        'statsmodels',
        'scikit-learn',
        'matplotlib',
        'seaborn',
        'folium',
        'geopandas',
        'larch',
        'pyyaml',
        'numpy',
        'matplotlib'  # Add any other dependencies your project needs
    ],
    classifiers=[
        'Programming Language :: Python :: 3',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
    ],
    python_requires='>=3.6',
)