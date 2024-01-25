# Welcome to the top runners dashboard!

## LINK DO NAGRANIA

https://youtu.be/2V1Vvq2AhYo?si=MXx8FXPqyTD4DB0y

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

- Python 3.11
- Poetry

### Installing

1. **Clone the repository**

```bash
git clone https://github.com/IgorKolodziej/project_me.git
cd your-repo-name
```

2. **Set up Python environment**

You need Python 3.11 installed on your system. You can download it from the official website:

https://www.python.org/downloads/

3. **Install Poetry**

Poetry is a tool for dependency management and packaging in Python. You can install it by running:

```bash
pip install poetry
```

4. **Install dependencies**

With Poetry installed, you can create a virtual environment with all necessary dependencies by running:

```bash
poetry install
```
##### Running the app
In order to start the app

```bash
poetry run python3 ./project_me/index.py
```

##### Development
5. **Set up pre-commit**
Set up pre-commit hooks by running:
```bash
poetry run pre-commit install
```
This will install pre-commit hooks for you. Now, every time you commit a change, pre-commit will run the hooks on all files that you have changed.

You can run all pre-commit hooks on all files by running:

```bash
poetry run pre-commit run --all-files
```
## Built With

* Dash - The web framework used
* Plotly
* Pandas
* Poetry - Dependency Management
* Pre-commit - Git hooks manager

## Authors

* Igor Kołodziej
* Mateusz Iwaniuk
* Nazarii Bihniak

