This folder contains metadata related to the data sets and the data itself.

- [`references.bib`](references.bib) is created by [src/references.R](src/references.R) by querying DOI and PubMed IDs within the dataset metadata (see below). A table linking the datasets to their associated citation keys and identifiers is available in [src/references.md](src/references.md)

- [`data.yaml`](data.yaml) and [`data.csv`](data.csv) contain metadata for each data set pulled from the corresponding GitHub issue (see [references.md](src/references.md)) for that data set. The corresponding source file is [src/pull_github_data_issues.R](src/pull_github_data_issues.R)
