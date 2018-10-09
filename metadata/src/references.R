#' ---
#' title: Dataset References
#' author: Garrick Aden-Buie
#' date: '`r strftime(Sys.time(), "on %F at %T")`'
#' output: github_document
#' ---

#+ setup, include = FALSE
library(tidyverse)
library(yaml)
library(RefManageR)
library(rcrossref)
knitr::opts_chunk$set(cache = TRUE)
options(knitr.kable.NA = '')

metadata <- read_yaml(here::here("metadata/data.yaml"))

pubs <- metadata %>%
  map(~ .[c("label", "year", "pubmed_id", "doi", "citation")]) %>%
  transpose() %>%
  modify_depth(2, ~ if (is.null(.)) NA_character_ else .) %>%
  map(~ map(., as.character)) %>%
  as_tibble() %>%
  unnest(label, year, doi, citation, .drop = FALSE) %>%
  select(label, year, doi, citation, pubmed_id)


get_bibtex <- function(doi, pubmed_id) {
  if ((is.null(doi) || is.na(doi)) &&
      (is.null(pubmed_id) || is.na(pubmed_id))) {
    return(NA_character_)
  }
  if (!is.na(doi)) {
    rcrossref::cr_cn(doi, format = "bibtex", raw = TRUE)
  } else {
    map_chr(
      pubmed_id,
      ~ RefManageR::ReadPubMed(.) %>%
        RefManageR::toBiblatex() %>%
        paste(collapse = "\n")
    ) %>%
      paste(collapse = "\n")
  }
}

#+ get-bibtex, include=FALSE
pubs <- pubs %>%
  mutate(
    bib = map2_chr(doi, pubmed_id, get_bibtex)
  ) %>%
  mutate(pubmed_id = map_chr(pubmed_id, ~paste(., collapse = ", "))) %>%
  write_csv(here::here("metadata", "references.csv"))

#+ write-bibtex, include=FALSE
pubs %>%
  filter(!is.na(bib)) %>%
  pull(bib) %>%
  write_lines(here::here("metadata", "references.bib"))

#+ pubs-table, echo=FALSE
pubmed_link <- function(x) {
  if (is.na(x) | x == "" | x == "NA") return("")
  x <- str_split(x, ", ")[[1]]
  x <- glue::glue("[{x}](https://www.ncbi.nlm.nih.gov/pubmed/{x})")
  paste(x, collapse = ", ")
}

doi_link <- function(doi) {
  if (is.na(doi) | doi == "") return("")
  glue::glue("[{doi}](https://doi.org/{doi})")
}

pubs %>%
  mutate(
    citekey = str_extract_all(bib, "@\\w+\\{[^, ]+"),
    citekey = map(citekey, ~ str_remove_all(., "@\\w+\\{")),
    citekey = map_chr(citekey, ~ paste0(., collapse = ", ")),
    doi = map_chr(doi, doi_link),
    pubmed_id = map_chr(pubmed_id, pubmed_link)
  ) %>%
  select(label, doi, pubmed_id, citekey) %>%
  mutate_all(~ ifelse(is.na(.), "", .)) %>%
  mutate_all(~ ifelse(. == "NA", "", .)) %>%
  rename(Label = label,
         DOI = doi,
         PubMed = pubmed_id,
         `BibTeX Citation Key` = citekey) %>%
  knitr::kable()
