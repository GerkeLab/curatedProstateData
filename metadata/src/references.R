#' ---
#' title: Dataset References
#' author: "<blockquote><strong>Updated:</strong>"
#' date: '`r strftime(Sys.time(), "%F at %T</blockquote>")`'
#' output: github_document
#' ---

#+ setup, include = FALSE
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)
library(yaml)
library(RefManageR)
library(rcrossref)
knitr::opts_chunk$set(cache = TRUE, include = FALSE, echo = FALSE)
options(knitr.kable.NA = '')

#+ load-metadata
metadata <- read_yaml(here::here("metadata/data.yaml"))

#+ functions
pull_pub_fields <- function(y) {
  if (length(y) == 1) y <- y[[1]]
  `%||%` <- function(x, y) if (is.null(x)) y else x
  data_frame(
    label = y$label,
    github_issue = y$github_issue,
    doi = y$doi %||% NA_character_,
    pubmed_id = y$pubmed_id %||% NA_character_
  )
}


get_bibtex <- function(source = c("doi", "pubmed_id"), ref) {
  switch(
    match.arg(source),
    "doi" = rcrossref::cr_cn(ref, format = "bibtex", raw = TRUE),
    "pubmed_id" = RefManageR::ReadPubMed(ref) %>%
      RefManageR::toBiblatex() %>%
      paste(collapse = "\n")
  )
}

extract_citekey <- function(bibtex) {
  str_extract_all(bibtex, "@\\w+\\{[^, ]+") %>%
    map(~ str_remove_all(., "@\\w+\\{")) %>%
    map_chr(~ paste0(., collapse = ", "))
}

#+ get-bibtex
data_list <- map_dfr(metadata, pull_pub_fields)

pubs <-
  data_list %>%
  gather(source, ref, doi:pubmed_id) %>%
  distinct() %>%
  arrange(github_issue, source) %>%
  filter(!is.na(ref)) %>%
  mutate(
    bib = map2_chr(source, ref, get_bibtex)
  ) %>%
  write_csv(here::here("metadata", "references.csv"))

#+ write-bibtex
pubs %>%
  filter(!is.na(bib)) %>%
  select(bib) %>%
  mutate(citekey = extract_citekey(bib)) %>%
  filter(!duplicated(citekey)) %>%
  pull(bib) %>%
  write_lines(here::here("metadata", "references.bib"))

#+ pubs-table, include=TRUE
pubmed_link <- function(x) {
  if (length(x) > 1) return(map_chr(x, pubmed_link))
  if (is.na(x) | x == "" | x == "NA") return("")
  glue::glue("[{x}](https://www.ncbi.nlm.nih.gov/pubmed/{x})")
}

doi_link <- function(doi) {
  if (length(doi) > 1) return(map_chr(doi, doi_link))
  if (is.na(doi) | doi == "") return("")
  glue::glue("[{doi}](https://doi.org/{doi})")
}

collapse <- function(x, with = ", ") {
  x <- unique(x)
  paste(x[!is.na(x)], collapse = with)
}

pubs %>%
  mutate(
    citekey = extract_citekey(bib),
    ref = ifelse(source == "pubmed_id", pubmed_link(ref), doi_link(ref))
  ) %>%
  group_by(label, github_issue, source) %>%
  summarize(
    ref = collapse(ref),
    citekey = collapse(citekey)
  ) %>%
  spread(source, ref) %>%
  group_by(label, github_issue) %>%
  summarize(
    doi = collapse(doi),
    pubmed_id = collapse(pubmed_id),
    citekey = collapse(citekey)
  ) %>%
  left_join(data_list[, c("label", "github_issue")], ., by = c("label", "github_issue")) %>%
  mutate(label = glue::glue("[{label}]({github_issue})")) %>%
  arrange(desc(doi), desc(pubmed_id), github_issue) %>%
  select(-starts_with("github_issue")) %>%
  distinct() %>%
  mutate_all(~ ifelse(is.na(.), "", .)) %>%
  mutate_all(~ ifelse(. == "NA", "", .)) %>%
  rename(Label = label,
         DOI = doi,
         PubMed = pubmed_id,
         `BibTeX Citation Key` = citekey) %>%
  knitr::kable()
