library(yaml)
library(gh)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

get_issues_with_label <- function(label) {
  gh("GET /repos/gerkelab/curatedProstateData/issues", labels = label) %>%
    roomba::roomba(c("id", "url", "body", "title"))
}

extract_body_as_yaml <- function(b) {
  if (!str_detect(b, "yaml")) return("")
  b %>%
    str_replace_all("\r\n", "\n") %>%
    str_replace_all("```(yaml)?", "") %>%
    str_replace_all("\n ?\n", "\n") %>%
    str_replace_all("\n ?\n", "\n") %>%
    str_replace("Source:", "source:") %>%
    paste("- ", .) %>%
    str_replace_all("\n", "\n   ") %>%
    str_trim("right")
}

#' WARNING: These functions curently expect the data-related issues to:
#'
#' 1. Be named with "Data -"
#' 2. Contain a yaml block
#' 3. Source may be outside or inside the yaml block
#' 4. No other information in the issue body

issues <- c("GEO", "cBioportal", "Other Source") %>%
  purrr::set_names() %>%
  purrr::map_dfr(get_issues_with_label, .id = "source")

issues %>%
  filter(str_detect(title, "Data - ")) %>%
  mutate(yaml = map_chr(body, extract_body_as_yaml)) %>%
  # Add github link to yaml
  mutate(
    url = str_remove(url, "api\\."),
    url = str_remove(url, "repos/"),
    yaml = str_replace(yaml, "(label: .+?\n)", paste0("\\1   github_issue: \"", url, "\"\n"))
  ) %>%
  write_csv(here::here("metadata/data.csv")) %>%
  pull(yaml) %>%
  yaml.load() %>%
  write_yaml(file = here::here("metadata/data.yaml"))
