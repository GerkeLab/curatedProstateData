library(yaml)
library(gh)
library(stringr)
library(dplyr)

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
  pull(body) %>%
  purrr::map_chr(extract_body_as_yaml) %>%
  yaml.load() %>%
  write_yaml(file = here::here("metadata/data.yaml"))
