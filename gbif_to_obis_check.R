# gbif_to_obis_check.R
# co-written with chatGPT

library(httr)
library(jsonlite)
library(stringr)
library(robis)
library(dplyr)
library(stringdist)

# Get all GitHub issues with pagination
get_all_issues <- function(owner, repo, token = NULL) {
  issues <- list()
  page <- 1
  repeat {
    url <- sprintf("https://api.github.com/repos/%s/%s/issues?state=open&per_page=100&page=%d", owner, repo, page)
    headers <- if (!is.null(token)) add_headers(Authorization = paste("token", token)) else NULL
    res <- GET(url, headers)
    stop_for_status(res)
    page_issues <- content(res, as = "parsed", simplifyDataFrame = TRUE)
    if (length(page_issues) == 0) break
    issues <- append(issues, page_issues)
    page <- page + 1
  }
  issues
}

# Extract GBIF dataset keys from issue text
extract_gbif_keys <- function(text) {
  pattern <- "https?://www.gbif.org/dataset/([0-9a-fA-F-]+)"
  matches <- str_match_all(text, pattern)
  unique(unlist(lapply(matches, function(m) m[,2])))
}

# Get dataset titles from GBIF
get_gbif_titles <- function(keys) {
  titles <- sapply(keys, function(k) {
    url <- paste0("https://api.gbif.org/v1/dataset/", k)
    res <- try(GET(url), silent = TRUE)
    if (inherits(res, "try-error") || status_code(res) != 200) return(NA)
    content(res)$title
  }, USE.NAMES = FALSE)
  na.omit(titles)
}

# Match titles to OBIS dataset titles with exact and fuzzy matching
match_titles <- function(gbif_titles, obis_titles, max_dist = 5) {
  result <- data.frame(title = gbif_titles, match_type = "no match", stringsAsFactors = FALSE)
  for (i in seq_along(gbif_titles)) {
    t <- gbif_titles[i]
    if (t %in% obis_titles) {
      result$match_type[i] <- "exact"
    } else {
      dists <- stringdist(t, obis_titles, method = "lv")
      min_dist <- min(dists)
      if (min_dist <= max_dist) {
        closest_match <- obis_titles[which.min(dists)]
        result$match_type[i] <- paste("fuzzy:", closest_match)
      }
    }
  }
  result
}

# Main workflow
owner <- "your-org"
repo <- "your-repo"
token <- Sys.getenv("GITHUB_PAT")

issues <- get_all_issues(owner, repo, token)
issue_bodies <- sapply(issues, function(x) x$body)
gbif_keys <- unique(extract_gbif_keys(issue_bodies))
gbif_titles <- get_gbif_titles(gbif_keys)
obis_titles <- obis_datasets()$title
result <- match_titles(gbif_titles, obis_titles)

# Save to markdown
md_output <- paste0("## GBIF Datasets and OBIS Match Summary\n\n", 
  paste0("- ", result$title, " — ", result$match_type, collapse = "\n"))

writeLines(md_output, "gbif_obis_summary.md")
