# Load necessary packages
library(tidyverse)
library(rvest)
library(jsonlite)

# Get list of html files in site folder
ids <- list.files("site", pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

# Create data frame of pages and their metadata for each html file
titles <- lapply(1:length(ids), function(i) {
  # Update progress
  cat(paste0("Building index...", round(i / length(ids) * 100, 2), "%\r"))
  
  # Create relative link to page
  id <- str_remove(ids[i], pattern = "site")
  
  # Extract page title
  title <- read_html(ids[i]) %>%
    html_node("title") %>%
    html_text()
  
  # Remove diacritics to assist in search
  title_ascii <- stringi::stri_trans_general(title, "Latin-ASCII")
  
  # Extract type of page from file path (document, genre, etc.)
  type <- str_extract(ids[i], "/[a-z]*/") %>%
    str_remove_all("[^a-z]")
  
  # If page has no type, assign it type "page"
  type <- ifelse(is.na(type), "page", type)
  
  # Collect text of page
  text <- read_html(ids[i]) %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = "; ")
  
  # Collect tables from page
  tables <- read_html(ids[i]) %>%
    html_nodes("td") %>%
    html_text() %>%
    paste(collapse = "; ")
  
  # Collect bulleted list items from page
  lists <- read_html(ids[i]) %>%
    html_nodes("li") %>%
    html_text() %>%
    paste(collapse = "; ")
  
  # Combine all text together (if searched, can slow down pages)
  all_text <- paste(text, tables, lists, sep = "; ") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_remove_all("\\n") %>%
    str_replace_all("\\s{2,}", replacement = " ")
  
  # Create one-row data frame for page
  tmp <- tibble(
    id = id,
    type = type,
    title = title,
    title_ascii = title_ascii,
    text = all_text
  )
  
  return(tmp)
}) %>%
  # Combine all data frames into single frame
  bind_rows()

# Convert data frame to json, append JS variable assignment to front
index <- paste0("var data = ", as.character(toJSON(titles, pretty = T)))

# Write file as index.js, loaded by each page
write_file(index, "site/index.js")

