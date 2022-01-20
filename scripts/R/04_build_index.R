library(tidyverse)
library(rvest)
library(jsonlite)

ids <- list.files("site", pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

titles <- lapply(1:length(ids), function(i) {
  cat(paste0("Building index...", round(i / length(ids) * 100, 2), "%\r"))
  
  id <- str_remove(ids[i], pattern = "site")
  
  title <- read_html(ids[i]) %>%
    html_node("title") %>%
    html_text()
  
  title_ascii <- stringi::stri_trans_general(title, "Latin-ASCII")
  
  type <- str_extract(ids[i], "/[a-z]*/") %>%
    str_remove_all("[^a-z]")
  
  type <- ifelse(is.na(type), "page", type)
  
  text <- read_html(ids[i]) %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = "; ")
  
  tables <- read_html(ids[i]) %>%
    html_nodes("td") %>%
    html_text() %>%
    paste(collapse = "; ")
  
  lists <- read_html(ids[i]) %>%
    html_nodes("li") %>%
    html_text() %>%
    paste(collapse = "; ")
  
  all_text <- paste(text, tables, lists, sep = "; ") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_remove_all("\\n") %>%
    str_replace_all("\\s{2,}", replacement = " ")
  
  
  tmp <- tibble(
    id = id,
    type = type,
    title = title,
    title_ascii = title_ascii,
    text = all_text
  )
  
  return(tmp)
}) %>%
  bind_rows()

index <- paste0("var data = ", as.character(toJSON(titles, pretty = T)))

write_file(index, "site/index.js")

