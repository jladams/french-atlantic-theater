# Load Necessary Packages
library(tidyverse)
library(rmarkdown)


# Read in Data
documents <- read_csv("./data/processed/db_tables/document.csv")
venues <- read_csv("./data/processed/db_tables/venue.csv")
performances <- read_csv("./data/processed/db_tables/performance.csv", col_types = "cncccccccD")
works <- read_csv("./data/processed/db_tables/work.csv")
genres <- read_csv("./data/processed/db_tables/genre.csv")
persons <- read_csv("./data/processed/db_tables/person.csv")

# Add prettier title to performances using date and venue names
perf_venue <- performances %>%
  left_join(venues) %>%
  mutate(performance = paste0(format(date, "%A, %B %d, %Y"), " at ", str_to_title(venue)))

# Write out the data we just created
write_csv(perf_venue, "./data/processed/perf_venue.csv")

# Creating pages ----------
## Default / Home ----------
rmarkdown::render(
  input = "./templates/default.Rmd",
  output_dir = "./site",
  output_file = "default.html",
  quiet = TRUE
)

# Fix image links
default <- read_file("./site/default.html") %>%
  str_replace_all("\\.\\.\\/site\\/images", "\\/images")

# Place one copy as "default" for Reclaim hosting purposes
# Place one copy as "index" because everyone else expects that
write_file(default, "./site/default.html")
write_file(default, "./site/index.html")

## About --------
rmarkdown::render(
  input = "./templates/about.Rmd",
  output_dir = "./site",
  output_file = "about.html",
  quiet = TRUE
)

# Fix image links
about <- read_file("./site/about.html") %>%
  str_replace_all("\\.\\.\\/site\\/images", "\\/images")

write_file(about, "./site/about.html")

## Category Pages -----------
### Create metadata -----------
category_df <- tibble(
  category = c("Documents", "Genres", "Performances", "Persons", "Venues", "Works"),
  cat_folder = c("document", "genre", "performance", "person", "venue", "work"),
  cat_file = c("../data/processed/db_tables/document.csv", "../data/processed/db_tables/genre.csv", "../data/processed/perf_venue.csv", "../data/processed/db_tables/person.csv", "../data/processed/db_tables/venue.csv", "../data/processed/db_tables/work.csv"),
  display_var = c("doc_title", "genre", "performance", "person", "venue", "work"),
  link_var = c("doc_id", "genre_id", "performance_id", "person_id", "venue_id", "work_id")
)

### Render Pages ----------
lapply(1:nrow(category_df), function(i) {
  rmarkdown::render(
    input = "./templates/category.Rmd",
    output_dir = paste0("./site/", category_df$cat_folder[i]),
    output_file = "index.html",
    params = list(
      category = category_df$category[i],
      cat_folder = category_df$cat_folder[i],
      cat_file = category_df$cat_file[i],
      display_var = category_df$display_var[i],
      link_var = category_df$link_var[i]
    )
  )
})


## Documents -------
lapply(1:nrow(documents), function(i) {
  print(paste0("Document ", i))
  
  document <- documents$title[i]
  document_subtitle <- format(documents$date[i], "%A, %B %d, %Y")
  document_id <- documents$doc_id[i]
  
  rmarkdown::render(
    input = "./templates/document.Rmd",
    output_dir = "./site/document",
    output_file = paste0(document_id, ".html"),
    params = list(
      document = document,
      document_id = document_id,
      document_subtitle = document_subtitle
    ), 
    quiet = TRUE
  )  
}) 

## Venues -------
lapply(1:nrow(venues), function(i) {
  print(paste0("Venue ", i))
  venue <- str_to_title(venues$venue[i])
  venue_id <- venues$venue_id[i]
  
  rmarkdown::render(
    input = "./templates/venue.Rmd",
    output_dir = "./site/venue",
    output_file = paste0(venue_id, ".html"),
    params = list(
      venue = venue,
      venue_id = venue_id
    ), 
    quiet = TRUE
  )
})


## Performances -------
lapply(1:nrow(perf_venue), function(i) {
  print(paste0("Performance ", i))
  performance <- perf_venue$performance[i]
  performance_id <- perf_venue$performance_id[i]
  
  rmarkdown::render(
    input = "./templates/performance.Rmd",
    output_dir = "./site/performance",
    output_file = paste0(performance_id, ".html"),
    params = list(
      performance = performance,
      performance_id = performance_id
    ), 
    quiet = TRUE
  )  
}) 


## Works -------
lapply(1:nrow(works), function(i) {
  print(paste0("Work ", i))
  work <- works$work[i]
  work_id <- works$work_id[i]
  
  rmarkdown::render(
    input = "./templates/work.Rmd",
    output_dir = "./site/work",
    output_file = paste0(work_id, ".html"),
    params = list(
      work = work,
      work_id = work_id
    ), 
    quiet = TRUE
  )  
}) 

## Genres -------
lapply(1:nrow(genres), function(i) {
  print(paste0("Genre ", i))
  genre <- genres$genre[i]
  genre_id <- genres$genre_id[i]
  
  rmarkdown::render(
    input = "./templates/genre.Rmd",
    output_dir = "./site/genre",
    output_file = paste0(genre_id, ".html"),
    params = list(
      genre = genre,
      genre_id = genre_id
    ), 
    quiet = TRUE
  )
})

## Persons -------
lapply(1:nrow(persons), function(i) {
  print(paste0("Person ", i))
  person <- persons$person[i]
  person_id <- persons$person_id[i]
  
  rmarkdown::render(
    input = "./templates/person.Rmd",
    output_dir = "./site/person",
    output_file = paste0(person_id, ".html"),
    params = list(
      person = person,
      person_id = person_id
    ), 
    quiet = TRUE
  )
})
