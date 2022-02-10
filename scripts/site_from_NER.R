# Meant to be run after Python scripts for Named Entity Recognition
# Create necessary directories
necessary_dirs <- c(
  "site", 
  "site/document", 
  "site/genre",
  "site/performance",
  "site/person",
  "site/venue",
  "site/work",
  "data/processed/db_tables"
)

for(dir in necessary_dirs) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Parse Data
source("scripts/R/01_parse_data.R")
rm(list = ls())

# Create Database
remote_db <- FALSE
source("scripts/R/02_create_db.R")
rm(list = ls())

# Create Website (this one takes a while, maybe 5 hours)
source("scripts/R/03_create_site.R")
rm(list = ls())

# Create search index so that the search boxes on each page work
source("scripts/R/04_build_index.R")
rm(list = ls())