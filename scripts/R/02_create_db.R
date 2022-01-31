# Load necessary packages
library(tidyverse)
library(lubridate)
library(DBI)

# Read in data and document metadata
df <- read_csv("./data/processed/all_docs.csv")
doc_meta <- read_csv("./data/processed/doc_meta.csv")

# Create time metadata for performances
time_meta <- read_csv("./data/original/TIME-tag.csv") %>%
  select(time = Word,
         first_time = First,
         second_time = Second,
         third_time = Third,
         continuous = Continuous) %>%
  mutate(
    across(
      first_time:third_time,
      ~ ifelse(!is.na(.x),
               str_replace_all(
                 paste0(
                   str_replace_all(
                     str_remove_all(.x, "[^0-9\\s]"), 
                     pattern = " ", 
                     replacement = ":"),
                   ":00"), 
                 pattern = ":0:",
                 replacement = ":00:"
               ),
               NA
      )
    )
  )

# Setting up document data
documents <- df %>%
  distinct(Date, doc_id) %>% # Keep unique date and doc id combos
  left_join(filter(df, Tag == "SECTION")) %>% # Join on sections tag as metadata
  select(1:4) %>% # Keep only relevant columns
  pivot_wider(names_from = "Tag", values_from = "Word") %>%
  select(-`NA`) %>%
  unnest(SECTION, keep_empty = TRUE) %>%
  # Deal with multiple "section" designations by collapsing into one semicolon separated row
  group_by(Date, doc_id) %>%
  mutate(SECTION = paste(SECTION, collapse = ";")) %>%
  mutate(SECTION = ifelse(SECTION == "NA", NA, SECTION)) %>%
  distinct() %>%
  # Add on document metadata
  full_join(doc_meta, by = "doc_id") %>%
  # Lower-case all column names
  rename_with(str_to_lower) %>%
  # Add new document title
  mutate(doc_title = paste0(title, " - ", format(date, "%A, %B %d, %Y")))

# Setting up venue data with locations (to be added to venue table)
venue_locations <- df %>%
  select(doc_id, venue_id, Tag, Word) %>%
  # keep only VENUE and LOCATION tags
  filter(Tag %in% c("VENUE", "LOCATION")) %>%
  pivot_wider(names_from = "Tag", values_from = "Word") %>%
  unnest(VENUE, keep_empty = TRUE) %>%
  unnest(LOCATION, keep_empty = TRUE) %>%
  # Collapse venues into single rows with semicolon separated locations
  group_by(venue = VENUE) %>%
  summarize(location = paste(unique(LOCATION), collapse = ";")) %>%
  # Get rid of junk/NA data
  filter(!is.na(venue)) %>%
  mutate(location = str_remove_all(location, ";NA|NA;|NA")) %>%
  mutate(location = ifelse(nchar(location) == 0, NA, location))

# Filter down to VENUE tags, add in venue ids and join locations
venues <- df %>%
  filter(Tag %in% c("VENUE")) %>%
  distinct(Word) %>%
  rename(venue = Word) %>%
  mutate(id = row_number()) %>%
  left_join(venue_locations, by = "venue") %>%
  select(venue_id = id, 
         venue, 
         location)

# Create linkage between document and objects
document_has_venue <- df %>%
  filter(Tag == "VENUE") %>%
  select(venue = Word, doc_id) %>%
  left_join(venues, by = "venue") %>%
  select(doc_id, 
         venue_id)

# Create linkage between document and performances
document_has_performance <- df %>%
  mutate(Tag = ifelse(Tag == "Day", "DAY", Tag)) %>%
  # Creates a unique performance id
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(venue_id != 0 & perf_id != 0) %>% 
  distinct(doc_id, performance_id)

# Link performances with dates
# (some performances were missing other data, this makes sure they get included)
performance_dates <- df %>%
  mutate(Tag = ifelse(Tag == "Day", "DAY", Tag)) %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(venue_id != 0) %>%
  filter(perf_id != 0) %>%
  distinct(performance_id, date = Date)

# Create performance table with date/day/time/price/etc
performances <- df %>%
  mutate(Tag = ifelse(Tag == "Day", "DAY", Tag)) %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(venue_id != 0) %>%
  filter(perf_id != 0) %>%
  select(performance_id, date = Date, venue_id, Word, Tag) %>%
  filter(Tag %in% c("DAY", "TIME", "PRICE")) %>%
  pivot_wider(names_from = Tag,
              values_from = Word) %>%
  unnest(DAY, keep_empty = TRUE) %>%
  unnest(TIME, keep_empty = TRUE) %>%
  unnest(PRICE, keep_empty = TRUE) %>%
  group_by(performance_id, venue_id, date) %>%
  summarize(DAY = paste(DAY, collapse = ";"),
            TIME = paste0(TIME, collapse = ";"),
            PRICE = paste(PRICE, collapse = ";")
  ) %>%
  rename_with(str_to_lower) %>%
  mutate(
    across(
      c(day, time, price),
      ~ ifelse(str_detect(.x, pattern = ";NA|NA;|NA"), NA, .x)
    )
  ) %>%
  left_join(time_meta) %>%
  # Reinclude performances that were missing due to lack of other metadata
  full_join(performance_dates) %>%
  mutate(first_time = ifelse(!is.na(first_time), as_datetime(paste0(date, " ", first_time)), NA),
         second_time = ifelse(!is.na(second_time), as_datetime(paste0(date, " ", second_time)), NA),
         third_time = ifelse(!is.na(third_time), as_datetime(paste0(date, " ", third_time)), NA))

# Table of unique works
works <- df %>%
  filter(Tag %in% c("WORK")) %>%
  distinct(work = Word) %>%
  mutate(work_id = row_number())

# Table of unique genres
genres <- df %>%
  filter(Tag %in% c("GENRE")) %>%
  distinct(genre = Word) %>%
  mutate(genre_id = row_number())

# Table to connect works and genres
work_has_genre <- df %>%
  filter(Tag %in% c("GENRE")) %>%
  select(genre = Word,
         doc_id,
         venue_id,
         perf_id,
         work_id) %>%
  # This joins every "GENRE" tag to every "WORK" tag based on matching document, venue, performance, and work id 
  right_join(select(filter(df, Tag == "WORK"), work = Word, doc_id, venue_id, perf_id, work_id)) %>%
  # Keep only unique combinations of work and genre as words
  distinct(work,
         genre) %>%
  # Get rid of blanks
  drop_na() %>%
  # Bring in proper ids from appropriate tables
  left_join(works) %>%
  left_join(genres) %>%
  distinct(work_id, genre_id) %>%
  arrange(work_id)

# Create combination of performances with works
performance_has_work <- df %>%
  # Recreate performance ID
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  # Keep only "WORK" tags
  filter(Tag == "WORK") %>%
  distinct(performance_id,
           work = Word,
           order_id) %>%
  # Bring in work ids from works table
  left_join(works, by = "work") %>%
  select(performance_id,
         work_id,
         order = order_id)

# Tags that might indicate people
person_work_tags <- c(
  "COMPOSER", 
  "PLAYWRIGHT", 
  "LIBRETTIST",
  "CHOREOGRAPHER",
  "SCENOGRAPHER",
  "TRANSLATOR"
)

person_performance_tags <- c(
  "ACTOR", 
  "SINGER",
  "PERFORMER",
  "MUSICIAN",
  "IMPRESARIO",
  "TROUPE",
  "DANSER",
  "ANIMAL",
  "TEACHER",
  "PYROTECHNIC ENGINEER"
)

# Create persons Table
persons <- df %>%
  filter(Tag %in% c(person_performance_tags, person_work_tags)) %>%
  distinct(person = Word) %>%
  mutate(person_id = row_number())

# Determine somebody's character/role if they have a person performance tag
actor_roles <- df %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0")),
         work_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag %in% c(person_performance_tags, "ROLE")) %>%
  distinct(performance_id,
           work_meta_id,
           person = Word,
           role = Tag)

roles <- character()

# If a tag is followed by "ROLE", assign that character to the place in "roles" vector which precedes it
for(i in 1:nrow(actor_roles)) {
  role <- NA
  if(i < nrow(actor_roles)) {
    if(actor_roles$role[i + 1] == "ROLE" & actor_roles$performance_id[i + 1] == actor_roles$performance_id[i]) {
      role <- actor_roles$person[i + 1]
    }    
  }
  
  roles[i] <- role
}

# Attach roles vector as "char_role" to identify characters played by each person
actor_roles$char_role <- roles

# Remove ROLE tag and collect person ids by joining to persons table
actor_roles <- actor_roles %>%
  filter(role != "ROLE") %>%
  left_join(persons, by = "person") %>%
  select(performance_id,
         person_id,
         work_meta_id,
         char_role)

# Create crosswalk to allow connections between persons and performances
tmp_works_crosswalk <- df %>%
  mutate(work_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag == "WORK") %>%
  select(work = Word,
         work_meta_id) %>%
  left_join(works) %>%
  distinct(work_meta_id,
           work_id)

# Link person and performance using performance_id, person_id, and work_meta_id
person_has_performance <- df %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0")),
         work_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag %in% c(person_performance_tags, person_work_tags)) %>%
  distinct(performance_id,
           work_meta_id,
           person = Word,
           role = Tag) %>%
  left_join(persons, by = "person") %>%
  select(performance_id,
         person_id,
         work_meta_id,
         role) %>%
  left_join(actor_roles) %>%
  left_join(tmp_works_crosswalk) %>%
  distinct(performance_id, 
           work_id, 
           person_id, 
           role, 
           char_role)

# Create crosswalk between works and persons (largely unnecessary because of the above)
work_has_person <- df %>%
  mutate(work_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag %in% c(person_work_tags, person_performance_tags, "WORK")) %>%
  select(Word, Tag, work_meta_id) %>%
  left_join(works, by = c("Word" = "work")) %>%
  left_join(drop_na(distinct(., work_meta_id, work_id)), by = "work_meta_id") %>%
  select(-work_id.x) %>%
  rename(work_id = work_id.y) %>%
  filter(Tag != "WORK") %>%
  left_join(persons, by = c("Word" = "person")) %>%
  distinct(work_id,
           person_id,
           role = Tag) %>%
  drop_na() %>%
  arrange(work_id, person_id)

# Write all data to local tables
write_csv(documents, "./data/processed/db_tables/document.csv")
write_csv(venues, "./data/processed/db_tables/venue.csv")
write_csv(performances, "./data/processed/db_tables/performance.csv")
write_csv(works, "./data/processed/db_tables/work.csv")
write_csv(genres, "./data/processed/db_tables/genre.csv")
write_csv(persons, "./data/processed/db_tables/person.csv")
write_csv(document_has_venue, "./data/processed/db_tables/document_has_venue.csv")
write_csv(document_has_performance, "./data/processed/db_tables/document_has_performance.csv")
write_csv(performance_has_work, "./data/processed/db_tables/performance_has_work.csv")
write_csv(person_has_performance, "./data/processed/db_tables/person_has_performance.csv")
write_csv(work_has_person, "./data/processed/db_tables/work_has_person.csv")
write_csv(work_has_genre, "./data/processed/db_tables/work_has_genre.csv")

# Connecting to Database --------------
# Write data to remote database if applicable
if(remote_db) {
  con <- dbConnect(RMariaDB::MariaDB(), group="sanders-performance")
  
  dbWriteTable(con, "document", documents, overwrite = TRUE)
  dbWriteTable(con, "venue", venues, overwrite = TRUE)
  dbWriteTable(con, "performance", performances, overwrite = TRUE)
  dbWriteTable(con, "work", works, overwrite = TRUE)
  dbWriteTable(con, "genre", genres, overwrite = TRUE)
  dbWriteTable(con, "person", persons, overwrite = TRUE)
  dbWriteTable(con, "document_has_venue", document_has_venue, overwrite = TRUE)
  dbWriteTable(con, "document_has_performance", document_has_performance, overwrite = TRUE)
  dbWriteTable(con, "performance_has_work", performance_has_work, overwrite = TRUE)
  dbWriteTable(con, "person_has_performance", person_has_performance, overwrite = TRUE)
  dbWriteTable(con, "work_has_person", work_has_person, overwrite = TRUE)
  dbWriteTable(con, "work_has_genre", work_has_genre, overwrite = TRUE)
  
  dbDisconnect(con) 
}


