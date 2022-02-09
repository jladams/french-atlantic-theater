# Setup ------------
# Load necessary packages
library(tidyverse)
library(lubridate)
library(DBI)

# Read in Data -----------
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

# Documents ---------
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

# Venues -----------
## Locations ------------
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

## Venue table ---------
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

# Linking Tables (Round 1) ---------
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

# Performances ------------
## Performance Dates ----------
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

## Performance Table -----------
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
  # Re-include performances that were missing due to lack of other metadata
  full_join(performance_dates) %>%
  mutate(first_time = ifelse(!is.na(first_time), as_datetime(paste0(date, " ", first_time)), NA),
         second_time = ifelse(!is.na(second_time), as_datetime(paste0(date, " ", second_time)), NA),
         third_time = ifelse(!is.na(third_time), as_datetime(paste0(date, " ", third_time)), NA))

# Works -----------

## Standardizing -----------------
w_standard <- read_csv("./data/original/standardizing/WORK-tag.csv", col_types = cols(.default = "c")) %>%
  # Something seems to be wrong with the ISNI column, just converting to NA for now
  mutate(
    ISNI = NA
  ) %>%
  select(
    Word,
    ISNI,
    VIAF,
    OCLC,
    WIKI,
    Google_Books,
    Name
  ) %>%
  # Clear out values that are just empty space from identifiers (convert to NA if only white-space characters are detected)
  mutate(
    across(
      ISNI:Name,
      ~ ifelse(str_detect(.x, "\\S"), .x, NA)
    )
  ) %>%
  distinct()

w_with_id <- w_standard %>%
  # This removes any rows which are entirely NA for identifiers (ISNI:Name)
  filter(
    if_any(ISNI:Name, ~ !is.na(.x))
  ) %>%
  # Each distinct combination of these variables gets assigned as a standardized "work" with a "work_id"
  group_by(ISNI, VIAF, OCLC, WIKI, Google_Books, Name) %>%
  # If there is no name given, just use the first title ("Word") that appears
  mutate(
    work_id = cur_group_id(),
    Name = ifelse(is.na(Name), first(Word), Name)
  )

# Figure out what the highest work_id that has been created so far
w_highest_id <- max(w_with_id$work_id)

# Create a dataset of works without IDs
w_without_id <- w_standard %>%
  filter(
    if_all(ISNI:Name, ~ is.na(.x))
  ) %>%
  distinct() %>%
  # Add a "Name" column for compatibility with the w_with_id data frame
  mutate(Name = Word) %>%
  group_by(Word) %>%
  # Add new id number to the highest id from the other data set to make sure they don't overlap
  mutate(work_id = cur_group_id() + w_highest_id)

### Titles ------------
# Table of unique work titles
titles <- df %>%
  filter(Tag %in% c("WORK")) %>%
  distinct(title = Word) %>%
  mutate(title_id = row_number())

# Combine the w_with_id and without_id tables
work_with_title <- bind_rows(w_with_id, w_without_id) %>%
  rename(
    title = Word,
    work = Name
  ) %>%
  arrange(work_id) %>%
  # Add the standardized works to the "titles" table
  right_join(
    titles, by = "title"
  )

# Fix missing values in work_id
# (This happens if the work is missing from the standardizing files)
wwt_highest <- max(work_with_title$work_id, na.rm = TRUE)

# Add person_ids to rows with none
wwt_nas <- work_with_title %>%
  ungroup() %>%
  filter(is.na(work_id)) %>%
  mutate(work_id = row_number() + wwt_highest)

# Remove rows with NA for work_id, add on the corrected data
work_with_title_fixed <- work_with_title %>%
  ungroup() %>%
  filter(!is.na(work_id)) %>%
  bind_rows(wwt_nas)

### Work Table --------------
# Create works table
works <- work_with_title_fixed %>%
  ungroup() %>%
  select(
    work_id,
    work,
    ISNI, 
    VIAF,
    OCLC,
    WIKI,
    Google_Books
  ) %>%
  distinct()

# Create linkage table between works and titles
work_has_title <- work_with_title_fixed %>%
  ungroup() %>%
  distinct(
    title_id,
    work_id
  )


# Genres ----------
# Table of unique genres
genres <- df %>%
  filter(Tag %in% c("GENRE")) %>%
  distinct(genre = Word) %>%
  mutate(genre_id = row_number())

# Linking Tables (Round 2) ---------------
# Table to connect works and genres
work_has_genre <- df %>%
  filter(Tag %in% c("GENRE")) %>%
  select(genre = Word,
         doc_id,
         venue_id,
         perf_id,
         title_id = work_id) %>%
  # This joins every "GENRE" tag to every "WORK" tag based on matching document, venue, performance, and work id (which becomes title_id)
  right_join(select(filter(df, Tag == "WORK"), title = Word, doc_id, venue_id, perf_id, title_id = work_id)) %>%
  # Keep only unique combinations of work and genre as words
  distinct(title,
           genre) %>%
  # Get rid of blanks
  drop_na() %>%
  # Bring in proper ids from appropriate tables
  left_join(titles) %>%
  left_join(work_has_title) %>%
  left_join(genres) %>%
  select(work_id, genre_id, title_id) %>%
  distinct() %>%
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
           title = Word,
           order_id) %>%
  # Bring in title ids from title table, then work ids from linking table
  left_join(titles, by = "title") %>%
  left_join(work_has_title, by = "title_id") %>%
  select(performance_id,
         work_id,
         title_id,
         order = order_id)

# People ----------------
## Person tags ----------
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

## Standardizing ------------
# Retrieve list of files in "standardizing" that contain person_performance_tags or person_work_tags in their names
s_files <- list.files("./data/original/standardizing", pattern = "*.csv", full.names = TRUE) %>%
  as_tibble_col(column_name = "files") %>%
  filter(
    str_detect(
      files, 
      pattern = str_c(c(person_performance_tags, person_work_tags), collapse = "|")
    )
  )

# Read in each of the "standardized" files and combine them
person_standard <- lapply(1:length(s_files$files), function(i) {
  
  tmp <- read_csv(s_files$files[i], col_types = cols(.default = "c"))
  
  return(tmp)
}) %>%
  bind_rows() %>%
  # Keep only these columns
  select(
    Word,
    ISNI,
    VIAF,
    WIKI,
    CESAR,
    OCLC,
    Name,
    Record_Link,
    ISNI_2,
    VIAF_2,
    WIKI_2,
    CESAR_2,
    Name_2
  ) %>%
  # Remove duplicate rows
  distinct()

# Create a data frame for any instances of a second person
# The second person gets the same column structure as the first so we can combine them
second_person <- person_standard %>%
  select(
    Word,
    ISNI = ISNI_2,
    VIAF = VIAF_2,
    WIKI = WIKI_2,
    CESAR = CESAR_2,
    Name = Name_2
  ) %>%
  distinct() %>%
  # Filter out any blanks
  filter(
    if_any(ISNI:Name, ~ !is.na(.x))
  )

# Recombine into a single data frame
both_persons <- person_standard %>%
  select(
    Word,
    ISNI,
    VIAF,
    WIKI,
    CESAR,
    Name
  ) %>%
  bind_rows(second_person) %>%
  distinct()

# Create a data frame of people who have any identifier
with_id <- both_persons %>%
  # This removes any rows which are entirely NA for identifiers (ISNI:Name)
  filter(
    if_any(ISNI:Name, ~ !is.na(.x))
  ) %>%
  # Each distinct combination of these variables gets assigned as a standardized "person" with a "person_id"
  group_by(ISNI, VIAF, WIKI, CESAR, Name) %>%
  # If there is no name given, just use the first alias ("Word") that appears
  mutate(
    person_id = cur_group_id(),
    Name = ifelse(is.na(Name), first(Word), Name)
  )

# Figure out what the highest person_id that has been created so far
highest_id <- max(with_id$person_id)

# Create a dataset of people without IDs
without_id <- both_persons %>%
  filter(
    if_all(ISNI:Name, ~ is.na(.x))
  ) %>%
  distinct() %>%
  # Add a "Name" column for compatibility with the with_id data frame
  mutate(Name = Word) %>%
  group_by(Word) %>%
  # Add new id number to the highest id from the other data set to make sure they don't overlap
  mutate(person_id = cur_group_id() + highest_id)

### Aliases --------------
# Create aliases table using the distinct names from df
aliases <- df %>%
  filter(Tag %in% c(person_performance_tags, person_work_tags)) %>%
  distinct(alias = Word) %>%
  mutate(alias_id = row_number())

# Combine the with_id and without_id tables
person_with_alias <- bind_rows(with_id, without_id) %>%
  rename(
    alias = Word,
    person = Name
  ) %>%
  arrange(person_id) %>%
  # Add the standardized persons to the "aliases" table
  right_join(
    aliases, by = "alias"
  )

# Fix missing values in person_id
# (This happens if the person is missing from the standardizing files)
pwa_highest <- max(person_with_alias$person_id, na.rm = TRUE)

# Add person_ids to rows with none
pwa_nas <- person_with_alias %>%
  ungroup() %>%
  filter(is.na(person_id)) %>%
  mutate(person_id = row_number() + pwa_highest)

# Remove rows with NA for person_id, add on the corrected data
person_with_alias_fixed <- person_with_alias %>%
  ungroup() %>%
  filter(!is.na(person_id)) %>%
  bind_rows(pwa_nas)

### Person Table --------------
# Create persons table
persons <- person_with_alias_fixed %>%
  ungroup() %>%
  select(
    person_id,
    person,
    ISNI, 
    VIAF,
    WIKI,
    CESAR
  ) %>%
  distinct()

# Create linkage table between persons and aliases
person_has_alias <- person_with_alias_fixed %>%
  ungroup() %>%
  distinct(
    alias_id,
    person_id
  )

## Determining Roles ------------
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
           alias = Word,
           role = Tag)

roles <- character()

# If a tag is followed by "ROLE", assign that character/role to the place in "roles" vector which precedes it
for(i in 1:nrow(actor_roles)) {
  role <- NA
  if(i < nrow(actor_roles)) {
    # If row i+1 is ROLE and both rows share a performance_id, that ROLE pertains to row i.
    if(actor_roles$role[i + 1] == "ROLE" & actor_roles$performance_id[i + 1] == actor_roles$performance_id[i]) {
      role <- actor_roles$alias[i + 1]
    }    
  }
  
  roles[i] <- role
}

# Attach roles vector as "char_role" to identify characters played by each person
actor_roles$char_role <- roles

# Remove ROLE tag and collect alias and person ids by joining to relevant tables
actor_roles <- actor_roles %>%
  filter(role != "ROLE") %>%
  left_join(aliases, by = "alias") %>%
  left_join(person_has_alias, by = "alias_id") %>%
  select(performance_id,
         person_id,
         alias_id,
         work_meta_id,
         char_role)

# Linking Tables (Round 3) ---------------
# Create crosswalk to allow connections between persons and performances
tmp_works_crosswalk <- df %>%
  mutate(title_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag == "WORK") %>%
  select(title = Word,
         title_meta_id) %>%
  left_join(titles) %>%
  left_join(work_has_title) %>%
  distinct(title_meta_id,
           work_id,
           title_id)

# Link person and performance using performance_id, person_id, and work_meta_id
person_has_performance <- df %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0")),
         title_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag %in% c(person_performance_tags, person_work_tags)) %>%
  distinct(performance_id,
           title_meta_id,
           alias = Word,
           role = Tag) %>%
  left_join(aliases, by = "alias") %>%
  left_join(person_has_alias, by = "alias_id") %>%
  select(performance_id,
         alias_id,
         person_id,
         title_meta_id,
         role) %>%
  left_join(actor_roles) %>%
  left_join(tmp_works_crosswalk) %>%
  distinct(performance_id, 
           title_id,
           work_id, 
           alias_id,
           person_id, 
           role, 
           char_role)

# Create crosswalk between works and persons (largely unnecessary because of the above)
work_has_person <- df %>%
  mutate(title_meta_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                               str_pad(venue_id, width = 6, side = "left", pad = "0"),
                               str_pad(perf_id, width = 6, side = "left", pad = "0"),
                               str_pad(work_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag %in% c(person_work_tags, person_performance_tags, "WORK")) %>%
  select(Word, Tag, title_meta_id) %>%
  left_join(titles, by = c("Word" = "title")) %>%
  left_join(drop_na(distinct(., title_meta_id, title_id)), by = "title_meta_id") %>%
  select(-title_id.x) %>%
  rename(title_id = title_id.y) %>%
  left_join(work_has_title, by = "title_id") %>%
  filter(Tag != "WORK") %>%
  left_join(aliases, by = c("Word" = "alias")) %>%
  left_join(person_has_alias, by = "alias_id") %>%
  distinct(title_id,
           work_id,
           alias_id,
           person_id,
           role = Tag) %>%
  drop_na() %>%
  arrange(title_id, work_id, alias_id, person_id)

# Writing Data --------------
## Local ----------
# Write all data to local tables
write_csv(documents, "./data/processed/db_tables/document.csv")
write_csv(venues, "./data/processed/db_tables/venue.csv")
write_csv(performances, "./data/processed/db_tables/performance.csv")
write_csv(works, "./data/processed/db_tables/work.csv")
write_csv(titles, "./data/processed/db_tables/title.csv")
write_csv(genres, "./data/processed/db_tables/genre.csv")
write_csv(persons, "./data/processed/db_tables/person.csv")
write_csv(aliases, "./data/processed/db_tables/alias.csv")
write_csv(document_has_venue, "./data/processed/db_tables/document_has_venue.csv")
write_csv(document_has_performance, "./data/processed/db_tables/document_has_performance.csv")
write_csv(performance_has_work, "./data/processed/db_tables/performance_has_work.csv")
write_csv(person_has_alias, "./data/processed/db_tables/person_has_alias.csv")
write_csv(person_has_performance, "./data/processed/db_tables/person_has_performance.csv")
write_csv(work_has_title, "./data/processed/db_tables/work_has_title.csv")
write_csv(work_has_person, "./data/processed/db_tables/work_has_person.csv")
write_csv(work_has_genre, "./data/processed/db_tables/work_has_genre.csv")

## To Database --------------
# Write data to remote database if applicable
if(remote_db) {
  con <- dbConnect(RMariaDB::MariaDB(), group="sanders-performance")
  
  dbWriteTable(con, "document", documents, overwrite = TRUE)
  dbWriteTable(con, "venue", venues, overwrite = TRUE)
  dbWriteTable(con, "performance", performances, overwrite = TRUE)
  dbWriteTable(con, "work", works, overwrite = TRUE)
  dbWriteTable(con, "title", titles, overwrite = TRUE)
  dbWriteTable(con, "genre", genres, overwrite = TRUE)
  dbWriteTable(con, "person", persons, overwrite = TRUE)
  dbWriteTable(con, "alias", aliases, overwrite = TRUE)
  dbWriteTable(con, "document_has_venue", document_has_venue, overwrite = TRUE)
  dbWriteTable(con, "document_has_performance", document_has_performance, overwrite = TRUE)
  dbWriteTable(con, "performance_has_work", performance_has_work, overwrite = TRUE)
  dbWriteTable(con, "person_has_performance", person_has_performance, overwrite = TRUE)
  dbWriteTable(con, "person_has_alias", person_has_alias, overwrite = TRUE)
  dbWriteTable(con, "work_has_title", work_has_title, overwrite = TRUE)
  dbWriteTable(con, "work_has_person", work_has_person, overwrite = TRUE)
  dbWriteTable(con, "work_has_genre", work_has_genre, overwrite = TRUE)
  
  dbDisconnect(con) 
}


