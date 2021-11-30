library(tidyverse)
library(lubridate)
library(DBI)

df <- read_csv("./data/processed/all_docs.csv")
doc_meta <- read_csv("./data/processed/doc_meta.csv")
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

documents <- df %>%
  distinct(Date, doc_id) %>%
  left_join(filter(df, Tag == "SECTION")) %>%
  select(1:4) %>%
  pivot_wider(names_from = "Tag", values_from = "Word") %>%
  select(-`NA`) %>%
  unnest(SECTION, keep_empty = TRUE) %>%
  group_by(Date, doc_id) %>%
  mutate(SECTION = paste(SECTION, collapse = ";")) %>%
  mutate(SECTION = ifelse(SECTION == "NA", NA, SECTION)) %>%
  distinct() %>%
  full_join(doc_meta, by = "doc_id") %>%
  rename_with(str_to_lower) %>%
  mutate(doc_title = paste0(title, " - ", format(date, "%A, %B %d, %Y")))

venue_locations <- df %>%
  select(doc_id, venue_id, Tag, Word) %>%
  filter(Tag %in% c("VENUE", "LOCATION")) %>%
  pivot_wider(names_from = "Tag", values_from = "Word") %>%
  unnest(VENUE, keep_empty = TRUE) %>%
  unnest(LOCATION, keep_empty = TRUE) %>%
  group_by(venue = VENUE) %>%
  summarize(location = paste(unique(LOCATION), collapse = ";")) %>%
  filter(!is.na(venue)) %>%
  mutate(location = str_remove_all(location, ";NA|NA;|NA")) %>%
  mutate(location = ifelse(nchar(location) == 0, NA, location))

venues <- df %>%
  filter(Tag %in% c("VENUE")) %>%
  distinct(Word) %>%
  rename(venue = Word) %>%
  mutate(id = row_number()) %>%
  left_join(venue_locations, by = "venue") %>%
  select(venue_id = id, 
         venue, 
         location)

document_has_venue <- df %>%
  filter(Tag == "VENUE") %>%
  select(venue = Word, doc_id) %>%
  left_join(venues, by = "venue") %>%
  select(doc_id, 
         venue_id)

document_has_performance <- df %>%
  mutate(Tag = ifelse(Tag == "Day", "DAY", Tag)) %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(venue_id != 0 & perf_id != 0) %>% 
  distinct(doc_id, performance_id)

performance_dates <- df %>%
  mutate(Tag = ifelse(Tag == "Day", "DAY", Tag)) %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(venue_id != 0) %>%
  select(-venue_id) %>%
  left_join(venues, by = c("Word" = "venue")) %>%
  group_by(doc_id) %>%
  fill(venue_id, .direction = "down") %>%
  ungroup() %>%
  filter(perf_id != 0) %>%
  distinct(performance_id, date = Date)

performances <- df %>%
  mutate(Tag = ifelse(Tag == "Day", "DAY", Tag)) %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(venue_id != 0) %>%
  select(-venue_id) %>%
  left_join(venues, by = c("Word" = "venue")) %>%
  group_by(doc_id) %>%
  fill(venue_id, .direction = "down") %>%
  ungroup() %>%
  filter(perf_id != 0) %>%
  select(performance_id, venue_id, Word, Tag) %>%
  filter(Tag %in% c("DAY", "TIME", "PRICE")) %>%
  pivot_wider(names_from = Tag,
              values_from = Word) %>%
  unnest(DAY, keep_empty = TRUE) %>%
  unnest(TIME, keep_empty = TRUE) %>%
  unnest(PRICE, keep_empty = TRUE) %>%
  group_by(performance_id, venue_id) %>%
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
  full_join(performance_dates) %>%
  mutate(first_time = ifelse(!is.na(first_time), as_datetime(paste0(date, " ", first_time)), NA),
         second_time = ifelse(!is.na(second_time), as_datetime(paste0(date, " ", second_time)), NA),
         third_time = ifelse(!is.na(third_time), as_datetime(paste0(date, " ", third_time)), NA))

works <- df %>%
  filter(Tag %in% c("WORK")) %>%
  distinct(work = Word) %>%
  mutate(work_id = row_number())

genres <- df %>%
  filter(Tag %in% c("GENRE")) %>%
  distinct(genre = Word) %>%
  mutate(genre_id = row_number())

work_has_genre <- df %>%
  filter(Tag %in% c("GENRE")) %>%
  select(genre = Word,
         doc_id,
         venue_id,
         perf_id,
         work_id) %>%
  right_join(select(filter(df, Tag == "WORK"), work = Word, doc_id, venue_id, perf_id, work_id)) %>%
  select(work,
         genre) %>%
  distinct() %>%
  drop_na() %>%
  left_join(works) %>%
  left_join(genres) %>%
  distinct(work_id, genre_id) %>%
  arrange(work_id)


performance_has_work <- df %>%
  mutate(performance_id = paste0(str_pad(doc_id, width = 6, side = "left", pad = "0"),
                                 str_pad(venue_id, width = 6, side = "left", pad = "0"),
                                 str_pad(perf_id, width = 6, side = "left", pad = "0"))) %>%
  filter(Tag == "WORK") %>%
  distinct(performance_id,
           work = Word,
           order_id) %>%
  left_join(works, by = "work") %>%
  select(performance_id,
         work_id,
         order = order_id)


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


persons <- df %>%
  filter(Tag %in% c(person_performance_tags, person_work_tags)) %>%
  distinct(person = Word) %>%
  mutate(person_id = row_number())


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

for(i in 1:nrow(actor_roles)) {
  role <- NA
  if(i < nrow(actor_roles)) {
    if(actor_roles$role[i + 1] == "ROLE" & actor_roles$performance_id[i + 1] == actor_roles$performance_id[i]) {
      role <- actor_roles$person[i + 1]
    }    
  }
  
  roles[i] <- role
}

actor_roles$char_role <- roles

actor_roles <- actor_roles %>%
  filter(role != "ROLE") %>%
  left_join(persons, by = "person") %>%
  select(performance_id,
         person_id,
         work_meta_id,
         char_role)

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


