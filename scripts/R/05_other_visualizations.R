library(tidyverse)
library(lubridate)
library(network)
library(igraph)
library(ggnetwork)

documents <- read_csv("./data/processed/db_tables/document.csv")
venues <- read_csv("./data/processed/db_tables/venue.csv")
performances <- read_csv("./data/processed/db_tables/performance.csv", col_types = "cnDccccccc")
works <- read_csv("./data/processed/db_tables/work.csv", col_types = "ncccccc")
titles <- read_csv("./data/processed/db_tables/title.csv")
genres <- read_csv("./data/processed/db_tables/genre.csv")
persons <- read_csv("./data/processed/db_tables/person.csv")
aliases <- read_csv("./data/processed/db_tables/alias.csv")
document_has_venue <- read_csv("./data/processed/db_tables/document_has_venue.csv")
document_has_performance <- read_csv("./data/processed/db_tables/document_has_performance.csv")
performance_has_work <- read_csv("./data/processed/db_tables/performance_has_work.csv")
person_has_alias <- read_csv("./data/processed/db_tables/person_has_alias.csv")
person_has_performance <- read_csv("./data/processed/db_tables/person_has_performance.csv")
work_has_title <- read_csv("./data/processed/db_tables/work_has_title.csv")
work_has_person <- read_csv("./data/processed/db_tables/work_has_person.csv")
work_has_genre <- read_csv("./data/processed/db_tables/work_has_genre.csv")


person_timeline <- person_has_performance %>%
  left_join(persons) %>%
  left_join(performances) %>%
  drop_na(date) %>%
  group_by(person) %>%
  filter(date == max(date) | date == min(date)) %>%
  mutate(val = ifelse(date == max(date), "max", "min")) %>%
  arrange(person_id, date) %>%
  distinct(person_id, person, role, date, val) %>%
  pivot_wider(names_from = "val",
              values_from = "date") %>%
  drop_na() %>%
  mutate(diff = as.numeric(max - min)) %>%
  filter(role != "ANIMAL")

ggplot(person_timeline, aes(y = 0, color = role)) +
  geom_curve(aes(x = min, xend = max, yend = 0), curvature = -0.75, size = 0.2) +
  scale_y_continuous(limits = c(0, 1),
                     expand = c(0,0)) +
  labs(
    x = "Year",
    y = ""
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~role)

ggsave("./other_output/images/person_timeline.png", width = 6, height = 3, dpi = 600)
ggsave("./images/person_timeline.png", width = 6, height = 3, dpi = 600)
ggsave("./site/images/person_timeline.png", width = 6, height = 3, dpi = 600)

person_network <- person_has_performance %>%
  distinct(performance_id, person_id) %>%
  left_join(distinct(person_has_performance, performance_id, person_id), by = "performance_id") %>%
  filter(person_id.x != person_id.y) %>%
  left_join(persons, by = c("person_id.x" = "person_id")) %>%
  left_join(persons, by = c("person_id.y" = "person_id")) %>%
  group_by(person_one = person.x,
           person_two = person.y) %>%
  tally() %>%
  pivot_wider(names_from = person_two,
              values_from = n, 
              values_fill = 0) %>%
  select(c(person_one, as.character(unique(.$person_one)))) %>%
  column_to_rownames("person_one") %>%
  as.matrix() %>%
  igraph::graph_from_adjacency_matrix(mode = "undirected")

person_gg <- person_network %>%
  ggnetwork()

ggplot(person_gg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(aes(label = name), size = 1) +
  geom_edges(size = 0.2, alpha = 0.2) +
  coord_equal() +
  theme_blank()

ggsave("./other_output/images/person_network.pdf", dpi = 600, width = 8, height = 8)
ggsave("./other_output/images/person_network.png", dpi = 600, width = 8, height = 8)
ggsave("./site/images/person_network.png", dpi = 600, width = 8, height = 8)
ggsave("./images/person_network.png", dpi = 600, width = 8, height = 8)

top_5_genres <- performance_has_work %>%
  left_join(performances) %>%
  left_join(works) %>%
  left_join(work_has_genre) %>%
  left_join(genres) %>%
  group_by(genre) %>%
  tally() %>%
  drop_na() %>%
  top_n(5)

genres_over_time <- performance_has_work %>%
  left_join(performances) %>%
  left_join(works) %>%
  left_join(work_has_genre) %>%
  left_join(genres) %>%
  mutate(date = floor_date(date, unit = "months")) %>%
  group_by(date, genre) %>%
  tally() %>%
  drop_na() %>%
  arrange(-n) %>%
  filter(genre %in% top_5_genres$genre)

ggplot(genres_over_time, aes(x = date, y = n)) +
  geom_line(aes(color = genre), alpha = 0.3) +
  geom_smooth(aes(color = genre), se = FALSE, size = .5) +
  theme_bw() +
  labs(
    x = "",
    y = "Monthly Performances",
    color = "Genre",
    title = "Top 5 Genres Over Time"
  )

ggsave("./other_output/images/genres_over_time.png", width = 5, height = 3, dpi = 300)
ggsave("./site/images/genres_over_time.png", width = 5, height = 3, dpi = 300)
ggsave("./images/genres_over_time.png", width = 5, height = 3, dpi = 300)



work_has_performance <- read_csv("./data/processed/db_tables/performance_has_work.csv")

heroes <- works %>%
  filter(work %in% c("l'Habitant de la Guadeloupe", "l'Héroïne Américaine", "le Héros Américain", "Paul et Virginie")) %>%
  left_join(work_has_performance) %>%
  left_join(performances)

yearly_heroes <- heroes %>%
  mutate(year = floor_date(date, "years")) %>%
  group_by(year, work) %>%
  tally() %>%
  pivot_wider(names_from = year, values_from = n) %>%
  pivot_longer(-work, names_to = "year", values_to = "n") %>%
  mutate(year = as.Date(year), n = ifelse(is.na(n), 0, n))

ggplot(yearly_heroes, aes(x = year, y = n, color = work)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("1789-07-14"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("1792-08-10"), linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "",
    y = "Performances",
    color = "",
    title = "Yearly Performances 1786-1793"
  )

ggsave("./other_output/images/heroes.png", width = 6, height = 3)
ggsave("./site/images/heroes.png", width = 6, height = 3)
ggsave("./images/heroes.png", width = 6, height = 3)



opera <- works %>%
  filter(work %in% c("Brutus", "la Mort de César", "Guillaume Tell", "la Folle Journée ou le Mariage de Figaro")) %>%
  left_join(work_has_performance) %>%
  left_join(performances)

yearly_opera <- opera %>%
  mutate(year = floor_date(date, "years")) %>%
  group_by(year, work) %>%
  tally() %>%
  pivot_wider(names_from = year, values_from = n) %>%
  pivot_longer(-work, names_to = "year", values_to = "n") %>%
  mutate(year = as.Date(year), n = ifelse(is.na(n), 0, n))

ggplot(yearly_opera, aes(x = year, y = n, color = work)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("1789-07-14"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("1792-08-10"), linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "",
    y = "Performances",
    color = "",
    title = "Yearly Performances 1787-1793"
  )

ggsave("./other_output/images/opera.png", width = 6, height = 3)
ggsave("./site/images/opera.png", width = 6, height = 3)
ggsave("./images/opera.png", width = 6, height = 3)

top_venues <- performance_has_work %>%
  left_join(performances) %>%
  left_join(venues) %>%
  filter(!is.na(venue)) %>%
  group_by(venue_id, venue) %>%
  tally() %>%
  ungroup() %>%
  top_n(4) %>%
  arrange(-n)

top_genres_per_venue <- performance_has_work %>%
  left_join(performances) %>%
  left_join(venues) %>%
  select(-title_id) %>%
  filter(venue_id %in% top_venues$venue_id) %>%
  left_join(work_has_genre) %>%
  left_join(genres) %>%
  filter(!is.na(genre_id)) %>%
  group_by(venue_id, venue, genre_id, genre) %>%
  tally() %>%
  group_by(venue_id, venue) %>%
  top_n(5)

top_venues_genre <- performance_has_work %>%
  left_join(performances) %>%
  left_join(venues) %>%
  select(-title_id) %>%
  filter(venue_id %in% top_venues$venue_id) %>%
  left_join(work_has_genre) %>%
  left_join(genres) %>%
  inner_join(select(top_genres_per_venue, -n)) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  group_by(year, venue_id, venue, genre_id, genre) %>%
  tally() %>%
  group_by(year, venue_id, venue) %>%
  top_n(5) %>%
  ungroup() %>%
  pivot_wider(names_from = "year", values_from = "n") %>%
  pivot_longer(cols = !matches("[venue|genre]"), names_to = "year", values_to = "n", values_drop_na = FALSE) %>%
  mutate(
    year = as.Date(year),
    n = ifelse(is.na(n), 0, n)
  )

for(v in unique(top_venues_genre$venue_id)) {
  tmp <- top_venues_genre %>%
    filter(venue_id == v)
  
  p <- ggplot(tmp, aes(x = year, y = n, color = genre)) +
    geom_line() +
    geom_point() +
    labs(
      x = "",
      y = "Performances",
      color = "Genre",
      title = "Yearly Performances",
      subtitle = paste0("Top Genres at ", unique(tmp$venue)[1])
    ) +
    theme_bw()
  
  ggsave(paste0("./other_output/images/top_genres_", v, ".png"), plot = p, width = 6, height = 3)
  ggsave(paste0("./site/images/top_genres_", v, ".png"), plot = p, width = 6, height = 3)
  ggsave(paste0("./images/top_genres_", v, ".png"), plot = p, width = 6, height = 3)
  
}


top_works_per_venue <- performance_has_work %>%
  left_join(performances) %>%
  left_join(venues) %>%
  left_join(works) %>%
  filter(venue_id %in% top_venues$venue_id) %>%
  group_by(venue_id, venue, work_id, work) %>%
  tally() %>%
  group_by(venue_id, venue) %>%
  top_n(5)

top_venues_work <- performance_has_work %>%
  left_join(performances) %>%
  left_join(venues) %>%
  left_join(works) %>%
  filter(venue_id %in% top_venues$venue_id) %>%
  inner_join(select(top_works_per_venue, -n)) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  group_by(year, venue_id, venue, work_id, work) %>%
  tally() %>%
  group_by(year, venue_id, venue) %>%
  top_n(5) %>%
  ungroup() %>%
  pivot_wider(names_from = "year", values_from = "n") %>%
  pivot_longer(cols = !matches("[venue|work]"), names_to = "year", values_to = "n", values_drop_na = FALSE) %>%
  mutate(
    year = as.Date(year),
    n = ifelse(is.na(n), 0, n)
  )

for(v in unique(top_venues_work$venue_id)) {
  tmp <- top_venues_work %>%
    filter(venue_id == v)
  
  p <- ggplot(tmp, aes(x = year, y = n, color = work)) +
    geom_line() +
    geom_point() +
    labs(
      x = "",
      y = "Performances",
      color = "Work",
      title = "Yearly Performances",
      subtitle = paste0("Top Works at ", unique(tmp$venue)[1])
    ) +
    theme_bw()
  
  ggsave(paste0("./other_output/images/top_works_", v, ".png"), plot = p, width = 6, height = 3)
  ggsave(paste0("./site/images/top_works_", v, ".png"), plot = p, width = 6, height = 3)
  ggsave(paste0("./images/top_works_", v, ".png"), plot = p, width = 6, height = 3)
  
}



