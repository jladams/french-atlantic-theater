library(tidyverse)

# Read in CSV
df <- read_csv("./data/original/bordeaux_rep_db.csv") %>%
  select(-index) %>%
  arrange(Date) %>%
  mutate(Tag = ifelse(Tag == "ORDRE", "ORDER", Tag))

# Create document IDs (not really necessary yet)
df$doc_id <- df %>%
  group_by(Links) %>%
  group_indices()

doc_meta <- df %>%
  distinct(doc_id, Title, File, Links)

write_csv(doc_meta, "./data/processed/doc_meta.csv")

df_list <- df %>%
  select(-Title, -File, -Links) %>%
  split(df$doc_id)

# Filter for testing
# df <- df %>%
#   filter(doc_id == 68)

test_tag <- function(data, row, category, category2 = NULL) {
  tmp <- FALSE
  
  if(is.na(data$Tag[row])) {
    return(tmp)
  }
  
  if(data$Tag[row] %in% category) {
    tmp <- TRUE
  }
  
  if(!is.null(category2) & row > 1) {
    if(data$Tag[row] %in% category & data$Tag[row-1] %in% category2) {
      tmp <- TRUE
    }
  }

  return(tmp)
  
  
}

tag_venues <- function(dat) {
  
  # Set up venue IDs
  venue <- 0
  venue_id <- c()
  
  
  
  for(i in 1:nrow(dat)) {
    
    # Venue
    if(test_tag(data = dat, row = i, category = c("VENUE"))) {
      venue <- venue + 1
    }
    
    venue_id[i] <- venue
    
    # If a venue is at the end of the document, assign it all the way back to the previous "DAY" tag
    if(!("WORK" %in% dat$Tag[i:nrow(dat)])) {
      for(j in i:1) {
        if(dat$Tag[j] %in% c("DAY", "VENUE")) {
          break
        }
      }
      
      venue_id[j:i] <- venue
      
    }
    
  }
  
  dat$venue_id <- venue_id
  
  return(dat)
}

t <- lapply(df_list, tag_venues) %>%
  bind_rows() %>%
  group_split(doc_id, venue_id)

tag_performances <- function(dat) {
  perf <- 0
  perf_id <- c()

  for(i in 1:nrow(dat)) {
    if(!is.na(dat$Tag[i])) {
      if(i > 1) {
        if((dat$Tag[i] == "TIME" & dat$Tag[i - 1] != "TIME" & dat$Tag[i - 1] != "DAY") & dat$Tag[i-1] != "WORK" | dat$Tag[i] == "DAY")  {
          perf <- perf + 1
        } 
        
      } else if(i == 1) {
        if(dat$Tag[i] == "DAY") {
          perf <- perf + 1
        }
      }
      
      if(!any(c("TIME", "DAY") %in% dat$Tag)) {
        perf <- 1
      }
      
      if(dat$Tag[i] == "VENUE") {
        perf <- 0
      }
      
    }
   
    perf_id[i] <- perf   
    
  }
  
  dat$perf_id <- perf_id 

  return(dat)
  
}

t2 <- lapply(t, tag_performances) %>%
  bind_rows() %>%
  mutate(perf_id = ifelse(Tag == "WORK" & perf_id == 0, 1, perf_id)) %>%
  group_split(doc_id, venue_id, perf_id)


tag_works <- function(dat) {
  # Set up work IDs
  work <- 0
  work_id <- c()
  
  for(i in 1:nrow(dat)) {
    if(test_tag(data = dat, row = i, category = "WORK", category2 = c("VENUE", "DAY"))) {
      work <- work + 1
    }
    
    work_id[i] <- work
  }
  
  
  dat$work_id <- work_id  
  
  return(dat)
}

t3 <- lapply(t2, tag_works) %>%
  bind_rows() %>%
  group_split(doc_id, venue_id, perf_id)

#following and preceeding
following <- c("suivi", "suiv.", "suiv", "suivi de", "suivi des", "suivi de la", "suivi d'", "suiv. de", "suivi d'", "suiv. des", "suiv. du", "suiv de", "suiv des", "suiv d'", "suiv du")
preceding  <- c("prec de", "prec des", "prec d'", "prec du", "prec. de", "prec. des", "prec. d'", "prec. du", "préc de", "préc des","préc d'", "préc du", "préc. de", "préc. des", "préc. d'", "préc. du", "précédé de", "précédé des","précédé du", "précédé d'", "précédés des","précédés du","précédés d'", "précédés du","précédée de","précédée des","précédée d'","précédée du","précédées de","précédées des","précédées d'","précédées du" )

#Performance and Ads
scheduled_performance <- c("Aujourd'hui","Auj","Aujourd","aujourd","Aujourdi")
future_performance <- c("Demain", "Incessament", "Incessamment","En attendant","demain")


tag_order <- function(dat) {
  
  order <- 0
  order_id <- c()
  
  for(i in 1:nrow(dat)) {
    if(is.na(dat$Tag[i])) {
      dat$Tag[i] <- "NA"
    }
    
    if(dat$Tag[i] == "ORDER") {
      
      if(dat$Word[i] %in% preceding) {
        # Find out how much to offset the order by
        # i:j will be the range of rows between the "preceding" tag and the next time signal/venue
        for(j in (i+1):nrow(dat)) {
          if(dat$Tag[j] %in% c("TIME", "VENUE", "ORDER", "DAY", "LOCATION")) {
            break
          }
        }
        
        # Count the number of works between "preceding" tag and next time signal/venue
        pre_offset <- sum(dat$Tag[i:j] == "WORK") + 1
        
        order <- order - pre_offset
        
      }
      
    }
    
    if(dat$Tag[i] %in% c("TIME", "VENUE", "DAY", "LOCATION")) {
      order <- 0
    }
    
    if(test_tag(data = dat, row = i, category = "WORK")) {
      order <- order + 1
    }
    
    
    order_id[i] <- order
    
  }
  
  dat$order_id <- order_id

  return(dat)  
  
}

t4 <- lapply(t3, tag_order) %>%
  bind_rows() %>%
  group_by(doc_id, venue_id, perf_id) %>%
  mutate(order_id = ifelse(Tag == "WORK", order_id, NA)) %>%
  mutate(order_id = dense_rank(order_id)) %>%
  mutate(performance = ifelse(any(Word %in% future_performance), "future performance", "scheduled performance"))

# Fix some iffy metadata
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

t4 <- t4 %>%
  mutate(work_id = ifelse(Tag %in% c(person_work_tags, person_performance_tags) & work_id == 0, 1, work_id),
         perf_id = ifelse(Tag %in% c(person_work_tags, person_performance_tags) & perf_id == 0, 1, perf_id))


write_csv(t4, "./data/processed/all_docs.csv")

