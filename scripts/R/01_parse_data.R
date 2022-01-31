library(tidyverse)

# Read in CSV
df <- read_csv("./data/original/bordeaux_rep_db.csv") %>%
  select(-index) %>%
  arrange(Date) %>%
  mutate(Tag = ifelse(Tag == "ORDRE", "ORDER", Tag))

# Create document IDs
df$doc_id <- df %>%
  group_by(Links) %>%
  group_indices()

# Extract document metadata, which will be used later to create pages
doc_meta <- df %>%
  distinct(doc_id, Title, File, Links)

write_csv(doc_meta, "./data/processed/doc_meta.csv")

# Split the documents into a list of smaller data frames based on document id
df_list <- df %>%
  select(-Title, -File, -Links) %>%
  split(df$doc_id)

# Helper function to test whether a given row of the data frame matches a particular tag
test_tag <- function(data, row, category, category2 = NULL) {
  
  # Default value is FALSE
  tmp <- FALSE
  
  # If the tag is blank, return FALSE
  if(is.na(data$Tag[row])) {
    return(tmp)
  }
  
  # If the tag matches the desired category, set the value to TRUE
  if(data$Tag[row] %in% category) {
    tmp <- TRUE
  }
  
  # If category2 is given, test whether the current row is in "category" and the previous row is in "category2"
  if(!is.null(category2) & row > 1) {
    if(data$Tag[row] %in% category & data$Tag[row-1] %in% category2) {
      tmp <- TRUE
    }
  }

  return(tmp)
  
  
}

# Function to identify venues in a data frame 
tag_venues <- function(dat) {
  
  # Set up venue IDs
  venue <- 0
  venue_id <- c()
  
  
  
  for(i in 1:nrow(dat)) {
    
    # If the tag is VENUE, add one to the venue ID
    if(test_tag(data = dat, row = i, category = c("VENUE"))) {
      venue <- venue + 1
    }
    
    venue_id[i] <- venue
    
    # If a venue is at the end of the document (no "WORK" tag comes after it), 
    # assign it all the way back to the previous "DAY" or "VENUE" tag
    if(!("WORK" %in% dat$Tag[i:nrow(dat)])) {
      for(j in i:1) {
        if(dat$Tag[j] %in% c("DAY", "VENUE")) {
          break
        }
      }
      
      venue_id[j:i] <- venue
      
    }
    
  }
  
  # Add the venue id column onto the data frame
  dat$venue_id <- venue_id
  
  # Return the data frame that now has the venue ids
  return(dat)
}

# Loop through the list of data frames (separated by document) and tag venues within each document
t <- lapply(df_list, tag_venues) %>%
  bind_rows() %>%
  group_split(doc_id, venue_id) # Split into a new list by both document and venue ids

# Function to identify performances within a document/venue tagged data frame
tag_performances <- function(dat) {
  perf <- 0
  perf_id <- c()
  
  
  # Loop through each row
  for(i in 1:nrow(dat)) {
    if(!is.na(dat$Tag[i])) {
      
      # Rules for row > 1
      if(i > 1) {
        
        # Tag is TIME and not preceded by TIME or DAY, or tag is DAY, add one to performance id
        if((dat$Tag[i] == "TIME" & dat$Tag[i - 1] != "TIME" & dat$Tag[i - 1] != "DAY") & dat$Tag[i-1] != "WORK" | dat$Tag[i] == "DAY")  {
          perf <- perf + 1
        } 
        
      # Rules for row == 1
      } else if(i == 1) {
        
        # If the tag is DAY add one to performance id
        if(dat$Tag[i] == "DAY") {
          perf <- perf + 1
        }
      }
      
      # If tag is TIME or DAY add one to performance id
      if(!any(c("TIME", "DAY") %in% dat$Tag)) {
        perf <- 1
      }
      
      # If we begin a new VENUE, set performance back to 0
      if(dat$Tag[i] == "VENUE") {
        perf <- 0
      }
      
    }
   
    perf_id[i] <- perf   
    
  }
  
  # Attach performance ids to data frame
  dat$perf_id <- perf_id 

  return(dat)
  
}

# Loop through the doc/venue data frames and tag performances
t2 <- lapply(t, tag_performances) %>%
  bind_rows() %>%
  mutate(perf_id = ifelse(Tag == "WORK" & perf_id == 0, 1, perf_id)) %>% # If a work doesn't have a performance, set that row to performance 1
  group_split(doc_id, venue_id, perf_id) # Split into new list based on doc/venue/performance

# Function to tag works within doc/venue/performance tagged data frame
tag_works <- function(dat) {
  # Set up work IDs
  work <- 0
  work_id <- c()
  
  # Loop through each row
  for(i in 1:nrow(dat)) {
    
    # If row is WORK and previous row is VENUE or DAY, add one to work id
    if(test_tag(data = dat, row = i, category = "WORK", category2 = c("VENUE", "DAY"))) {
      work <- work + 1
    }
    
    work_id[i] <- work
  }
  
  
  dat$work_id <- work_id  
  
  return(dat)
}

# Loop through doc/venue/performance and tag works
t3 <- lapply(t2, tag_works) %>%
  bind_rows() %>%
  group_split(doc_id, venue_id, perf_id) # Split again into list of data frames based on doc/venue/performance

# following and preceding words
following <- c("suivi", "suiv.", "suiv", "suivi de", "suivi des", "suivi de la", "suivi d'", "suiv. de", "suivi d'", "suiv. des", "suiv. du", "suiv de", "suiv des", "suiv d'", "suiv du")
preceding  <- c("prec de", "prec des", "prec d'", "prec du", "prec. de", "prec. des", "prec. d'", "prec. du", "préc de", "préc des","préc d'", "préc du", "préc. de", "préc. des", "préc. d'", "préc. du", "précédé de", "précédé des","précédé du", "précédé d'", "précédés des","précédés du","précédés d'", "précédés du","précédée de","précédée des","précédée d'","précédée du","précédées de","précédées des","précédées d'","précédées du" )

# Performance and Ads
scheduled_performance <- c("Aujourd'hui","Auj","Aujourd","aujourd","Aujourdi")
future_performance <- c("Demain", "Incessament", "Incessamment","En attendant","demain")

# Function to tag order of works within performance
tag_order <- function(dat) {
  
  order <- 0
  order_id <- c()
  
  for(i in 1:nrow(dat)) {
    
    # Convert NA values into NA strings
    if(is.na(dat$Tag[i])) {
      dat$Tag[i] <- "NA"
    }
    
    # Rules for if there are order-defining tags
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
    
    # Reset order to 0 if we encounter a TIME, VENUE, DAY, or LOCATION tag
    if(dat$Tag[i] %in% c("TIME", "VENUE", "DAY", "LOCATION")) {
      order <- 0
    }
    
    # Add 1 to order if we encounter a WORK tag
    if(test_tag(data = dat, row = i, category = "WORK")) {
      order <- order + 1
    }
    
    
    order_id[i] <- order
    
  }
  
  dat$order_id <- order_id

  return(dat)  
  
}

# Loop through list of doc/venue/performance data frames and tag work order
t4 <- lapply(t3, tag_order) %>%
  bind_rows() %>%
  group_by(doc_id, venue_id, perf_id) %>%
  mutate(order_id = ifelse(Tag == "WORK", order_id, NA)) %>% # If the tag isn't WORK, set the order to NA
  mutate(order_id = dense_rank(order_id)) %>% # Set order ids to start at 1 (corrects for offsets from "preceding" tags that create negative or 0 order numbers)
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

# Write out tagged data
write_csv(t4, "./data/processed/all_docs.csv")

