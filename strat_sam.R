################################################################################
##### STRATIFIED SAMPLE OF COVID-19 PREPRINTS. STRATIFICATION BY PREPRINT  #####
##### SERVER, PREPRINT CATEGORY, MONTH INDICATING WHEN A PREPRINT WAS      #####
##### PUBLISHED, AND NUMBER OF AUTHORS OF A PREPRINT                       #####
##### Authors: Robbie van Aert, Michele Nuijten, and Anton                 #####
##### Olsson-Collentine                                                    #####
################################################################################

rm(list = ls())

### Load data of COVID-19 preprints
setwd("C:/SURFdrive/Corona")
dat <- read.csv("dat.csv")

### Exclude studies that did not specify the number of authors
dat <- subset(dat, dat$nr_authors > 0)

### Create a variable indicating the month a preprint was published
dates <- as.POSIXct(dat$date_upl, format = "%d-%m-%y") 
dat$month <- format(dates, "%B")

### Create a categorical variable of the number of authors of a preprint
dat$cat_nr_authors <- as.character(cut(dat$nr_authors, 
                                       breaks = c(0, 1, 2, 3, 10, 200)))

draws <- 1000 # Sample size

### Empty dataframe for storing the results
out <- data.frame(pp_ID = numeric(), title = character(), doi = character(), 
                  url = character(), first_authors = character(), 
                  last_author = character(), first_inst = character(), 
                  last_inst = character(), nr_authors = numeric(), 
                  date_upl = character(), site = character(), version = numeric(),
                  url_pdf = character(), time_downl = character(), 
                  downl_succes = numeric(), category = character(), 
                  month = character(), cat_nr_authors = character())

set.seed(12820)

### Sample a bit more draws to make sure that duplicates with only one case in a
# stratum can be replace by another case
for (i in 1:(draws+round(draws*0.1)))
{
  
  cat("progress = ", i/draws*100, "%", fill = TRUE)
  
  ### Create a subset of the data based on a sampled preprint server
  tab_site <- as.data.frame(table(site = dat$site))
  sam_site <- sample(tab_site$site, size = 1, replace = TRUE, prob = tab_site$Freq)
  sub <- subset(dat, dat$site == sam_site)
  
  ### Create a subset of the data based on a sampled category
  tab_category <- as.data.frame(table(category = sub$category))
  sam_category <- sample(tab_category$category, size = 1, replace = TRUE, 
                         prob = tab_category$Freq)
  sub <- subset(sub, sub$category == sam_category)
  
  ### Create a subset of the data based on a sampled publication month of a 
  # preprint
  tab_month <- as.data.frame(table(month = sub$month))
  sam_month <- sample(tab_month$month, size = 1, replace = TRUE, prob = tab_month$Freq)
  sub <- subset(sub, sub$month == sam_month)
  
  ### Create a subset of the data based on a sampled number of preprint authors
  tab_authors <- as.data.frame(table(authors = sub$cat_nr_authors))
  sam_authors <- sample(tab_authors$authors, size = 1, replace = TRUE, 
                        prob = tab_authors$Freq)
  sub <- subset(sub, sub$cat_nr_authors == sam_authors)
  
  ### Sample one preprint from the created subset of preprints
  out[i, ] <- sub[sample(1:nrow(sub), size = 1), ]
  
}

### Checks to see whether the sampled characteristics match those in the population
table(out$site)/draws
table(dat$site)/nrow(dat)

table(out$category)/draws 
table(dat$category)/nrow(dat)

table(out$month)/draws 
table(dat$month)/nrow(dat)

table(out$cat_nr_authors)/draws 
table(dat$cat_nr_authors)/nrow(dat)

### Check for duplicates in the sample
dupl <- out$pp_ID[duplicated(out$pp_ID)]

### Vector with row number of duplicate
dupl_row <- which(duplicated(out$pp_ID))

for (m in 1:length(dupl))
{
  ### Subset of preprints that match the characteristics of the duplicate 
  # preprint
  sub_dupl <- subset(dat, dat$site == out$site[out$pp_ID == dupl[m]][1] & 
                       dat$category == out$category[out$pp_ID == dupl[m]][1] &
                       dat$month == out$month[out$pp_ID == dupl[m]][1] & 
                       dat$cat_nr_authors == out$cat_nr_authors[out$pp_ID == dupl[m]][1])
  
  ### Remove duplicates in the subset to avoid sampling a duplicate again
  sub_dupl <- sub_dupl[!sub_dupl$pp_ID %in% out$pp_ID, ]
  
  if (nrow(sub_dupl) == 0)
  { # If there is only one preprint with these characteristics and this preprint
    # is already included, continue with next duplicate
    next
  }
  
  ### Sample one preprint from the created subset of preprints and replace data
  # of duplicate with new data
  out[dupl_row[m], ] <- sub_dupl[sample(1:nrow(sub_dupl), size = 1), ]
}

### Remove the duplicates that could not be replaced from the sample
out_nodupl <- out[!duplicated(out$pp_ID), ]

### Select the specified number of draws
sam <- out_nodupl[1:draws, ]

### Store the sample
write.csv(sam, file = "sam.csv")