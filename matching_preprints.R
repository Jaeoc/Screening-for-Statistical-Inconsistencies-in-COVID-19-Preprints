################################################################################
##### R CODE FOR MATCHING COVID-19 PREPRINTS WITH A COMPARABLE             #####
##### NON-COVID-19 PREPRINT                                                #####
##### Authors: Robbie van Aert, Michele Nuijten, and Anton                 #####
##### Olsson-Collentine                                                    #####
################################################################################

rm(list = ls())

### Load descriptive data of all non-COVID-19 preprints
dat <- readRDS("dat_all_pp.rds")

### Load data of COVID-19 preprints
dat_covid <- read.csv("dat.csv")

### Remove preprints in the set of non-COVID-19 preprints that are also in the 
# COVID-19 preprints
dat <- dat[!dat$doi %in% dat_covid$doi, ]

### Exclude some preprints that list covid or corona in the title
dat <- dat[!grepl(pattern = "COVID", x = dat$title), ]
dat <- dat[!grepl(pattern = "Covid", x = dat$title), ]
dat <- dat[!grepl(pattern = "Corona", x = dat$title), ]
dat <- dat[!grepl(pattern = "corona", x = dat$title), ]
dat <- dat[!grepl(pattern = "SARS-CoV-2", x = dat$title), ]

### Load descriptive data of sampled COVID-19 preprints
sam <- read.csv("sam.csv")

### Create a variable indicating the number of authors of a preprint
dat$nr_authors <- lengths(regmatches(dat$authors, gregexpr(";", dat$authors)))+1

### Create a categorical variable of the number of authors of a preprint
dat$cat_nr_authors <- as.numeric(cut(dat$nr_authors, 
                                     breaks = c(0, 1, 2, 10, 200)))

### Create a variable indicating the month a preprint was published
dates <- as.POSIXct(dat$date, format = "%Y-%m-%d") 
dat$month <- as.numeric(format(dates, "%m"))

### Empty dataframe for storing the results
matches <- data.frame(doi = character(), title = character(),
                      authors = character(), author_corresponding = character(), 
                      author_corresponding_institution = character(),
                      date = character(), version = numeric(), type = character(),
                      license = character(), category = character(), 
                      published = character(), server = character(), 
                      nr_authors = numeric(), cat_nr_authors = character(),
                      month = as.numeric(), matched_pp_ID = numeric())

for (i in 1:nrow(sam))
{
  
  if (i %in% round(seq(1, nrow(sam), length.out = 100)))
  {
    cat("progress = ", round(i/nrow(sam)*100, 1), "%", fill = TRUE)
  }
  
  ### Create stable subset of data matching on version and category
  sub_stable <- subset(dat, as.numeric(dat$version) == sam[i,"version"] & 
                         dat$category == sam[i,"category"])
  
  ### Subset to match on month, number of authors, and server
  sub <- subset(sub_stable, sub_stable$month == sam[i,"month"] &
                  sub_stable$cat_nr_authors == sam[i,"cat_nr_authors"] &
                  sub_stable$server == sam[i,"site"])
  
  if (nrow(sub) > 0)
  { # If the subset contains data, randomly sample one of the preprints
    matches[i,1:(ncol(matches)-1)] <- sub[sample(1:nrow(sub), size = 1), ]
    matches[i,"matched_pp_ID"] <- sam[i,"pp_ID"]
    
    ### Remove sampled preprint to avoid duplicates
    dat <- dat[!dat$doi %in% matches[i,"doi"], ]
    
  } else
  { # Create a subset based on adjacent months
    sub <- subset(sub_stable, sub_stable$month >= sam[i,"month"]-1 &
                    sub_stable$month <= sam[i,"month"]+1 & 
                    sub_stable$cat_nr_authors == sam[i,"cat_nr_authors"] &
                    sub_stable$server == sam[i,"site"])
    
    if (nrow(sub) > 0)
    {
      matches[i,1:(ncol(matches)-1)] <- sub[sample(1:nrow(sub), size = 1), ]
      matches[i,"matched_pp_ID"] <- sam[i,"pp_ID"]
      
      ### Remove sampled preprint to avoid duplicates
      dat <- dat[!dat$doi %in% matches[i,"doi"], ]
      
    } else
    { # Create subset based on adjacent category of number of authors
      sub <- subset(sub_stable, sub_stable$month == sam[i,"month"] &
                      sub_stable$cat_nr_authors >= sam[i,"cat_nr_authors"]-1 &
                      sub_stable$cat_nr_authors <= sam[i,"cat_nr_authors"]+1 &
                      sub_stable$server == sam[i,"site"])
      
      if (nrow(sub) > 0)
      { 
        matches[i,1:(ncol(matches)-1)] <- sub[sample(1:nrow(sub), size = 1), ]
        matches[i,"matched_pp_ID"] <- sam[i,"pp_ID"]
        
        ### Remove sampled preprint to avoid duplicates
        dat <- dat[!dat$doi %in% matches[i,"doi"], ]
        
      } else
      { # Create subset while ignoring the server a sample COVID-19 preprint is from
        sub <- subset(sub_stable, sub_stable$month == sam[i,"month"] &
                        sub_stable$cat_nr_authors == sam[i,"cat_nr_authors"])
        
        if (nrow(sub) > 0)
        {
          matches[i,1:(ncol(matches)-1)] <- sub[sample(1:nrow(sub), size = 1), ]
          matches[i,"matched_pp_ID"] <- sam[i,"pp_ID"]
          
          ### Remove sampled preprint to avoid duplicates
          dat <- dat[!dat$doi %in% matches[i,"doi"], ]
          
        } else
        { # Return NAs if no comparable non-COVID-19 preprint was found
          matches[i,1:(ncol(matches)-1)] <- NA
          matches[i,"matched_pp_ID"] <- sam[i,"pp_ID"]
        }
      }
    }
  }
  
}

### Store the matched preprints
write.csv(matches, file = "matches.csv")