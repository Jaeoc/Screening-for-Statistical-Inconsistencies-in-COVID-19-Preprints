#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Code to check consistency of reported values in preprints
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl)


#********************************************************************************
#Packages and sourced functions----
#********************************************************************************

#Checking functions
source("functions_checking.R") 

#********************************************************************************
#Run checks and save results----
#********************************************************************************

#Get file paths to completed codebooks
codebook_files <- list.files("../data/pilot_codebooks", full.names = TRUE)
#Change "pilot_codebooks" to relevant folder later

#Read completed codebooks
preprints <- lapply(codebook_files, read.csv, header = TRUE, skip = 1, stringsAsFactors = FALSE)
#NB, in type_stat column an empty cell is read as "" but in all other columns as NA 
#If we don't use stringsAsFactors = FALSE this leads to problems. Maybe worth
#fixing when updating the excel template

#Drop rows with only Nas. Because some empty cells not coded as NA must can't use
#preprints <- lapply(preprints, function(x) x[!rowSums(is.na(x)) == ncol(x),]) 
#But must use something like the below (which may be a little less robust)
preprints <- lapply(preprints, function(x)  x[!is.na(x$page),]) #drop empty rows
  

#Check values
preprint_results <- lapply(preprints, split_check_bind)

#drop empty columns for more readable results. 
#because sample_size checker function uses all columns value1:20 can't drop earlier
#EDIT: dropping columns becomes problematic if we wish to collate results
#Better to leave for now
#preprint_results <- lapply(preprint_results, function(x) x[,!colSums(is.na(x)) == nrow(x)])

#Get preprint IDs 
preprint_ID <- unlist(strsplit(codebook_files, split = "_|\\.")) #split by _ and . ('.' because of .csv)
preprint_ID <- grep("[[:digit:]]", preprint_ID, value = TRUE) #get IDs, assumes no other numbers in file path

#Append preprint IDs to each dataframe
for(preprint in seq_along(preprint_ID)){
  preprint_results[[preprint]]$ID <- preprint_ID[preprint]
}

#Collate and save dataset for analysis
preprint_results <- do.call(rbind, preprint_results)
write.csv(preprint_results, "../data/checked_results.csv", row.names = FALSE)
