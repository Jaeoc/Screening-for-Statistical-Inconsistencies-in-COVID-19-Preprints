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
preprints <- lapply(codebook_files, read.csv, header = TRUE, skip = 1,
                    stringsAsFactors = FALSE, na.strings = c(""),
                    colClasses = c(reported = "character"))
#a) Reading in the reported column as a character is important, because it 
#allows us to compare our computed value to the specificity used in the preprint
#(i.e., to the same decimal). 
#b) If we don't specify na.strings then for character vectors the "" is not read as NA


#Drop rows with only NAs.
preprints <- lapply(preprints, function(x) x[!rowSums(is.na(x)) == ncol(x),])
  

#Check reported values
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
