#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Code to check consistency of reported values in preprints
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl)
#Robbie van Aert


#********************************************************************************
#Packages and sourced functions----
#********************************************************************************

source("functions_checking.R") 
#Functions to compute p-values and ratios from this source file still need to be
#amended to work for the current approach

#********************************************************************************
#Additional functions----
#********************************************************************************
#For a cleaner script, move these functions to the file "functions_checking.r"
#after finishing amending the remaining functions

check_percentile <- function(percentage, numerator, denominator){
  abs(percentage - numerator / denominator * 100) <= 0.1 #use tolerance == 0.1% to account for rounding errors
}


#reformulated this function to fit
check_test_diag <- function(percentage, tp, tn, fp, fn,
                            which_diag = c("accu", "sens", "spec",  "ppv", "npv")){
  
  
  ### Arguments:
  #     - percentage = value to compare against
  #     - tp = number of true positives
  #     - tn = number of true negatives
  #     - fp = number of false positives
  #     - fn = number of false negatives
  #     - which_diag = test_diag (sens, spec, accu, ppv, npv)
  
  which_diag <- match.arg(which_diag)
  
  if(which_diag == "accu"){
    coded <- (tp+tn)/(tp+tn+fp+fn) # Accuracy
  } else if(which_diag == "sens"){
    coded <- tp/(tp+fn) # Sensitivity
  } else if(which_diag == "spec"){
    coded <- tn/(tn+fp) # Specificity
  } else if(which_diag == "ppv"){
    coded <- tp/(tp+fp) # Positive predictive value
  } else if (which_diag == "npv"){
    coded <- tn/(tn+fn) # Negative predictive value
  }
  
  abs(percentage - coded*100) <= 0.1 #use tolerance == 0.1% to account for rounding errors
  
}


check_sample_size <- function(total_sample, subgroup_cols, dat){ #also checks marginals
  
  sum_subgroups <- rowSums(dat[,subgroup_cols], na.rm = TRUE)
  
  (total_sample - sum_subgroups) == 0
  
}


#**Wrapper functions----
checker <- function(split_x){ #takes as input a dataframe with a single type of type_stat
  #split by type_stat before applying function
  
  if(split_x$type_stat[1] == "perc_ratio"){
    
    split_x$check <- check_percentile(percentage = split_x$reported,
                                      numerator = split_x$num,
                                      denominator = split_x$denom)
    
    
  } else if(split_x$type_stat[1] %in% c("sample_size", "marginals")){ #needs to be improved so we can separate the two in the report
    
    split_x$check <- check_sample_size(total_sample = split_x$reported,
                                       subgroup_cols = paste0("value", 1:20),
                                       dat = split_x)
    
  }else if(split_x$type_stat[1] %in% c("accu", "sens", "spec",  "ppv", "npv")){
    
    split_x$check <- check_test_diag(percentage = split_x$reported, 
                                     tp = split_x$tp,
                                     tn = split_x$tn,
                                     fp = split_x$fp,
                                     fn = split_x$fn,
                                     which_diag = split_x$type_stat[1])
    
  } else { #temporary solution, see preprint[[5]], which wrote type_stat = "pec_ratio"
    warning("type_stat misspelt in at least one case, examine 'check' variable for 'SPELLING' value")
    split_x$check <- "SPELLING" 
  }
  
  split_x #outputs the same dataframe but with a column called "check" {TRUE/FALSE} appended
  
}


split_check_bind <- function(x){ 
  #takes as input a dataframe with one or multiple types of type_stat to check
  
  split_x <- split(x, x$type_stat)
  split_x <- lapply(split_x, checker)
  do.call(rbind, split_x)
  
  #outputs the same dataframe but with a column called "check" {TRUE/FALSE} appended
}


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
