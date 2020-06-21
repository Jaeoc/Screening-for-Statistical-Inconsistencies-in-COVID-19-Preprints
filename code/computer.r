#********************************************************************************

##Project: Screening for staistical Inconsistencies in COVID-19 reprints
##Script purpose: Code to check consistency of reported values coded from preprints
##Code: 
#Anton Olsson Collentine (j.a.e.olssoncollentine@uvt.nl)
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
#Move these functions to the file "functions_checking.r" after finishing amending
#the remaining functions

check_percentile <- function(percentage, numerator, denominator){
  abs(percentage - numerator / denominator * 100) <= 0.1 #use tolerance == 0.1% to account for rounding errors
}


#reformulated this function to fit
check_test_diag <- function(percentage, tp, tn, fp, fn, which_diag = c("accu", "sens", "spec",  "ppv", "npv")){
  
  
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


check_sample_size <- function(total_sample, subgroup_cols, dat){
  
  sum_subgroups <- rowSums(dat[,subgroup_cols], na.rm = TRUE)
  
  (total_sample - sum_subgroups) == 0
  
}

checker <- function(split_x){ #can probably be improved
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
  }
  
  split_x #out
  
}


split_check_bind <- function(x){
  
  split_x <- split(x, x$type_stat)
  if(is.data.frame(x)) checker(x) #If only one type of type_stat
  else {
    split_x <- lapply(split_x, checker)
    do.call(rbind, split_x)
  }
  
  
}


#********************************************************************************
#Run checks----
#********************************************************************************

#Load, read, and check codebooks
codebook_files <- list.files("../pilot_codebooks", full.names = TRUE)
#Change "pilot_codebooks" to relevant folder later
preprints <- lapply(codebook_files, read.csv, header = TRUE, skip = 1)
#Drop rows with only Nas. NB, type_stat is read as empty but all other empty cells as NAs 
preprints <- lapply(preprints, function(x) x[!is.na(x$page),]) #drop



#Check
preprint_results <- lapply(preprints, split_check_bind)

#problems:
#also drop all columns with only empty?
#not all sample size check for [[7]] returns NA for some reason
