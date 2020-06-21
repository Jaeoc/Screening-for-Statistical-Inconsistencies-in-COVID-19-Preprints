#********************************************************************************

##Project: Corona-coding
##Script purpose: Example of automatic codebook checker and report generator
##Code: Anton Olsson Collentine (j.a.e.olssoncollentine@uvt.nl)

#********************************************************************************


#********************************************************************************
#Packages and sourced functions----
#********************************************************************************

if(!require(readxl)){install.packages("openxlsx")}
if(!require(openxlsx)){install.packages("openxlsx")}
#https://stackoverflow.com/questions/19414605/export-data-from-r-to-excel

library(readxl) #for reading excel files
library(openxlsx) #for writing excel files 
source("functions_checking.R")

#********************************************************************************
#Additional functions----
#********************************************************************************


check_percentile <- function(percentage, numerator, denominator){
  abs(percentage - numerator / denominator * 100) <= 0.1 #use tolerance == 0.1% to account for rounding errors
}

check_interval <- function(included){
  included == 1 #1 = yes, 0 = no
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
    
    split_x$check <- check_percentile(percentage = split_x$compare,
                                      numerator = split_x$value1,
                                      denominator = split_x$value2)
    
  } else if(split_x$type_stat[1] == "interval"){
    
    split_x$check <- check_interval(included = split_x$compare)
    
  } else if(split_x$type_stat[1] == "sample_size"){
    
    split_x$check <- check_sample_size(total_sample = split_x$compare,
                                       subgroup_cols = paste0("value", 1:9),
                                       dat = split_x)
    
  }else if(split_x$type_stat[1] %in% c("accu", "sens", "spec",  "ppv", "npv")){
    
    split_x$check <- check_test_diag(percentage = split_x$compare, 
                                     tp = split_x$value1,
                                     tn = split_x$value2,
                                     fp = split_x$value3,
                                     fn = split_x$value4,
                                     which_diag = split_x$type_stat[1])
  }
  
  split_x #out
  
}


split_check_bind <- function(x){
  
  split_x <- split(x, x$type_stat)
  split_x <- lapply(split_x, checker)
  do.call(rbind, split_x)
  
  
}

#********************************************************************************
#Create codebooks----
#********************************************************************************


#First we would create the code books so that this does not have to be done
#manually, i.e., load the pdfs and create corresponding empty excel code books.

pdfs <- list.files("../pdfs")
codebook_names <- gsub(".pdf", "", pdfs)
col_names <- c("page", "type_stat", "compare", paste0("value", 1:9), "comments")
#Note that  create 10 columns for inputting values
empty_df <- matrix(nrow = 0, ncol = length(col_names))
colnames(empty_df) <- col_names
write.xlsx(empty_df, file = paste0("../codebooks/", codebook_names, ".xlsx"))
#warning message can be ignored for now

#These have the names ID_#. When coding the coder can the add their initials
#so that the name becomes ID_#_initials


#********************************************************************************
#Run checks and print reports----
#********************************************************************************

#Load, read, and check codebooks
codebook_files <- list.files("../completed_codebooks", full.names = TRUE)
preprints <- lapply(codebook_files, read_excel)
preprint_results <- lapply(preprints, split_check_bind)

#Get preprint IDs for printing reports
preprint_ID <- unlist(strsplit(codebook_files, split = "_"))
preprint_ID <- grep("[[:digit:]]", preprint_ID, value = TRUE)

#Get when each codebook was last edited to make report nicer (see report_template.rmd)
last_edited <- file.info(codebook_files)$mtime
last_edited <- gsub(" .*", "", last_edited) #save only date and not time

#Get coder initials to make report nicer (see report_template.rmd)
initials <- gsub(".*_(.*)\\..*", "\\1", codebook_files)
#".*"matches anything (.) unlimited number of times (*)
#the content inside of "()" is the matching group, that is then used to replace
#the whole string in "\\1"

#create reports
for(preprint in seq_along(preprints)){
  
  rmarkdown::render("report_template.rmd",
                    params = list(auto_title = paste0("Consistency check for preprint ", preprint_ID[preprint])),
                    output_file = paste0("report_", preprint_ID[preprint], ".pdf"),
                    output_dir = "../reports")
}

#If we are interested in having this a continually updating project, we could then
#probably set things up so that source(computer.r) is run each time new completed
#codebooks are uploaded, as well as add some code to generate a summary dataset
#and overall report for all preprint results
