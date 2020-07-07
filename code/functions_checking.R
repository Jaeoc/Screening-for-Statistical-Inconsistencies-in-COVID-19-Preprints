#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Source functions for checking consistency of reported results
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl), Robbie van Aert &
#Michèle Nuijten
#********************************************************************************


#********************************************************************************
#Helper functions----
#********************************************************************************

#helper function to ensure we compare computed and reported values with the same
#number of decimals
#takes as input a numeric value as a character and outputs number of decimals 
get_rounding_decimals <- function(reported){ 
  #NB! Assumes the "reported" column was read in as a character variable!
  
  a <- strsplit(reported, "\\.")
  #split by decimal point, if there is one
  
  decimals <- lapply(a, function(x) if(length(x) == 2) nchar(x[2]) else 0)
  #the lapply is just to vectorize the function so it applies to each reported value
  #if there's a decimal,  we get two vectors from the strsplit 
  #if so count characters after decimal, else if no decimal return 0
  
  unlist(decimals) #return vector of decimals per reported value
}


#Helper function for 'check_p' function below
#transform correlation to t-statistic
r2t <- function(r, df){
  r / (sqrt((1 - r^2) / df))
}
#********************************************************************************
#Internal functions----
#********************************************************************************

#All functions in this section output a TRUE/FALSE value
#Because the reported value is loaded as a character vector it must always be
#converted into a numeric value before comparison

#Check reported percentage
check_percentage <- function(percentage, numerator, denominator){
  decimals <- get_rounding_decimals(percentage)
  computed <- numerator / denominator * 100
  
  #compare computed and reported value with same number of decimals
  as.numeric(percentage) - round(computed, decimals) == 0
}


#Check reported Accuracy, Sensitivity, Specificity, or
#positive/negative predictive values
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
  
  #compare computed and reported value with same number of decimals
  decimals <- get_rounding_decimals(percentage)
  as.numeric(percentage) - round(coded*100, decimals) == 0
  
}

#Check whether reported total sample size corresponds to subgroups
#also applicable to marginal values
check_sample_size <- function(total_sample, subgroup_cols, dat){ 
  
  sum_subgroups <- rowSums(dat[,subgroup_cols], na.rm = TRUE)
  
  as.numeric(total_sample) - sum_subgroups == 0
  
}


#Function to check reported ratios against computed
check_ratio <- function(reported, n11, n12, n21, n22,
                        ratio = c("odds_ratio", "risk_ratio", "risk_diff"))
{
  ### ROBBIE: NOW USING n11, n12, n21, AND n22 INSTEAD OF a, b, c, AND d. THIS 
  # IS BETTER BECAUSE c ALSO REFERS TO CONCATINATING OBJECTS
  
  #********************************************
  #           #   Out1    Out2    # Total  #
  #********************************************
  # Group 1   #   n11      n12      #   n1   #
  # Group 2   #   n21      n22      #   n2   #
  #********************************************
  #https://handbook-5-1.cochrane.org/chapter_9/box_9_2_a_calculation_of_risk_ratio_rr_odds_ratio_or_and.htm
  
  if(ratio == "odds_ratio"){
    
    p1 <- n11/n12
    p2 <- n21/n22
    computed <- p1/p2
    
  }else if(ratio == "risk_ratio"){
    
    p1 <- n11/(n11+n12)
    p2 <- n21/(n21+n22)
    computed <- p1/p2
    
  }else if(ratio == "risk_diff"){
    
    p1 <- n11/(n11+n12)
    p2 <- n21/(n21+n22)
    computed <- p1 - p2
  }
  
  #compare computed and reported value with same number of decimals
  decimals <- get_rounding_decimals(reported)
  as.numeric(reported) - round(computed, decimals) == 0
}




### Function to check the reported p-value based on the reported test statistic and 
# degrees of freedom
check_p <- function(test_type = c("t", "F", "z", "r", "chi2", "Q"),
                    reported_p,
                    test_stat,
                    df1 = NA,
                    df2 = NA,
                    two_tailed = TRUE)
{
  ### Arguments:
  #     - test_type: the type of test you want to calculate the p-value for
  #        * t = t-test
  #        * F = F-test
  #        * z = z-test
  #        * cor = correlation
  #        * chi2 = chi-squared-test
  #        * Q = Q-test
  #     - reported_p: reported p-value
  #     - test_stat: the test statistic
  #     - df1: degrees of freedom related to the design
  #     - df2: degrees of freedom related to the observations
  #     - two_tailed: do you want to calculate the two-tailed p-value
  
  
  #EDIT: we can't check number of decimals automatically because is set to
  #consistent across a column. May just have to decide on a tolerance in the
  #final output p-value
  
  #EDIT: instead of having checks of the DFs here, it is better to have them in
  #the wrapper function "checker"
  
  ### ROBBIE: WE MAY WANT TO CHANGE THIS FUNCTION TO AVOID USING t AND F AS OBJECTS.
  
  # compute p-values ---------------------------------------------------------
  
  # for each test, calculate the p-value that belongs to the observed test
  
  if(test_type == "t"){
    
    computed <- pt(-1 * abs(test_stat), df2)
    
  } else if(test_type == "F"){
    
    computed <- pf(test_stat, df1, df2, lower.tail = FALSE)
    
  } else if(test_type == "z"){
    
    computed <- pnorm(abs(test_stat), lower.tail = FALSE)
    
  } else if(test_type == "r"){
    
    t <- r2t(test_stat, df2)
    computed <- pt(-1 * abs(t), df2)
    
  } else if(test_type == "chi2" | test_type == "Q"){
    
    computed <- pchisq(test_stat, df1, lower.tail = FALSE)
    
  }
  
  # compute two-tailed ------------------------------------------------------
  
  if (!is.na(computed) &
      (test_type == "t" | test_type == "z" | test_type == "r") & 
      two_tailed) {
    
    computed <- computed * 2
    
  }
  # Check and return ------------------------------------------------------------------
  
  #compare computed and reported value with same number of decimals
  decimals <- get_rounding_decimals(reported_p)
  as.numeric(reported_p) - round(computed, decimals) == 0 
}


#********************************************************************************
#Wrapper functions----
#********************************************************************************

#Main function that checks reported statistics against recomputed
checker <- function(split_x){
  #takes as input a dataframe with a single type of type_stat
  #split by type_stat before applying function
  
  if(split_x$type_stat[1] == "perc_ratio"){
    
    split_x$check <- check_percentage(percentage = split_x$reported,
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
    
  } else if(split_x$type_stat[1] %in% c("odds_ratio", "risk_ratio", "risk_diff")){
    
    split_x$check <- check_ratio(reported = split_x$reported,
                                 n11 = split_x$n11, n12 = split_x$n12, 
                                 n21 = split_x$n21, n22 = split_x$n22,
                                 ratio = split_x$type_stat[1])
    
  }else if(split_x$type_stat[1] %in% c("t", "F", "z", "r", "chi2", "Q")){
    
    ### ROBBIE: ASSUMING THAT ALL P-VALUES ARE TWO-TAILED IS TRICKY. MAYBE 
    # COMPUTING P-VALUES BOTH ONE AND TWO-TAILED AND IF BOTH ARE DIFFERENT 
    # THEN CONCLUDING THAT IT IS INCONSISTENT.
    
    split_x$check <- check_p(test_type = split_x$type_stat[1],
                             reported_p = split_x$reported,
                             test_stat = split_x$test_stat,
                             df1 = split_x$df1,
                             df2 = split_x$df2,
                             two_tailed = TRUE)
    
  }else { #If misspelt, see preprint[[5]], which wrote type_stat = "pec_ratio".
    warning("type_stat misspelt in at least one case, examine 'check' variable for 'SPELLING' value")
    split_x$check <- "SPELLING" 
  }
  
  split_x #outputs the same dataframe but with a column called "check" {TRUE/FALSE} appended
  
}

#temporary
preprint_ID <- unlist(strsplit(codebook_files, split = "_|\\.")) #split by _ and . ('.' because of .csv)
preprint_ID <- grep("[[:digit:]]", preprint_ID, value = TRUE) #get IDs, assumes no other numbers in file path

#Wrapper function to the above to make it applicable to multiple types of statistics
split_check_bind <- function(x, preprint_ID){ 
  #takes as input a dataframe with one or multiple types of type_stat to check
  
  split_x <- split(x, x$type_stat)
  
  ### ROBBIE: ALTHOUGH I REALLY LIKE THE lapply() IMPLEMENTATION HERE, IT MIGHT 
  # BE BETTER TO USE A FOR LOOP FOR THIS. THE REASON IS THAT WE CAN VERY QUICKLY 
  # TRACE BACK WHERE type_stat WAS MISSPELLED IN CASE OF A FOR LOOP. THAT IS, 
  # WE CAN THROW THE WARNING THAT IT IS IN PREPRINT NUMBER X, LINE Y, AND IT WAS
  # MISSPELLED AS "pec_ratio". THIS CANNOT EASILY BE DONE WITH lapply().
  
    #EDIT Anton: can be done by adding a line above the first split_x exctracting
  #preprint ID, and another line behind do.call that prints a warning and
  #rownumber/input value if it finds a SPELLING value in the check variable
  #Might even be better to implement at a higher level, the user level?
  # -> No, would make things messy
  
  split_x <- lapply(split_x, checker)
  p
  names(split_x) <- NULL # To avoid automatically assigning row names
  do.call(rbind, split_x)
  
  #outputs the same dataframe but with a column called "check" {TRUE/FALSE} appended
}


#********************************************************************************
#Other functions----
#********************************************************************************

### Function to sample two tables that need to be extracted from a preprint
sample_tab <- function(nr_tab, pp_ID)
{
  ### Arguments:
  # - nr_tab = total number of tables in preprint
  # - pp_ID = Preprint ID
  
  set.seed(pp_ID) # Seed based on preprint ID
  
  extr_tab <- sample(1:nr_tab, size = 2)
  
  print(paste0("Tables ", extr_tab[1], " and ", extr_tab[2], " need to be extracted"))
}

# ### Example
# sample_tab(nr_tab = 4, pp_ID = 977)

