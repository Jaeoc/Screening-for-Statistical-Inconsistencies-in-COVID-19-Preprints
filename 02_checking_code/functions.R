#********************************************************************************
##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Source functions for checking consistency of reported results
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl), Robbie van Aert &
#Mich�le Nuijten
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
  #NB! in csv2 files, a decimal point is a comma and not a dot
  
  decimals <- lapply(a, function(x) if(length(x) == 2) nchar(x[2]) else 0)
  #the lapply is just to vectorize the function so it applies to each reported value
  #if there's a decimal,  we get two vectors from the strsplit 
  #if so count characters after decimal, else if no decimal return 0
  
  unlist(decimals) #return vector with # decimals per reported value
}


#helper function to round values ending in 5 up
round_up = function(value, decimals) { #rounds up .5, function from: https://stackoverflow.com/questions/12688717/round-up-from-5
  #value = numeric value to round
  #decimals = numeric number of decimals to round to
  posneg = sign(value)
  z = abs(value)*10^decimals
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^decimals
  z*posneg
}

#helper function to round values ending in 5 down
#this is necessary because of representation errors in certain values
#compare round(0.2225, 3) and round(0.2235, 3)
round_down <- function(value, decimals){
  #NB! this function only works for values ending in 5! it will round values with higher decimals down incorrectly.
  #value = numeric value to round
  #decimals = numeric number of decimals to round to
 posneg <- sign(value)
 z <- abs(value)*10^decimals
 z <-  z + sqrt(.Machine$double.eps)
 z <-  trunc(z)
 z <-  z/10^decimals
 z*posneg
}

#Helper function to compare computed and reported results with correct rounding
#Many people round 2.5 to 3, but many computer programs to 2
#we thus round values ending in 5 both up and down and consider both correct
compare_reported_equivalence <- function(computed, reported){
  
  decimals <- get_rounding_decimals(reported)
  
  ifelse(substr(computed, nchar(computed), nchar(computed)) == "5",{ #ifelse because vectorization
    #if a value ends in 5
    #round both up and down, and if either one matched reported result consider correct
    #note that numeric e.g., 2.500 is read by R as 2.5
    down <- as.numeric(reported) - round_down(computed, decimals) == 0
    up <- as.numeric(reported) - round_up(computed, decimals) == 0
    down + up > 0 #check if at least one rounding is correct
    
  }, as.numeric(reported) - round(computed, decimals) == 0)
}
  
compare_reported_nonequivalence <- function(computed, reported, comparison){
  #This function will only be applied to comparisons %in% c(">", "<")
    ifelse(comparison == "<",
           computed < as.numeric(reported),
           computed > as.numeric(reported))
  
}

compare_reported <- function(computed, reported, comparison){
  
  ifelse(is.na(comparison) | comparison == "=",
         compare_reported_equivalence(computed, reported), #if comparison is =
         compare_reported_nonequivalence(computed, reported, comparison)) #if comparison is <>
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
check_percentage <- function(percentage, numerator, denominator, comparison){
  computed <- numerator / denominator * 100
  
  #compare computed and reported value with same number of decimals
  compare_reported(computed, percentage, comparison)
}


#Check reported Accuracy, Sensitivity, Specificity, or
#positive/negative predictive values
check_test_diag <- function(percentage, tp, tn, fp, fn,
                            which_diag = c("accu", "sens", "spec",  "ppv", "npv"),
                            comparison){
  
  
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
  coded <- coded*100 #turn into percentage instead of proportion
  compare_reported(coded, percentage, comparison)
  
}

#Check whether reported total sample size corresponds to subgroups
#also applicable to marginal values
check_sample_size <- function(total_sample, subgroup_cols, dat){ 
  
  sum_subgroups <- rowSums(dat[,subgroup_cols], na.rm = TRUE)
  
  as.numeric(total_sample) - sum_subgroups == 0
  
}


#Function to check reported ratios against computed
check_ratio <- function(reported, n11, n12, n21, n22,
                        ratio = c("odds_ratio", "risk_ratio", "risk_diff"),
                        comparison)
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
  compare_reported(computed, reported, comparison)
}




### Function to check the reported p-value based on the reported test statistic and 
# degrees of freedom
check_p <- function(test_type = c("t_test", "F_test", "z", "r", "chi2", "Q"),
                    reported_p,
                    test_stat,
                    df1 = NA,
                    df2 = NA,
                    two_tailed = FALSE,
                    comparison)
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
  #     - df1: First observed degrees of freedom (for F-test: related to the design) [NB! Adopted so that coders always extract df1 if only 1 DF)
  #     - df2: Second observed degrees of freedom (for F-test: related to the observations)
  #     - two_tailed: assume all tests were two-tailed?
  #     - comparison: <> NA (NA == '=')
  
  
  # compute p-values ---------------------------------------------------------
  
  # for each test, calculate the p-value that belongs to the observed test
  
  if(test_type == "t_test"){
    
    computed <- pt(-1 * abs(test_stat), df1)
    
  } else if(test_type == "F_test"){
    
    computed <- pf(test_stat, df1, df2, lower.tail = FALSE)
    
  } else if(test_type == "z"){
    
    computed <- pnorm(abs(test_stat), lower.tail = FALSE)
    
  } else if(test_type == "r"){
    
    t <- r2t(test_stat, df1)
    computed <- pt(-1 * abs(t), df1)
    
  } else if(test_type == "chi2" | test_type == "Q"){
    
    computed <- pchisq(test_stat, df1, lower.tail = FALSE)
    
  }
  
  # Check and return ------------------------------------------------------------------
  #Either two-tailed only (for t, z and r)
  #or compare against both two- and one-tailed (for t, z, r)
  #For other tests (chi2, q, F) always one-tailed (see code above)
  
  if(test_type == "t_test" | test_type == "z" | test_type == "r"){
         if(two_tailed) { #if assuming all test are two-tailed tests
           
           computed <- computed * 2 #make two-tailed
           compare_reported(computed, reported_p, comparison)
           
         } else { #else allow tests to be either one- or two-tailed
           one_tailed <- compare_reported(computed, reported_p, comparison)
           two_tailed <- compare_reported(computed*2, reported_p, comparison)
           one_tailed + two_tailed > 0 #check if either one-tailed or two-tailed matches reported
         }
    
    } else{ #else if test_type == chi2, F or Q (one-tailed)
      compare_reported(computed, reported_p, comparison)
      }
  

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
                                      denominator = split_x$denom,
                                      comparison = split_x$comparison)
    
    
  } else if(split_x$type_stat[1] %in% c("sample_size", "marginals")){ #needs to be improved so we can separate the two in the report
    
    split_x$check <- check_sample_size(total_sample = split_x$reported,
                                       subgroup_cols = paste0("value", 1:40),
                                       dat = split_x)
    
  }else if(split_x$type_stat[1] %in% c("accu", "sens", "spec",  "ppv", "npv")){
    
    split_x$check <- check_test_diag(percentage = split_x$reported, 
                                     tp = split_x$tp,
                                     tn = split_x$tn,
                                     fp = split_x$fp,
                                     fn = split_x$fn,
                                     which_diag = split_x$type_stat[1],
                                     comparison = split_x$comparison)
    
  } else if(split_x$type_stat[1] %in% c("odds_ratio", "risk_ratio", "risk_diff")){
    
    split_x$check <- check_ratio(reported = split_x$reported,
                                 n11 = split_x$n11, n12 = split_x$n12, 
                                 n21 = split_x$n21, n22 = split_x$n22,
                                 ratio = split_x$type_stat[1],
                                 comparison = split_x$comparison)
    
  }else if(split_x$type_stat[1] %in% c("t_test", "F_test", "z", "r", "chi2", "Q")){
    
    split_x$check <- check_p(test_type = split_x$type_stat[1],
                             reported_p = split_x$reported,
                             test_stat = split_x$test_stat,
                             df1 = split_x$df1,
                             df2 = split_x$df2,
                             two_tailed = FALSE,
                             comparison = split_x$comparison)
    
  }else { #If misspelt, e.g., see pilot preprint[[5]], which wrote type_stat = "pec_ratio".
    split_x$check <- "SPELLING" #the function split_check_bind will throw a warning
  }
  
  split_x #outputs the same dataframe but with a column called "check" {TRUE/FALSE} appended
  
}


#Wrapper function to the above to make it applicable to multiple types of statistics
#And provide warning when type_stat was misspelt
split_check_bind <- function(x){ 
  #takes as input a dataframe with one or multiple types of type_stat to check
  
  if(!any(colnames(x) == "ID")) stop("Append column 'ID' to dataframe before running this function")
  
  #If someone saves their .csv as .csv2 we have ',' instead of '.', which will throw errors
  #this can only affect two columns. Test_stat column must be as numeric and reported as character
  x$reported <- gsub(pattern = ",",replacement = ".", x = x$reported)
  x$test_stat <- as.numeric(gsub(pattern = ",",replacement = ".", x = x$test_stat))
  
  #Temporary row order variable since the rbind below changes the order
  x$input_row_order <- formatC(1:nrow(x), #gives numbers of shape 01, 02 (for more robustness)
                               width = 2, format = "d", flag = "0")
  
  #split, apply function, recombine into dataframe
  split_x <- split(x, x$type_stat) 
  split_x <- lapply(split_x, checker)
  out <- do.call(rbind, split_x)
  
  #cleanup
  out <- out[order(out$input_row_order),] #get back input row-order
  out$input_row_order <- NULL #for neater output
  row.names(out) <- NULL #rbind automatically assigns row names
  
  #Check for any misspelt type_stat input
  spelling_mistake <- which(out$check == "SPELLING")
  
  if(length(spelling_mistake) > 0)
    warning(paste0("Spelling mistake in preprint ID ", out$ID[1], ": type_stat misspelt as '",
                   out$type_stat[spelling_mistake], "'", " in line ", spelling_mistake,
                   collapse = "\n")) #collapse \n to make each warning a new row
  
  
  #outputs the same dataframe but with a column called "check" {TRUE/FALSE} appended
  out 
  
}

