#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Source functions for checking consistency of reported results
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl) &
#Robbie van Aert
#********************************************************************************


#********************************************************************************
#Internal functions----
#********************************************************************************
#All functions in this section output a TRUE/FALSE value
#except the small helper function r2t which outputs a t-statistic


#Check reported percentage
check_percentage <- function(percentage, numerator, denominator){
  abs(percentage - numerator / denominator * 100) <= 0.1 #use tolerance == 0.1% to account for rounding errors
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
  
  abs(percentage - coded*100) <= 0.1 #use tolerance == 0.1% to account for rounding errors
  
}

#Check whether reported total sample size corresponds to subgroups
#also applicable to marginal values
check_sample_size <- function(total_sample, subgroup_cols, dat){ 
  
  sum_subgroups <- rowSums(dat[,subgroup_cols], na.rm = TRUE)
  
  (total_sample - sum_subgroups) == 0
  
}


#Function to check reported ratios against computed
check_ratio <- function(reported, a, b, c, d,
                        ratio = c("odds_ratio", "risk_ratio", "risk_diff"))
{
  #********************************************
  #           #   Out1      Out2    # Total  #
  #********************************************
  # Group 1   #     a        b      #   n1   #
  # Group 2   #     c        d      #   n2   #
  #********************************************
  #https://handbook-5-1.cochrane.org/chapter_9/box_9_2_a_calculation_of_risk_ratio_rr_odds_ratio_or_and.htm
  
  ratio = match.arg(ratio)
  
  if(ratio = "odds_ratio"){
    
    p1 <- a/c
    p2 <- b/d
    computed = p1/p2
    
  }else if(ratio == "risk_ratio"){
    
    p1 <- a/(a+b)
    p2 <- c/(c+d)
    computed <- p1/p2
    
  }else if(ratio = "risk_diff"){
    
    p1 <- a/(a+b)
    p2 <- c/(c+d)
    computed <- p1 - p2
  }
  
  abs(reported - computed) <= 0.01
  #check that rounding is correct to the second decimal
}


#Helper function for 'check_p' function below
#transform correlation to t-statistic
r2t <- function(r, df){
  r / (sqrt((1 - r^2) / df))
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
  
  abs(reported_p - computed) <= 0.001 
  #use tolerance == 0.001 to account for rounding errors
  #in other words, check that rounding to the third decimal is correct but not beyond
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
                                 a = split_x$a, b = split_x$b, c = split_x$c, d = split_x$d,
                                 ratio = split_x$type_stat[1])
    
  }else if(split_x$type_stat[1] %in% c("t", "F", "z", "r", "chi2", "Q")){
    
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


#Wrapper function to the above to make it applicable to multiple types of statistics
split_check_bind <- function(x){ 
  #takes as input a dataframe with one or multiple types of type_stat to check
  
  split_x <- split(x, x$type_stat)
  split_x <- lapply(split_x, checker)
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

