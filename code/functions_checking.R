################################################################################
##### FUNCTIONS FOR CHECKING RESULTS REPORTED IN PREPRINTS ON              #####
##### COVID-19 SARS-CoV-2                                                  ##### 
################################################################################

### Function to compute the accuracy, sensitivity, specificity, positive 
# predictive value, and negative predictive value
test_diag <- function(tp, tn, fp, fn, test_dec)
{
  ### Arguments:
  #     - tp = number of true positives
  #     - tn = number of true negatives
  #     - fp = number of false positives
  #     - fn = number of false negatives
  #     - test_dec = number of digits to be printed
  
  accu <- (tp+tn)/(tp+tn+fp+fn) # Accuracy
  sens <- tp/(tp+fn) # Sensitivity
  spec <- tn/(tn+fp) # Specificity
  ppv <- tp/(tp+fp) # Positive predictive value
  npv <- tn/(tn+fn) # Negative predictive value
  
  return(print(data.frame(accu = accu, sens = sens, spec = spec, ppv = ppv, 
                          npv = npv), digits = test_dec))
  
}

### Example using data in Figure 1E for the "Normal" category, p. 15 of 
# Fu et al. (2020), file "ID_964.pdf". Results of this analysis are reported in 
# Supplementary Table 1 on p. 16.
# tp <- 4945
# tn <- 2430+2408+2426+2491
# fp <- 13+19+31+10
# fn <- 11+14+15+9
# 
# test_diag(tp = tp, tn = tn, fp = fp, fn = fn)

################################################################################

### Function to compute the degrees of freedom (df) based on sample size and 
# test

df_n <- function(n, 
                 test = c("t_1sample",
                          "t_2sample",
                          "correlation",
                          "chi2_2by2",
                          "anova_1way"),
                 n_groups)
{
  ### Arguments:
  #     - n = the total sample size
  #     - test = the type of test for which df should be calculated
  #        * t_1sample: a one-sample (or paired/dependent samples) t-test
  #        * t_2sample: a two-sample (or unpaired/independenten samples) t-test
  #        * correlation: a correlation
  #        * chi2_2by2: a chi-square test related to a 2 by 2 frequency table
  #        * anova_1way: a one-way ANOVA (i.e., no interaction terms)
  #     - n_groups = number of groups/conditions being compared. 
  #       relevant for a 1-way ANOVA. Can be left blank for other tests
  
  if(test == "t_1sample"){
    
    df <- n - 1
    return(df)
    
  } else if(test == "t_2sample" | test == "correlation"){
    
    df <- n - 2
    return(df)
    
  } else if(test == "chi2_2by2"){
    
    df <- 1
    return(df)
    
  } else if(test == "anova_1way"){
    
    df1 <- n_groups - 1
    df2 <- n - n_groups
    
    return(c(df1 = df1, df2 = df2))
  }
}

################################################################################

### Function to compute the p-value based on the reported test statistic and 
# degrees of freedom

compute_p <- function(test_type = c("t", "F", "z", "cor", "chi2", "Q"),
                      test_stat,
                      df1 = NA,
                      df2 = NA,
                      test_dec,
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
  #     - test_stat: the test statistic
  #     - df1: degrees of freedom related to the design
  #     - df2: degrees of freedom related to the observations
  #     - test_dec: the number of decimals with which the test statistic was 
  #       reported. If t = 2.20, test_dec == 2
  #     - two_tailed: do you want to calculate the two-tailed p-value
  
  # take correct rounding into account
  # first calculate range of test statistics
  lower <- test_stat - (.5 / 10 ^ test_dec)
  upper <- test_stat + (.5 / 10 ^ test_dec)
  
  # compute p-values ---------------------------------------------------------
  
  # for each test, calculate the p-value that belongs to the observed test
  # statistic, but also the range of p-values that should be considered 
  # correct, if we take correct rounding of the test statistic into account
  
  if(test_type == "t"){
    
    if(is.na(df2) & !is.na(df1)) stop("df2 is missing. Did you accidentally fill them out under df1?")
    
    lower_p <- pt(-1 * abs(upper), df2)
    computed <- pt(-1 * abs(test_stat), df2)
    upper_p <- pt(-1 * abs(lower), df2)
    
  } else if(test_type == "F"){
    
    lower_p <- pf(upper, df1, df2, lower.tail = FALSE)
    computed <- pf(test_stat, df1, df2, lower.tail = FALSE)
    upper_p <- pf(lower, df1, df2, lower.tail = FALSE)
    
  } else if(test_type == "z"){
    
    lower_p <- pnorm(abs(upper), lower.tail = FALSE)
    computed <- pnorm(abs(test_stat), lower.tail = FALSE)
    upper_p <- pnorm(abs(lower), lower.tail = FALSE)
    
  } else if(test_type == "cor"){
    
    if(is.na(df2) & !is.na(df1)) stop("df2 is missing. Did you accidentally fill them out under df1?")
    
    t_lower <- r2t(lower, df2)
    t <- r2t(test_stat, df2)
    t_upper <- r2t(upper, df2)
    
    lower_p <- pt(-1 * abs(t_upper), df2)
    computed <- pt(-1 * abs(t), df2)
    upper_p <- pt(-1 * abs(t_lower), df2)
    
  } else if(test_type == "chi2" | test_type == "Q"){
    
    if(is.na(df1) & !is.na(df2)) stop("df1 is missing. Did you accidentally fill them out under df2?")
    
    lower_p <- pchisq(upper, df1, lower.tail = FALSE)
    computed <- pchisq(test_stat, df1, lower.tail = FALSE)
    upper_p <- pchisq(lower, df1, lower.tail = FALSE)
    
  }
  
  # compute two-tailed ------------------------------------------------------
  
  if (!is.na(computed) &
      (test_type == "t" | test_type == "Z" | test_type == "r") & 
      two_tailed) {
    
    lower_p <- lower_p * 2
    computed <- computed * 2
    upper_p <- upper_p *2
    
  }
  
  # return ------------------------------------------------------------------
  
  return(list(computed_p = computed,
              range = data.frame(lowest_p = lower_p,
                                 highest_p = upper_p)))
}


# Helper function to transform correlations into t-values by use of raw r and 
# degrees of freedom.
r2t <- function(r, df){
  t <- r / (sqrt((1 - r^2) / df))
  return(t)
}

################################################################################

### Function for computing t-statistic for two-independent samples design and 
# both variances assumed to be equal and unequal
indep_t <- function(m1, m2, sd1, sd2, n1, n2)
{
  
  ### Variances assumed to be equal
  pool <- sqrt((sd1^2*(n1-1)+sd2^2*(n2-1))/(n1+n2-2)) # Pooled standard deviation
  se_equal <- pool*sqrt(1/n1+1/n2) # Standard error 
  tval_equal <- (m1-m2)/se_equal
  
  ### Variances assumed to be unequal
  se_unequal <- sqrt(sd1^2/n1+sd2^2/n2) # Standard error
  tval_unequal <- (m1-m2)/se_unequal
  
  return(data.frame(tval_equal, se_equal, tval_unequal, se_unequal))
}

# ### Example using data of preprint ID_977, Table 2, p. 11, gender
# m1 <- c(7.6, 7.56, 7.64)
# m2 <- c(8, 8.04, 7.96)
# sd1 <- c(7.5, 7.46, 7.54)
# sd2 <- c(8.1, 8.06, 8.14)
# n1 <- 1068
# n2 <- 531
# 
# indep_t(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2)

################################################################################

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

################################################################################

### Function for computing Chi^2 statistic without and with continuity correction
# (only in case of a 2x2 table)
chisq_test <- function(col1, col2, text_dec)
{
  ### We assume that the contingency table has the following structure:
  
  #################################
  #          #   Y = 0     Y = 1  #
  #################################
  # X = 0    #     a         b    #
  # X = 1    #     c         d    #
  # X = ...  #    ...       ...   #
  #################################
  
  ### Arguments:
  # - col1 = cell frequencies in the first column of the contingency table
  # - col2 = cell frequencies in the second column of the contingency table
  # - text_dec = number of decimals are used for reporting the chi^2-statistic
  
  x <- matrix(c(col1, col2), ncol = 2)
  
  chi2 <- round(chisq.test(x)$statistic, text_dec)
  
  chi2_cor <- ifelse(length(col1) == 2, 
                     round(chisq.test(x, correct = FALSE)$statistic, text_dec), NA)
  
  return(data.frame(chi2 = as.numeric(chi2), chi2_cor = as.numeric(chi2_cor)))
}

# ### Example
# col1 <- c(342, 205)
# col2 <- c(726, 326)
# chisq_test(col1 = col1, col2 = col2, text_dec = 2)

################################################################################

### Function for computing effect sizes based on binary data. Currently, relative 
# risk is implemented
bin_es <- function(a, b, c, d, n1, n2, text_dec)
{
  #############################################
  #           #   Out1      Out2    # Total  #
  #############################################
  # Group 1   #     a        b      #   n1   #
  # Group 2   #     c        d      #   n2   #
  #############################################
  
  ### Check which arguments are submitted and compute cell frequencies if necessary
  if (!missing(n1) & !missing(n2))
  {
    if (!missing(a) & !missing(c))
    {
      b <- n1-a
      d <- n2-c
    } else if (!missing(b) & !missing(d))
    {
      a <- n1-b
      c <- n2-d
    }
  }
  
  p1 <- a/(a+b)
  p2 <- c/(c+d)
  rr <- round(p1/p2, text_dec)
  
  return(data.frame(rr))
}

# ### Example
# bin_es(a = 16, b = 99-16, c = 18, d = 101-18, text_dec = 3)
# bin_es(a = 16, n1 = 99, c = 18, n2 = 101, text_dec = 3)
# bin_es(b = 99-16, n1 = 99, d = 101-18, n2 = 101, text_dec = 3)