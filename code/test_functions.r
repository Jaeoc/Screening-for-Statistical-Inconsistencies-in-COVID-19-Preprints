#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Code to that functions give expected output
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl)


#********************************************************************************
#Packages, sourced functions, and test data----
#********************************************************************************
library(testthat)
source("functions.r")

dat <- readxl::read_excel("../fake_test_data_0001.xlsx", skip = 1)
dat <- dat[!is.na(dat$page),] #drop empty rows, temporary
dat$reported <- as.character(dat$reported)

#********************************************************************************
#Tests----
#********************************************************************************


dat_split <- split(dat, dat$type_stat)


test_that("Sample size correctly checked", {
  results <- checker(dat_split$sample_size)
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  expect_true(results$check[3], TRUE) #check so works with many value cols
  
})


test_that("Marginals correctly checked", {
  results <- checker(dat_split$marginals)
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
})


test_that("Perc_ratio correctly checked", {
  results <- checker(dat_split$perc_ratio)
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
})

test_that("Test_diag basic computations", {
  results <- checker(dat_split$sens) #sensitivity
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
  results <- checker(dat_split$spec) #specificity
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect. EDIT: the new rounding sets 
  
  results <- checker(dat_split$ppv) #posetive predictive value
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
  results <- checker(dat_split$npv) #negative predictive value
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
  results <- checker(dat_split$accu) #accuracy
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
})

test_that("ratios correctly checked",{
  results <- checker(dat_split$odds_ratio)
  expect_equal(results$check[1], TRUE)
  expect_equal(results$check[2], FALSE)
  
  results <- checker(dat_split$risk_ratio)
  expect_equal(results$check[1], TRUE)
  expect_equal(results$check[2], FALSE)
  
  results <- checker(dat_split$risk_diff)
  expect_equal(results$check[1], TRUE)
  expect_equal(results$check[2], FALSE)
})

#Rounding
rounding_values <- c(0,
                     0.1,
                     0.02,
                     2.37,
                     24.1,
                     37,
                     38.9,
                     0.00002,
                     1.5, #for testing the rounding up/down
                     0.005,
                     25)

rounding_values <- as.character(rounding_values)

test_that("Rounding down and up from .5", {
  expect_equal(round_up(2.5, 0), 3)
  expect_equal(round_down(0.0003445, 6), 0.000344) #these values rounded up with round() because of representation error
  expect_equal(round_down(0.2235, 3), 0.223) #on my machine
  
})



test_that("Rounding decimals function output", {
  out <- get_rounding_decimals(rounding_values)
  
  expect_is(out, "numeric")
  expect_length(out, length(rounding_values))
  
})

test_that("Comparing computed/reported rounding", {
  expect_is(compare_reported(reported = "0.23", computed = 0.23), "logical") #check output type
  expect_equal(compare_reported(reported = "0.23", computed = 0.23), TRUE)
  expect_equal(compare_reported(reported = "0.23", computed = 0.235), TRUE) #both round up and round down
  expect_equal(compare_reported(reported = "0.23", computed = 0.225), TRUE) #should give correct
  expect_equal(compare_reported(reported = "1.23", computed = 1.23234234), TRUE)
  expect_equal(compare_reported(reported = "0.237", computed = 0.2353), FALSE)
})
