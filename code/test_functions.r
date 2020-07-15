#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Code to that functions give expected output
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl)


#********************************************************************************
#Packages, sourced functions, and test data----
#********************************************************************************
library(testthat)
source("functions_checking.r")

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

test_that("Test_diag correctly checked", {
  results <- checker(dat_split$sens) #sensitivity
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
  results <- checker(dat_split$spec) #specificity
  expect_equal(results$check[1], TRUE) #identify correct
  expect_equal(results$check[2], FALSE) #identify incorrect
  
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

tp <- 90
tn <- 24
fp <- 8
fn <- 10

#TO DO
#Write testing functions to ensure the rounding comparison is working properly


#********************************************************************************
#Functions being tested----
#********************************************************************************


