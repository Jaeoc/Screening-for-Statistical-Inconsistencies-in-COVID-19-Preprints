#********************************************************************************

##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: Code to check consistency of reported values in preprints
##Code: 
#Anton Olsson-Collentine (j.a.e.olssoncollentine@uvt.nl)


#********************************************************************************
#Packages and sourced functions----
#********************************************************************************

#Checking functions
source("functions.R") 

#Helper function to read in csv files whether separated by , or ;
#inspired from: https://stackoverflow.com/questions/33417242/how-to-check-if-csv-file-has-a-comma-or-a-semicolon-as-separator
read_csv1_and2 <- function(csv_file, ...){
  line1 <- readLines(csv_file, n = 1)
  if(grepl(";", line1)) read.csv2(csv_file, ..., dec = ",") else read.csv(csv_file, ...)
}
#********************************************************************************
#Run checks and save results----
#********************************************************************************
#Note: if multiple coder have coded the same file for pilot testing, these must be loaded separately
#e.g., by adding codebook_files <- grep(pattern = "MN", x = codebook_files, value = TRUE)

#Change "." to relevant folder later
codebook_files <- list.files(".", pattern = ".csv",
                             full.names = TRUE)

#Read completed codebooks
preprints <- lapply(codebook_files, read_csv1_and2, header = TRUE, skip = 1,
                    stringsAsFactors = FALSE, na.strings = c(""), 
                    colClasses = c(reported = "character"))

#a) Reading in the reported column as a character is important, because it 
#allows us to compare our computed value to the specificity used in the preprint
#(i.e., to the same decimal). 
#b) If we don't specify na.strings then for character vectors the "" is not read as NA


#Drop rows with only NAs.
preprints <- lapply(preprints, function(x) x[!rowSums(is.na(x)) == ncol(x),])

#Get preprint IDs 
preprint_ID <- unlist(strsplit(codebook_files, split = "_|\\.")) #split by _ and . ('.' because of .csv)
preprint_ID <- grep("[[:digit:]]", preprint_ID, value = TRUE) #get IDs, assumes no other numbers in file path

#Append preprint IDs to each dataframe. NB! must be done before applying the split_check_bind function
#This enables informative warning messages if something is misspelt
for(preprint in seq_along(preprint_ID)){
  preprints[[preprint]]$ID <- preprint_ID[preprint]
  names(preprints)[preprint] <- paste0("ID_", preprint_ID[preprint]) #also add list-name for easier back-tracking
}

#Check reported values
preprint_results <- lapply(preprints, split_check_bind)

#drop empty columns for more readable results. 
#because sample_size checker function uses all columns value1:20 can't drop earlier
#EDIT: dropping columns becomes problematic if we wish to collate results
#Better to leave for now
#preprint_results <- lapply(preprint_results, function(x) x[,!colSums(is.na(x)) == nrow(x)])

#It because data is input in excel, it is possible to get extra columns (named e.g., x, x1 etc)
#drop any of these so we can collate results
coded_columns <- c('page','type_stat','reported','num','denom','tp','tn','fp','fn','comparison','test_stat','df1',
             'df2','n11','n12','n21','n22','value1','value2','value3','value4','value5','value6','value7',
             'value8','value9','value10','value11','value12','value13','value14','value15','value16','value17',
             'value18','value19','value20', 'value21', 'value22', 'value23', 'value24', 
             'value25', 'value26', 'value27', 'value28', 'value29', 'value30', 'value31', 
             'value32', 'value33', 'value34', 'value35', 'value36', 'value37', 'value38', 
             'value39', 'value40', 'ID','check') 
preprint_results <- lapply(preprint_results, function(x) x[,coded_columns])

#Collate and save dataset for analysis
preprint_results <- do.call(rbind, preprint_results)
# save_name <- "summary_round1.csv" #add save name
write.csv(preprint_results, save_name, row.names = FALSE)
