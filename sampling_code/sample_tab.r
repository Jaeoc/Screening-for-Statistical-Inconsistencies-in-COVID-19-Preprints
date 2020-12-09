#********************************************************************************
##Project: Screening for statistical inconsistencies in COVID-19 preprints
##Script purpose: function to sample tables from a preprint with > 2 tables
##Code: Robbie van Aert 
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

