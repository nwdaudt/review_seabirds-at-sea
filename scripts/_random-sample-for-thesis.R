##
## Random selection of papers (for my PhD *thesis*)
## 
## ------------------------------------------------------------------------- ###

### Libraries ####
library(dplyr)
library(stringr)

### Files 'at_sea_ship_surveys' ####

## Path -- all files are living in my Dropbox...
path <- "/home/nicholas/Dropbox/Nico_PhD/at_sea_ship_surveys/"

# List file names inside path
files <- list.files(path = path)

# Split 'files' vector into a data.frame with four columns
files <- 
  files %>% 
  stringr::str_split_fixed(pattern = "_", n = 4) %>% 
  as.data.frame()

# Remove the last 9 rows, which are from folder/.doc names
files <- head(files, -9)

# Rename columns to meaningful names
files <- 
  files %>% 
  dplyr::transmute(year = as.numeric(V1),
                   authors = V2,
                   journal = V3,
                   title = gsub('.{4}$', '', V4)) %>%
  dplyr::filter(year >= 1971) %>%
  dplyr::filter(year < 2022)

### Files 'yet to be screened regarding backward citations' ####

files_yet_to_be_screened_for_refs <- 
  list.files(path = "/home/nicholas/Dropbox/Nico_PhD/")

# Split 'files_yet_to_be_screened' vector into a data.frame with four columns
files_yet_to_be_screened_for_refs <- 
  files_yet_to_be_screened_for_refs %>% 
  stringr::str_split_fixed(pattern = "_", n = 4) %>% 
  as.data.frame()

# Remove the last 8 rows, which are from folder or .doc names and tidy up column names
files_yet_to_be_screened_for_refs <- 
  head(files_yet_to_be_screened_for_refs, -8) %>% 
  dplyr::transmute(year = as.numeric(V1),
                   authors = V2,
                   journal = V3,
                   title = gsub('.{4}$', '', V4))

### Merge both, check duplicates, create a column indicating 5-yr bins ####

files_all <- 
  rbind(files,
        files_yet_to_be_screened_for_refs)

## Check if any duplicates, just in case... 
duplicated_rows_dplyr <- 
  files_all  %>% 
  dplyr::group_by(across(everything()))  %>% 
  dplyr::filter(n() > 1) %>% 
  dplyr::ungroup()
# > Yes -- one file that I have the paper as PDF and its Supp Mat as DOCX)

## Get rid of the duplicated line
files_all <- files_all[! duplicated(files_all),]

## Create a '5-yr' column to, next, get the random sample
files_all <- 
  files_all %>% 
  dplyr::mutate(five_yr_bin = dplyr::case_when(
    year >= 1971 & year<= 1975 ~ "1971_1975",
    year >= 1976 & year<= 1980 ~ "1976_1980",
    year >= 1981 & year<= 1985 ~ "1981_1985",
    year >= 1986 & year<= 1990 ~ "1986_1990",
    year >= 1991 & year<= 1995 ~ "1991_1995",
    year >= 1996 & year<= 2000 ~ "1996_2000",
    year >= 2001 & year<= 2005 ~ "2001_2005",
    year >= 2006 & year<= 2010 ~ "2006_2010",
    year >= 2011 & year<= 2015 ~ "2011_2015",
    year >= 2016 & year<= 2020 ~ "2016_2020"
  ), .before = year)

# barplot(table(files_all$five_yr_bin))
# barplot(table(files_all$year))

### Randomly sample 10 papers per 'five_yr_bin' ####
set.seed(1)

files_random <- data.frame()

five_yr_bins <- unique(files_all$five_yr_bin)

for(five_yr_bin in five_yr_bins){
  # print(five_yr_bin)
  
  to_filter <- as.character(five_yr_bin)
  
  tmp <- files_all %>% dplyr::filter(five_yr_bin == to_filter)
  
  tmp_sample <- tmp[sample(nrow(tmp), 10), ]
  
  files_random <- rbind(files_random,
                        tmp_sample)
  
  rm("five_yr_bin", "to_filter", "tmp", "tmp_sample")
}

# barplot(table(files_random$year))
# barplot(table(files_random$five_yr_bin))

### ---------- SAVE RANDOM PAPER SELECTION ---------- ###
write.csv(files_random,
          file = "./data/random-selection-for-thesis.csv",
          row.names = FALSE)
