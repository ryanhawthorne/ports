# Using tabulaPDF to extract pages

library(tabulapdf)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

# set path to directory where files are saved
pdf_dir <- "G:\\My Drive\\OV - ports\\Transnet\\Vessels\\"


## starting 2018 and 2019
# year by year, select areas manually, starting with 2018-2019
f <- paste0(pdf_dir,"201801.pdf")

# manually select areas
# locate_areas(f)

# Get a list of all PDF files in the directory for 2018 and 2019
pdf_files1819 <- list.files(pdf_dir, pattern = "20(1[8-9])(0[1-9]|1[0-2])\\.pdf", full.names = TRUE)
yearmon <- str_extract(pdf_files1819, "20(1[8-9])(0[1-9]|1[0-2])")


# Initialize an empty list to store extracted tables
extracted_data1819 <- list()

# Loop over each PDF file
for (pdf in pdf_files1819) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
# Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c(42.07980,  45.98583, 322.78050, 786.35769)),
                             guess = FALSE)
    
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "20(1[8-9])(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the year and month as the key
    extracted_data1819[[yearmon]] <- tables
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}

## now 2020

# Get a list of all PDF files in the directory for 2020
f <- paste0(pdf_dir,"202001.pdf")

# manually select areas
# locate_areas(f)

# Get a list of all PDF files in the directory for Jan, Feb 2020
pdf_files20JanFeb <- list.files(pdf_dir, pattern = "2020(0[1-2])\\.pdf", full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data20JanFeb <- list()

# Loop over each PDF file
for (pdf in pdf_files20JanFeb) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c(42.07980,  52.11727, 284.43341, 786.35769)),
                             guess = FALSE)
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "2020(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data20JanFeb[[yearmon]] <- tables
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}

# March-Dec 2020
f <- paste0(pdf_dir,"202003.pdf")
# locate_areas(f)

# Get a list of all PDF files in the directory for Jan, Feb 2020
pdf_files20Mar21Mar <- list.files(pdf_dir, pattern = "(2020(0[3-9]|1[0-2])|2021(0[1-3]))\\.pdf", 
                                  full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data20Mar21Mar <- list()

# Loop over each PDF file
for (pdf in pdf_files20Mar21Mar) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c(76.11990,  43.57229, 326.24565, 620.00518)),
                             guess = FALSE)
    
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "(2020|2021)(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data20Mar21Mar[[yearmon]] <- tables
    
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}

## 2021 April and May
f <- paste0(pdf_dir,"202104.pdf")
# locate_areas(f)

table <-  extract_tables(f,
                                       area = list(c(76.4,  43.57229, 326.24565, 620.00518)),
                                                 guess = FALSE)

extracted_data21Apr <- list("202104" = table)

f <- paste0(pdf_dir,"202105.pdf")

table <-  extract_tables(f,
                                       area = list(c(76.4,  43.57229, 326.24565, 620.00518)),
                                       guess = FALSE)
extracted_data21May <- list("202105" = table)

# 2021 June - 

#f <- paste0(pdf_dir,"202201.pdf")
#locate_areas(f)

#77.77526  42.02293 326.24108 622.1660

# 2021 Nov

# f <- paste0(pdf_dir,"202111.pdf")
# locate_areas(f) 
#  74.11885  49.06122 326.95417 618.04082 

f <- paste0(pdf_dir,"202111.pdf")

table <-  extract_tables(f,
                         area = list(c(74.11885,  49.06122, 326.95417, 618.04082)),
                         guess = FALSE)
extracted_data21Nov <- list("202111" = table)


# 2021 Dec

#f <- paste0(pdf_dir,"202112.pdf")
#locate_areas(f)

#76.73395  44.55703 327.89725 623.69922 

# Get a list of all PDF files in the directory for June-Dec 2021, except November which did not follow pattern
pdf_files21JunDec <- list.files(pdf_dir, pattern = "2021(0[6-9]|10|12)\\.pdf", 
                                  full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data21JunDec <- list()
# note August missing on TNPA website

# Loop over each PDF file
for (pdf in pdf_files21JunDec) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c(76.7,  47.57229, 330, 620.9518)),
                             guess = FALSE)
    
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "(2021)(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data21JunDec[[yearmon]] <- tables
    
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}


# 2022 Jan
f <- paste0(pdf_dir,"202201.pdf")
# locate_areas(f)

table <-  extract_tables(f,
                                       area = list(c( 66.02221,27.11440,347.58751,774.28600 )),
                                       guess = FALSE)
extracted_data22Jan <- list("202201" = table)

# 2022 Mar (no feb)
f <- paste0(pdf_dir,"202203.pdf")
# locate_areas(f)

table <-  extract_tables(f,
                                       area = list(c(96.95403,18.59502,399.14842,816.26864)),
                                       guess = FALSE)

extracted_data22Mar <- list("202203" = table)

# 2022 Apr
f <- paste0(pdf_dir,"202204.pdf")

 table <-  extract_tables(f,
                                       area = list(c(43.36753,  22.25212, 324.93283, 772.66524)),
                                       guess = FALSE)
extracted_data22Apr <- list("202204" = table)

# 

# 2022 May
f <- paste0(pdf_dir,"202205.pdf")
table <-  extract_tables(f,
                                       area = list(c(57.91475,  57.35185, 336.47213, 788.74168)),
                                       guess = FALSE)
extracted_data22May <- list("202205" = table)

#2022 June-Aug
# first check size
f <- paste0(pdf_dir,"202206.pdf")
# locate_areas(f)

# Get a list of all PDF files in the directory for 2022
pdf_files22JunJul <- list.files(pdf_dir, pattern = "2022(0[6-7])\\.pdf", 
                                full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data22JunJul <- list()

# note August missing on TNPA website

# Loop over each PDF file
for (pdf in pdf_files22JunJul) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c( 86.34898,  13.84076, 273.29006, 577 
 )),
                             guess = FALSE)
    
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "(2022)(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data22JunJul[[yearmon]] <- tables
    
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}

# 2022 Aug
f <- paste0(pdf_dir,"202208.pdf")
table <-  extract_tables(f,
                                       area = list(c(32.93724,13.17636,246.58419,577.11977)),
                                       guess = FALSE)

extracted_data22Aug <- list("202208" = table)

# 2022 Sep - Feb 2023
# Get a list of all PDF files in the directory for 2022
pdf_files22Sep23Jan <- list.files(pdf_dir, pattern = "(2022(0[9]|1[0-2])|20230[1-2])\\.pdf", 
                                full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data22Sep23Jan <- list()

# note August missing on TNPA website

# Loop over each PDF file
for (pdf in pdf_files22Sep23Jan) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c( 40.60328, 11.63999, 341.19344, 795.04677)),
                             guess = FALSE)
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "(2022|2023)(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data22Sep23Jan[[yearmon]] <- tables
    
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}

# sep 40.60328  11.63999 341.19344 806.08067
# 2022 nov 40.60328  19.52134 341.19344 824.99592 
# 2022 dec 40.60328  30.55524 323.88196 795.04677
# 2023 01 39.02951  13.21626 341.19344 813.96202 

# 2023 Feb

f <- paste0(pdf_dir,"202302.pdf")
table <-  extract_tables(f,
                                       area = list(c(51.61967,11.63999,355.35737,807.65694)),
                                       guess = FALSE)

extracted_data23Feb <- list("202302" = table)

#202303 100.09180  15.42303 405.40327 816.16879 

f <- paste0(pdf_dir,"202303.pdf")
table <- extract_tables(f,
                                       area = list(c(100.09180,15.42303,405.40327,816.16879)),
                                       guess = FALSE)
extracted_data23Mar <- list("202303" = table)

# 202304  43.75082  32.13151 336.47213 813.96202 

f <- paste0(pdf_dir,"202304.pdf")
table <-  extract_tables(f,
                                       area = list(c(43.75082,32.13151,336.47213,813.96202)),
                                       guess = FALSE)

extracted_data23Apr <- list("202304" = table)

# 202305  57.91475  68.38575 338.04590 780.86033 
f <- paste0(pdf_dir,"202305.pdf")
table <-  extract_tables(f,
                                       area = list(c(57.91475,  68.38575, 338.04590, 780.86033)),
                                       guess = FALSE)
extracted_data23May <- list("202305" = table)

# 202306  88.57446  51.06979 275.51555 532.53927 
f <- paste0(pdf_dir,"202306.pdf")
table <-  extract_tables(f,
                                       area = list(c(88.57446,  51.06979, 275.51555, 532.53927)),
                                       guess = FALSE)
extracted_data23Jun <- list("202306" = table)

# 202307  88.57446  15.40538 313.34886 583.80685
f <- paste0(pdf_dir,"202307.pdf")
 table <-  extract_tables(f,
                                       area = list(c(88.57446,  15.40538, 313.34886, 583.80685)),
                                       guess = FALSE)
extracted_data23Jul <- list("202307" = table)

# 202308  32.93724  13.17636 204.29990 583.80685 
f <- paste0(pdf_dir,"202308.pdf")
table <-  extract_tables(f,
                                       area = list(c(32.93724,  13.17636, 204.29990, 583.80685)),
                                       guess = FALSE)
extracted_data23Aug <- list("202308" = table)


# 202309  43.75082  13.21626 353.78360 813.96202 
f <- paste0(pdf_dir,"202309.pdf")
table <-  extract_tables(f,
                                       area = list(c(43.75082,  13.21626, 353.78360, 813.96202)),
                                       guess = FALSE)
extracted_data23Sep <- list("202309" = table)

# 202310   42.17705  11.63999 353.78360 812.38575 
f <- paste0(pdf_dir,"202310.pdf")
table <-  extract_tables(f,
                                       area = list(c(42.17705,  11.63999, 353.78360, 812.38575)),
                                       guess = FALSE)
extracted_data23Oct <- list("202310" = table)

# 202311   37.45574  19.52134 345.91475 795.04677 
# 202312 40.60328  35.28405 339.61967 787.16541 
# 202401 42.17705  10.06371 358.50491 813.96202 

# Get a list of all PDF files in the directory for 2022
pdf_files23Nov24Jan <- list.files(pdf_dir, pattern = "(2023(1[1-2])|202401)\\.pdf", 
                                  full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data23Nov24Jan <- list()

# Loop over each PDF file
for (pdf in pdf_files23Nov24Jan) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c( 37.45574, 10.06371, 358.50491, 813.96202)),
                             guess = FALSE)
    
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "(202[3-4])(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data23Nov24Jan[[yearmon]] <- tables
    
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}


# 202402  56.34098  13.21626 361.65245 813.96202 
f <- paste0(pdf_dir,"202402.pdf")
table <-  extract_tables(f,
                                       area = list(c(56.34098,  13.21626, 361.65245, 813.96202)),
                                       guess = FALSE)
extracted_data24Feb <- list("202402" = table)

# 202403 100.091802   9.117942 408.550815 825.626419 
f <- paste0(pdf_dir,"202403.pdf")
table <-  extract_tables(f,
                                       area = list(c(100.091802,   9.117942, 408.550815, 825.626419)),
                                       guess = FALSE)
extracted_data24Mar <- list("202403" = table)

# 202404  40.60328  46.31795 339.61967 795.04677 
f <- paste0(pdf_dir,"202404.pdf")
table <-  extract_tables(f,
                                       area = list(c(40.60328,  46.31795, 339.61967, 795.04677)),
                                       guess = FALSE)
extracted_data24Apr <- list("202404" = table)

# 202405   57.91475  66.80948 341.19344 782.43660 
f <- paste0(pdf_dir,"202405.pdf")
table <-  extract_tables(f,
                                       area = list(c(57.91475,  66.80948, 341.19344, 782.43660)),
                                       guess = FALSE)
extracted_data24May <- list("202405" = table)

# 202406 86.34898  48.84076 277.74104 528.08122 
f <- paste0(pdf_dir,"202406.pdf")
table <-  extract_tables(f,
                                       area = list(c(86.34898,  48.84076, 277.74104, 528.08122)),
                                       guess = FALSE)
extracted_data24Jun <- list("202406" = table)

# 202407  88.57446  13.17636 317.79984 581.57782 
f <- paste0(pdf_dir,"202407.pdf")
table <-  extract_tables(f,
                                       area = list(c(88.57446,  13.17636, 317.79984, 581.57782)),
                                       guess = FALSE)
extracted_data24Jul <- list("202407" = table)

# 202408  35.16273  13.17636 264.38810 586.03587 
f <- paste0(pdf_dir,"202408.pdf")
table <-  extract_tables(f,
                                       area = list(c(35.16273,  13.17636, 264.38810, 586.03587)),
                                       guess = FALSE)
extracted_data24Aug <- list("202408" = table)

# 202409  43.75082  11.63999 364.80000 813.96202 
# 202410 42.17705  11.63999 361.65245 813.96202 
# 202411 40.60328  19.52134 350.63606 798.19931 
# 202412 40.60328  36.86032 344.34098 793.47050 

# Get a list of all PDF files in the directory for 2024 September-December
pdf_files24SepDec <- list.files(pdf_dir, pattern = "2024(0[9]|1[0-2])\\.pdf", 
                                full.names = TRUE)

# Initialize an empty list to store extracted tables
extracted_data24SepDec <- list()

# Loop over each PDF file
for (pdf in pdf_files24SepDec) {
  cat("Processing:", pdf, "\n")  # Print the file being processed
  
  # Try to extract tables, handle errors if any
  tryCatch({
    tables <- extract_tables(pdf,
                             area = list(c(40.60328,  11.63999, 364.80000, 813.96202 )),
                             guess = FALSE)
    
    # extract the year and month from name of pdf file
    yearmon <- str_extract(pdf, "2024(0[1-9]|1[0-2])")
    
    # Store the extracted tables in the list with the filename as the key
    extracted_data24SepDec[[yearmon]] <- tables
    
  }, error = function(e) {
    cat("Error processing file:", pdf, "\n", e$message, "\n")
  })
}

# 202501  39.02951  13.27625 358.50491 817.17456 
f <- paste0(pdf_dir,"202501.pdf")
table <-  extract_tables(f,
                                       area = list(c(39.02951,  13.27625, 358.50491, 817.17456)),
                                       guess = FALSE)
extracted_data25Jan <- list("202501" = table)


# 202502  55.653190   7.083923 355.602200 763.755459 
f <- paste0(pdf_dir,"202502.pdf")
table <-  extract_tables(f,
                                       area = list(c( 55.653190, 7.083923, 355.602200, 763.755459 )),
                                       guess = FALSE)
extracted_data25Feb <- list("202502" = table)

# tester
f <- paste0(pdf_dir,"202502.pdf")
# locate_areas(f)

# create empty list
extracted_data <- list()

# combine the lists of data into one big list
extracted_data <- c(extracted_data1819, extracted_data20JanFeb, extracted_data20Mar21Mar, 
                    extracted_data21Apr, extracted_data21May, extracted_data21JunDec,
                    extracted_data21Nov, extracted_data22Jan, 
                    extracted_data22Mar, extracted_data22Apr, extracted_data22May, 
                    extracted_data22JunJul, extracted_data22Aug, extracted_data22Sep23Jan, 
                    extracted_data23Feb, extracted_data23Mar, extracted_data23Apr, 
                    extracted_data23May, extracted_data23Jun, extracted_data23Jul, 
                    extracted_data23Aug, extracted_data23Sep, extracted_data23Oct, 
                    extracted_data23Nov24Jan, extracted_data24Feb, extracted_data24Mar, 
                    extracted_data24Apr, extracted_data24May, extracted_data24Jun, 
                    extracted_data24Jul, extracted_data24Aug, extracted_data24SepDec, 
                    extracted_data25Jan, extracted_data25Feb)

saveRDS(extracted_data, "extracted_data.rds")

# table tibbles out of list

extracted_data <- readRDS("extracted_data.rds")

# Function to extract tibble and add name as column
extract_and_name <- function(sublist, date) {
  sublist[[1]] %>%
    mutate(date = date) %>%
    select(date, everything())
}

# Extract tibbles and add names
extracted_tibbles <- lapply(names(extracted_data), function(date) {
  extract_and_name(extracted_data[[date]], date)
})

# function to clean cargo types

cargo_type <- function(tibble) {
  tibble %>%
  mutate(cargo_type = ifelse(grepl("^[A-Z]+( [A-Z]+)+(:+)*$", ...1), ...1, 
         ifelse(grepl("^[A-Z]+(:+)*$", ...1), ...1,
                NA))) %>%
    mutate(cargo_type = case_when(...1 == "GENERAL CARGO Breakbulk/Conv"~ "GENERAL CARGO",
                                  ...1 == "BULK Bulk Dry" ~ "BULK",
                                  ...1 == "BULK Bulk Dry Bulk Liquid" ~ "BULK",
                                  ...1 == "CONTAINERS Container Cellular Container Non-Cellular" ~ "CONTAINERS",
                                  ...1 == "CONTAINERS Container Cellular Container Non-Cellular Reefer" ~ "CONTAINERS",
                                  ...1 == "TANKERS Tanker - Oil" ~ "TANKERS",
                                  ...1 == "TANKERS Tanker - Oil Tanker - Chemical Tanker - LPG Tanker - Bitumen" ~ "TANKERS",
                                  ...1 == "RO-RO Load On Roll Off Roll On Roll Off" ~ "CAR / VEHICLE CARRIERS",
                                  TRUE ~ cargo_type),
           ...1 = case_when(...1 == "GENERAL CARGO Breakbulk/Conv"~ "Breakbulk/Conv",
                            ...1 == "BULK Bulk Dry" ~ "Bulk Dry",
                            ...1 == "BULK Bulk Dry Bulk Liquid" ~ "Bulk Dry Bulk Liquid",
                            ...1 == "CONTAINERS Container Cellular Container Non-Cellular" ~ "Container Non-Cellular",
                            ...1 == "CONTAINERS Container Cellular Container Non-Cellular Reefer" ~ "Container Non-Cellular Reefer",
                            ...1 == "TANKERS Tanker - Oil" ~ "Tanker - Oil",
                            ...1 == "TANKERS Tanker - Oil Tanker - Chemical Tanker - LPG Tanker - Bitumen" ~ "Tanker - Oil Tanker - Chemical Tanker - LPG Tanker - Bitumen",
                            ...1 == "RO-RO Load On Roll Off Roll On Roll Off" ~ "RO-RO Load On Roll Off Roll On Roll Off",
                            TRUE ~ ...1)) %>%
    rename(cargo_detail = ...1) %>%
    select(cargo_type, date,everything()) %>%
    fill(cargo_type, .direction = "down") %>%
    filter(cargo_type %in% c("BULK", "GENERAL CARGO", "CONTAINERS", "TANKERS", "CAR / VEHICLE CARRIERS")) %>%
    filter(!cargo_detail %in% c("BULK", "GENERAL CARGO", "CONTAINERS", "TANKERS", "CAR / VEHICLE CARRIERS")) 
  
  }

# Apply cargo_type to each sublist
cargo_tibbles <- lapply(extracted_tibbles, cargo_type)


# Function to clean a single tibble: remove hyphens and empty columns
clean_tibble <- function(tibble) {
  tibble %>%
    mutate(across(4:last_col() & where(is.character), ~str_remove_all(., "-"))) %>%
    mutate(across(4:last_col() & where(is.character), ~str_remove_all(., " "))) %>%
    mutate(across(4:last_col() & where(is.character), ~str_trim(.))) %>%
    mutate(across(4:last_col(), as.numeric)) %>%
    select(where(~!all(is.na(.) | . == "" | str_trim(.) == "")))
  mutate
}

# Apply clean_tibble to each sublist
cleaned_tibbles <- lapply(cargo_tibbles, clean_tibble)

# check

lapply(cleaned_tibbles,dim)
View(cleaned_tibbles[[54]])

# function to move data in 3rd column to fourth column when fourth column is empty and tibble has more than 21 columns
extra_col <- function(tbl) {
  if (!is.data.frame(tbl)) {
    stop("Input must be a data frame (or tibble).")
  }
  if (ncol(tbl) != 22) {
    warning("Tibble does not have 22 columns. Returning input unchanged.")
    return(tbl)
  }
  
  is_empty_or_na <- function(x) {
    is.na(x) | (is.character(x) & trimws(x) == "")
  }
  
  for (i in 1:nrow(tbl)) {
    if (!is_empty_or_na(tbl[i, 4]) && is_empty_or_na(tbl[i, 5])) {
      tbl[i, 5] <- tbl[i, 4]
    }
  }
  
  tbl <- tbl %>%
    select(1:3, 5:ncol(.))
  
  return(tbl)
  
  
}

# Apply extra_col to each sublist
extra_col_tibbles <- lapply(cleaned_tibbles, extra_col)

# Function to standardize column names
standardize_names <- function(tibble) {
  tibble %>%
    setNames(c("CARGO", "RB_NO", "RB_GT", "DBN_NO", "DBN_GT", 
               "EL_NO", "EL_GT", "NGQ_NO", "NGQ_GT", "PE_NO", "PE_GT", 
               "MB_NO", "MB_GT", "CPT_NO", "CPT_GT", "SB_NO", "SB_GT", "TOT_NO", "TOT_GT",
               "monyear")) 
}


named_tibbles <- lapply(cleaned_tibbles, standardize_names)

# Combine tibbles into one tibble
combined_tibble <- bind_rows(named_tibbles)


# Combine all standardized tibbles
combined_df <- bind_rows(standardized_list)
