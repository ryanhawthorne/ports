# import and clean pdfs from Transnet

# Load necessary libraries
library(tidyverse)
library(pdftools)


y2018m01_raw <- pdf_text("G:\\My Drive\\OV - ports\\Transnet\\Vessels\\201801.pdf") %>%
  str_split(pattern = "\n") %>%
  unlist() %>%
  as.data.frame() %>%
  rename("col1" = ".") # name text column to be separated

# detect first column as string variable
names_indicator <- y2018m01_raw[4, ]

# keep relevant rows
y2018m01 <- y2018m01_raw[8:nrow(y2018m01_raw),] %>%
  as.data.frame() %>%
  rename("col" = ".") # name text column to be separated
  

# separate columns by width
y2018m01 <- y2018m01 %>%
  separate_wider_delim(col = col, 
           delim = "   ",
           names_sep = c(""),
           too_few = "align_start") # Adjust columns as needed

# trim all columns
y2018m01 <- y2018m01 %>%
  mutate(across(everything(), ~str_trim(.))) %>%
  select(-c(2:5))

y2018m01 <- y2018m01 %>%
mutate(
  cargo_type = ifelse(grepl("^[A-Z]+( [A-Z]+)+(:+)*$", col1), col1, 
                      ifelse(grepl("^[A-Z]+(:+)*$", col1), col1,
                             NA))
) %>%
  select(cargo_type,col1:col71) %>%
  fill(cargo_type, .direction = "down") 

# remove rows that aren't useful
y2018m01 <- y2018m01 %>%
  filter(cargo_type %in% c("BULK", "GENERAL CARGO", "CONTAINERS", "TANKERS")) 

# remove total rows
y2018m01 <- y2018m01 %>%
  filter(!col1 %in% c("BULK", "GENERAL CARGO", "CONTAINERS", "TANKERS")) 
