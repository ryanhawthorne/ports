# import and clean vessels dataset

# Load necessary libraries
library(tidyverse)
library(readxl)

# import vessels spreadsheet
vessels_raw <- read_excel("G:\\My Drive\\OV - ports\\Transnet\\Vessels\\vessels.xlsx") |>
  rename_with(~ paste0("col",seq_along(.)))

# extract main cargo


vessels <- vessels_raw |>
  mutate(
    cargo_type = ifelse(grepl("^[A-Z]+( [A-Z]+)+(:+)*$", col1), col1, 
                        ifelse(grepl("^[A-Z]+(:+)*$", col1), col1,
                                      NA)),
    year = str_extract(col1, "\\b(1[0-9]{3}|2[0-9]{3})\\b"),  # Extract 4-digit year
    month = str_extract(col1, "\\b(JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER)\\b")) |> # Extract full or short month name OR numeric month)
  mutate(year = ifelse(row_number() == 1, 2018, year),
         month = ifelse(row_number() == 1,"JANUARY", month)) |> # Fill in missing year and month
  mutate(cargo_type = case_when(col1 == "GENERAL CARGO Breakbulk/Conv"~ "GENERAL CARGO",
                                col1 == "BULK Bulk Dry" ~ "BULK",
                                col1 == "BULK Bulk Dry Bulk Liquid" ~ "BULK",
                                col1 == "CONTAINERS Container Cellular Container Non-Cellular" ~ "CONTAINERS",
                                col1 == "CONTAINERS Container Cellular Container Non-Cellular Reefer" ~ "CONTAINERS",
                                col1 == "TANKERS Tanker - Oil" ~ "TANKERS",
                                col1 == "TANKERS Tanker - Oil Tanker - Chemical Tanker - LPG Tanker - Bitumen" ~ "TANKERS",
                                col1 == "RO-RO Load On Roll Off Roll On Roll Off" ~ "CAR / VEHICLE CARRIERS",
                                TRUE ~ cargo_type),
         col1 = case_when(col1 == "GENERAL CARGO Breakbulk/Conv"~ "Breakbulk/Conv",
                          col1 == "BULK Bulk Dry" ~ "Bulk Dry",
                          col1 == "BULK Bulk Dry Bulk Liquid" ~ "Bulk Dry Bulk Liquid",
                          col1 == "CONTAINERS Container Cellular Container Non-Cellular" ~ "Container Non-Cellular",
                          col1 == "CONTAINERS Container Cellular Container Non-Cellular Reefer" ~ "Container Non-Cellular Reefer",
                          col1 == "TANKERS Tanker - Oil" ~ "Tanker - Oil",
                          col1 == "TANKERS Tanker - Oil Tanker - Chemical Tanker - LPG Tanker - Bitumen" ~ "Tanker - Oil Tanker - Chemical Tanker - LPG Tanker - Bitumen",
                          col1 == "RO-RO Load On Roll Off Roll On Roll Off" ~ "RO-RO Load On Roll Off Roll On Roll Off",
                          TRUE ~ col1)
         ) |>
  fill(year, .direction = "down") |>
  fill(month, .direction = "down")

# fill missing data

vessels <- vessels |>
  fill(cargo_type, .direction = "down") 

# remove data not interested in
# rename cargo detail
vessels <- vessels |>
  rename(cargo_detail = col1) %>%
  select(cargo_type, cargo_detail, year, month, col2:col24)

# remove rows that aren't useful
vessels <- vessels |>
  filter(cargo_type %in% c("BULK", "GENERAL CARGO", "CONTAINERS", "TANKERS", "CAR / VEHICLE CARRIERS")) 

# remove total rows
vessels <- vessels |>
  filter(!cargo_detail %in% c("BULK", "GENERAL CARGO", "CONTAINERS", "TANKERS", "CAR / VEHICLE CARRIERS")) 

# fill missing year and month
vessels <- vessels |>
  mutate(year = case_when(row_number() > 46*12*4 & row_number() < 46*12*5 ~ "2022",
                          row_number() > 46*12*5 & row_number() < 46*12*6 ~ "2023",
                          row_number() > 46*12*6 & row_number() < 46*12*7 ~ "2024",
                          TRUE ~ year)) 

# remove -

vessels <- vessels |>
  mutate(across(col2:col24, ~str_remove_all(., "-")))

# clean cells that have multiple observations concatenated

vessels <- vessels |>
  separate_rows(col2, sep = " ") |>
  separate_rows(col3, sep = "(\\d{3}\\s+\\d{3}) | (\\d{2}\\s+\\d{3})")
  
