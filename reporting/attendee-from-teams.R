# Copyright 2020 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

## https://www.bclaws.ca/civix/document/id/oic/oic_cur/contact

### The raw file must be extract DURING the meeting and can only be done by the 
### meeting organizer

### Script purpose:
### Read all raw MS Teams files in the raw-participants folder
### Parse out ministry names and summary by ministry/org name
### Join with ministry name/ abbreviations table

### TODO:
### write code to automatically add to cop-data-part-ministries.csv


library(readr)
library(safepaths)
library(purrr)
library(tools)
library(lubridate)
library(janitor)
library(dplyr)
library(stringr)

## Set the raw path
raw_participants_path <- use_network_path('7. Data Science CoP/data/raw-participants/')
crosswalk_file <- use_network_path('7. Data Science CoP/data/ministry-name-abbrevation.csv')

## Get all .csv pathss
csv_paths <- list.files(raw_participants_path, pattern = "*.csv", full.names = TRUE)

## read in raw MS Teams data
raw_participants <- map_df(csv_paths, ~ {
  d <- read_csv(.x, col_types = c("ccc"), col_select = c("Full Name", "Timestamp"))
  d$Timestamp <- mdy_hms(d$Timestamp, tz = "America/Vancouver")
  d$Date <- as.Date(d$Timestamp)
  d$Timestamp <- NULL
  d$event <- file_path_sans_ext(basename(.x))
  distinct(d, .keep_all = TRUE)
  clean_names(d)
})

## teams introduced a new format :(
## going with a manually approach ATM
path <- csv_paths[1]
d <- read_csv(path, col_types = c("ccc"), col_select = c("Full Name", "UTC Event Timestamp"))
d$`UTC Event Timestamp` <- mdy_hms(d$`UTC Event Timestamp`)
d$Date <- as.Date(d$`UTC Event Timestamp`)
d$`UTC Event Timestamp` <- NULL
d$event <- file_path_sans_ext(basename(path))
distinct(d, .keep_all = TRUE)

## because
raw_participants <- clean_names(d)

## read in crosswalk table
min_abbr <- read_csv(crosswalk_file, col_types = c("cc"))

## Calculate counts by ministry
count_by_ministry <- raw_participants %>% 
  mutate(abbreviation = ifelse(str_detect(full_name, ":EX"), full_name, 'External')) %>%
  mutate(abbreviation = sub(".*\\s", "", trimws(abbreviation))) %>%
  mutate(abbreviation = gsub(":EX", "", abbreviation)) %>% 
  left_join(min_abbr, by = c("abbreviation")) %>% 
  distinct(full_name, event, date, ministry) %>% 
  count(event, date, ministry) %>% 
  relocate(n, .before = ministry)

## Unknown ministries. This is worth manually checking
count_by_ministry$ministry[is.na(count_by_ministry$ministry)] <- "Unknown"

## Total number of the event
total_by_event <- count_by_ministry %>% 
  mutate(date = max(date)) |> 
  group_by(event, date) %>% 
  summarise(n = sum(n)) %>% 
  relocate(n, .before = date)

