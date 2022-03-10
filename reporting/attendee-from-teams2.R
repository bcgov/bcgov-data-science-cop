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

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(hrbrthemes)
library(readr)
library(safepaths)

## Set the raw path
raw_participants_path <- use_network_path('7. Data Science CoP/data/raw-participants/')
crosswalk_file <- use_network_path('7. Data Science CoP/data/ministry-name-abbrevation.csv')
cop_data_path <- use_network_path('7. Data Science CoP/data/cop-data-part-ministries.csv')

## Get all .csv paths
csv_paths <- list.files(raw_participants_path, pattern = "*.csv", full.names = TRUE)

## read in raw MS Teams data
path <- csv_paths[4]
d <- read_csv(path)
d$Date <- as.Date(d$`Timestamp`, "%d/%m/%Y")
d$event <- file_path_sans_ext(basename(path))
distinct(d, .keep_all = TRUE)

raw_participants <- clean_names(d)

## read in crosswalk table
min_abbr <- read_csv(crosswalk_file, col_types = c("cc"))

## Calculate counts by ministry
count_by_ministry <- raw_participants %>% 
  mutate(abbreviation = gsub(":EX", "", ministry)) %>% 
  distinct(full_name, event, date, abbreviation) %>%
  left_join(min_abbr, by = c("abbreviation")) %>% 
  count(`event-title` = event, date, ministry) %>% 
  relocate(`number-participants` = n, .before = ministry)

## Unknown ministries. This is worth manually checking
count_by_ministry$ministry[is.na(count_by_ministry$ministry)] <- "Unknown"

## Total number of the event
total_by_event <- count_by_ministry %>% 
  mutate(date = max(date)) |> 
  group_by(`event-title`, date) %>% 
  summarise(n = sum(`number-participants`)) %>% 
  relocate(n, .before = date)

## Append a couple of extra columns for merging
count_by_ministry$org = NA
count_by_ministry$min_name_raw = NA
cop_data_file <- read_csv(cop_data_path)
cop_data_file <- rbind(cop_data_file, count_by_ministry)

