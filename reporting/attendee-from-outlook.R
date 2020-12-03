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

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(hrbrthemes)
library(readr)

## navigate to event in your calendar.
## Go to the 'tracking' tab
## Click button labelled 'Copy Status to Clipboard'
## Run code below to read from clipboard into R
attendees_raw <- read_delim(clipboard(), delim = "\t")

attendees <- attendees_raw %>%
  filter(Response %in% c("Tentative", "Accepted")) %>% 
  mutate(ministry = ifelse(str_detect(Name, ":EX"), Name, 'External')) %>%
  mutate(ministry = sub(".*\\s", "", trimws(ministry))) %>%
  mutate(ministry = gsub(":EX", "", ministry))

by_ministry <- attendees %>%
  count(ministry)


## A nice plot
ggplot(by_ministry) +
  geom_col(aes(x = fct_reorder(ministry, n, .desc = TRUE), y = n, fill = ministry)) +
  scale_fill_viridis_d(option = "B") +
  guides(fill = FALSE) +
  labs(x = "Ministry", y = "Number of People Who Registered") +
  theme_ft_rc(axis_title_size = 20,axis_text_size = 16,
              grid = FALSE) +
  theme(axis.text = element_text(size = 10))
