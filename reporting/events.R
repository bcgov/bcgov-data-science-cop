# Copyright 2019 Province of British Columbia
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


# Libraries --------------------------------------------------------------------

library(readr)
library(janitor)
library (lubridate)
library(dplyr)
library(ggplot2)
library(waffle)
library(RColorBrewer)
library(officer)


# Variables --------------------------------------------------------------------

if(.Platform$OS.type == "windows"){
  lan_data_dir <- "//SFP.idir.bcgov/S177/S7792/Operations/Data Science and Analytics/Data Science CoP"
}

## Note: the file share must be mounted on your local filesystem
if(.Platform$OS.type == "unix"){
  ## Macbook path
  lan_data_dir <- "/Volumes/Operations/Data Science and Analytics/Data Science CoP"
}


# Load -------------------------------------------------------------------------

event_part <- read_csv(file.path(lan_data_dir, "cop-data-events.csv"), col_types = c("ccddDccdc")) %>% 
  clean_names()

part_by_min <- read_csv(file.path(lan_data_dir, "cop-data-part-ministries.csv"), col_types = c("cDdcc")) %>% 
  clean_names()


# Munging ----------------------------------------------------------------------

#number of cop events
event_part %>% 
  filter(cop_event_date < today()) %>% 
  count()
  
#number of cop participants
event_part %>%
  filter(cop_event_date < today()) %>%
  select(in_person_participants, on_line_participants) %>%
  mutate(on_line_participants = replace_na(on_line_participants, 0)) %>%
  mutate(participants = in_person_participants + on_line_participants) %>%
  summarise(sum(participants))

#number of training events
event_part %>%
  filter(cop_event_date < today(),
         event_type == "training") %>% 
  count()

#number of overall training participants (all in-person)
event_part %>%
  filter(cop_event_date < today(),
         event_type == "training") %>%
  summarise(sum(in_person_participants))

#average waitlist
event_part %>%
  mutate(waitlist = suppressWarnings(as.numeric(wait_list))) %>% 
  filter(cop_event_date < today(),
         event_type == "training") %>%
  summarise(mean(waitlist, na.rm = TRUE))

#number of Ministries
part_by_min %>% 
  distinct(ministry) %>% 
  count()


# Plotting ---------------------------------------------------------------------

colourCount = length(unique(part_by_min$ministry))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#waffle plot of Min participation in all events
waffle_plot <- part_by_min %>% 
  group_by(ministry) %>% 
    summarise(count = sum(number_part)) %>% 
    ggplot(aes(fill = ministry, values = count)) +
   geom_waffle(n_rows = 7) +
 scale_fill_manual(values = getPalette(colourCount), name = NULL) +
  coord_equal() +
  labs(title = "Data Science CoP Events: Participation Numbers by Ministry",
       caption = "Includes Data from\nAugust 2018 - October 2019 Events") +
  theme_enhance_waffle() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .95, face = "italic")) +
  guides(fill=guide_legend(ncol = 3, bycol = TRUE))


# Outputs ----------------------------------------------------------------------

summary_slide <- read_pptx()  %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = "Test Foo", location = ph_location_type(type = "title")) %>% 
  ph_with(value = waffle_plot, location = ph_location_type(type = "body"))

print(summary_slide, target = "reporting/ds-cop-reporting.pptx") 



