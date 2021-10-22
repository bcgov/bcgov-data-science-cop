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


## Libraries -------------------------------------------------------------------

library(readr)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(waffle) #dev version remotes::install_github("hrbrmstr/waffle")
library(RColorBrewer)
library(officer)
library(safepaths) ## https://github.com/bcgov/safepaths

dir.create("reporting/out", showWarnings = FALSE)

## Load ------------------------------------------------------------------------

event_part <- read_csv(use_network_path("7. Data Science CoP/data/cop-data-events.csv"),
           col_types = c("ccddDccdc")) %>%
  clean_names()

part_by_min <- read_csv(use_network_path("7. Data Science CoP/data/cop-data-part-ministries.csv"),
           col_types = c("cDdcc")) %>%
  clean_names()

org_acronyms <- read_csv(use_network_path("7. Data Science CoP/data/ministry-name-abbrevation.csv"),
           col_types = c("cc")) %>%
  clean_names()


## Munging ---------------------------------------------------------------------

#number of cop events
num_events <- event_part %>% 
  filter(date < today()) %>% 
  count()
  

#number of cop participants
part_count <- event_part %>%
  filter(date < today()) %>%
  select(date, event_type, in_person_participants, on_line_participants) %>%
  mutate(on_line_participants = replace_na(on_line_participants, 0),
         in_person_participants = replace_na(in_person_participants, 0)) %>%
  mutate(participants = in_person_participants + on_line_participants) 

num_part <- part_count %>%
  summarise(sum(participants))


#number of training events
num_train_events <- event_part %>%
  filter(date < today(),
         event_type %in% c("workshop", "webinar")) %>% 
  count()


#number of overall training participants (all in-person)
num_trained <- part_count %>%
  filter(event_type %in% c("workshop", "webinar")) %>%
  summarise(num_trained = sum(participants))


#average waitlist
# event_part %>%
#   mutate(waiting_list = suppressWarnings(as.numeric(waiting_list))) %>%
#   filter(date < today(),
#          event_type %in% c("workshop", "webinar")) %>%
#   summarise(mean(waiting_list, na.rm = TRUE))


#number of Ministries/Organizations
part_by_min %>% 
  distinct(ministry) %>% 
  filter(!ministry %in% c("Unknown", "External")) %>% 
  count()


#number of participants by Ministry
num_by_min <- part_by_min %>%
  filter(!ministry %in% c("Unknown", "External")) %>% 
  select(number_participants) %>% 
  sum()


## Plotting --------------------------------------------------------------------

## waffle plot of Ministry participation in all events

#colour palette
colourCount <-  length(unique(part_by_min$ministry))
getPalette <-  colorRampPalette(brewer.pal(9, "Set1"))


#waffle plot
waffle_plot <- part_by_min %>%
  filter(!ministry %in% c("Unknown", "External")) %>% 
  mutate(ministry2 = stringr::str_wrap(ministry, 45)) %>% 
  group_by(ministry2) %>%
  summarise(count = sum(number_participants)) %>%
  ggplot(aes(fill = ministry2, values = count)) +
  geom_waffle(n_rows = 10) +
  scale_fill_manual(values = getPalette(colourCount), name = NULL) +
  coord_equal() +
  labs(
    title = "CoP Participation Numbers by Ministry or Organization",
    subtitle = paste0(
      "(based on ",
      num_by_min,
      " participants who provided organization information)"
    )
  ) +
  theme_enhance_waffle() +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = .07, face = "bold"),
    plot.subtitle = element_text(hjust = .07, face = "italic", size = 10)
  ) +
  guides(fill = guide_legend(ncol = 3, bycol = TRUE))


## Outputs --------------------------------------------------------------------


#save waffle chart as png
ggsave("reporting/out/cop-report-waffle.png", 
       waffle_plot_png,
       width = 18,
       height = 9)


#pptx CoP reporting slide
top_text <- fpar(
    ftext("Data Science CoP Summary\n",
          prop = fp_text(bold = FALSE,
                         font.size = 40)),
    ftext(paste0(num_events, " events, including ", num_train_events,
                         " training sessions (workshops & webinars)\n", num_part, " participants overall, ",
                         num_trained, " attended training sessions"),
          prop = fp_text(font.size = 16, color = "grey30")),
    fp_p = fp_par(text.align = "center"))


summary_slide <- read_pptx()  %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = top_text, location = ph_location_type(type = "title")) %>% 
  ph_with(value = waffle_plot, location = ph_location_type(type = "body")) %>% 
  ph_with(value = "August 2018 - September 2021", location = ph_location_type(type = "ftr")) 

print(summary_slide, target = paste0("reporting/out/ds-cop-reporting_", format(Sys.time(), "%Y-%m-%d"), ".pptx")) 

