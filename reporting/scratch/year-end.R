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


# load libraries ---------------------------------------------------------------
library(readr)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(safepaths) ## https://github.com/bcgov/safepaths
library(cowplot)

# load data --------------------------------------------------------------------
event_participation <-
  read_csv(use_network_path("7. Data Science CoP/data/cop-data-events.csv"),
           col_types = c("ccddDccdc")) %>%
  clean_names()


# plot settings ----------------------------------------------------------------

#event colours
event_colours = c("meet-up" = "#1f78b4",
                  "webinar" = "#ff7f00",
                  "workshop" = "#33a02c")

#event bar chart theme
theme_bar <- theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(),
    axis.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11))


# munge data -------------------------------------------------------------------

#number of cop participants by year
participation_count <- event_participation %>%
  filter(date < today()) %>%
  select(date, event_type, in_person_participants, on_line_participants) %>%
  mutate(on_line_participants = replace_na(on_line_participants, 0),
         in_person_participants = replace_na(in_person_participants, 0)) %>%
  mutate(participants = in_person_participants + on_line_participants) 

#participation by year and event
participation_sum <- participation_count %>%
  mutate(year = year(date)) %>% 
  mutate(event_type = recode(event_type, social = "meet-up")) %>% 
  group_by(year, event_type) %>% 
  summarise(total_part = sum(participants),
            total_event = length(event_type)) 

#number of events by year
event_sum <- participation_sum %>%
  group_by(year) %>% 
  summarise(n_events = sum(total_event),
            n_parts = sum(total_part))


# plot  ------------------------------------------------------------------------

#bar chart of number of participants
p <- ggplot() +
  geom_col(data = participation_sum,
           aes(x = year, y = total_part, fill = event_type),
           alpha = 0.6) +
  geom_text(data = event_sum,
            aes(x = year, y = n_parts, label = paste(n_events, "events")),
            nudge_y = 20,
            fontface = "bold",
            size = 4) +
  labs(
    title = "bcgov Data Science Community of Practice Annual Participation",
    x = NULL,
    y = "Number of Participants"
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 600),
                     breaks = seq(0, 600, 100)) +
  scale_fill_manual(name = NULL,
                    values = event_colours) +
  theme_minimal() +
  theme_bar +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 13, face = "bold"))
p




# output  ----------------------------------------------------------------------

logo_file <- here::here("reporting/data/cop-logo-updated.png")

end_year_plot <- ggdraw(p) +
    draw_image(logo_file,
             x = .2, y = .95,
             hjust = .5, vjust = 1.3,
             width = 0.3, height = 0.3)
end_year_plot

ggsave("reporting/out/end-of-year-summary.png", 
       end_year_plot,
       width = 18,
       height = 9)
