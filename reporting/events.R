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


## Libraries -------------------------------------------------------------------

library(readr)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(waffle)
library(RColorBrewer)
library(officer)


## Variables -------------------------------------------------------------------

if(.Platform$OS.type == "windows"){
  lan_data_dir <- "//SFP.idir.bcgov/S177/S7792/Operations/Data Science and Analytics/7. Data Science CoP"
}

## Note: the file share must be mounted on your local filesystem
if(.Platform$OS.type == "unix"){
  ## Macbook path
  lan_data_dir <- "/Volumes/Operations/Data Science and Analytics/7. Data Science CoP"
}


## Load ------------------------------------------------------------------------

event_part <- read_csv(file.path(lan_data_dir, "cop-data-events.csv"), col_types = c("ccddDccdc")) %>% 
  clean_names()

part_by_min <- read_csv(file.path(lan_data_dir, "cop-data-part-ministries.csv"), col_types = c("cDdcc")) %>% 
  clean_names()


## Munging ---------------------------------------------------------------------

#number of cop events
num_events <- event_part %>% 
  filter(cop_event_date < today()) %>% 
  count()
  

#number of cop participants
part_count <- event_part %>%
  filter(cop_event_date < today()) %>%
  select(cop_event_date, event_type, in_person_participants, on_line_participants) %>%
  mutate(on_line_participants = replace_na(on_line_participants, 0),
         in_person_participants = replace_na(in_person_participants, 0)) %>%
  mutate(participants = in_person_participants + on_line_participants) 

num_part <- part_count %>%
  summarise(sum(participants))


#cumulative number of participants over time
cumulative_part <- part_count %>% 
  arrange(cop_event_date) %>% 
  mutate(cumsum_part = cumsum(participants))

cumulative_trained <- part_count %>% 
  filter(event_type == "training") %>% 
  arrange(cop_event_date) %>% 
  mutate(cumsum_trained = cumsum(participants))


#number of training events
num_train_events <- event_part %>%
  filter(cop_event_date < today(),
         event_type == "training") %>% 
  count()


#number of overall training participants (all in-person)
num_trained <- part_count %>%
  filter(event_type == "training") %>%
  summarise(sum(participants))


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


#number of part by Ministry
num_by_min <- part_by_min %>% 
  select(number_part) %>% 
  sum()


#future goals
future <- tribble(
  ~date, ~cumsum_part, ~cumsum_trained,
  "2020-05-15", 412, 223,
  "2020-12-31", 500, 300
)
  

## Plotting --------------------------------------------------------------------

colourCount <-  length(unique(part_by_min$ministry))
getPalette <-  colorRampPalette(brewer.pal(9, "Set1"))


#waffle plot of Min participation in all events
waffle_plot <- part_by_min %>% 
  group_by(ministry) %>% 
    summarise(count = sum(number_part)) %>% 
    ggplot(aes(fill = ministry, values = count)) +
   geom_waffle(n_rows = 7) +
 scale_fill_manual(values = getPalette(colourCount), name = NULL) +
  coord_equal() +
  labs(title = "CoP Participation Numbers by Ministry",
       subtitle = paste0("(based on ", num_by_min, " participants who provided organization information)")) +
  theme_enhance_waffle() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .07, face = "bold"),
        plot.subtitle = element_text(hjust = .07, face = "italic", size = 10)) +
  guides(fill=guide_legend(ncol = 2, bycol = TRUE))


#save waffle chart as png
ggsave("reporting/cop-report-waffle.png", 
       waffle_plot,
       width = 18,
       height = 9)


#line plot of cumulative participation over time + goals
cumulative_plot <- ggplot() +
  geom_rect(aes(xmin = as_date("2020-01-01"),
                xmax = as_date("2020-12-31"),
                ymin = 0, ymax = Inf),
            alpha = 0.3, fill = "grey70") +
  geom_line(data = cumulative_part,
            mapping = aes(cop_event_date, cumsum_part),
            colour = "#0c2c84", size = 1.5) +
  geom_line(data = future,
            mapping = aes(as_date(date), cumsum_part),
            colour = "#0c2c84", linetype = "dashed", size = 1.5) +
  geom_line(data = cumulative_trained,
            mapping = aes(cop_event_date, cumsum_trained),
            colour = "#1d91c0", size = 1.5) +
   geom_line(data = future,
            mapping = aes(as_date(date), cumsum_trained),
            colour = "#1d91c0", linetype = "dashed", size = 1.5) +
  annotate("text",
           label = "All Events",
           x = as_date("2019-11-01"), y = 360,
           colour = "#0c2c84", size = 8) +
  annotate("text",
           label = "Training Events",
           x = as_date("2019-09-21"), y = 213,
           colour = "#1d91c0", size = 8) +
   annotate("text",
           label = "2020 CoP Goals",
           x = as_date("2020-06-20"), y = 485,
           colour = "#99000d",
           fontface = "bold", size = 9) +
     annotate("text",
           label = "Actuals—Solid Lines\nTargets—Dotted Lines",
           x = as_date("2020-07-01"), y = 50,
           colour = "grey50",
           size = 6) +
  labs(x = NULL, y = NULL,
       title = "Staff Participation in Data Science CoP\n") +
  scale_x_date(date_breaks = "4 month",
               date_labels = "%b %y") +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = -.12,
                                  face = "bold",
                                  size = 26,
                                  colour = "grey30"),
        axis.text = element_text(size = 18))
     as_date("2020-12-31")
   
     
#save line chart as png
ggsave("reporting/cop-participation.png", 
       cumulative_plot,
       width = 11,
       height = 9)



## Outputs ---------------------------------------------------------------------

#pptx reporting slide
top_text <- fpar(
    ftext("Data Science CoP Summary\n", prop = fp_text(bold = FALSE, font.size = 40)),
    ftext(paste0(num_events, " events, including ", num_train_events,
                         " training days\n", num_part, " participants overall, ",
                         num_trained, " attended training days"), prop = fp_text(font.size = 16, color = "grey30")),
    fp_p = fp_par(text.align = "center"))

summary_slide <- read_pptx()  %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = top_text, location = ph_location_type(type = "title")) %>% 
  ph_with(value = waffle_plot, location = ph_location_type(type = "body")) %>% 
  ph_with(value = "Includes August 2018 - May 2020 Events", location = ph_location_type(type = "ftr")) 

print(summary_slide, target = paste0("reporting/ds-cop-reporting_", format(Sys.time(), "%Y-%m-%d"), ".pptx")) 



