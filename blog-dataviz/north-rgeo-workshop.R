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


## Data Visualization for Data Science CoP @Work Blog Post for November 2019


## Load libraries
library(bcmaps)
library(dplyr)
library(ggplot2)
library(ggspatial)


## List of particpant home cities
participant_cities <- c("Victoria", "Fort St. John", "Fort St. James",
                     "Prince George", "Kamloops", "Kelowna", "Mackenzie",
                     "Smithers", "Surrey", "Williams Lake")


## Filter all cities in bcmaps::bc_cities() for home cities
city_locations <- bc_cities() %>% 
  filter(NAME %in% participant_cities)


## Make a map 
ggplot() +
  geom_sf(data = bc_bound(), fill = "#ffffcc", alpha = 0.5) +
  geom_sf(data = city_locations, size = 4, colour = "#006837") +
  geom_sf_text(data = city_locations, aes(label = NAME),
               size = 2.5, nudge_y = -30000, fontface = "bold") +
  theme_minimal() +
  annotation_scale(pad_x = unit(1.5, "cm"), pad_y = unit(1, "cm"),
                   location = "bl",  style = "ticks", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "grid",
                         height = unit(1.5, "cm"), width = unit(.75, "cm")) +
  labs(title = "Home Cities of BC Public Servants Who Participated in the\nbcgov North Area R Geospatial Training Event",
       y = NULL,
       x = NULL) +
  theme(plot.title = element_text(face = "bold"))


## Save to PNG
ggsave("blog-dataviz/bcgov-rgeospatial-participant-cities-map.png")


