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


# Libraries --------------------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(here)
library(safepaths) ## https://github.com/bcgov/safepaths


# Load -------------------------------------------------------------------------

feedback <- read_csv(use_network_path("7. Data Science CoP/training-events/2020-05-06_intro-ds-r/Introduction to Data Science with R Online Workshop.csv")) %>%
           clean_names()

feedback_plot <- feedback %>% 
  select(where_do_you_work) %>% 
  filter(where_do_you_work != "Response") %>% 
  drop_na() %>% 
  group_by(where_do_you_work) %>% 
  count() %>% 
  ungroup() %>% 
  # mutate(prop = round(n/sum(n) * 100, digits = 0)) %>% 
  mutate(prop = scales::percent(n/sum(n))) %>% 
  ggplot(aes(n, where_do_you_work)) +
  geom_col(fill = "blue", alpha = 0.5) +
  geom_text(aes(label = prop), hjust = -.2) +
  theme_minimal() +
  labs(x = NULL, y = NULL,
       title = "Where do you work?") +
  theme(panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold",
                                  hjust = -.2))

ggsave("surveys/tmp/feedback_plot.jpg", feedback_plot, width = 16, units = )








