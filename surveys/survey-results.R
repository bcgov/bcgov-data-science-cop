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

# Variables --------------------------------------------------------------------

# if(.Platform$OS.type == "windows"){
lanpath
# }
# 
# ## Note: the file share must be mounted on your local filesystem
# if(.Platform$OS.type == "unix"){
#   ## Macbook path
lanpath
# }


# Load -------------------------------------------------------------------------

# results <- read_csv(file.path(lan_data_dir, "results-survey424739 - final.csv"),
#                     col_types = c("cccccccccccccccccccccccccc")) %>% 
#            clean_names()
# 
# write_csv(results, "surveys/tmp/survey_results.csv")
results <- read_csv(here::here("surveys/tmp/survey_results.csv"))


# Munging ----------------------------------------------------------------------

participation <- results %>% count()


events <- results %>% 
  select(starts_with("please")) %>% 
  pivot_longer(everything(), names_to = "question", values_to = "answer") %>% 
  mutate(question = str_remove(question, "please_select_events_from_the_list_below_that_you_would_you_like_to_see_the_data_science_co_p_host_in_2020_"), 
         question = recode(question, "co_p_member_led_data_science_learning_club_s" = "cop_member_led_data_science_learning_clubs"))


events_plot <- events %>% 
  filter(question != "other" & answer != "N/A") %>% 
  group_by(question, answer) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(labels = str_replace_all(question, "_", " "),
         labels2 = str_wrap(labels, 25)) %>% 
  ggplot(aes(answer, n)) +
  geom_col(fill = "#1f78b4", alpha = .7) +
  facet_wrap(~ labels2) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "What Events Would You Like to See the Data Science Community of Practice Host?")

ggsave("surveys/tmp/events_plot.jpg", events_plot)


events %>% 
  filter(question == "other") %>% 
  drop_na()


what <- results %>% 
  select(starts_with("what_type_of")) %>% 
  pivot_longer(everything(), names_to = "question", values_to = "answer") %>% 
  mutate(question = str_remove(question, "what_type_of_training_event_you_would_like_to_see_the_data_science_co_p_host_in_2020_")) 


what_plot <- what %>% 
  filter(question != "other" & answer != "N/A") %>% 
  group_by(question, answer) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(labels = str_replace_all(question, "_", " "),
         labels2 = str_wrap(labels, 30)) %>% 
  ggplot(aes(answer, n)) +
  geom_col(fill = "#1f78b4", alpha = .7) +
  facet_wrap(vars(labels2)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
         plot.title = element_text(size = 14),
         axis.text = element_text(size = 10),
        strip.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "What Types of Training Would You Like to See the Data Science Community of Practice Host?")

ggsave("surveys/tmp/what_plot.jpg", what_plot)


what %>% 
  filter(question == "other") %>% 
  drop_na()


languages <- results %>% 
  select(starts_with("what_programming_languages")) %>%
  drop_na()


topics <- results %>% 
  select(starts_with("are_there_any_other_specific")) %>%
  drop_na()
  
  
suggestions <- results %>% 
  select(starts_with("do_you_have_any_additional_suggestions")) %>%
  drop_na()
  


