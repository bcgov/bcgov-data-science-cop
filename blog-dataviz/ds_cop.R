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


## Data Visualization for Data Science CoP @Work Blog Post for August 2019


library(dplyr)
library(ggplot2)
library(waffle)

parc <- tribble(
  ~ministry, ~num_reps,
  "Health", 4,
  "Education", 4,
  "Environment", 3,
  "Citizens' Services (OCIO)", 2,
  "Citizens' Services (PSA)" , 2,
  "Transport and Infrastructure", 1,
  "JTT (BC Stats)", 1,
  "FLNRORD", 1,
  "Finance", 1
)

parc %>% 
  ggplot(aes(fill = ministry, values = num_reps)) +
  geom_waffle(n_rows = 4) +
  scale_fill_viridis_d(name = "Ministry") +
  coord_equal() +
  labs(title = "Data Science Community of Practice Meeting (July 24, 2019)",
       subtitle = "Attendance by Ministry. Tools mentioned that were used: R, Python, PowerBI, Oracle, MATLAB, WAKA, H2O, TensorFlow") +
  theme_enhance_waffle() +
  theme_void()
  
  
  