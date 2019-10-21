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

# Libraries ---------------------------------------------------------------

library(dplyr)
library(readr)


# Variables ---------------------------------------------------------------

if(.Platform$OS.type == "windows"){
  lan_data_dir <- "//SFP.idir.bcgov/S177/S7792/Operations/Data Science and Analytics/Data Science CoP"
}

## Note: the file share must be mounted on your local filesystem
if(.Platform$OS.type == "unix"){
  ## Macbook path
  lan_data_dir <- "/Volumes/Operations/Data Science and Analytics/Data Science CoP"
}

if (!exists("tmp")) dir.create('tmp', showWarnings = FALSE)


# Load --------------------------------------------------------------------

event_part <- read_csv(file.path(lan_data_dir, "cop-data-events.csv"), col_types = c("ccddDccdc"))

part_by_min <- read_csv(file.path(lan_data_dir, "cop-data-part-ministries.csv"), col_types = c("cDdcc"))

