# Summarize the PDO

# Authors: Sara E Miller, Jane Sullivan
# Contact: sara.miller@alaska.gov, jane.sullivan1@alaska.gov
# Last edited: May 2022

# set-up ----
out.label <-  "2023_forecast"
START_YEAR <- 1970 # Model start year
YEAR <- 2023 # Forecast year

devtools::install_github("commfish/fngr")
library(tidyverse)
library(ggrepel)
library(fngr)
windowsFonts(Times=windowsFont("Times New Roman"))
theme_set(theme_report())

pdo_path2 <- paste0(out.label, "/data/pdo/")
if(!exists(pdo_path2)){dir.create(pdo_path2)}

# access pdo---- 
raw_data<-read.csv('https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_PDO.csv', fill=TRUE, header=TRUE) 
write.csv(raw_data, paste0(pdo_path2, "/raw_pdo_accessed_", Sys.Date(), ".csv")) 

# summarize pdo----
files <- list.files(file.path(pdo_path2)) # searchable file list
pdo_file <- file.path(pdo_path2, files[which(grepl("raw_pdo", files))]) # find csv
#note: if already have a file with date accessed in folder, the code will not run,; delete the old file

raw_pdo <- read.csv(pdo_file)

raw_pdo %>%
  drop_na() %>%
  select (-c(X)) %>%
  slice(-1) %>%
  dplyr::mutate(year = lubridate::year(time), 
              month = lubridate::month(time)) %>%
  select (-c(time, index))  %>%
  filter(year >= START_YEAR & year < YEAR) %>% 
  mutate(year = as.numeric(year))  %>% 
  mutate(PDO = as.numeric(PDO))  %>% 
  mutate(month = as.numeric(month))  %>% 
  filter(year > START_YEAR-1 & year < YEAR) %>% 
  as.data.frame() -> pdo 

# summarize and save (annual pdo)
pdo %>%
  group_by(year) %>%
  dplyr::summarise(mean_pdo = mean(PDO)) %>%
  write.csv(., paste0(pdo_path2, "/pdo_annual_summary.csv"))

# summarize and save
pdo %>%
  mutate(month = as.numeric(month))  %>% 
  dplyr::filter(month == 5 | month == 6 | month == 7) %>%
  group_by(year) %>%
  dplyr::summarise(mean_May_Aug_pdo = mean(PDO)) %>%
  write.csv(., paste0(pdo_path2, "/pdo_MJJ_summary.csv"))