library(tidyverse)
library(wildRtrax)
library(lubridate)

relative_root <- "/volumes"
rugr_data <- read_csv("/users/alexandremacphail/desktop/rugrtags.csv")

rugr_files <- rugr_data %>%
  filter(species_code == "RUGR") %>%
  slice(1,100,1000) %>%
  mutate(group = as.numeric(str_extract(location,"^[^-]*")),
         group = case_when(group > 999 ~ as.character(group),
                           group > 99 & group < 1000 ~ paste0("0",group),
                           group > 10 & group < 100 ~ paste0("00", group),
                           group > 0 & group < 10 ~ paste0("000", group),
         location = str_replace(location, "^[^-]*", group)) %>%
  mutate(rebuild_path_sm3 = paste0(relative_root,"/BUdata/ABMI/ARU/ABMI-EH/", year(recording_date), "/01/",
                                   "ABMI-", group, "/ABMI-", location, "/ABMI-", location, "_0+1_", format(recording_date,"%Y%m%d_%H%M%S"),".wac"),
         rebuild_path_sm4 = str_replace(str_replace(rebuild_path_sm3,".wac",".wav"),"_0\\+1_", "_")) %>%
  select(rebuild_path_sm3, rebuild_path_sm4) %>%
  mutate(path_exists_sm3 = file.exists(rebuild_path_sm3),
         path_exists_sm4 = file.exists(rebuild_path_sm4)) %>%
  pivot_longer(!(rebuild_path_sm3:rebuild_path_sm4), values_to = "exists", names_to = "path_type") %>%
  filter(!exists == FALSE)
