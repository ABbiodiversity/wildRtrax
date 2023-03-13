library(tidyverse)

AP <- "/users/alexandremacphail/APN/AnalysisPrograms"

late19 <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/intake/2022/ABMI-EH", file_type = "all", extra_cols = F)

vis <- read_csv("/users/alexandremacphail/desktop/visdep.csv") %>%
  filter(!grepl('HF2',Site),
         !`ARU File Prefix` == "VNA") %>%
  mutate(year = year(`Field Date`)) %>%
  filter(year %in% c(2019),
         grepl('596|597|598|628|630|661|662|749',Site)) %>%
  mutate(location = str_c("ABMI-",Site,"-",Quadrant),
         visitDate = parse_date_time(str_c(`Field Date`," ",`ARU Time Set`), "%Y-%m-%d %H:%M")) %>%
  select(location, visitDate) %>%
  mutate(visitDate = force_tz(visitDate, "US/Mountain"))

lates <- late19 %>%
  group_by(location) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(recording_date_time = force_tz(recording_date_time, "US/Mountain"),
         recording_date_timez = recording_date_time - minutes(60)) %>%
  inner_join(., vis, by = c("location" = "location")) %>%
  mutate(diff_mins = interval(recording_date_timez, visitDate) %/% minutes()) %>%
  select(location, recording_date_time, visitDate, diff_mins) %>%
  arrange(-diff_mins)

library(tictoc)
tic()
.GlobalEnv$wt_run_ap(late19 %>% filter(location == "ABMI-596-NW") %>% slice(1:120), output_dir = "/users/alexandremacphail/desktop/eh19late", path_to_ap = AP)
toc()

eh19_g <- .GlobalEnv$wt_glean_ap(x = late19 %>% filter(location == "ABMI-596-NW") %>% slice(1:120), input_dir = "/users/alexandremacphail/desktop/eh19late")


ggplot(late19, aes(x=julian,y=location)) + geom_point() +
  theme_bw() +
  geom_vline(xintercept = 210, colour = "red") +
  geom_vline(xintercept = 90, colour = "red")




locs <- lates %>% select(location) %>% distinct() %>% pull()
