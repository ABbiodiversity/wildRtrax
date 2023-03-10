library(tidyverse)

fired <- c("Zachary A Kahn","Jamie-Lynn Greter")

dn <- data

dn %>%
  select(location, recording_date, observer, status) %>%
  distinct() %>%
  group_by(observer, status) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = status, values_from = n) %>%
  rowwise() %>%
  mutate(t = sum(c_across(!observer), na.rm=T),
         comp = sum(c_across(Transcribed:`Bad Weather`), na.rm=T) / t)

dn %>%
  filter(!species_code %in% abiotic_codes) %>%
  mutate(u = case_when(grepl('^U',species_code) ~ "Unknown", TRUE ~ "Else")) %>%
  group_by(observer, u) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = u, values_from = n) %>%
  mutate(prop = Unknown / (Else + Unknown))

dn %>%
  filter(!species_code %in% abiotic_codes,
         !grepl('^U',species_code)) %>%
  group_by(observer) %>%
  summarise(n = n_distinct(species_code)) %>%
  ungroup()

dn %>%
  filter(!species_code %in% abiotic_codes,
         !grepl('^U',species_code)) %>%
  group_by(observer, tag_clip_status) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = tag_clip_status, values_from = n) %>%
  mutate(prop = NEEDS_REVIEW / (`NA` + NEEDS_REVIEW))










 rj <- read_csv('/users/alexandremacphail/desktop/data-1678470891102.csv') %>%
   pivot_wider(names_from = tag_clip_is_verified, values_from = count) %>%
   mutate(`TRUE` = replace_na(`TRUE`,0)) %>%
   mutate(n = round(`TRUE`/(`FALSE` + `TRUE`),2))

