library(tidyverse)

read_csv("/users/alexandremacphail/desktop/data-1680297494513.csv") %>%
  pivot_wider(names_from = tag_clip_is_verified, values_from = count, values_fill = 0) %>%
  mutate(prop = round(`TRUE` / (`TRUE` + `FALSE`), 2)) %>%
  filter(grepl('^Crooked',project_full_nm))
