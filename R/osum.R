library(seewave)
library


osum_2018 <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/osum/2018", file_type = "all", extra_cols = F)

osum_2019 <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/osum/2019", file_type = "all", extra_cols = F)

osum_2020 <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/osum/2020/wav", file_type = "all", extra_cols = F)

bird18 <- read.csv("/users/alexandremacphail/desktop/osum/bird18.csv") %>%
  as_tibble() %>%
  select(new_f) %>%
  pull()

bird19 <- read.csv("/users/alexandremacphail/desktop/osum/bird19.csv") %>%
  as_tibble() %>%
  select(new_f) %>%
  pull()

ob19 <- osum_2019 %>%
  filter(file_name %in% bird19) %>%
  map(.x = .$file_path, .f = ~file.copy(.x, to = "/users/alexandremacphail/desktop/osum/files"))
