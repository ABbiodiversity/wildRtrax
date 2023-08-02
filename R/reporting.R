#' Build automatic reports from analyzed data in wildRtrax
#'
#' @section `wt_report` details:
#'
#' @description Build reports automatically using custom settings
#'
#' @param data Input data from wt_download_report
#' @param what NULL, conclusion
#' @param output_filename Character; name of the output file name
#'
#' @import markdown
#' @export
#'
#' @examples
#' \dontrun{
#' wt_report(data = my_data, what = c(NULL, "conclusion"))
#' }
#'
#' @return A document with the report

wt_report <-
  function (data,
            what = c(NULL, "conclusion"),
            output_filename = "report") {
    rmd_org <- unique(data$organization)

    # Example usage:
    rmd_content_intro <- "
---
title: My Report
output:
  html_document:
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
---

<style type='text/css'>
  body{
  font-family: Montserrat;
}
</style>

# Introduction

This is an experimental function from wildRtrax.

## Species counts
```{r eval=T, include=T, echo=F}

library(tidyverse)

in_tbl_wtd <- data

aplot <- in_tbl_wtd %>%
  group_by(species_code) %>%
  tally() %>%
  filter(n > 10) %>%
  ggplot(., aes(x=reorder(species_code,n),y=n,fill=species_code)) +
  geom_bar(stat='identity') +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw() +
  xlab('Species')

aplot

```

This shows that `r in_tbl_wtd %>% group_by(species_code) %>% tally() %>% arrange(-n) %>% slice(1) %>% pull(species_code)` is the most abundant species.

### Time of arrival

Here's another graph.

```{r, eval=T, echo=F}

in_tbl_wtd2 <- data

aplot2 <- in_tbl_wtd2 %>%
  group_by(species_code) %>%
  add_tally() %>%
  filter(n > 10) %>%
  mutate(julian = lubridate::yday(recording_date_time)) %>%
  ggplot(., aes(x=reorder(species_code,julian), y=julian, fill=species_code)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw() +
  xlab('Species')

aplot2

```

"
    rmd_content_intro <- gsub("My Report", rmd_org, rmd_content_intro)
    rmd_content_intro <-
      gsub(
        "Species counts",
        paste0("Species counts in organization ", rmd_org),
        rmd_content_intro
      )

    rmd_content_conclusion <- "

## Conclusion

The concluding thoughts.

"

    if (is.null(what)) {
      rmd_merge_content <- rmd_content_intro
    } else if (what == "conclusion") {
      rmd_merge_content <-
        paste(rmd_content_intro, rmd_content_conclusion)
    } else {
      stop("Did nothing")
    }

    rmd_file <- paste0(output_filename, ".Rmd")
    writeLines(rmd_merge_content, con = rmd_file)

    rmarkdown::render(rmd_file, output_format = "html_document")

    file.remove(rmd_file)

  }
