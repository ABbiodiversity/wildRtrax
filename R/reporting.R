#' Ttitle
#' Description of family
#'
#' @section `wt_report` details:
#'
#' @description
#'
#' @param data
#' @param type basic, advanced
#' @param style abmi.themes, gitbook, flaired
#'
#' @import markdown
#' @export
#'
#' @examples
#' \dontrun{
#' wt_report(data = my_data, type = c("basic", "advanced"), style = "gitbook")
#' }
#'
#' @return A pdf document with the report

wt_report <- function (data, output_file = "report.html", output_dir = getwd(), open) {

  in_tbl_wtd <- raw_basic_data

  aplot <- in_tbl_wtd %>%
    group_by(species_code) %>%
    tally() %>%
    filter(n > 10) %>%
    ggplot(., aes(x=reorder(species_code,n),y=n,fill=species_code)) +
    geom_bar(stat="identity") +
    coord_flip() +
    abmi.themes::scale_fill_abmi() +
    abmi.themes::theme_abmi()

  save(aplot,"aplot.R")

  stitch(aplot, system.file("misc", "knitr-template.Rmd", package = "knitr"))

  report_path <- path.expand(file.path(output_dir, output_file))

  if (open == "auto") {
    browseURL(report_path)
  }

}

