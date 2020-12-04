library(shiny)
library(ggplot2)
library(abmi.themes)
library(dplyr)
library(purrr)

server <- function(input, output, session) {

  output$plot <- renderPlot({

    ns <- 1:8
    results <- purrr::map_dbl(.x = ns,
                              .f = ~ wildRtrax::wt_prob_det(
                              species_code = input$species,
                              survey_length = input$survey,
                              number_of_surveys = .x))

    df <- data.frame(ns, results) %>%
      mutate(results = round(results, digits = 2))

    abmi.themes::add_abmi_fonts()

    p <- ggplot(df) +
      geom_line(aes(x = ns, y = results), color = input$colour, linetype = "dashed") +
      geom_point(aes(x = ns, y = results), size = 6.5, color = input$colour) +
      geom_text(aes(x = ns, y = results, label = results),
                size = 3, nudge_x = 0.25, nudge_y = -0.015, color = input$colour) +
      labs(x = paste0("Number of (", input$survey, " minute) surveys conducted"),
           y = "Probability of Detection",
           title = paste0("Probability of detection for ", input$species),
           subtitle = "Given that the species is present at the site.") +
      scale_x_continuous(breaks = seq(1, 8, by = 1)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
      theme_abmi()

    abmi.themes::add_logo(p)

  })

  output$image <- renderImage({

    file_name <- paste0(input$species, '.jpg')
    file_path <- file.path(system.file(paste0("extdata/images/", file_name), package = "wildRtrax"))

    list(src = file_path,
         width = 500,
         height = 350)

  }, deleteFile = FALSE)

}
