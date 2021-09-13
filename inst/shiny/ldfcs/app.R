#-----------------------------------------------------------------------------------------------------------------------

# Title: Acoustic index and LDFC example.
# Description: Using data from ABMI's Ecosystem Health program from a single ARU deployed from March - July 2017 in boreal Alberta. These data are made available via an API.
# Authors: Alex MacPhail
# Date: 2021-09-13

#-----------------------------------------------------------------------------------------------------------------------

# Path to Google Drive
root <- "/volumes/GoogleDrive/Shared drives/wildRtrax/"

library(shiny)
library(shinythemes)
library(magick)
library(plotly)
library(tidyverse)

# Load data
data <- read.csv(paste0(root, "data/scanner_output.csv"), stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------------------------------------------------


ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Acoustic indices and LDFCs"),
  helpText(
    "This app allows you to dynamically explore acoustic indices and LDFCs derived from acoustic media output
        from wildRtrax functions (wt_audio_scanner and wt_run_ap). Acoustic indices are summaries of different sound energy measurements.
        LDFCs (long-duration false-colour spectrograms) are layered visualizations of the acoustic indices over long periods of time."
  ),
  tabsetPanel(
    tabPanel(
      "Data",
      helpText(
        "Choose a range of times of year. Numbers indicate julian date (i.e. days after January 1)"
      ),
      sliderInput(
        "juliandate",
        "Time of year",
        min = 60,
        max = 210,
        value = c(60, 90)
      ),
      helpText(
        "Choose a range of times of day. Numbers indicate hour of day (i.e. 05:35:00 would be shortened to 5)"
      ),
      sliderInput(
        "tod",
        "Time of day",
        min = 0,
        max = 23,
        value = c(0, 2)
      ),
      tabPanel("Plot", plotlyOutput("Indices")),
      mainPanel(
        h4("Acoustic index descriptions (Towsey 2018)"),
        h5(
          "Acoustic complexity: Quantifies the relative change in acoustic intensity in each bin of the amplitude spectrogram. It is widely used measured of biophony in environmental recordings but it sensitive to non-biological sound sources, such as rain."
        ),
        h5(
          "Background Noise: An estimate of the background noise in each one-minute recording. Equal to the mode of the energy distribution in the waveform envelope."
        ),
        h5(
          "High Frequency Cover: The fraction of noise-reduced spectrogram cells that exceed 3 dB in the high-frequency band (8000-11025 Hz)."
        ),
        h5(
          "Low Frequency Cover: The fraction of noise-reduced spectrogram cells that exceed 3 dB in the low-frequency band (1-1000 Hz)."
        ),
        h5(
          "Mid Frequeny Cover: The fraction of noise-reduced spectrogram cells that exceed 3 dB in the mid-frequency band (1000-8000 Hz)."
        ),
        h5(
          "Normalized Differenced Soundscape Index: The difference between the biophonic and anthropophonic energy signals."
        ),
        h5(
          "Signal to noise ratio: Difference between the maximum decibel value in the decibel envelope and the decibel value of the Background Noise."
        ),
        h5(
          "Temporal Entropy: Entropy of the energy (squared amplitude) values of the signal derived from the waveform envelope. Normalized to unit area and treated as a probability mass function."
        )
      )
    ),
    tabPanel(
      "LDFC",
      helpText("Flip the color scheme"),
      selectInput("negate", "Flip colours", choices = c("Yes", "No")),
      helpText(
        "Stack the LDFCs by spatial location - useful if you're looking at multiple locations at once."
      ),
      selectInput("split", "Stack by location", choices = c("Yes", "No")),
      helpText(
        "This is the long-duraton false-colour spectrogram, go back to the Data tab to change the range of dates of times"
      ),
      mainPanel(plotOutput("ldfc"))
    ),
    tabPanel("Summary",
             fluidRow(
               mainPanel(
                 titlePanel("Results"),
                 downloadButton('download', "Download the data"),
                 tableOutput("view"),
               )
             )),
    tabPanel("Audio",
             actionButton("play", "Play!"),
             uiOutput("af"))
  )
)


server <- function(input, output) {

  test <- reactive({
    data %>% ########## REPLACE data WITH DATA FROM THE GOOGLE DRIVE SERVER ################
      mutate(hour = lubridate::hour(recording_date_time)) %>%
      filter(
        julian >= input$juliandate[1] & julian <= input$juliandate[2],
        hour >= input$tod[1] & hour <= input$tod[2]
      ) %>%
      select(
        location,
        recording_date_time,
        julian,
        hour,
        index_variable,
        index_value,
        file_path
      ) %>%
      mutate(
        recording_date = as.character(recording_date_time),
        julian = as.integer(julian),
        hour = as.integer(hour)
      ) %>%
      select(location,
             recording_date,
             julian,
             hour,
             index_variable,
             index_value,
             file_path) %>%
      distinct()
  })
  output$view <- renderTable({
    test()
  }, border = T, striped = T)
  output$download <-
    downloadHandler(
      filename = function() {
        "ldfc_app_results.csv"
      },
      content = function(fname) {
        write.csv(test(), fname)
      }
    )
  audio_files <- reactive({
    data %>%
      mutate(hour = lubridate::hour(recording_date_time)) %>%
      filter(
        julian >= input$juliandate[1] & julian <= input$juliandate[2],
        hour >= input$tod[1] & hour <= input$tod[2]
      ) %>%
      select(file_path) %>%
      unlist()
  })
  observeEvent(input$play, {
    output$af <-
      renderUI({
        tags$audio(
          src = audio_files(),
          type = "audio/wav",
          controls = T,
          autoplay = F
        )
      })
  })
  eless <- reactive({
    data %>%
      mutate(hour = lubridate::hour(recording_date_time)) %>%
      filter(
        julian >= input$juliandate[1] & julian <= input$juliandate[2],
        hour >= input$tod[1] & hour <= input$tod[2]
      )
  })
  output$Indices <- renderPlotly({
    #Plot a graph of the indices
    ggplotly(
      ggplot(
        eless(),
        aes(x = julian, y = index_value, colour = index_variable)
      ) +
        geom_boxplot() +
        theme_bw() +
        facet_wrap(~ index_variable, scales = "free_y") +
        theme(
          panel.spacing.x = unit(0.25, "lines"),
          panel.spacing.y = unit(1.5, "lines"),
          axis.title.x = element_text(vjust = -1.0)
        ) +
        xlab("Julian Date") + ylab("Acoustic index value") +
        scale_colour_viridis_d(guide = guide_legend()) +
        ggtitle("Average acoustic index values for each julian date")
    )
  })
  i2 <- reactive({
    i <- data %>%
      mutate(hour = lubridate::hour(recording_date_time)) %>%
      filter(
        julian >= input$juliandate[1] & julian <= input$juliandate[2],
        hour >= input$tod[1] & hour <= input$tod[2]
      ) %>%
      select(location, path) %>%
      distinct() %>%
      group_by(location) %>%
      group_map(~ image_append(image_read(.x$path))) %>%
      unlist()

    if (input$split == "Yes" & input$negate == "Yes") {
      pic <-
        image_scale(image_negate(image_append(image_join(i), stack = T)), "6000x4000!")
      return(pic)
    } else if (input$split == "No" & input$negate == "No") {
      pic <-
        image_scale(image_append(image_join(i), stack = F), "3960x1400!")
      return(pic)
    } else if (input$split == "No" & input$negate == "Yes") {
      pic <-
        image_negate(image_scale(
          image_annotate(image_append(image_join(i), stack = F), text = "name"),
          "3960x1400!"
        ))
      return(pic)
    } else if (input$split == "Yes" & input$negate == "No") {
      pic <-
        image_scale(image_append(image_join(i), stack = T), "6000x4000!")
      return(pic)
    }
  })
  output$ldfc <- renderPlot({
    image_ggplot(i2())
  })

}

shinyApp(ui = ui, server = server)
