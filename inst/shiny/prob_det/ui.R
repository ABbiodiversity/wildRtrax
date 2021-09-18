library(shiny)

ui <- fluidPage(

  titlePanel("Detection Probability"),

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "species",
                  label = "Select a species:",
                  choices = c("Ovenbird" = "OVEN",
                              "Clay-colored Sparrow" = "CCSP",
                              "Olive-sided Flycatcher" = "OSFL",
                              "Tennessee Warbler" = "TEWA",
<<<<<<< HEAD
                              "White-throated Sparrow" = "WTSP",
=======
                              "White-throated Sparrow" = "WTSP"),
>>>>>>> c10f21c61c7af1c2d7f37285d9bf3e17897f7914
                              "Alder Flycatcher" = "ALFL",
                              "Swainson's Thrush" = "SWTH",
                              "Common Nighthawk" = "CONI",
                              "Lincoln's Sparrow" = "LISP",
<<<<<<< HEAD
                              "Yellow-rumped Warbler" = "YRWA"),
=======
                              "Yellow-rumped Warbler" = "YRWA",
>>>>>>> c10f21c61c7af1c2d7f37285d9bf3e17897f7914
                  selected = "OSFL"),
      numericInput(inputId = "survey",
                   label = "Acoustic survey length (in minutes):",
                   value = 2,
                   min = 1,
                   max = 3,
                   step = 1),
      radioButtons(inputId = "colour",
                   label = "Display colour:",
                   choices = c("Navy blue" = "#2D415B",
                               "Green" = "#A8AF8C",
                               "Light blue" = "#829EBC",
                               "Purple" = "#9A839C",
                               "Red" = "#C7641F",
                               "Grey" = "#A19E99",
                               "Dark blue" = "#26507D",
                               "Dark green" = "#309C62"),
                   selected = "#2D415B"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    plotOutput("plot", height = 600),
                    plotOutput("image", height = 600),
                    cellArgs = list(style = "padding: 10px"))),
      width = 9
    )
  )
)
