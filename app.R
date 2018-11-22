#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.suppressMissingContextError = TRUE)
library(DT)
if (basename(here::here()) == "climater") {
  devtools::load_all(here::here())
} else {
  library(climater)
}

settings <- default_settings()
settings$output$show <- FALSE
set_global_data_repo("repo_1", settings = settings)
# set_global_data_repo("repo_2", settings = settings)
dat_db_0 <- data_read_db(vsn = "v3")
source("04-load.R")

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  # Application title
  titlePanel("Your climate"),

  inputPanel(
    radioButtons("use_geo_auto", label = "Automatically detect location",
      choices = c("yes", "no"), selected = "no", inline = TRUE),

    numericInput("geo_lat", label = "Your latitude (decimal sep: .)",
      value = 0, min = -90, max = 90),

    numericInput("geo_long", label = "Your longtiude (decimal sep: .)",
      value = 0, min = -180, max = 180)
  ),

  inputPanel(
    sliderInput("msr_distance", label = "Max. distance to destination",
      min = 0,
      max = 1000,
      value = 100,
      step = 1,
      ticks = FALSE),

    selectInput("time_month", label = "Month of year",
      choices = 1:12, selected = 7),
    # sliderInput("time_month", label = "Month of year",
    #   min = 1, max = 12, value = 7, step = 1),

    sliderInput("msr_temp_min", label = "Min. temperature",
      min = min(dat_db_0$msr_temp_min, na.rm = TRUE) %>% floor(),
      max = max(dat_db_0$msr_temp_min, na.rm = TRUE) %>% ceiling(),
      value = mean(dat_db_0$msr_temp_min, na.rm = TRUE),
      step = 0.5,
      ticks = FALSE),

    sliderInput("msr_temp_max", label = "Max. temperature",
      min = min(dat_db_0$msr_temp_max, na.rm = TRUE) %>% floor(),
      max = max(dat_db_0$msr_temp_max, na.rm = TRUE) %>% ceiling(),
      value = mean(dat_db_0$msr_temp_min, na.rm = TRUE),
      step = 0.5,
      ticks = FALSE),

    # sliderInput("msr_temp_avg", label = "Avg. temperature",
    #   min = min(dat_db_0$msr_temp_avg, na.rm = TRUE) %>% floor(),
    #   max = max(dat_db_0$msr_temp_avg, na.rm = TRUE) %>% ceiling(),
    #   value = mean(dat_db_0$msr_temp_avg, na.rm = TRUE),
    #   step = 0.5,
    #   ticks = FALSE),

    # sliderInput("msr_precip_min", label = "Min. precipitation",
    #   min = min(dat_db_0$msr_precip_min, na.rm = TRUE) %>% floor(),
    #   max = max(dat_db_0$msr_precip_min, na.rm = TRUE) %>% ceiling(),
    #   value = mean(dat_db_0$msr_precip_min, na.rm = TRUE),
    #   step = 0.5,
    #   ticks = FALSE),
    #
    # sliderInput("msr_precip_max", label = "Max. precipitation",
    #   min = min(dat_db_0$msr_precip_max, na.rm = TRUE) %>% floor(),
    #   max = max(dat_db_0$msr_precip_max, na.rm = TRUE) %>% ceiling(),
    #   # value = mean(dat_db_0$msr_precip_min, na.rm = TRUE),
    #   value = 10,
    #   step = 0.5,
    #   ticks = FALSE),

    sliderInput("msr_precip_avg", label = "Rain days per month",
      min = min(dat_db_0$msr_precip_avg, na.rm = TRUE) %>% floor(),
      max = max(dat_db_0$msr_precip_avg, na.rm = TRUE) %>% ceiling(),
      value = 10,
      step = 1,
      ticks = FALSE),

    # shinyWidgets::sliderTextInput("msr_precip_avg", label = "Rain days per month",
    #   choices = input_precip(),
    #   selected = 0, grid = TRUE),

    sliderInput("msr_sundur_avg", label = "Sunshine hours per day",
      min = min(dat_db_0$msr_sundur_avg, na.rm = TRUE) %>% floor(),
      max = max(dat_db_0$msr_sundur_avg, na.rm = TRUE) %>% ceiling(),
      value = mean(dat_db_0$msr_sundur_avg, na.rm = TRUE),
      step = 1,
      ticks = FALSE)
  ),

  inputPanel(
    shiny::checkboxGroupInput("msr_inputs_chosen", label = "Choose inputs to use",
      choices = c(
        "Distance" = "msr_distance",
        # "Max. distance to destination" = "msr_distance",
        "Month" = "time_month",
        "Min. temperature" = "msr_temp_min",
        "Max. temperature" = "msr_temp_max",
        # "msr_temp_avg",
        # "msr_precip_min",
        # "msr_precip_max",
        "Rain days" = "msr_precip_avg",
        "Sunshine hours" = "msr_sundur_avg"
      ),
      selected = c("time_month", "msr_temp_avg")
    ),

    # shiny::selectInput("msr_inputs_chosen", label = "Choose inputs to use",
    #   choices = list(
    #     "Distance" = "msr_distance",
    #     # "Max. distance to destination" = "msr_distance",
    #     "Month" = "time_month",
    #     "Min. temperature" = "msr_temp_min",
    #     "Max. temperature" = "msr_temp_max",
    #     # "msr_temp_avg",
    #     # "msr_precip_min",
    #     # "msr_precip_max",
    #     "Rain days" = "msr_precip_avg",
    #     "Sunshine hours" = "msr_sundur_avg"
    #   ),
    #   selectize = TRUE,
    #   multiple = TRUE,
    #   selected = c("time_month", "msr_temp_avg")
    # ),

    selectInput("knn", label = "Number of recommendations",
      choices = 1:50, selected = 3),

    # selectInput("dist_measures", label = "Distance measures",
    #   choices = default_dist_measures(), selected = 1, multiple = TRUE),

    # selectInput("dist_measure_final", label = "Distance measures for ensemble",
    #   choices = default_dist_measures(), selected = 1),

    actionButton("do", "Submit", icon = icon("refresh")),

    # JS for automatic detection of geo location
    # Source: https://github.com/AugustT/shiny_geolocation
    tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);

        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }

        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("geo_lat_auto", coords.latitude);
            Shiny.onInputChange("geo_long_auto", coords.longitude);
          }, 1100)
        }
      });
              ')
  ),

  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel("Recommendations", shiny::dataTableOutput("prediction")),
      tabPanel("Inputs", shiny::dataTableOutput("input")),
      tabPanel("Geo lat auto", shiny::textOutput("geo_lat_auto")),
      tabPanel("Geo long auto", shiny::textOutput("geo_long_auto")),
      tabPanel("Geo lat", shiny::textOutput("geo_lat")),
      tabPanel("Geo long", shiny::textOutput("geo_long")),
      tabPanel("Distances", shiny::textOutput("dat_distances"))
    ),
    width = 12
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  dat_distance <- reactive({
    dat_geo <- data.frame(
      dim_latitude = ifelse(is.null(v <- input$geo_lat_auto), 1, v),
      dim_longitude = ifelse(is.null(v <- input$geo_long_auto), 1, v)
    )
    compute_geo_distance_v2(p_1 = dat_geo, p_2 = dat_station) %>%
      dplyr::pull(msr_distance) %>% round()
  })
  dist_measure <- "euclidean"
  dist_measure_final <- dist_measure

  observe({
    input$geo_lat_auto
    updateSliderInput(session, "msr_distance",
      value = round(mean(dat_distance(), na.rm = TRUE)),
      min = min(dat_distance(), na.rm = TRUE),
      max = max(dat_distance(), na.rm = TRUE),
      step = 1
    )
  })

  dat_input_rea <- reactive({
    dat <- data.frame(
      dim_latitude = ifelse(input$use_geo_auto == "yes",
        ifelse(is.null(v <- input$geo_lat_auto), 0, as.numeric(v)),
        ifelse(is.null(v <- input$geo_lat), 0, as.numeric(v))
      ),
      dim_longitude = ifelse(input$use_geo_auto == "yes",
        ifelse(is.null(v <- input$geo_long_auto), 0, as.numeric(v)),
        ifelse(is.null(v <- input$geo_long), 0, as.numeric(v))
      ),
      time_month = as.numeric(input$time_month),
      msr_temp_min = as.numeric(input$msr_temp_min),
      msr_temp_max = as.numeric(input$msr_temp_max),
      # msr_temp_avg = as.numeric(input$msr_temp_avg),
      # msr_precip_min = as.numeric(input$msr_precip_min),
      # msr_precip_max = as.numeric(input$msr_precip_max),
      msr_precip_avg = as.numeric(input$msr_precip_avg),
      msr_sundur_avg = as.numeric(input$msr_sundur_avg),
      msr_distance = as.numeric(input$msr_distance)
    )

    msr_inputs_chosen <- input$msr_inputs_chosen
    # print(msr_inputs_chosen)
    msr_inputs_chosen <- c("dim_latitude", "dim_longitude", msr_inputs_chosen)
    dat[ , msr_inputs_chosen]
  })

  dat_output_rea <- eventReactive(input$do, {
    dat_input <- dat_input_rea()
    knn <- as.numeric(input$knn)
    dist_measures <- input$dist_measures
    dist_measure_final <- input$dist_measure_final

    model_result <- model_run(
      dat_input = dat_input,
      dat_db = dat_db_msr,
      dat_station = dat_station,
      dist_measures = dist_measures,
      dist_measure_final,
      knn = knn
    )

    model_result
  })

  output$prediction <- shiny::renderDataTable({
    prediction <- dat_output_rea()$prediction %>%
      dat_transform_relevant_columns() %>%
      dplyr::mutate(msr_distance = msr_distance %>% round(4)) %>%
      dat_transform_names_to_label()

    prediction

  }, options = list(scrollX = TRUE))
  # TODO-20180705: encapsulate column selection in own function to be more
  # flexible and make code easier to maintain

  output$input <- shiny::renderDataTable({
    dat_output_rea()$input %>%
      dat_transform_names_to_label()
  }, options = list(scrollX = TRUE))

  output$geo_lat_auto <- shiny::renderPrint({
    input$geo_lat_auto
  })

  output$geo_long_auto <- shiny::renderPrint({
    input$geo_long_auto
  })

  output$geo_lat <- shiny::renderPrint({
    input$geo_lat
  })

  output$geo_long <- shiny::renderPrint({
    input$geolocation
  })

  output$dat_distances <- shiny::renderPrint({
    dat_distance() %>% summary()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
