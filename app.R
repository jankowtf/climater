#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Package dependencies ----------------------------------------------------

library(magrittr)
library(shiny)
library(DT)
library(here)
library(philentropy)
library(simputation)
library(googleway)
library(dplyr)
library(fs)
library(here)
library(purrr)
library(purrrlyr)
library(rio)
library(stringr)
library(tibble)
library(tidyr)
library(usethis)

# library(promises)
# library(future)

# Core package:
runtype_local <- basename(here::here()) == "climater"
if (runtype_local) {
  devtools::load_all(here::here())
} else {
  library(climater)
}

# Options and settings ----------------------------------------------------

options(scipen = 10)
# options(shiny.suppressMissingContextError = TRUE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = recover)

settings <- default_settings()
settings$output$show <- FALSE
set_global_data_repo("repo_1", settings = settings)
# set_global_data_repo("repo_2", settings = settings)
map_key <- settings$api_keys$google_maps

# Initialization aspects --------------------------------------------------

if (runtype_local) {
  dat_db_0 <- data_read_db(vsn = "v3")
  source("app_load_data.R")
} else {
  dat_db <- readRDS("db_v3.rds")
  dat_db_0 <- dat_db
  dat_db_msr <- readRDS("db_msr_v3.rds")
  dat_station <- readRDS("station_v3.rds")
}

if ("future" %in% loadedNamespaces()) {
  plan(multiprocess)
}

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  # Title -----
  titlePanel("Your Perfect Climate - Traveller's App"),

  if (settings$ui_mode_map) {
    # inputPanel(
    div(
      h4("1. Enter your location into the text field"),
      googleway::google_mapOutput(outputId = "map", height = "220px")
    )
    # )
  } else {
    inputPanel(
      radioButtons("use_geo_auto", label = "Automatically detect location",
        choices = c("yes", "no"), selected = "no", inline = TRUE),

      numericInput("geo_lat", label = "Your latitude (decimal sep: .)",
        value = 0, min = -90, max = 90),

      numericInput("geo_long", label = "Your longtiude (decimal sep: .)",
        value = 0, min = -180, max = 180)
    )
  },

  div(
    h4("2. Choose your favourite climate using the handles"),
    inputPanel(

      selectInput("time_month", label = "Month of your travel",
        # choices = 1:12, selected = 7, multiple = FALSE),
        choices = list(
          "January" = 1,
          "Feburary" = 2,
          "March" = 3,
          "April" = 4,
          "May" = 5,
          "June" = 6,
          "July" = 7,
          "August" = 8,
          "September" = 9,
          "October" = 10,
          "November" = 11,
          "December" = 12
        ), selected = 7, multiple = FALSE),

      sliderInput("msr_distance", label = "Max. distance to destination (km)",
        min = 0,
        max = 10000,
        value = 5000,
        step = 1,
        ticks = FALSE),

      sliderInput("msr_temp_min", label = "Daily min. temperature (°C)",
        min = min(dat_db_0$msr_temp_min, na.rm = TRUE) %>% floor(),
        max = max(dat_db_0$msr_temp_min, na.rm = TRUE) %>% ceiling(),
        value = mean(dat_db_0$msr_temp_min, na.rm = TRUE),
        step = 0.5,
        ticks = FALSE),

      sliderInput("msr_temp_max", label = "Daily max. temperature (°C)",
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

      sliderInput("msr_sundur_avg", label = "Sunshine hours per day",
        min = min(dat_db_0$msr_sundur_avg, na.rm = TRUE) %>% floor(),
        max = max(dat_db_0$msr_sundur_avg, na.rm = TRUE) %>% ceiling(),
        value = mean(dat_db_0$msr_sundur_avg, na.rm = TRUE),
        step = 1,
        ticks = FALSE)
    )
  ),

  inputPanel(
    #   shiny::checkboxGroupInput("msr_inputs_chosen", label = "Choose inputs to use",
    #     choices = c(
    #       "Distance" = "msr_distance",
    #       # "Max. distance to destination" = "msr_distance",
    #       "Month" = "time_month",
    #       "Min. temperature" = "msr_temp_min",
    #       "Max. temperature" = "msr_temp_max",
    #       # "msr_temp_avg",
    #       # "msr_precip_min",
    #       # "msr_precip_max",
    #       "Rain days" = "msr_precip_avg",
    #       "Sunshine hours" = "msr_sundur_avg"
    #     ),
    #     selected = c("time_month", "msr_temp_avg")
    #   ),

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

    # selectInput("knn", label = "Number of recommendations",
    #   choices = 1:50, selected = 3),

    actionButton("do", "Submit", icon = icon("refresh"))

    # JS for automatic detection of geo location
    # Source: https://github.com/AugustT/shiny_geolocation
    # tags$script('
    #   $(document).ready(function () {
    #     navigator.geolocation.getCurrentPosition(onSuccess, onError);
    #
    #     function onError (err) {
    #       Shiny.onInputChange("geolocation", false);
    #     }
    #
    #     function onSuccess (position) {
    #       setTimeout(function () {
    #         var coords = position.coords;
    #         console.log(coords.latitude + ", " + coords.longitude);
    #         Shiny.onInputChange("geolocation", true);
    #         Shiny.onInputChange("geo_lat_auto", coords.latitude);
    #         Shiny.onInputChange("geo_long_auto", coords.longitude);
    #       }, 1100)
    #     }
    #   });
    # ')
  ),

  div(
    h3("Results"),
    if (!settings$dev_mode) {
      if (settings$ui_mode_tabs) {
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("Dream location",
              h3("Location with your perfect climate"),
              shiny::dataTableOutput("model_output_prime")
            ),
            tabPanel("Alternatives",
              h3("Alternative suggestions"),
              shiny::dataTableOutput("model_output_alts")
            )
          ),
          width = 12
        )
      } else {
        mainPanel(
          h3("Your dream location"),
          shiny::dataTableOutput("model_output_prime"),
          h3("Alternative suggestions"),
          shiny::dataTableOutput("model_output_alts"),
          width = 12
        )
      }
    } else {
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("Map", googleway::google_mapOutput(outputId = "map_model_ouput_prime")),
          tabPanel("Dream location", shiny::dataTableOutput("model_output_prime")),
          tabPanel("Alternatives", shiny::dataTableOutput("model_output_alts")),
          tabPanel("Inputs", shiny::dataTableOutput("model_input")),
          # tabPanel("Geo lat auto", shiny::textOutput("geo_lat_auto")),
          # tabPanel("Geo long auto", shiny::textOutput("geo_long_auto")),
          # tabPanel("Geo lat", shiny::textOutput("geo_lat")),
          # tabPanel("Geo long", shiny::textOutput("geo_long")),
          tabPanel("Geo location", shiny::tableOutput("geo_loc")),
          tabPanel("Distances", shiny::tableOutput("dat_distances"))
        ),
        width = 12
      )
    }
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Geolocation -----
  gm_google_map_react <- reactive({
    input$map_place_search
    gm_value <- google_map(key = map_key, search_box = TRUE,
      # event_return_type = 'json')
      event_return_type = 'list')
    gm_value
  })

  output$map <- renderGoogle_map({
    # gm_value <- google_map(key = map_key, search_box = TRUE,
    #   # event_return_type = 'json')
    #   event_return_type = 'list')
    # print(gm_value)
    # gm_value
    gm_google_map_react()
  })

  gm_geoloc_react <- eventReactive(input$map_place_search, {
    event <- input$map_place_search
    list(
      lat = event$lat,
      lon = event$lon
    )
  })
  observeEvent(input$map_place_search, {
    # event <- input$map_place_search
    gm_geoloc_react()
  })

  # Time transformation into cyclical variable(s) -----
  dat_db_msr <- dat_db_msr %>%
    dplyr::mutate(
      time_month_sin = sin(2 * pi * time_month / 12),
      time_month_cos = cos(2 * pi * time_month / 12)
    )

  # dat_distance_react <- reactive({
  #   dat_geo <- tibble::tibble(
  #     dim_latitude = ifelse(is.null(v <- input$geo_lat_auto), 1, v),
  #     dim_longitude = ifelse(is.null(v <- input$geo_long_auto), 1, v)
  #   )
  #   compute_geo_distance_v3(p_1 = dat_geo, p_2 = dat_station)
  # })

  # observe({
  #   input$geo_lat_auto
  #   updateSliderInput(session, "msr_distance",
  #     value = round(mean(dat_distance_react()  %>%
  #         dplyr::pull(msr_distance) %>% round(), na.rm = TRUE)),
  #     min = min(dat_distance_react()  %>%
  #         dplyr::pull(msr_distance) %>% round(), na.rm = TRUE),
  #     max = max(dat_distance_react()  %>%
  #         dplyr::pull(msr_distance) %>% round(), na.rm = TRUE),
  #     step = 1
  #   )
  # })

  dat_distance_react <- reactive({
    dat_geo <- tibble::tibble(
      dim_latitude = ifelse(is.null(v <- gm_geoloc_react()$lat), 1, v),
      dim_longitude = ifelse(is.null(v <- gm_geoloc_react()$lon), 1, v)
    )
    compute_geo_distance_v3(p_1 = dat_geo, p_2 = dat_station)
  })

  observeEvent(input$map_place_search, {
    updateSliderInput(session, "msr_distance",
      value = round(mean(dat_distance_react()  %>%
          dplyr::pull(msr_distance) %>% round(), na.rm = TRUE)),
      min = min(dat_distance_react()  %>%
          dplyr::pull(msr_distance) %>% round(), na.rm = TRUE),
      max = max(dat_distance_react()  %>%
          dplyr::pull(msr_distance) %>% round(), na.rm = TRUE),
      step = 1
    )
  })

  dat_input_react <- reactive({
    dat <- tibble::tibble(
      # dim_latitude = ifelse(input$use_geo_auto == "yes",
      #   ifelse(is.null(v <- input$geo_lat_auto), 0, as.numeric(v)),
      #   ifelse(is.null(v <- input$geo_lat), 0, as.numeric(v))
      # ),
      dim_latitude = ifelse(is.null(v <- gm_geoloc_react()$lat), 0, as.numeric(v)),

      # dim_longitude = ifelse(input$use_geo_auto == "yes",
      #   ifelse(is.null(v <- input$geo_long_auto), 0, as.numeric(v)),
      #   ifelse(is.null(v <- input$geo_long), 0, as.numeric(v))
      # ),
      dim_longitude = ifelse(is.null(v <- gm_geoloc_react()$lon), 0, as.numeric(v)),

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

    # Time transformation into cyclical variable(s) -----
    dat <- dat %>%
      dplyr::mutate(
        time_month_sin = sin(2 * pi * time_month / 12),
        time_month_cos = cos(2 * pi * time_month / 12)
      )

    # msr_inputs_chosen <- input$msr_inputs_chosen
    msr_inputs_chosen <- c(
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
    ) %>%
      unname()
    msr_inputs_chosen <- c("dim_latitude", "dim_longitude", msr_inputs_chosen)

    # Implicit inclusion of cos and sin verion of `time_month` -----
    if (any(stringr::str_detect(msr_inputs_chosen, "time"))) {
      msr_inputs_chosen <- c(msr_inputs_chosen, "time_month_sin", "time_month_cos")
    }

    # dat[ , msr_inputs_chosen]
    msr_inputs_chosen <- names(dat) %in% msr_inputs_chosen
    dat[ , !msr_inputs_chosen] <- NA
    dat
  })

  dat_model_output_prime_react <- eventReactive(input$do, {
    dat_input <- dat_input_react()
    # knn <- as.numeric(input$knn)
    knn <- settings$number_of_recommendations

    # future({
    model_run_prime(
      dat_input = dat_input,
      dat_db = dat_db_msr,
      dat_station = dat_station,
      knn = knn
    )$model_output
    # }, globals = list(
    #   settings = settings,
    #   model_run_v7 = model_run_v7,
    #   dat_db_msr = dat_db_msr,
    #   dat_input = dat_input,
    #   dat_station = dat_station,
    #   knn = knn
    # ))
  })

  # Capture distance of prime location -----
  dat_distance_prime_react <- reactive({
    dat_this <- dat_model_output_prime_react()

    if (!nrow(dat_this)) {
      tibble::tibble()
    } else {
      dat_this %>%
        dplyr::select(msr_distance) %>%
        dplyr::pull()
    }
  })

  dat_model_output_alts_react <- eventReactive(input$do, {
    dat_input <- dat_input_react()
    # knn <- as.numeric(input$knn)
    knn <- settings$number_of_recommendations

    # future({
    model_run(
      dat_input = dat_input,
      dat_db = dat_db_msr,
      dat_station = dat_station,
      knn = knn,
      msr_distance_prime = dat_distance_prime_react(),
      dat_model_output_prime_react = dat_model_output_prime_react(),
      session = session,
      expand_weight_grid = settings$expand_weight_grid
    )$model_output
    # }, globals = list(
    #   settings = settings,
    #   model_run = model_run,
    #   dat_db_msr = dat_db_msr,
    #   dat_input = dat_input,
    #   dat_station = dat_station,
    #   knn = knn,
    #   session = session
    # ))
  })

  # Output table for prime location -----
  output$model_output_prime <- shiny::renderDataTable({
    dat_this <- dat_model_output_prime_react()

    retval <- if (!nrow(dat_this)) {
      tibble::tibble(`Incompatible input settings` = NA)
    } else {
      dat_this %>%
        dat_transform_monthnumbers_to_monthnames() %>%
        dat_add_co2() %>%
        dat_transform_relevant_columns_minimal() %>%
        dplyr::mutate(msr_distance = msr_distance %>% round(4)) %>%
        dat_transform_names_to_label()
    }
    retval
  }, options = list(
    scrollX = TRUE,
    scrollY = "100px",
    paging = FALSE,
    lengthChange = FALSE,
    searching = FALSE,
    order = list(list(1, 'asc')))
  )
  # TODO-20180705: encapsulate column selection in own function to be more
  # flexible and make code easier to maintain

  # Output table for alternative locations -----
  output$model_output_alts <- shiny::renderDataTable({
    dat_this <- dat_model_output_alts_react()

    retval <- if (!nrow(dat_this)) {
      tibble::tibble(`Incompatible input settings` = NA)
    } else {
      dat_this %>%
        dat_transform_monthnumbers_to_monthnames() %>%
        dat_add_co2() %>%
        dat_transform_relevant_columns(dev_mode = settings$dev_mode) %>%
        dplyr::mutate(msr_distance = msr_distance %>% round(4)) %>%
        dplyr::distinct() %>%
        dplyr::arrange(msr_distance) %>%
        dat_transform_names_to_label()
    }
    retval
  }, options = list(
    scrollX = TRUE,
    scrollY = "400px",
    pageLength = 100,
    lengthChange = FALSE,
    searching = FALSE)
    # order = list(list(1, 'asc')))
  )
  # TODO-20180705: encapsulate column selection in own function to be more
  # flexible and make code easier to maintain

  output$model_input <- shiny::renderDataTable({
    dat_model_output_alts_react()$model_input %>%
      dat_transform_names_to_label()
  }, options = list(scrollX = TRUE))

  # output$geo_lat_auto <- shiny::renderPrint({
  #   input$geo_lat_auto
  # })
  #
  # output$geo_long_auto <- shiny::renderPrint({
  #   input$geo_long_auto
  # })
  #
  # output$geo_lat <- shiny::renderPrint({
  #   input$geo_lat
  # })
  #
  # output$geo_long <- shiny::renderPrint({
  #   input$geolocation
  # })
  output$geo_loc <- shiny::renderTable({
    dat_input_react() %>%
      dplyr::select(dim_latitude, dim_longitude)
  })


  output$dat_distances <- shiny::renderTable({
    dat_distance_react()  #%>%
    # dplyr::pull(msr_distance) %>%
    # round() %>%
    # summary()
    # dplyr::select(dim_station, msr_distance)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

