#' @title Mini UI
#'
#' @export
antadraft_addin <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  text <- con$selection[[1]]$text

  antadraft_gadget()
}


#' @import rvg
#' @import miniUI
#' @import shiny
#' @import shinyFiles
#' @importFrom tibble rownames_to_column
antadraft_gadget <- function() {

  ui <- miniPage(

    gadgetTitleBar("Antares data"),

    miniTabstripPanel(
      miniTabPanel("Raw files", icon = icon("upload"),
                   miniContentPanel(
                     shinyDirButton( "csv_dirs", "Folder select", "Please select a folder containing load files", FALSE ),
                     dataTableOutput(outputId = "csv_files")
                   )),
      miniTabPanel("Errors", icon = icon("check-square"),
                   miniContentPanel(
                     dataTableOutput(outputId = "data_viewer") )
                   ),

      miniTabPanel("Times series", icon = icon("area-chart"),
                   miniContentPanel(
                     dateRangeInput(inputId = "date_range", label = "Date range input:",
                                    start = Sys.Date()-2, end = Sys.Date() + 2),
                     selectInput(inputId = "country_id", label = "Choose a country", choices= character(0)),
                     plotOutput("ts_plot")
                  )
      )


    )
  )

  server <- function(input, output, session) {
    volumes <- getVolumes()
    shinyDirChoose(input, 'csv_dirs', roots= volumes , filetypes=c('csv'))

    output$csv_files <- renderDataTable({
      req(input$csv_dirs)
      path_data <- input$csv_dirs$path
      agg_files <- list.files(paste(path_data, collapse = "/"), pattern = "(\\.csv)$", full.names = TRUE)

      if( length(agg_files) ) {
        out <- file.info(agg_files)
        out <- rownames_to_column(out, var = "filename")
      } else out <- tibble()
      out
    })

    load_db <- eventReactive(input$csv_dirs, {
      req(input$csv_dirs)
      path_data <- input$csv_dirs$path
      read_load_files(paste(path_data, collapse = "/"))
    })

    load_datamart <- eventReactive(req(load_db()), {
      db <- fortify_from_rules(raw_db = load_db() )
      db

    })

    observeEvent(load_datamart(), {
      dat <- req(load_datamart())
      updateDateRangeInput(session = session,
                           inputId = "date_range", start = min(dat$DateTime),
                           end = max(dat$DateTime) )
      updateSelectInput(session = session, inputId = "country_id", choices = unique(dat$country) )
    })

    errors <- eventReactive(req(load_datamart()), {
      qualcon(load_datamart())
    })

    errors_periods <- eventReactive(req(errors()), {
      fortify_qualcon(errors())
    })


    output$data_viewer <- renderDataTable({
      dat <- errors_periods()
      dat
    })

    output$ts_plot <- renderPlot({
      req(input$date_range)
      req(input$country_id)
      req(load_datamart())
      db <- load_datamart()
      require("ggplot2")
      gg <- db %>%
        filter(country %in% input$country_id, between(as.Date(DateTime), input$date_range[1], input$date_range[2])) %>%
        ggplot(aes(x = DateTime, y = CTY)) + geom_line()
      print(gg)
    })

    observeEvent(input$done, {
      stopApp()
    })
    observeEvent(input$cancel, {
      stopApp()
    })

  }

  runGadget(ui, server)
}


