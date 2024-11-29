# mod_file_loader.R

#' File Loader Module UI
#'
#' @description A Shiny module UI for uploading JSON files.
#'
#' @param id Module ID
#'
#' @return A `fileInput` UI element for selecting a JSON file.
#' @export
file_loader_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fileInput(ns("file"), "Load existing mapping", accept = ".json")
}

#' File Loader Module Server
#'
#' @description A Shiny module server for handling JSON file uploads and parsing.
#'
#' @param id Module ID
#'
#' @return A list containing reactive values `json_data` and `file_path`.
#' @export
file_loader_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    json_data <- shiny::reactiveVal(NULL)
    file_path <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      file_path(input$file$datapath)
      json_data(jsonlite::fromJSON(input$file$datapath))
    })

    return(list(
      json_data = json_data,
      file_path = file_path
    ))
  })
}
