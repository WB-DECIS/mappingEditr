# mod_download.R

#' Download Module UI
#'
#' @description A Shiny module UI for downloading the updated JSON file.
#'
#' @param id Module ID
#'
#' @return A `downloadButton` UI element.
#' @export
download_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("download"), "Download JSON", class = "btn-primary")
}

#' Download Module Server
#'
#' @description A Shiny module server for handling JSON file download.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#' @param file_path A reactive value containing the original file path.
#'
#' @return None.
#' @export
download_server <- function(id, json_data, file_path) {
  shiny::moduleServer(id, function(input, output, session) {
    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("updated_", basename(file_path()))
      },
      content = function(file) {
        json_data_to_save <- jsonlite::toJSON(json_data(), pretty = TRUE, auto_unbox = TRUE)
        writeLines(json_data_to_save, file)
      }
    )
  })
}
