#' Download Module UI
#'
#' @description A Shiny module UI for downloading the updated JSON file.
#'
#' @param id Module ID
#'
#' @return A `downloadButton` UI element.
#' @export
download_ui <- function(id) {
  # Create a namespace function based on the given ID
  ns <- shiny::NS(id)

  # Create a download button with a specific ID and label
  shiny::downloadButton(
    outputId = ns("download"), # Namespaced ID for the download button
    label = "Download mapping file", # Button label
    class = "btn-primary" # Optional CSS class for styling
  )
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
  # Define the module server logic
  shiny::moduleServer(id, function(input, output, session) {

    # Define the download handler for the button
    output$download <- shiny::downloadHandler(

      # Dynamic filename: prepend "updated_" to the original file's basename
      filename = function() {
        paste0("updated_", basename(file_path()))
      },
      # Define the content of the file to be downloaded
      content = function(file) {
        # Convert the reactive JSON data to a formatted JSON string
        json_data_to_save <- jsonlite::toJSON(json_data(),
                                              pretty = TRUE,
                                              auto_unbox = TRUE,
                                              na = "string")

        # Write the JSON string to the file path provided by the `downloadHandler`
        writeLines(json_data_to_save, file)
      }
    )
  })
}
