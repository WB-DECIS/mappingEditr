#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # File loader module
  file_loader <- file_loader_server("file_loader")
  json_data <- file_loader$json_data
  file_path <- file_loader$file_path

  # Table selector module
  table_selector <- table_selector_server("table_selector", json_data)
  selected_table_name <- table_selector$selected_table_name
  update_table_choices <- table_selector$update_table_choices  # Get the update function

  # Table editor module
  table_editor <- table_editor_server("table_editor", selected_table_name, json_data)
  selected_table <- table_editor$selected_table
  selected_rows <- table_editor$selected_rows

  # Row manager module
  row_manager_server("row_manager", selected_table_name, selected_table, selected_rows, json_data)

  # Table manager module
  table_manager_server("table_manager", json_data, selected_table_name, update_table_choices)

  # Download module
  download_server("download", json_data, file_path)
}
