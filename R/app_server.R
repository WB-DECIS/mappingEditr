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

  # Add row module
  add_row_server("add_row", selected_table_name, selected_table, selected_rows, json_data)
  # Delete row module
  delete_row_server("delete_row", selected_table_name, selected_table, selected_rows, json_data)
  # Add table module
  add_table_server("add_table", json_data, selected_table_name, update_table_choices)
  # Delete table module
  delete_table_server("delete_table", json_data, selected_table_name, update_table_choices)

  # Download module
  download_server("download", json_data, file_path)
}
