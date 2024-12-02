#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input,
                       output,
                       session) {
  # Instance selector module
  instance_selector <- select_fmr_server("fmr_selector")
  selected_instance_url <- instance_selector$selected_instance_url

  # DSD selector module
  dsd_selector <- select_dsd_server("select_dsd",
                                    selected_instance_url)
  selected_dsd_id <- dsd_selector$selected_dsd_id

  # Initialize new mapping module
  initialize_map_server("initialize_map",
                        json_data,
                        selected_instance_url,
                        selected_dsd_id)
  # File loader module
  file_loader <- file_loader_server("file_loader")
  json_data <- file_loader$json_data
  file_path <- file_loader$file_path

  # Table selector module
  table_selector <- select_table_server("table_selector",
                                          json_data)
  selected_table_name <- table_selector$selected_table_name
  update_table_choices <- table_selector$update_table_choices  # Get the update function

  # Table definition module
  define_mapping_type_server(
    id = "table_definition",
    json_data = json_data,
    selected_table_name = selected_table_name
  )

  # Table editor module
  table_editor <- edit_table_server("table_editor",
                                      selected_table_name,
                                      json_data)
  selected_table <- table_editor$selected_table
  selected_rows <- table_editor$selected_rows

  # Add row module
  add_row_server("add_row",
                 selected_table_name,
                 selected_table,
                 selected_rows,
                 json_data)
  # Delete row module
  delete_row_server("delete_row",
                    selected_table_name,
                    selected_table,
                    selected_rows,
                    json_data)
  # Add table module
  add_table_server("add_table",
                   json_data,
                   selected_table_name,
                   update_table_choices)
  # Delete table module
  delete_table_server("delete_table",
                      json_data,
                      selected_table_name,
                      update_table_choices)

  # Download module
  download_server("download",
                  json_data,
                  file_path)

  # List viewer module
  view_list_server("view_list",
                   file_path)
}
