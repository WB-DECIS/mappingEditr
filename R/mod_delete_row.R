# mod_row_manager.R

#' Delete row Module UI
#'
#' @description A Shiny module UI for deleting rows in the table.
#'
#' @param id Module ID
#'
#' @return A UI with buttons to delete rows.
#' @export
delete_row_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("delete_row"), "Delete Selected Row", class = "btn-warning")
}

#' Row Manager Module Server
#'
#' @description A Shiny module server for managing rows in the selected table.
#'
#' @param id Module ID
#' @param selected_table_name A reactive value containing the name of the selected table.
#' @param selected_table A reactive value containing the selected table data.
#' @param selected_rows A reactive value containing the indices of selected rows.
#' @param json_data A reactive value containing the JSON data.
#'
#' @return None.
#' @export
delete_row_server <- function(id, selected_table_name, selected_table, selected_rows, json_data) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$delete_row, {
      shiny::req(selected_table(), selected_rows())
      current_table <- selected_table()
      if (length(selected_rows()) > 0) {
        updated_table <- current_table[-selected_rows(), , drop = FALSE]

        # Update the main json_data
        full_data <- json_data()
        full_data$representation[[selected_table_name()]] <- updated_table
        json_data(full_data)
        selected_table(updated_table)

        shiny::showNotification("Row deleted successfully!", type = "message")
      } else {
        shiny::showNotification("Please select a row to delete.", type = "warning")
      }
    })
  })
}
