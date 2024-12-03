# mod_row_manager.R

#' Add row Module UI
#'
#' @description A Shiny module UI for adding rows in the table.
#'
#' @param id Module ID
#'
#' @return A UI with buttons to add rows.
#' @export
add_row_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("add_row"), "Add Row", class = "btn-info me-2")
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
add_row_server <- function(id, selected_table_name, selected_table, selected_rows, json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$add_row, {
      shiny::req(selected_table())
      current_table <- selected_table()
      new_row <- as.data.frame(lapply(current_table, function(x) NA), stringsAsFactors = FALSE)
      updated_table <- rbind(current_table, new_row)
      selected_table(updated_table)

      # Update the main json_data
      full_data <- json_data()
      full_data$representation[[selected_table_name()]] <- updated_table
      json_data(full_data)
      selected_table(updated_table)
      # shiny::showNotification("Success!", type = "message")
    })

  })
}
