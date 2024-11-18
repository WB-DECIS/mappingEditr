# mod_row_manager.R

#' Row Manager Module UI
#'
#' @description A Shiny module UI for adding and deleting rows in the table.
#'
#' @param id Module ID
#'
#' @return A UI with buttons to add and delete rows.
#' @export
row_manager_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex justify-content-start mb-3",
      shiny::actionButton(ns("add_row"), "Add Row", class = "btn-info me-2"),
      shiny::actionButton(ns("delete_row"), "Delete Selected Row", class = "btn-warning")
    )
  )
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
row_manager_server <- function(id, selected_table_name, selected_table, selected_rows, json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$add_row, {
      shiny::req(selected_table())
      current_table <- selected_table()
      new_row <- as.data.frame(lapply(current_table, function(x) NA), stringsAsFactors = FALSE)
      updated_table <- rbind(current_table, new_row)
      selected_table(updated_table)

      # Update the main json_data
      full_data <- json_data()
      full_data[[selected_table_name()]] <- updated_table
      json_data(full_data)
    })

    shiny::observeEvent(input$delete_row, {
      shiny::req(selected_table(), selected_rows())
      current_table <- selected_table()
      if (length(selected_rows()) > 0) {
        updated_table <- current_table[-selected_rows(), , drop = FALSE]
        selected_table(updated_table)

        # Update the main json_data
        full_data <- json_data()
        full_data[[selected_table_name()]] <- updated_table
        json_data(full_data)

        shiny::showNotification("Row deleted successfully!", type = "message")
      } else {
        shiny::showNotification("Please select a row to delete.", type = "warning")
      }
    })
  })
}
