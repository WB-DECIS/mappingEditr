# mod_table_delete.R

#' Delete table Module UI
#'
#' @description A Shiny module UI for deleting tables in the JSON data.
#'
#' @param id Module ID
#'
#' @return A UI with buttons to delete tables.
#' @export
delete_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("delete_table"), "Delete Table", class = "btn-danger")
}

#' Delete table Module Server
#'
#' @description A Shiny module server for deleting tables.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the full data.
#' @param selected_table_name A reactive value containing the name of the selected table.
#' @param update_table_choices A function to update the table choices in the selector.
#'
#' @return None.
#' @export
delete_table_server <- function(id, json_data, selected_table_name, update_table_choices) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$delete_table, {
      shiny::req(selected_table_name())
      shiny::showModal(shiny::modalDialog(
        title = "Delete Table",
        paste("Are you sure you want to delete the table:", selected_table_name(), "?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete_table"), "Delete Table", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_delete_table, {
      shiny::req(selected_table_name())
      full_data <- json_data()
      full_data[[selected_table_name()]] <- NULL
      json_data(full_data)
      # Use the update function from the table selector module
      update_table_choices(choices = names(full_data))
      shiny::removeModal()
      shiny::showNotification("Table deleted successfully!", type = "message")
    })
  })
}
