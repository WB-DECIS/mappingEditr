# mod_table_delete.R

#' Delete Table Module UI
#'
#' @description A Shiny module UI for deleting tables in the JSON data.
#'
#' @param id Module ID
#'
#' @return A UI with a button to trigger the deletion of a table.
#' @export
delete_table_ui <- function(id) {
  ns <- shiny::NS(id) # Create a namespace for the module
  shiny::actionButton(
    ns("delete_table"),   # Button ID within the module's namespace
    "Delete Table",       # Button label
    class = "btn-danger"  # Bootstrap class for styling (red button)
  )
}

#' Delete Table Module Server
#'
#' @description A Shiny module server for deleting tables from the JSON data.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the full dataset (list of tables).
#' @param selected_table_name A reactive value containing the name of the currently selected table.
#' @param update_table_choices A function to update the table choices in the selector dropdown.
#'
#' @return None. The function modifies the `json_data` reactive value in place.
#' @export
delete_table_server <- function(id, json_data, selected_table_name, update_table_choices) {
  shiny::moduleServer(id, function(input, output, session) {
    # STEP 1: Show modal if "Delete table" is clicked
    shiny::observeEvent(input$delete_table, {
      shiny::req(selected_table_name()) # Ensure a table is selected

      # Show a confirmation modal dialog
      shiny::showModal(shiny::modalDialog(
        title = "Delete Table", # Modal title
        paste(
          "Are you sure you want to delete the table:",
          selected_table_name(), "?" # Dynamic confirmation message
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),                           # Cancel button to close the modal
          shiny::actionButton(
            session$ns("confirm_delete_table"),                   # Button ID for confirmation
            "Delete Table",                                       # Button label
            class = "btn-danger"                                 # Styling for a danger action
          )
        )
      ))
    })

    # Handle table deletion when the user confirms
    shiny::observeEvent(input$confirm_delete_table, {
      shiny::req(selected_table_name()) # Ensure a table is still selected
      full_data <- json_data()          # Get the current dataset
      full_data$representation[[selected_table_name()]] <- NULL # Remove the selected table
      json_data(full_data)              # Update the reactive dataset

      # STEP 2: Update the table selector to reflect the change
      update_table_choices(choices = names(full_data))

      # Close the modal dialog
      shiny::removeModal()

      # Show a success notification
      shiny::showNotification("Table deleted successfully!", type = "message")
    })
  })
}
