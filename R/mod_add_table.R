# mod_table_add.R

#' Add Table Module UI
#'
#' @description A Shiny module UI for adding tables to the JSON data.
#'
#' @param id Module ID
#'
#' @return A UI with a button to trigger the addition of a table.
#' @export
add_table_ui <- function(id) {
  ns <- shiny::NS(id) # Create a namespace for the module
  shiny::actionButton(
    ns("add_table"),     # Button ID within the module's namespace
    "Add Table",         # Button label
    class = "btn-success me-2" # Bootstrap classes for styling (green button with spacing)
  )
}

#' Add Table Module Server
#'
#' @description A Shiny module server for adding tables to the JSON data.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data (list of tables).
#' @param selected_table_name A reactive value containing the name of the currently selected table.
#' @param update_table_choices A function to update the table choices in the selector dropdown.
#'
#' @return None. The function modifies the `json_data` reactive value in place.
#' @export
add_table_server <- function(id, json_data, selected_table_name, update_table_choices) {
  shiny::moduleServer(id, function(input, output, session) {
    # STEP 1: Show modal if "Add table" if clicked
    shiny::observeEvent(input$add_table, {
      show_add_table_modal(ns = session$ns) # Show modal for table creation
    })

    # STEP 2: Add new table
    shiny::observeEvent(input$confirm_add_table, {
      shiny::req(input$new_table_name, input$table_type) # Ensure inputs are not NULL or empty

      new_table_name <- input$new_table_name # Capture the new table name from the modal input

      # Check if the table name is valid and does not already exist
      if (new_table_name != "" && !new_table_name %in% names(json_data())) {
        full_data <- json_data() # Get the current dataset
        # Create the new table to the dataset
        full_data <- create_mapping_table(
          full_data,
          new_table_name,
          table_type = input$table_type
        )
        json_data(full_data) # Update the reactive dataset with the new table

        # STEP 3: Update the table choices in the table dropdown menu
        update_table_choices(
          choices = names(full_data),
          selected = new_table_name # Auto-select the newly added table
        )

        shiny::removeModal() # Close the modal dialog
        shiny::showNotification("New table added successfully!", type = "message") # Success notification
      } else {
        # Error notification if the table name is invalid or already exists
        shiny::showNotification("Invalid table name or table already exists.", type = "error")
      }
    })
  })
}

# Utility functions

#' Generate UI for Add Table Modal
#'
#' This function generates a modal dialog UI object for adding a new table.
#'
#' @param ns A namespace function, typically `session$ns`, to ensure unique IDs in the Shiny app.
#' @param title The title of the modal dialog. Defaults to "Add New Table".
#' @param table_types A character vector of table type options. Defaults to c("Fixed", "Mapping").
#' @param selected_type The default selected table type. Defaults to "Fixed".
#'
#' @return A Shiny modal dialog UI object.
#' @export
generate_add_table_modal <- function(ns,
                                     title = "Add New Table",
                                     table_types = c("Fixed", "Mapping"),
                                     selected_type = "Fixed") {
  shiny::modalDialog(
    title = title, # Modal title
    shiny::textInput(
      ns("new_table_name"), # Input for entering the table name
      "Enter new table name:"
    ),
    shiny::radioButtons(
      ns("table_type"), # Input for selecting the table type
      "Select table type:",
      choices = table_types,   # Available table types
      selected = selected_type # Default selected type
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),                          # Close modal without changes
      shiny::actionButton(ns("confirm_add_table"), "Add Table") # Confirm addition
    )
  )
}

#' Show Add Table Modal
#'
#' This function shows a modal dialog for adding a new table, using `generate_add_table_modal`.
#'
#' @param ns A namespace function for unique IDs.
#' @param title Title of the modal dialog.
#' @param table_types A vector of table type options.
#' @param selected_type The default selected table type.
#'
#' @export
show_add_table_modal <- function(ns,
                                 title = "Add New Table",
                                 table_types = c("Fixed", "Mapping"),
                                 selected_type = "Fixed") {
  # Generate the modal dialog UI
  modal <- generate_add_table_modal(
    ns,
    title,
    table_types,
    selected_type
  )
  shiny::showModal(modal) # Display the modal dialog
}
