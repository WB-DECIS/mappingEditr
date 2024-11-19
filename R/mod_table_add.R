# mod_table_add.R

#' Add table Module UI
#'
#' @description A Shiny module UI for adding tables in the JSON data.
#'
#' @param id Module ID
#'
#' @return A UI with buttons to add tables.
#' @export
add_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("add_table"), "Add Table", class = "btn-success me-2")
}

#' Add table Module Server
#'
#' @description A Shiny module server for adding tables to the JSON data.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#' @param selected_table_name A reactive value containing the name of the selected table.
#' @param update_table_choices A function to update the table choices in the selector.
#'
#' @return None.
#' @export
add_table_server <- function(id, json_data, selected_table_name, update_table_choices) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$add_table, {
      shiny::showModal(shiny::modalDialog(
        title = "Add New Table",
        shiny::textInput(session$ns("new_table_name"), "Enter new table name:"),
        shiny::radioButtons(session$ns("table_type"), "Select table type:",
                            choices = c("Fixed", "Mapping"),
                            selected = "Fixed"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_add_table"), "Add Table")
        )
      ))
    })

    shiny::observeEvent(input$confirm_add_table, {
      shiny::req(input$new_table_name, input$table_type)
      new_table_name <- input$new_table_name
      if (new_table_name != "" && !new_table_name %in% names(json_data())) {
        full_data <- json_data()
        if (input$table_type == "Fixed") {
          full_data[[new_table_name]] <- data.frame(FIXED = character(0), stringsAsFactors = FALSE)
        } else {
          full_data[[new_table_name]] <- data.frame(FROM = character(0), TO = character(0), stringsAsFactors = FALSE)
        }
        json_data(full_data)
        # Use the update function from the table selector module
        update_table_choices(choices = names(full_data), selected = new_table_name)
        shiny::removeModal()
        shiny::showNotification("New table added successfully!", type = "message")
      } else {
        shiny::showNotification("Invalid table name or table already exists.", type = "error")
      }
    })
  })
}
