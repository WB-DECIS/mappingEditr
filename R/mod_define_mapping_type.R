# Table Definition Module

#' Table Definition Module UI
#'
#' @param id Module ID
#' @return UI elements for the table definition module.
#' @export
define_mapping_type_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("table"))
  )
}

#' Table Definition Module Server
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#' @param selected_table_name A reactive expression for the selected table name.
#' @return Nothing; modifies `json_data` in place when a table type is defined.
#' @export
define_mapping_type_server <- function(id, json_data, selected_table_name) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_table <- shiny::reactiveVal(NULL)

    shiny::observeEvent(selected_table_name(), {
      table_name <- selected_table_name()
      shiny::req(json_data())
      shiny::req(table_name)
      table_data <- select_correct_table(json_data = json_data(), table_name = table_name)

      if (!is.data.frame(table_data)) {
        show_define_mapping_type_modal(
          ns = session$ns,
          table_name = table_name
        )

        shiny::observeEvent(input$confirm_define_mapping_type, {
          shiny::req(input$table_type)
          full_data <- json_data()
          full_data <- create_mapping_table(
            full_data,
            table_name,
            table_type = input$table_type
          )
          json_data(full_data)
          # Update selected_table with the new data
          table_data <- select_correct_table(json_data = full_data, table_name = table_name)
          selected_table(table_data)
          shiny::removeModal()
          shiny::showNotification(
            paste0(table_name, " table type successfully defined!"),
            type = "message"
          )
        })
      } else {
        # Update selected_table when table_data is a data.frame
        selected_table(table_data)
      }
    })

    output$table <- DT::renderDT({
      shiny::req(selected_table())
      DT::datatable(selected_table(), editable = TRUE, selection = 'single')
    })
  })
}

#' Creates a modal for user to define the mapping type
#'
#' @description Creates a modal dialog for defining the type of mapping.
#'
#' @param ns A namespace function, typically `session$ns`, to ensure unique IDs in the Shiny app.
#' @param title The title of the modal dialog. Defaults to "Define Table Type".
#' @param table_name The name of the selected table (to be displayed to the user).
#' @param table_types A character vector of table type options. Defaults to c("Fixed", "Mapping").
#' @param selected_type The default selected table type. Defaults to "Fixed".
#'
#' @return A Shiny modal dialog UI object.
#' @export
define_mapping_type_modal <- function(ns,
                                      title = "Define Table Type",
                                      table_name,
                                      table_types = c("Fixed", "Mapping"),
                                      selected_type = "Fixed") {
  shiny::modalDialog(
    title = title,
    shiny::tagList(
      shiny::h4("Selected Table:"),
      shiny::p(table_name), # Display the table name
      shiny::radioButtons(
        ns("table_type"),
        "Select table type:",
        choices = table_types,
        selected = selected_type
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_define_mapping_type"), "Confirm")
    )
  )
}

#' Show Define Mapping Type Modal
#'
#' This function shows a modal dialog for adding a new table, using `define_mapping_type_modal`.
#'
#' @param ns A namespace function for unique IDs.
#' @param title Title of the modal dialog.
#' @param table_types A vector of table type options.
#' @param selected_type The default selected table type.
#'
#' @export
show_define_mapping_type_modal <- function(ns,
                                           title = "Define Mapping Type",
                                           table_name,
                                           table_types = c("Fixed", "Mapping"),
                                           selected_type = "Fixed") {
  modal <- define_mapping_type_modal(ns,
                                     title,
                                     table_name,
                                     table_types,
                                     selected_type)
  shiny::showModal(modal) # Requires a live session
}



