# mod_table_selector.R

#' Table Selector Module UI
#'
#' @description A Shiny module UI for selecting tables from the JSON data.
#'
#' @param id Module ID
#'
#' @return A UI output for selecting a table to edit.
#' @export
table_selector_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("table_selector"))
}

#' Table Selector Module Server
#'
#' @description A Shiny module server for handling table selection.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#'
#' @return A list containing the reactive `selected_table_name` and an `update_table_choices` function.
#' @export
table_selector_server <- function(id, json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table_selector <- shiny::renderUI({
      shiny::req(json_data())
      tables <- names(json_data())
      shiny::selectInput(session$ns("table_choice"), "Select table to edit:", choices = tables)
    })

    selected_table_name <- shiny::reactive({
      input$table_choice
    })

    # Function to update table choices
    update_table_choices <- function(choices, selected = NULL) {
      shiny::updateSelectInput(session, "table_choice", choices = choices, selected = selected)
    }

    return(list(
      selected_table_name = selected_table_name,
      update_table_choices = update_table_choices
    ))
  })
}
