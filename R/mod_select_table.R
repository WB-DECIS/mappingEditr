# Table Selector Module

#' Table Selector Module UI
#'
#' @param id Module ID
#' @return A `selectInput` for selecting a table to edit.
#' @export
select_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectInput(
    inputId = ns("table_choice"),
    label = "Select table to edit:",
    choices = NULL  # Initialize with NULL; choices will be set in the server
  )
}

#' Table Selector Module Server
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#' @return A reactive expression for the selected table name.
#' @export
select_table_server <- function(id, json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Update the selectInput choices whenever json_data changes
    shiny::observeEvent(json_data(), {
      shiny::req(json_data())
      tables <- names(json_data())

      # Preserve current selection if possible
      current_selection <- input$table_choice
      if (!is.null(current_selection) && current_selection %in% tables) {
        selected <- current_selection
      } else {
        selected <- if (length(tables) > 0) tables[1] else NULL
      }

      shiny::updateSelectInput(
        session,
        inputId = "table_choice",
        choices = tables,
        selected = selected
      )

      # Use the update function from the table selector module
      update_table_choices(session = session,
                           choices = tables,
                           selected = input$table_choice)

      # Reactive value to track the selected table name
      selected_table_name <- shiny::reactive({
        shiny::req(input$table_choice)
        input$table_choice
      })
    })
  })



  return(list(
    selected_table_name = selected_table_name,
    update_table_choices = update_table_choices
  ))
}

# Function to update table choices (e.g., when tables are added or deleted)
update_table_choices <- function(session,
                                 choices,
                                 selected = NULL) {
  shiny::updateSelectInput(session,
                           "table_choice",
                           choices = choices,
                           selected = selected)
}
