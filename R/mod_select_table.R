# mod_table_selector.R

#' Table Selector Module UI
#'
#' @description A Shiny module UI for selecting tables from the JSON data.
#'
#' @param id Module ID
#'
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
#' @description A Shiny module server for handling table selection.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#'
#' @return A list containing the reactive `selected_table_name` and an `update_table_choices` function.
#' @export
select_table_server <- function(id, json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Update the selectInput choices whenever json_data changes
    shiny::observeEvent(json_data(), {
      shiny::req(json_data())
      #browser()
      choices <- parse_list_for_choices(json_data())
      tables <- choices$flat

      # Preserve current selection if possible
      current_selection <- input$table_choice
      if (!is.null(current_selection) && current_selection %in% tables) {
        selected <- current_selection
      } else {
        selected <- tables[1]  # Default to the first table if current selection is invalid
      }

      shiny::updateSelectInput(
        session,
        inputId = "table_choice",
        choices = choices$nested,
        selected = selected
      )
    })

    selected_table_name <- shiny::reactive({
      input$table_choice
    })

    # Function to update table choices (e.g., when tables are added or deleted)
    update_table_choices <- function(session, choices, selected = NULL) {
      shiny::updateSelectInput(session, "table_choice", choices = choices, selected = selected)
    }

    return(list(
      selected_table_name = selected_table_name,
      update_table_choices = update_table_choices
    ))
  })
}

#' Parse a List to Generate Nested Choices for Shiny Inputs
#'
#' This function processes a given list to create a nested structure suitable
#' for the `choices` argument in Shiny `selectInput()` components. It takes
#' two elements from the input list: `components` and `representation`, and
#' organizes them into a named list for dropdown menus.
#'
#' @param data_list A list with at least two elements:
#'   - `components`: A vector of component names.
#'   - `representation`: A named list of representations.
#' @return A named list where:
#'   - The `components` key contains a single element with a name and value of "components".
#'   - The `representation` key contains a list with the names of `representation` elements.
#' @examples
#' input_list <- list(
#'   components = c("mpg", "cyl", "disp"),
#'   representation = list(FREQ = NULL, REF_AREA = NULL, MEASURE = NULL)
#' )
#' parse_list_for_choices(input_list)
#'
#' @export
parse_list_for_choices <- function(data_list) {
  names_component <- "components"
  names_representation <- names(data_list$representation)
  flat <- c(names_component, names_representation)

  nested <- list(
    `components` = list(components = names_component),
    `representation` = setNames(as.list(names_representation), names_representation)
  )

  return(list(flat = flat,
              nested = nested))
}

