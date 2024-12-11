# mod_table_editor.R

#' Table Editor Module UI
#'
#' @description A Shiny module UI for displaying and editing the selected table.
#'
#' @param id Module ID
#'
#' @return A `DTOutput` for displaying the editable table.
#' @export
edit_table_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns("table"))
}

#' Table Editor Module Server
#'
#' @description A Shiny module server for handling table editing.
#'
#' @param id Module ID
#' @param selected_table_name A reactive value containing the name of the selected table.
#' @param json_data A reactive value containing the JSON data.
#'
#' @return A list containing the reactive `selected_table` and `selected_rows`.
#' @export
edit_table_server <- function(id, selected_table_name, json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_table <- shiny::reactiveVal(NULL)

    shiny::observeEvent(selected_table_name(), {
      shiny::req(json_data(), selected_table_name())
      selected_data <- select_correct_table(json_data = json_data(),
                                            table_name = selected_table_name())

      if (is.data.frame(selected_data)) {
        selected_table(selected_data)
      } else {
        selected_table(as.data.frame(selected_data))
      }
    })

    output$table <- DT::renderDT({
      shiny::req(selected_table())
      DT::datatable(selected_table(), editable = TRUE, selection = 'single')
    })

    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      updated_data <- selected_table()
      updated_data[info$row, info$col] <- info$value
      selected_table(updated_data)

      # Update the main json_data
      full_data <- update_correct_table(json_data = json_data(),
                                        table_name = selected_table_name(),
                                        updated_data = updated_data)
      json_data(full_data)
    })

    selected_rows <- shiny::reactive({
      input$table_rows_selected
    })

    return(list(
      selected_table = selected_table,
      selected_rows = selected_rows
    ))
  })
}
