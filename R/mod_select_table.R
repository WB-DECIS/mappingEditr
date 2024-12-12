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
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("table_choice"),
      label = "Select table to edit:",
      choices = NULL  # Initialize with NULL; choices will be set in the server
    )#,
      #DT::DTOutput(ns("table"))
  )
}

#' Table Selector Module Server
#'
#' @description A Shiny module server for handling table selection.
#'
#' @param id Module ID
#' @param json_data A reactive value containing the JSON data.
#' @param selected_instance_url A reactive value containing FMR base URL.
#'
#' @return A list containing the reactive `selected_table_name` and an `update_table_choices` function.
#' @export
select_table_server <- function(id, json_data, selected_instance_url) {
  shiny::moduleServer(id, function(input, output, session) {

    selected_table_data <- shiny::reactiveVal(NULL)
    # Update the dropdown menu choices ----
    shiny::observeEvent(json_data(), {
      #browser()
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
    # Show modal if the selected table is not yet defined ----
    selected_table_name <- shiny::reactive({
      input$table_choice
    })
    shiny::observeEvent(selected_table_name(), {
      #browser()
      shiny::req(json_data())
      shiny::req(selected_table_name())
      # Show modal to define table type if table does not exist yet
      table_name <- selected_table_name()
      table_data <- select_correct_table(json_data = json_data(), table_name = table_name)

      if (!is.data.frame(table_data)) {
        show_define_mapping_type_modal(
          ns = session$ns,
          table_name = table_name
        )
      } else {
        # Update selected_table when table_data is a data.frame
        selected_table_data(table_data)
      }
    })
    # Create a new table on user confirmation ----
    shiny::observeEvent(input$confirm_define_mapping_type, {
      shiny::req(input$table_type, selected_table_name(), json_data(), selected_instance_url())
      full_data <- json_data()
      table_name <- selected_table_name()
      fmr_url <- selected_instance_url()

      if (input$table_type == "Mapping") {
        # Get codelist values
        codelist_df <- fetch_cl(table_name = table_name, instance_url = fmr_url)
        # Render the DataTable
        output$codelist_table <- DT::renderDT({
          shiny::req(codelist_df)
          DT::datatable(
            codelist_df,
            selection = "multiple",  # Allow multiple row selection
            rownames = FALSE
          )
        })

        # Show modal with codelist data
        show_codelist_modal(session$ns, codelist_df)
      } else {
        # Handle other types as before
        # e.g., if Fixed
        full_data <- create_mapping_table(
            full_data,
            table_name,
            table_type = input$table_type
          )
        json_data(full_data)
        # Update selected_table with the new data
        table_data <- select_correct_table(json_data = full_data, table_name = table_name)
        selected_table_data(table_data)
        shiny::removeModal()
        shiny::showNotification(
          paste0(selected_table_name(), " table type successfully defined!"),
          type = "message"
        )
      }
    })

    # Step 3 & 4: After user selects rows and clicks confirm in the modal
    observeEvent(input$confirm_codelist_selection, {
      # Get selected rows from DT
      sel <- input$codelist_table_rows_selected
      req(sel, length(sel) > 0)

      # Extract selected rows from codelist_data
      selected_ids <- codelist_df[sel, "ID"]
      selected_labels <- codelist_df[sel, "LABEL"]

      # Create a three-column data frame: SOURCE, TARGET, LABEL
      # SOURCE initially NA or empty (your choice), TARGET from IDs, LABEL from labels
      populated_df <- data.frame(
        SOURCE = NA_character_,
        TARGET = selected_ids,
        LABEL = selected_labels,
        stringsAsFactors = FALSE
      )

      # Integrate into full_data
      full_data <- json_data()
      full_data <- create_mapping_table(
        full_data,
        selected_table_name(),
        table_type = "Mapping",
        populated_df = populated_df
      )

      # Update json_data
      json_data(full_data)

      # Close the modal and notify user
      shiny::removeModal()
      shiny::showNotification(
        paste0(selected_table_name(), " mapping table successfully created!"),
        type = "message"
      )
    })
      # #browser()
      # shiny::req(input$table_type)
      # full_data <- json_data()
      # table_name <- selected_table_name()
      # full_data <- create_mapping_table(
      #   full_data,
      #   table_name,
      #   table_type = input$table_type,
      #   fmr_url = selected_instance_url()
      # )
      # json_data(full_data)
      # # Update selected_table with the new data
      # table_data <- select_correct_table(json_data = full_data, table_name = table_name)
      # selected_table_data(table_data)
      # shiny::removeModal()
      # shiny::showNotification(
      #   paste0(table_name, " table type successfully defined!"),
      #   type = "message"
    #  )
    #})

    # Render table in main view pane ----
    # This ensures that whenever selected_table_data changes, the table updates
    output$table <- DT::renderDT({
      req(selected_table_data())
      DT::datatable(
        selected_table_data(),
        editable = TRUE,
        selection = 'single'
      )
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

#' Show Codelist Modal
#'
#' Displays a modal dialog in a Shiny app with a DataTable for selecting rows from a codelist.
#' The modal includes a cancel button to close the dialog and a confirm button to finalize the selection.
#'
#' @param ns A namespace function, typically `session$ns`, used to ensure unique IDs for Shiny inputs and outputs.
#' @param codelist_df A data frame representing the codelist to display. This data frame should contain
#'        the data to be shown in the DataTable, typically with columns like `ID` and `LABEL`.
#'
#' @return None. The function is used for its side effect of displaying a modal dialog in the Shiny app.
#'
#' @examples
#' # Example usage in a Shiny app:
#' shinyServer(function(input, output, session) {
#'   ns <- session$ns
#'   codelist_df <- data.frame(ID = c("A1", "B2"), LABEL = c("Label A1", "Label B2"))
#'   show_codelist_modal(ns, codelist_df)
#' })
#'
#' @export
show_codelist_modal <- function(ns, codelist_df) {
  shiny::showModal(
    shiny::modalDialog(
      title = "Select Rows",
      DT::DTOutput(ns("codelist_table")),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("confirm_codelist_selection"), "Confirm")
      )
    )
  )
}

