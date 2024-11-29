# Test UI side
test_that("delete_table_ui renders correctly", {
  # Generate the UI
  ui <- delete_table_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for further inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the button contains the correct label
  expect_match(ui_html, ">Delete Table<", fixed = TRUE)

  # Check that the button has the correct class for styling
  expect_match(ui_html, "class=\"btn btn-default action-button btn-danger\"", fixed = TRUE)

  # Check that the button has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-delete_table\"", fixed = TRUE)
})

# Test server side

test_that("delete_table_server deletes the selected table correctly", {
  # Sample data
  sample_data <- list(
    "Table1" = data.frame(A = 1:5),
    "Table2" = data.frame(B = 6:10),
    "Table3" = data.frame(C = 11:15)
  )

  # Reactive values and functions
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal("Table2")

  # Mock function to track updates to table choices
  update_table_choices_called <- FALSE
  updated_choices <- NULL
  update_table_choices <- function(choices) {
    update_table_choices_called <<- TRUE
    updated_choices <<- choices
  }

  # Use testServer to simulate the module's server logic
  testServer(
    delete_table_server,
    args = list(
      json_data = json_data,
      selected_table_name = selected_table_name,
      update_table_choices = update_table_choices
    ),
    {
      # Before deletion
      expect_equal(names(json_data()), c("Table1", "Table2", "Table3"))
      expect_equal(selected_table_name(), "Table2")

      # Simulate clicking the "Delete Table" button
      session$setInputs(delete_table = 1)

      # Simulate clicking the "Confirm Delete Table" button in the modal
      session$setInputs(confirm_delete_table = 1)

      # After deletion
      expect_false("Table2" %in% names(json_data()))
      expect_equal(names(json_data()), c("Table1", "Table3"))

      # Check that update_table_choices was called with updated choices
      expect_true(update_table_choices_called)
      expect_equal(updated_choices, c("Table1", "Table3"))
    }
  )
})

test_that("delete_table_server handles no selected table gracefully", {
  # Sample data
  sample_data <- list(
    "Table1" = data.frame(A = 1:5),
    "Table2" = data.frame(B = 6:10)
  )

  # Reactive values and functions
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal(NULL)  # No table selected

  # Mock function
  update_table_choices_called <- FALSE
  update_table_choices <- function(choices) {
    update_table_choices_called <<- TRUE
  }

  testServer(
    delete_table_server,
    args = list(
      json_data = json_data,
      selected_table_name = selected_table_name,
      update_table_choices = update_table_choices
    ),
    {
      # Simulate clicking the "Delete Table" button
      session$setInputs(delete_table = 1)

      # Since no table is selected, deletion should not proceed
      # Check that json_data remains unchanged
      expect_equal(names(json_data()), c("Table1", "Table2"))

      # Ensure that update_table_choices was not called
      expect_false(update_table_choices_called)
    }
  )
})
