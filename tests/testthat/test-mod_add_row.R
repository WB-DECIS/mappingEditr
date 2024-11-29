# Test UI

test_that("add_row_ui renders correctly", {
  # Generate the UI
  ui <- add_row_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the button contains the correct label
  expect_match(ui_html, ">Add Row<", fixed = TRUE)

  # Check that the button has the correct classes
  expect_match(ui_html, "class=\"btn btn-default action-button btn-info me-2\"", fixed = TRUE)

  # Check that the button has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-add_row\"", fixed = TRUE)
})

# Test server

test_that("add_row_server adds a new row to the selected table", {
  # Sample data
  sample_table <- data.frame(
    Column1 = 1:3,
    Column2 = letters[1:3],
    stringsAsFactors = FALSE
  )

  sample_data <- list(
    "Table1" = sample_table
  )

  # Reactive values
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal("Table1")
  selected_table <- reactiveVal(sample_table)
  selected_rows <- reactiveVal(NULL)  # Not used in this module

  testServer(
    add_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Before adding the row
      expect_equal(nrow(selected_table()), 3)
      expect_equal(nrow(json_data()[["Table1"]]), 3)

      # Simulate clicking the "Add Row" button
      session$setInputs(add_row = 1)

      # After adding the row
      expect_equal(nrow(selected_table()), 4)
      expect_equal(nrow(json_data()[["Table1"]]), 4)

      # Check that the new row contains NA values
      expect_true(all(is.na(selected_table()[4, ])))
    }
  )
})

test_that("add_row_server handles no selected table gracefully", {
  # Reactive values
  json_data <- reactiveVal(list())
  selected_table_name <- reactiveVal(NULL)  # No table selected
  selected_table <- reactiveVal(NULL)
  selected_rows <- reactiveVal(NULL)

  testServer(
    add_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Add Row" button
      session$setInputs(add_row = 1)

      # Since no table is selected, nothing should happen
      expect_null(selected_table())
      expect_equal(json_data(), list())
    }
  )
})

test_that("add_row_server adds a row to an empty table", {
  # Sample data
  sample_table <- data.frame(
    Column1 = numeric(0),
    Column2 = character(0),
    stringsAsFactors = FALSE
  )

  sample_data <- list(
    "EmptyTable" = sample_table
  )

  # Reactive values
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal("EmptyTable")
  selected_table <- reactiveVal(sample_table)
  selected_rows <- reactiveVal(NULL)

  testServer(
    add_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Before adding the row
      expect_equal(nrow(selected_table()), 0)
      expect_equal(nrow(json_data()[["EmptyTable"]]), 0)

      # Simulate clicking the "Add Row" button
      session$setInputs(add_row = 1)

      # After adding the row
      expect_equal(nrow(selected_table()), 1)
      expect_equal(nrow(json_data()[["EmptyTable"]]), 1)

      # Check that the new row contains NA values
      expect_true(all(is.na(selected_table()[1, ])))
    }
  )
})

test_that("add_row_server updates json_data correctly", {
  # Sample data
  sample_table <- data.frame(
    Column1 = 1:2,
    Column2 = letters[1:2],
    stringsAsFactors = FALSE
  )

  sample_data <- list(
    "Table1" = sample_table
  )

  # Reactive values
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal("Table1")
  selected_table <- reactiveVal(sample_table)
  selected_rows <- reactiveVal(NULL)

  testServer(
    add_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Add Row" button
      session$setInputs(add_row = 1)

      # Verify that json_data is updated
      updated_json_data <- json_data()
      expect_equal(nrow(updated_json_data[["Table1"]]), 3)
      expect_true(all(is.na(updated_json_data[["Table1"]][3, ])))
    }
  )
})

test_that("add_row_server adds multiple rows when button is clicked multiple times", {
  # Sample data
  sample_table <- data.frame(
    Column1 = 1:2,
    Column2 = letters[1:2],
    stringsAsFactors = FALSE
  )

  sample_data <- list(
    "Table1" = sample_table
  )

  # Reactive values
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal("Table1")
  selected_table <- reactiveVal(sample_table)
  selected_rows <- reactiveVal(NULL)

  testServer(
    add_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Add Row" button three times
      session$setInputs(add_row = 1)
      session$setInputs(add_row = 2)
      session$setInputs(add_row = 3)

      # After adding three rows
      expect_equal(nrow(selected_table()), 5)
      expect_equal(nrow(json_data()[["Table1"]]), 5)

      # Check that the new rows contain NA values
      expect_true(all(is.na(selected_table()[3:5, ])))
    }
  )
})


