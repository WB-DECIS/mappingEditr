# Test UI

test_that("delete_row_ui renders correctly", {
  # Generate the UI
  ui <- delete_row_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the button contains the correct label
  expect_match(ui_html, ">Delete Selected Row<", fixed = TRUE)

  # Check that the button has the correct class
  expect_match(ui_html, "class=\"btn btn-default action-button btn-warning\"", fixed = TRUE)

  # Check that the button has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-delete_row\"", fixed = TRUE)
})

# Test server side

test_that("delete_row_server deletes selected rows correctly", {
  # Sample data
  sample_table <- data.frame(
    Column1 = 1:5,
    Column2 = letters[1:5],
    stringsAsFactors = FALSE
  )

  sample_data <- list(
    "Table1" = sample_table
  )

  # Reactive values
  json_data <- reactiveVal(sample_data)
  selected_table_name <- reactiveVal("Table1")
  selected_table <- reactiveVal(sample_table)
  selected_rows <- reactiveVal(c(2, 4))  # Rows to delete

  testServer(
    delete_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Before deletion
      expect_equal(nrow(selected_table()), 5)
      expect_equal(nrow(json_data()[["Table1"]]), 5)

      # Simulate clicking the "Delete Selected Row" button
      session$setInputs(delete_row = 1)

      # After deletion
      expect_equal(nrow(selected_table()), 3)
      expect_equal(nrow(json_data()[["Table1"]]), 3)

      # Check that the correct rows were deleted
      remaining_rows <- selected_table()
      expect_equal(remaining_rows$Column1, c(1, 3, 5))
      expect_equal(remaining_rows$Column2, letters[c(1, 3, 5)])
    }
  )
})

test_that("delete_row_server handles no selected rows gracefully", {
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
  selected_rows <- reactiveVal(integer(0))  # No rows selected

  testServer(
    delete_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Delete Selected Row" button
      session$setInputs(delete_row = 1)

      # Verify that the table remains unchanged
      expect_equal(nrow(selected_table()), 3)
      expect_equal(nrow(json_data()[["Table1"]]), 3)
    }
  )
})

test_that("delete_row_server handles NULL selected_table gracefully", {
  # Reactive values
  json_data <- reactiveVal(list())
  selected_table_name <- reactiveVal("Table1")
  selected_table <- reactiveVal(NULL)  # No table selected
  selected_rows <- reactiveVal(c(1))  # Rows selected

  testServer(
    delete_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Delete Selected Row" button
      session$setInputs(delete_row = 1)

      # Verify that json_data remains unchanged
      expect_equal(json_data(), list())
    }
  )
})

test_that("delete_row_server deletes all rows when all are selected", {
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
  selected_rows <- reactiveVal(1:2)  # All rows selected

  testServer(
    delete_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Delete Selected Row" button
      session$setInputs(delete_row = 1)

      # Verify that the table is now empty
      expect_equal(nrow(selected_table()), 0)
      expect_equal(nrow(json_data()[["Table1"]]), 0)
    }
  )
})

test_that("delete_row_server handles NULL selected_rows gracefully", {
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
  selected_rows <- reactiveVal(NULL)  # selected_rows is NULL

  testServer(
    delete_row_server,
    args = list(
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Simulate clicking the "Delete Selected Row" button
      session$setInputs(delete_row = 1)

      # Verify that the table remains unchanged
      expect_equal(nrow(selected_table()), 3)
      expect_equal(nrow(json_data()[["Table1"]]), 3)
    }
  )
})

# test_that("delete_row_server handles empty json_data gracefully", {
#   # Reactive values
#   json_data <- reactiveVal(list())
#   selected_table_name <- reactiveVal("Table1")
#   selected_table <- reactiveVal(data.frame(Column1 = numeric(0), Column2 = character(0)))
#   selected_rows <- reactiveVal(1)  # Attempting to delete from an empty table
#
#   testServer(
#     delete_row_server,
#     args = list(
#       selected_table_name = selected_table_name,
#       selected_table = selected_table,
#       selected_rows = selected_rows,
#       json_data = json_data
#     ),
#     {
#       # Simulate clicking the "Delete Selected Row" button
#       session$setInputs(delete_row = 1)
#
#       # Verify that json_data remains unchanged
#       expect_equal(json_data(), list())
#       # Verify that selected_table remains unchanged
#       expect_equal(nrow(selected_table()), 0)
#     }
#   )
# })

