# test_mod_row_manager.R

library(testthat)
library(shiny)

test_that("row_manager_server adds and deletes rows correctly", {
  # Mock reactives
  selected_table_name <- reactiveVal("table1")
  selected_table <- reactiveVal(data.frame(col1 = character(0), stringsAsFactors = FALSE))
  selected_rows <- reactiveVal(NULL)
  json_data <- reactiveVal(list(table1 = data.frame(col1 = character(0), stringsAsFactors = FALSE)))

  shiny::testServer(
    row_manager_server,
    args = list(
      id = "test_row_manager",
      selected_table_name = selected_table_name,
      selected_table = selected_table,
      selected_rows = selected_rows,
      json_data = json_data
    ),
    {
      # Test adding a row
      session$setInputs(add_row = 1)
      session$flushReact()

      expect_equal(nrow(selected_table()), 1)

      # Simulate selecting a row
      selected_rows(1)
      # Test deleting a row
      session$setInputs(delete_row = 1)
      session$flushReact()

      expect_equal(nrow(selected_table()), 0)
    }
  )
})
