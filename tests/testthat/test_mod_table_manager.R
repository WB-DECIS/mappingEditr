# test_mod_table_manager.R

library(testthat)
library(shiny)

test_that("table_manager_server adds and deletes tables correctly", {
  # Mock reactives
  json_data <- reactiveVal(list())
  selected_table_name <- reactiveVal(NULL)
  update_table_choices_called <- FALSE
  update_table_choices <- function(choices, selected = NULL) {
    update_table_choices_called <<- TRUE
    expect_true("new_table" %in% choices)
  }

  shiny::testServer(
    table_manager_server,
    args = list(
      id = "test_table_manager",
      json_data = json_data,
      selected_table_name = selected_table_name,
      update_table_choices = update_table_choices
    ),
    {
      # Simulate adding a table
      session$setInputs(add_table = 1)
      session$flushReact()

      # Simulate modal inputs
      session$setInputs(new_table_name = "new_table", table_type = "Fixed")
      session$setInputs(confirm_add_table = 1)
      session$flushReact()

      # Check if table is added
      expect_true("new_table" %in% names(json_data()))
      expect_true(update_table_choices_called)

      # Reset flag
      update_table_choices_called <<- FALSE

      # Simulate deleting a table
      selected_table_name("new_table")
      session$setInputs(delete_table = 1)
      session$flushReact()

      # Simulate modal confirmation
      session$setInputs(confirm_delete_table = 1)
      session$flushReact()

      # Check if table is deleted
      expect_false("new_table" %in% names(json_data()))
      expect_true(update_table_choices_called)
    }
  )
})
