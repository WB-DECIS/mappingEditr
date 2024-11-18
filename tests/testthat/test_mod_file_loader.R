# test_mod_file_loader.R

library(testthat)
library(shiny)
library(jsonlite)

path_to_json <-  test_path("testdata", "input.json")

test_that("file_loader_server loads JSON data correctly", {
  shiny::testServer(
    file_loader_server,
    args = list(id = "test_file_loader"),
    {
      # Simulate file input
      session$setInputs(file = list(datapath = path_to_json))

      # Create a sample JSON file for testing
      # sample_json <- '{"table1": [{"col1": "value1", "col2": "value2"}]}'
      # writeLines(sample_json, "test_data.json")

      # Trigger reactive expressions
      session$flushReact()

      # Check if file_path is set correctly
      expect_equal(file_path(), path_to_json)

      # Check if json_data is loaded correctly
      expect_equal(json_data(), jsonlite::fromJSON(path_to_json))

      # Clean up
      unlink(path_to_json)
    }
  )
})
