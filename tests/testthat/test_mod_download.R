# test_mod_download.R

library(testthat)
library(shiny)

test_that("download_server generates correct download content", {
  # Mock reactives
  json_data <- reactiveVal(list(table1 = data.frame(col1 = "value1", stringsAsFactors = FALSE)))
  file_path <- reactiveVal("original_file.json")

  shiny::testServer(
    download_server,
    args = list(
      id = "test_download",
      json_data = json_data,
      file_path = file_path
    ),
    {
      # Test filename
      expect_equal(output$download$filename(), "updated_original_file.json")

      # Test content
      temp_file <- tempfile()
      output$download$content(temp_file)
      saved_data <- jsonlite::fromJSON(temp_file)
      expect_equal(saved_data, json_data())

      # Clean up
      unlink(temp_file)
    }
  )
})
