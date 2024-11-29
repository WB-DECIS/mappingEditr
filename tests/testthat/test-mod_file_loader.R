# Test UI side


test_that("file_loader_ui renders correctly", {
  # Generate the UI
  ui <- file_loader_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the fileInput contains the correct label
  expect_match(ui_html, ">Load existing mapping<", fixed = TRUE)

  # Check that the input has the correct accept attribute
  expect_match(ui_html, "accept=\".json\"", fixed = TRUE)

  # Check that the input has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-file\"", fixed = TRUE)
})

# test server side

test_that("file_loader_server handles file upload and parsing correctly", {
  # Create a temporary JSON file
  temp_json <- tempfile(fileext = ".json")
  sample_data <- list(
    name = "Test",
    value = 123
  )
  write_json(sample_data, temp_json)

  # Simulate the file input object
  file_input <- list(
    name = basename(temp_json),
    size = file.info(temp_json)$size,
    type = "application/json",
    datapath = temp_json
  )

  testServer(
    file_loader_server,
    args = list(),
    {
      # Before file upload
      expect_null(json_data())
      expect_null(file_path())

      # Simulate file upload by setting input$file
      session$setInputs(file = file_input)

      # After file upload
      expect_equal(file_path(), temp_json)
      expect_equal(json_data(), sample_data)
    }
  )

  # Clean up temporary file
  unlink(temp_json)
})

# test_that("file_loader_server handles invalid JSON file gracefully", {
#   # Create a temporary invalid JSON file
#   temp_json <- tempfile(fileext = ".json")
#   writeLines("Invalid JSON Content", temp_json)
#
#   # Simulate the file input object
#   file_input <- list(
#     name = basename(temp_json),
#     size = file.info(temp_json)$size,
#     type = "application/json",
#     datapath = temp_json
#   )
#
#   testServer(
#     file_loader_server,
#     args = list(),
#     {
#       # Simulate file upload by setting input$file
#       session$setInputs(file = file_input)
#
#       # Attempt to read invalid JSON should result in an error
#       expect_error(
#         {
#           session$flushReact()
#         },
#         regexp = "parse error"
#       )
#
#       # json_data and file_path should not be updated
#       expect_null(json_data())
#       expect_equal(file_path(), temp_json)
#     }
#   )
#
#   # Clean up temporary file
#   unlink(temp_json)
# })

test_that("file_loader_server handles missing file input gracefully", {
  testServer(
    file_loader_server,
    args = list(),
    {
      # Before any file upload
      expect_null(json_data())
      expect_null(file_path())

      # Simulate no file uploaded
      session$setInputs(file = NULL)

      # json_data and file_path should remain NULL
      expect_null(json_data())
      expect_null(file_path())
    }
  )
})

# test_that("file_loader_server handles non-JSON file upload gracefully", {
#   # Create a temporary text file
#   temp_txt <- tempfile(fileext = ".txt")
#   writeLines("This is a text file.", temp_txt)
#
#   # Simulate the file input object
#   file_input <- list(
#     name = basename(temp_txt),
#     size = file.info(temp_txt)$size,
#     type = "text/plain",
#     datapath = temp_txt
#   )
#
#   testServer(
#     file_loader_server,
#     args = list(),
#     {
#       # Simulate file upload by setting input$file
#       session$setInputs(file = file_input)
#
#       # Attempt to read non-JSON file should result in an error
#       expect_error(
#         {
#           session$flushReact()
#         },
#         regexp = "parse error"
#       )
#
#       # json_data and file_path should not be updated
#       expect_null(json_data())
#       expect_equal(file_path(), temp_txt)
#     }
#   )
#
#   # Clean up temporary file
#   unlink(temp_txt)
# })

test_that("file_loader_server handles JSON with special characters", {
  # Create a JSON file with special characters
  temp_json <- tempfile(fileext = ".json")
  sample_data <- list(
    text = "Special characters: é, ñ, ü, 中文, العربية"
  )
  write_json(sample_data, temp_json)

  # Simulate the file input object
  file_input <- list(
    name = basename(temp_json),
    size = file.info(temp_json)$size,
    type = "application/json",
    datapath = temp_json
  )

  testServer(
    file_loader_server,
    args = list(),
    {
      # Simulate file upload by setting input$file
      session$setInputs(file = file_input)

      # After file upload
      expect_equal(file_path(), temp_json)
      expect_equal(json_data(), sample_data)
    }
  )

  # Clean up temporary file
  unlink(temp_json)
})

