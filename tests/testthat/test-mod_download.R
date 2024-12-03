# input_path <- testthat::test_path("testdata/input.json")
# json_input <- readr::read_lines(input_path)
#
# # Mock reactive values
# mock_json_data <- reactiveVal(jsonlite::fromJSON(json_input))
# mock_file_path <- reactiveVal(input_path)
#
# # Test the download_server module
# test_that("download_server correctly saves updated JSON", {
#   # Create a temporary file to simulate the download destination
#   temp_file <- tempfile(fileext = ".json")
#
#   # Mock session environment for the module
#   testServer(download_server, args = list(
#     id = "test",
#     json_data = mock_json_data,
#     file_path = mock_file_path
#   ), {
#     browser()
#     # Trigger the content function in downloadHandler
#     session$output$download$content(temp_file)
#
#     # Read the saved file and compare it to the input JSON data
#     saved_data <- fromJSON(temp_file)
#     expected_data <- mock_json_data()
#
#     # Check if the saved JSON matches the reactive JSON data
#     expect_equal(saved_data, expected_data)
#   })
#
#   # Clean up temporary file
#   unlink(temp_file)
# })
#
# # Test the filename generation
# test_that("download_server generates correct filename", {
#   # Mock session environment for the module
#   testServer(download_server, args = list(
#     id = "test",
#     json_data = mock_json_data,
#     file_path = mock_file_path
#   ), {
#     # Call the filename function
#     generated_filename <- session$output$download$filename()
#
#     # Check if the generated filename matches the expected format
#     expect_equal(generated_filename, "updated_input.json")
#   })
# })
