# Test UI side

test_that("select_fmr_ui renders correctly", {
  # Generate the UI
  ui <- select_fmr_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the selectInput contains the correct label
  expect_match(ui_html, ">Select FMR instance:<", fixed = TRUE)

  # Check that the input has the correct choices
  expect_match(ui_html, ">FMR QA<", fixed = TRUE)
  expect_match(ui_html, ">FMR PROD<", fixed = TRUE)

  # Check that the default selected value is "FMR QA"
  expect_match(ui_html, "value=\"FMR QA\" selected", fixed = TRUE)

  # Check that the input has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-instance_choice\"", fixed = TRUE)
})

# Test server side

test_that("select_fmr_server returns correct URL for default selection", {
  testServer(
    select_fmr_server,
    args = list(),
    {
      # Before any selection change
      session$setInputs(instance_choice = "FMR QA")
      expect_equal(selected_instance_url(), "https://fmrqa.worldbank.org/")
    }
  )
})

test_that("select_fmr_server updates URL when selection changes", {
  testServer(
    select_fmr_server,
    args = list(),
    {
      # Set initial selection to "FMR QA"
      session$setInputs(instance_choice = "FMR QA")
      expect_equal(selected_instance_url(), "https://fmrqa.worldbank.org/")

      # Change selection to "FMR PROD"
      session$setInputs(instance_choice = "FMR PROD")
      expect_equal(selected_instance_url(), "https://fmr.worldbank.org/")
    }
  )
})

test_that("select_fmr_server handles invalid selection gracefully", {
  testServer(
    select_fmr_server,
    args = list(),
    {
      # Set an invalid selection
      session$setInputs(instance_choice = "INVALID_SELECTION")
      expect_null(selected_instance_url())
    }
  )
})

# test_that("select_fmr_server handles NULL input gracefully", {
#   testServer(
#     select_fmr_server,
#     args = list(),
#     {
#       # No selection made
#       session$setInputs(instance_choice = NULL)
#       expect_null(selected_instance_url())
#     }
#   )
# })
