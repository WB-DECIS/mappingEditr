# Test UI side

test_that("select_table_ui renders correctly", {
  # Generate the UI
  ui <- select_table_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the selectInput contains the correct label
  expect_match(ui_html, ">Select table to edit:<", fixed = TRUE)

  # Check that the input has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-table_choice\"", fixed = TRUE)

  # Check that the choices are initialized to NULL (no options)
  expect_match(ui_html, "<select[^>]*></select>", perl = TRUE)
})

# Test server side

test_that("select_table_server's selected_table_name reflects user selection", {
  # Reactive values
  json_data <- reactiveVal(list("Table1" = data.frame(), "Table2" = data.frame()))

  testServer(
    select_table_server,
    args = list(json_data = json_data),
    {
      # Simulate user selecting "Table2"
      session$setInputs(table_choice = "Table2")

      # Verify that selected_table_name() returns "Table2"
      expect_equal(selected_table_name(), "Table2")
    }
  )
})

test_that("select_table_server's selected_table_name reflects user selection", {
  # Reactive values
  json_data <- reactiveVal(list("Table1" = data.frame(), "Table2" = data.frame()))

  testServer(
    select_table_server,
    args = list(json_data = json_data),
    {
      # Simulate user selecting "Table2"
      session$setInputs(table_choice = "Table2")

      # Verify that selected_table_name() returns "Table2"
      expect_equal(selected_table_name(), "Table2")
    }
  )
})

test_that("select_table_server preserves current selection when possible", {
  # Reactive values
  json_data <- reactiveVal(list("Table1" = data.frame(), "Table2" = data.frame()))

  testServer(
    select_table_server,
    args = list(json_data = json_data),
    {
      # Simulate user selecting "Table2"
      session$setInputs(table_choice = "Table2")

      # Update json_data without changing tables
      json_data(list("Table1" = data.frame(), "Table2" = data.frame()))

      # Trigger reactivity
      session$flushReact()

      # Verify that the selected value remains "Table2"
      expect_equal(input$table_choice, "Table2")
      expect_equal(selected_table_name(), "Table2")
    }
  )
})

# test_that("select_table_server defaults to first table when current selection is invalid", {
#   # Reactive values
#   json_data <- reactiveVal(list("Table1" = data.frame(), "Table2" = data.frame()))
#
#   testServer(
#     select_table_server,
#     args = list(json_data = json_data),
#     {
#       # Simulate user selecting an invalid table
#       session$setInputs(table_choice = "InvalidTable")
#
#       # Update json_data
#       json_data(list("Table1" = data.frame(), "Table2" = data.frame()))
#
#       # Trigger reactivity
#       session$flushReact()
#
#       # Verify that the selected value defaults to "Table1"
#       expect_equal(input$table_choice, "Table1")
#       expect_equal(selected_table_name(), "Table1")
#     }
#   )
# })


# test_that("select_table_server handles empty json_data gracefully", {
#   # Reactive values
#   json_data <- reactiveVal(list())
#
#   testServer(
#     select_table_server,
#     args = list(json_data = json_data),
#     {
#       # Trigger reactivity
#       session$flushReact()
#
#       # Check that the selectInput choices are empty
#       choices <- session$getInputs("table_choice")$options
#       expect_equal(length(choices), 0)
#
#       # Verify that selected_table_name() is NULL
#       expect_null(selected_table_name())
#     }
#   )
# })


# test_that("select_table_server's update_table_choices function updates choices", {
#   # Reactive values
#   json_data <- reactiveVal(list("Table1" = data.frame(), "Table2" = data.frame()))
#
#   testServer(
#     select_table_server,
#     args = list(json_data = json_data),
#     {
#       # Update choices using update_table_choices
#       update_table_choices(session = session, choices = c("Table3", "Table4"), selected = "Table4")
#
#       # Trigger reactivity
#       session$flushReact()
#
#       # Check that the selectInput choices are updated
#       choices <- session$getInputs("table_choice")$options
#       choice_values <- sapply(choices, function(option) option$value)
#       expect_equal(choice_values, c("Table3", "Table4"))
#
#       # Verify that the selected value is "Table4"
#       expect_equal(input$table_choice, "Table4")
#     }
#   )
# })


# test_that("select_table_server updates choices when json_data changes", {
#   # Reactive values
#   json_data <- reactiveVal(list())
#
#   testServer(
#     select_table_server,
#     args = list(json_data = json_data),
#     {
#       # Initially, json_data is empty
#       expect_null(input$table_choice)
#
#       # Update json_data with new tables
#       json_data(list("Table1" = data.frame(), "Table2" = data.frame()))
#
#       # Trigger reactivity
#       #session$flushReact()
#
#       # Check that the selectInput choices are updated
#       choices <- session$getInputs("table_choice")$options
#       choice_values <- sapply(choices, function(option) option$value)
#       expect_equal(choice_values, c("Table1", "Table2"))
#
#       # Check that the selected value is the first table
#       expect_equal(input$table_choice, "Table1")
#     }
#   )
# })

test_that("parse_list_for_choices returns correctly structured output", {
  # Input list
  input_list <- list(
    components = c("mpg", "cyl", "disp"),
    representation = list(FREQ = NULL, REF_AREA = NULL, MEASURE = NULL)
  )

  # Expected output
  expected_output <- list(
    `components` = list(components = "components"),
    `representation` = list(
      FREQ = "FREQ",
      REF_AREA = "REF_AREA",
      MEASURE = "MEASURE"
    )
  )

  # Run function and compare output
  result <- parse_list_for_choices(input_list)
  expect_equal(result, expected_output)
})

test_that("parse_list_for_choices handles empty representation", {
  # Input list with empty representation
  input_list <- list(
    components = c("mpg", "cyl", "disp"),
    representation = list()
  )

  # Expected output
  expected_output <- list(
    `components` = list(components = "components"),
    `representation` = list()
  )

  # Run function and compare output
  result <- parse_list_for_choices(input_list)
  expect_equal(result, expected_output)
})

test_that("parse_list_for_choices handles missing representation", {
  # Input list without representation
  input_list <- list(
    components = c("mpg", "cyl", "disp")
  )

  # Expected output
  expected_output <- list(
    `components` = list(components = "components"),
    `representation` = list()
  )

  # Run function and compare output
  result <- parse_list_for_choices(input_list)
  expect_equal(result, expected_output)
})
