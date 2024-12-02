# tests/testthat/test-define_mapping_type_server.R

test_that("Modal dialog is shown when selected table is not a data.frame", {

  # Mocking showModal: A mock function mock_showModal sets a flag showModal_called when invoked.
  # Using with_mock: Temporarily replace shiny::showModal with mock function during the test.
  # Triggering the Observer: `session$flushReact()` ensures reactive expressions are evaluated.
  # Assertion: Check that showModal_called is TRUE, indicating mock_showModal was called.

  # Mock functions
  showModal_called <- FALSE
  mock_showModal <- function(ui) {
    showModal_called <<- TRUE
  }

  # Replace shiny::showModal with mock function
  with_mock(
    'shiny::showModal' = mock_showModal,
    {
      # Reactive values
      json_data <- reactiveVal(list(
        "Table1" = list(),  # Not a data.frame
        "Table2" = data.frame(A = 1:5)
      ))
      selected_table_name <- reactiveVal("Table1")

      testServer(
        define_mapping_type_server,
        args = list(
          json_data = json_data,
          selected_table_name = selected_table_name
        ),
        {
          # Run the observeEvent by triggering selected_table_name()
          session$flushReact()

          # Check that showModal was called
          expect_true(showModal_called)
        }
      )
    }
  )
})

test_that("json_data is updated when user confirms table type", {
  # Mocking create_mapping_table: Simulate the function's behavior and capture its arguments.
  # Mocking removeModal and showNotification: Track if these functions are called.
  # Simulating User Input: Set inputs table_type and confirm_define_mapping_type to simulate the user action.
  # Assertions: Verify that create_mapping_table was called with correct arguments and that json_data was updated accordingly.

  # Mock functions
  create_mapping_table_called <- FALSE
  create_mapping_table_args <- NULL
  mock_create_mapping_table <- function(full_data, table_name, table_type) {
    create_mapping_table_called <<- TRUE
    create_mapping_table_args <<- list(
      full_data = full_data,
      table_name = table_name,
      table_type = table_type
    )
    # Simulate the creation of a data.frame
    full_data$representation[[table_name]] <- data.frame(Type = table_type)
    return(full_data)
  }

  removeModal_called <- FALSE
  mock_removeModal <- function() {
    removeModal_called <<- TRUE
  }

  showNotification_called <- FALSE
  notification_message <- NULL
  mock_showNotification <- function(ui, type) {
    showNotification_called <<- TRUE
    notification_message <<- ui
  }

  # Replace functions with mocks
  with_mock(
    'shiny::removeModal' = mock_removeModal,
    'shiny::showNotification' = mock_showNotification,
    'create_mapping_table' = mock_create_mapping_table,
    {
      # Reactive values
      json_data <- reactiveVal(list(
        "Table1" = list()  # Not a data.frame
      ))
      selected_table_name <- reactiveVal("Table1")

      testServer(
        define_mapping_type_server,
        args = list(
          json_data = json_data,
          selected_table_name = selected_table_name
        ),
        {
          # Trigger the modal display
          session$flushReact()

          # Simulate user input for table type
          session$setInputs(table_type = "Fixed")
          session$setInputs(confirm_define_mapping_type = 1)

          # Assertions
          expect_true(create_mapping_table_called)
          expect_equal(create_mapping_table_args$table_name, "Table1")
          expect_equal(create_mapping_table_args$table_type, "Fixed")

          # Check that json_data was updated
          updated_data <- json_data()
          expect_true(is.data.frame(updated_data[["Table1"]]))
          expect_equal(updated_data[["Table1"]], data.frame(Type = "Fixed"))

          # Check that removeModal and showNotification were called
          expect_true(removeModal_called)
          expect_true(showNotification_called)
          expect_equal(notification_message, "Table1 table type successfully defined!")
        }
      )
    }
  )
})

test_that("No action when selected table is already a data.frame", {
  # Assertion: Check that showModal_called is FALSE, indicating mock_showModal was NOT called.

  # Mock functions
  showModal_called <- FALSE
  mock_showModal <- function(ui) {
    showModal_called <<- TRUE
  }

  with_mock(
    'shiny::showModal' = mock_showModal,
    {
      # Reactive values
      json_data <- reactiveVal(list(
        "Table2" = data.frame(A = 1:5)
      ))
      selected_table_name <- reactiveVal("Table2")

      testServer(
        define_mapping_type_server,
        args = list(
          json_data = json_data,
          selected_table_name = selected_table_name
        ),
        {
          # Trigger the observer
          session$flushReact()

          # Check that showModal was not called
          expect_false(showModal_called)
        }
      )
    }
  )
})

test_that("Handles NULL selected_table_name gracefully", {
  # Reactive values
  json_data <- reactiveVal(list(
    "Table1" = list()
  ))
  selected_table_name <- reactiveVal(NULL)

  testServer(
    define_mapping_type_server,
    args = list(
      json_data = json_data,
      selected_table_name = selected_table_name
    ),
    {
      # Trigger the observer
      session$flushReact()

      # Since selected_table_name is NULL, nothing should happen
      # No need to mock functions as they shouldn't be called
      expect_true(TRUE)  # Test passes if no errors occur
    }
  )
})

test_that("Handles NULL json_data gracefully", {
  # Reactive values
  json_data <- reactiveVal(NULL)
  selected_table_name <- reactiveVal("Table1")

  testServer(
    define_mapping_type_server,
    args = list(
      json_data = json_data,
      selected_table_name = selected_table_name
    ),
    {
      # Trigger the observer
      session$flushReact()

      # Since json_data is NULL, nothing should happen
      # No need to mock functions as they shouldn't be called
      expect_true(TRUE)  # Test passes if no errors occur
    }
  )
})
