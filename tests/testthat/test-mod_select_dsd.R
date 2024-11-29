# test UI side

test_that("select_dsd_ui renders correctly", {
  # Generate the UI
  ui <- select_dsd_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the selectInput contains the correct label
  expect_match(ui_html, ">Select DSD:<", fixed = TRUE)

  # Check that the input has the correct namespaced ID
  expect_match(ui_html, "id=\"test_id-dsd_choice\"", fixed = TRUE)

  # Check that the choices are initialized to NULL (no options)
  expect_match(ui_html, "<select[^>]*></select>", perl = TRUE)
})

# test server side
test_that("select_dsd_server's selected_dsd_id reflects user selection", {
  # Mock data
  mock_api_response <- list(
    data = list(
      structures = list(
        dataStructures = list(
          dataStructure = list(
            list(id = "DSD1"),
            list(id = "DSD2")
          )
        )
      )
    )
  )

  # Reuse previous mock functions
  mock_GET <- function(url, ...) {
    response <- list(
      status_code = 200,
      content = mock_api_response
    )
    class(response) <- "response"
    return(response)
  }

  mock_content <- function(response, as = "parsed", type = NULL, ...) {
    return(response$content)
  }

  mock_parse_dsd_list <- function(dsd_info) {
    # Extract DSD IDs from the mocked API response
    dsd_list <- lapply(dsd_info$data$structures$dataStructures$dataStructure, function(dsd) dsd$id)
    return(unlist(dsd_list))
  }

  mock_showNotification <- function(ui, ...) { }

  with_mock(
    'httr::GET' = mock_GET,
    'httr::content' = mock_content,
    'parse_dsd_list' = mock_parse_dsd_list,
    'shiny::showNotification' = mock_showNotification,
    {
      # Reactive values
      selected_instance_url <- reactiveVal("https://mock-api.example.com/")

      testServer(
        select_dsd_server,
        args = list(selected_instance_url = selected_instance_url),
        {
          # Trigger the observer
          session$flushReact()

          # Simulate user selecting "DSD2"
          session$setInputs(dsd_choice = "DSD2")

          # Verify that selected_dsd_id() returns "DSD2"
          expect_equal(selected_dsd_id(), "DSD2")
        }
      )
    }
  )
})



# test_that("select_dsd_server updates dsd_choice on successful API call", {
#   # Mock data
#   mock_api_response <- list(
#     data = list(
#       structures = list(
#         dataStructures = list(
#           dataStructure = list(
#             list(id = "DSD1"),
#             list(id = "DSD2")
#           )
#         )
#       )
#     )
#   )
#
#   # Mock functions
#   mock_GET <- function(url, ...) {
#     response <- list(
#       status_code = 200,
#       content = mock_api_response
#     )
#     class(response) <- "response"
#     return(response)
#   }
#
#   mock_content <- function(response, as = "parsed", type = NULL, ...) {
#     return(response$content)
#   }
#
#   mock_parse_dsd_list <- function(dsd_info) {
#     # Extract DSD IDs from the mocked API response
#     dsd_list <- lapply(dsd_info$data$structures$dataStructures$dataStructure, function(dsd) dsd$id)
#     return(unlist(dsd_list))
#   }
#
#   # Mock shiny notification functions
#   mock_showNotification <- function(ui, ...) { }
#
#   with_mock(
#     'httr::GET' = mock_GET,
#     'httr::content' = mock_content,
#     'parse_dsd_list' = mock_parse_dsd_list,
#     'shiny::showNotification' = mock_showNotification,
#     {
#       # Reactive values
#       selected_instance_url <- reactiveVal("https://mock-api.example.com/")
#
#       testServer(
#         select_dsd_server,
#         args = list(selected_instance_url = selected_instance_url),
#         {
#           # Trigger the observer by changing selected_instance_url
#           session$flushReact()
#
#           # Check that dsd_choice is updated
#           choices <- session$getInputs("dsd_choice")$options
#           choice_values <- sapply(choices, function(option) option$value)
#           expect_equal(choice_values, c("DSD1", "DSD2"))
#
#           # dsd_list should be updated
#           expect_equal(dsd_list(), c("DSD1", "DSD2"))
#         }
#       )
#     }
#   )
# })

