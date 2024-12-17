# Test UI

test_that("initialize_map_ui renders correctly", {
  # Generate the UI
  ui <- initialize_map_ui("test_id")

  # Check that the output is a shiny.tag (UI element)
  expect_true(inherits(ui, "shiny.tag"))

  # Render the UI to HTML for inspection
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check that the button contains the correct label
  expect_match(ui_html, ">Initialize new mapping<", fixed = TRUE)

  # Check that the button has the correct ID
  expect_match(ui_html, "id=\"test_id-initialize_map\"", fixed = TRUE)

  # Check that the button has the correct class
  expect_match(ui_html, "class=\"btn btn-default action-button btn-primary\"", fixed = TRUE)
})

# Test server side
test_that("initialize_map_server shows warning if no DSD is selected", {
  # Mock shiny notification function
  mock_showNotification <- function(ui, ...) {
    cat(ui, "\n")  # Mock implementation just prints the notification
  }

  with_mocked_bindings(
    {
      # Reactive values
      json_data <- reactiveVal(NULL)
      selected_instance_url <- reactiveVal("https://mock-api.example.com/")
      selected_dsd_id <- reactiveVal(NULL)  # No DSD selected

      testServer(
        initialize_map_server,
        args = list(
          json_data = json_data,
          selected_instance_url = selected_instance_url,
          selected_dsd_id = selected_dsd_id
        ),
        {
          # Simulate button click
          session$setInputs(initialize_map = 1)

          # Expect no change in json_data
          expect_null(json_data())
        }
      )
    },
    showNotification = mock_showNotification
  )
})

# test_that("initialize_map_server updates json_data on successful API call", {
#   # Mock data and functions
#   mock_api_response <- list(
#     data = list(
#       structures = list(
#         dataStructure = list(
#           dimensions = c("dim1", "dim2"),
#           attributes = c("attr1"),
#           measures = c("meas1")
#         )
#       )
#     )
#   )
#
#   mock_GET <- function(url, ...) {
#     response <- list(
#       status_code = 200,
#       content = mock_api_response
#     )
#     class(response) <- "response"
#     return(response)
#   }
#
#   mock_parse_dsd_id <- function(dsd_id) {
#     return(paste0("parsed-", dsd_id))
#   }
#
#   mock_extract_concepts_from_dsd <- function(dsd_info) {
#     return(list(
#       dimensions = c("dim1", "dim2"),
#       attributes = c("attr1"),
#       measures = c("meas1")
#     ))
#   }
#
#   with_mocked_bindings(
#     {
#       # Reactive values
#       json_data <- reactiveVal(NULL)
#       selected_instance_url <- reactiveVal("https://mock-api.example.com/")
#       selected_dsd_id <- reactiveVal("DSD1")
#
#       testServer(
#         initialize_map_server,
#         args = list(
#           json_data = json_data,
#           selected_instance_url = selected_instance_url,
#           selected_dsd_id = selected_dsd_id
#         ),
#         {
#           # Simulate button click
#           session$setInputs(initialize_map = 1)
#
#           # Expect json_data to be updated correctly
#           expected_data <- list(
#             dim1 = NA,
#             dim2 = NA,
#             attr1 = NA,
#             meas1 = NA
#           )
#           expect_equal(json_data(), expected_data)
#         }
#       )
#     },
#     GET = mock_GET,
#     parse_dsd_id = mock_parse_dsd_id,
#     extract_concepts_from_dsd = mock_extract_concepts_from_dsd
#   )
# })

# test_that("initialize_map_server handles API failure gracefully", {
#   mock_GET <- function(url, ...) {
#     stop("API error")
#   }
#
#   mock_showNotification <- function(ui, ...) {
#     cat(ui, "\n")
#   }
#
#   with_mocked_bindings(
#     {
#       # Reactive values
#       json_data <- reactiveVal(NULL)
#       selected_instance_url <- reactiveVal("https://mock-api.example.com/")
#       selected_dsd_id <- reactiveVal("DSD1")
#
#       testServer(
#         initialize_map_server,
#         args = list(
#           json_data = json_data,
#           selected_instance_url = selected_instance_url,
#           selected_dsd_id = selected_dsd_id
#         ),
#         {
#           # Simulate button click
#           session$setInputs(initialize_map = 1)
#
#           # Expect json_data to remain NULL
#           expect_null(json_data())
#         }
#       )
#     },
#     GET = mock_GET,
#     showNotification = mock_showNotification
#   )
# })

# test_that("initialize_map_server handles processing errors gracefully", {
#   mock_GET <- function(url, ...) {
#     response <- list(
#       status_code = 200,
#       content = list()
#     )
#     class(response) <- "response"
#     return(response)
#   }
#
#   mock_extract_concepts_from_dsd <- function(dsd_info) {
#     stop("Processing error")
#   }
#
#   mock_showNotification <- function(ui, ...) {
#     cat(ui, "\n")
#   }
#
#   with_mocked_bindings(
#     {
#       # Reactive values
#       json_data <- reactiveVal(NULL)
#       selected_instance_url <- reactiveVal("https://mock-api.example.com/")
#       selected_dsd_id <- reactiveVal("DSD1")
#
#       testServer(
#         initialize_map_server,
#         args = list(
#           json_data = json_data,
#           selected_instance_url = selected_instance_url,
#           selected_dsd_id = selected_dsd_id
#         ),
#         {
#           # Simulate button click
#           session$setInputs(initialize_map = 1)
#
#           # Expect json_data to remain NULL
#           expect_null(json_data())
#         }
#       )
#     },
#     GET = mock_GET,
#     extract_concepts_from_dsd = mock_extract_concepts_from_dsd,
#     showNotification = mock_showNotification
#   )
# })

test_that("create_concepts_to_cl_lkup returns expected output", {
  # A minimal mock response object
  resp_mock <- list(
    DataStructure = list(
      list(
        dimensionList = list(
          dimensions = list(
            list(
              concept = "urn:sdmx:org.sdmx.infomodel.conceptscheme.Concept=SDMX:TEST_SCHEME(1.0).FREQ",
              representation = list(representation = "urn:sdmx:org.sdmx.infomodel.codelist.Codelist=SDMX:CL_FREQ(2.1)")
            )
          )
        ),
        attributeList = list(
          attributes = list(
            list(
              concept = "urn:sdmx:org.sdmx.infomodel.conceptscheme.Concept=SDMX:TEST_SCHEME(1.0).UNIT",
              representation = list(representation = "urn:sdmx:org.sdmx.infomodel.codelist.Codelist=SDMX:CL_UNIT(1.0)")
            )
          )
        ),
        measures = list(
          list(
            concept = "urn:sdmx:org.sdmx.infomodel.conceptscheme.Concept=SDMX:TEST_SCHEME(1.0).OBS_VALUE",
            # No representation codelist for measures in this mock
            representation = list()
          )
        )
      )
    )
  )

  result <- create_concepts_to_cl_lkup(resp_mock)

  # Basic structural tests
  expect_s3_class(result, "data.frame")
  expect_true(all(c("concept_id", "codelist_id") %in% names(result)))
  expect_equal(nrow(result), 3)

  # Check values
  freq_row <- result[result$concept_id == "FREQ", ]
  expect_equal(freq_row$codelist_id, "SDMX:CL_FREQ(2.1)")

  unit_row <- result[result$concept_id == "UNIT", ]
  expect_equal(unit_row$codelist_id, "SDMX:CL_UNIT(1.0)")

  obs_value_row <- result[result$concept_id == "OBS_VALUE", ]
  expect_true(is.na(obs_value_row$codelist_id))
})




