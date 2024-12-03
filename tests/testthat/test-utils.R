# test_parse_dsd_id.R

test_that("parse_dsd_id works for standard input", {
  dsd_id <- "WB.DATA360:DS_DATA360(1.2)"
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "WB.DATA360/DS_DATA360/1.2")
})

test_that("parse_dsd_id handles missing version", {
  dsd_id <- "WB.DATA360:DS_DATA360"
  expect_error(parse_dsd_id(dsd_id), "Invalid DSD ID format")
})

test_that("parse_dsd_id handles different agency and artifact", {
  dsd_id <- "OECD.STATISTICS:ECON_DATA(2.5)"
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "OECD.STATISTICS/ECON_DATA/2.5")

  dsd_id <- "WB:WDI(1.0)"
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "WB/WDI/1.0")
})

test_that("parse_dsd_id handles invalid format", {
  dsd_id <- "InvalidFormat"
  expect_error(parse_dsd_id(dsd_id), "Invalid DSD ID format")
})

test_that("parse_dsd_id handles additional whitespace", {
  dsd_id <- "  WB.DATA360 : DS_DATA360 ( 1.2 ) "
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "WB.DATA360/DS_DATA360/1.2")
})

test_that("parse_dsd_id handles special characters", {
  dsd_id <- "AGENCY.ARTIFACT:ID-123(3.4.5)"
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "AGENCY.ARTIFACT/ID-123/3.4.5")
})

test_that("parse_dsd_id handles numeric IDs and versions", {
  dsd_id <- "AGENCY.ARTIFACT:12345(6789)"
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "AGENCY.ARTIFACT/12345/6789")
})

test_that("parse_dsd_id handles multiple dots in agency_artifact", {
  dsd_id <- "AGENCY.DIVISION.DEPARTMENT:ID(1.0)"
  result <- parse_dsd_id(dsd_id)
  expect_equal(result, "AGENCY.DIVISION.DEPARTMENT/ID/1.0")
})

test_that("parse_dsd_id throws error on empty input", {
  dsd_id <- ""
  expect_error(parse_dsd_id(dsd_id), "Invalid DSD ID format")
})

# Test extract_concepts_from_dsd()

test_that("extract_concepts_from_dsd works correctly with valid input", {
  # Sample DSD JSON structure
  dsd_json <- list(
    DataStructure = list(
      list(
        id = "DS_DATA360",
        dimensionList = list(
          dimensions = list(
            list(id = "FREQ"),
            list(id = "REF_AREA"),
            list(id = "INDICATOR")
          )
        ),
        attributeList = list(
          attributes = list(
            list(id = "AGG_METHOD"),
            list(id = "UNIT_TYPE")
          )
        ),
        measures = list(
          list(id = "OBS_VALUE")
        )
      )
    )
  )

  result <- extract_concepts_from_dsd(dsd_json)

  expect_equal(result$dimensions, c("FREQ", "REF_AREA", "INDICATOR"))
  expect_equal(result$attributes, c("AGG_METHOD", "UNIT_TYPE"))
  expect_equal(result$measures, c("OBS_VALUE"))
})

test_that("extract_concepts_from_dsd handles missing elements", {
  # DSD JSON with missing attributes
  dsd_json <- list(
    DataStructure = list(
      list(
        id = "DS_DATA360",
        dimensionList = list(
          dimensions = list(
            list(id = "FREQ"),
            list(id = "REF_AREA")
          )
        ),
        # attributeList is missing
        measures = list(
          list(id = "OBS_VALUE")
        )
      )
    )
  )

  result <- extract_concepts_from_dsd(dsd_json)

  expect_equal(result$dimensions, c("FREQ", "REF_AREA"))
  expect_equal(result$attributes, character(0))
  expect_equal(result$measures, c("OBS_VALUE"))
})

test_that("extract_concepts_from_dsd handles empty DataStructure", {
  # DSD JSON with empty DataStructure
  dsd_json <- list(
    DataStructure = list()
  )

  expect_error(extract_concepts_from_dsd(dsd_json), "Invalid DSD JSON structure")
})

test_that("extract_concepts_from_dsd handles missing DataStructure", {
  # DSD JSON with no DataStructure
  dsd_json <- list()

  expect_error(extract_concepts_from_dsd(dsd_json), "Invalid DSD JSON structure")
})

test_that("extract_concepts_from_dsd handles missing IDs", {
  # DSD JSON with elements missing IDs
  dsd_json <- list(
    DataStructure = list(
      list(
        id = "DS_DATA360",
        dimensionList = list(
          dimensions = list(
            list(),  # Missing id
            list(id = "REF_AREA")
          )
        ),
        attributeList = list(
          attributes = list(
            list(id = "AGG_METHOD"),
            list()  # Missing id
          )
        ),
        measures = list(
          list(id = "OBS_VALUE")
        )
      )
    )
  )

  result <- extract_concepts_from_dsd(dsd_json)

  expect_equal(result$dimensions, c(NA, "REF_AREA"))
  expect_equal(result$attributes, c("AGG_METHOD", NA))
  expect_equal(result$measures, c("OBS_VALUE"))
})

# test_parse_dsd_list.R

library(testthat)
library(stringr)

test_that("parse_dsd_list works correctly with valid input", {
  api_response <- list(
    DataStructure = list(
      list(
        id = "DS_ENTERPRISE_SURVEY",
        agencyId = "WB.TEST",
        version = "1.0"
      ),
      list(
        id = "DS_DATA360",
        agencyId = "WB.DATA360",
        version = "1.2"
      )
    )
  )

  result <- parse_dsd_list(api_response)

  expected <- c(
    "WB.TEST:DS_ENTERPRISE_SURVEY(1.0)",
    "WB.DATA360:DS_DATA360(1.2)"
  )

  expect_equal(result, expected)
})

test_that("parse_dsd_list handles missing DataStructure", {
  api_response <- list()

  expect_error(parse_dsd_list(api_response), "Invalid API response")
})

test_that("parse_dsd_list handles empty DataStructure", {
  api_response <- list(
    DataStructure = list()
  )

  expect_error(parse_dsd_list(api_response), "Invalid API response")
})

test_that("parse_dsd_list handles missing fields", {
  api_response <- list(
    DataStructure = list(
      list(
        id = "DS_ENTERPRISE_SURVEY",
        agencyId = NULL,
        version = "1.0"
      ),
      list(
        id = NULL,
        agencyId = "WB.DATA360",
        version = "1.2"
      ),
      list(
        id = "DS_DATA360",
        agencyId = "WB.DATA360",
        version = NULL
      )
    )
  )

  result <- parse_dsd_list(api_response)

  expected <- character(0)  # No valid entries
  expect_equal(result, expected)
})

test_that("parse_dsd_list handles partial missing fields", {
  api_response <- list(
    DataStructure = list(
      list(
        id = "DS_ENTERPRISE_SURVEY",
        agencyId = "WB.TEST",
        version = "1.0"
      ),
      list(
        id = "DS_DATA360",
        agencyId = NULL,
        version = "1.2"
      ),
      list(
        id = "DS_ANOTHER",
        agencyId = "WB.AGENCY",
        version = "2.0"
      )
    )
  )

  result <- parse_dsd_list(api_response)

  expected <- c(
    "WB.TEST:DS_ENTERPRISE_SURVEY(1.0)",
    "WB.AGENCY:DS_ANOTHER(2.0)"
  )

  expect_equal(result, expected)
})

test_that("parse_dsd_list handles extra fields", {
  api_response <- list(
    DataStructure = list(
      list(
        id = "DS_ENTERPRISE_SURVEY",
        agencyId = "WB.TEST",
        version = "1.0",
        extraField = "Extra"
      ),
      list(
        id = "DS_DATA360",
        agencyId = "WB.DATA360",
        version = "1.2",
        names = list(
          list(locale = "en", value = "Data 360 Data Structure Definition")
        )
      )
    )
  )

  result <- parse_dsd_list(api_response)

  expected <- c(
    "WB.TEST:DS_ENTERPRISE_SURVEY(1.0)",
    "WB.DATA360:DS_DATA360(1.2)"
  )

  expect_equal(result, expected)
})

# test-create_mapping_table.R

test_that("create_mapping_table creates a Fixed table", {
  full_data <- list()
  full_data <- create_mapping_table(full_data, "FixedTable", "Fixed")

  # Check that the table was added
  expect_true("FixedTable" %in% names(full_data))

  # Check the structure of the Fixed table
  expect_true(is.data.frame(full_data$representation[["FixedTable"]]))
  expect_equal(names(full_data$representation[["FixedTable"]]), "FIXED")
  expect_equal(nrow(full_data$representation[["FixedTable"]]), 0) # Ensure the table is empty
})

test_that("create_mapping_table creates a Mapping table", {
  full_data <- list()
  full_data <- create_mapping_table(full_data, "MappingTable", "Mapping")

  # Check that the table was added
  expect_true("MappingTable" %in% names(full_data))

  # Check the structure of the Mapping table
  expect_true(is.data.frame(full_data$representation[["MappingTable"]]))
  expect_equal(names(full_data$representation[["MappingTable"]]), c("SOURCE", "TARGET"))
  expect_equal(nrow(full_data$representation[["MappingTable"]]), 0) # Ensure the table is empty
})

test_that("create_mapping_table does not modify full_data for Implicit type", {
  full_data <- list(existing_table = data.frame(x = 1))
  modified_data <- create_mapping_table(full_data, "existing_table", "Implicit")
  modified_data <- create_mapping_table(modified_data, "new_table", "Implicit")

  # Check that the original data is unchanged
  expect_equal(full_data, modified_data)

  # Ensure the new table was not added
  expect_equal(names(full_data), names(modified_data))
})

test_that("create_mapping_table throws an error for invalid table_type", {
  full_data <- list()

  # Expect an error if the table_type is invalid
  expect_error(
    create_mapping_table(full_data, "InvalidTable", "InvalidType")
  )
})

test_that("create_mapping_table can update an existing table", {
  full_data <- list(FixedTable = data.frame(FIXED = c("value1")))
  full_data <- create_mapping_table(full_data, "FixedTable", "Fixed")

  # Check that the FixedTable was updated
  expect_true("FixedTable" %in% names(full_data))
  expect_equal(names(full_data$representation[["FixedTable"]]), "FIXED")
  expect_equal(nrow(full_data$representation[["FixedTable"]]), 0) # The table should be reset
})
