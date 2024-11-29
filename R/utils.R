fetch_json <- function() {
  url <- "https://raw.githubusercontent.com/tonyfujs/data360-lkups/main/wb_hlo_lkups.json"
  res <- httr::GET(url)
  json_content <- httr::content(res, "text")
  json_data <- jsonlite::fromJSON(json_content)
  return(json_data)
}

#' Parse DSD ID to URL Format
#'
#' @description
#' Parses a DSD ID of the form "AgencyID.ArtifactID:ID(Version)" and converts it into a URL-like format "AgencyID.ArtifactID/ID/Version".
#'
#' @param dsd_id A character string representing the DSD ID, e.g., "WB.DATA360:DS_DATA360(1.2)".
#'
#' @return A character string with the parsed DSD ID in the format "AgencyID.ArtifactID/ID/Version".
#'
#' @examples
#' parse_dsd_id("WB.DATA360:DS_DATA360(1.2)")
#' # Returns "WB.DATA360/DS_DATA360/1.2"
#'
#' @export
parse_dsd_id <- function(dsd_id) {
  # Ensure that stringr functions are used with package::function() notation
  dsd_id <- stringr::str_trim(dsd_id)

  # Regular expression pattern to parse the DSD ID
  pattern <- "^(.*?)\\s*:\\s*(.*?)\\s*\\(\\s*(.*?)\\s*\\)$"

  # Match the pattern
  matches <- stringr::str_match(dsd_id, pattern)

  if (is.na(matches[1, 1])) {
    stop("Invalid DSD ID format.")
  } else {
    agency_artifact <- matches[1, 2]
    id <- matches[1, 3]
    version <- matches[1, 4]

    # Construct the output
    parsed_id <- paste0(agency_artifact, "/", id, "/", version)
    return(parsed_id)
  }
}

#' Extract Concepts from DSD JSON
#'
#' @description
#' Extracts all the concept IDs from the dimensions, attributes, and measures in a Data Structure Definition (DSD) JSON object returned by the FMR API.
#'
#' @param dsd_json A list representing the parsed JSON response from the FMR API containing the DSD information.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{dimensions}}{A character vector of dimension IDs.}
#'   \item{\code{attributes}}{A character vector of attribute IDs.}
#'   \item{\code{measures}}{A character vector of measure IDs.}
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming you have the JSON response as a string
#' json_string <- '{"DataStructure": [...]}'
#' dsd_json <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
#' concepts <- extract_concepts_from_dsd(dsd_json)
#' }
#'
#' @export
extract_concepts_from_dsd <- function(dsd_json) {
  # Initialize empty vectors
  dimension_ids <- character(0)
  attribute_ids <- character(0)
  measure_ids <- character(0)

  # Check that dsd_json$DataStructure is valid
  if (!is.null(dsd_json$DataStructure) && length(dsd_json$DataStructure) > 0) {
    data_structure <- dsd_json$DataStructure[[1]]  # Assuming the first DataStructure

    # Extract dimensions
    if (!is.null(data_structure$dimensionList$dimensions)) {
      dimensions <- data_structure$dimensionList$dimensions
      dimension_ids <- sapply(dimensions, function(x) if (!is.null(x$id)) x$id else NA)
    }

    # Extract attributes
    if (!is.null(data_structure$attributeList$attributes)) {
      attributes <- data_structure$attributeList$attributes
      attribute_ids <- sapply(attributes, function(x) if (!is.null(x$id)) x$id else NA)
    }

    # Extract measures
    if (!is.null(data_structure$measures)) {
      measures <- data_structure$measures
      measure_ids <- sapply(measures, function(x) if (!is.null(x$id)) x$id else NA)
    }
  } else {
    stop("Invalid DSD JSON structure: 'DataStructure' element is missing or empty.")
  }

  return(list(
    dimensions = as.character(dimension_ids),
    attributes = as.character(attribute_ids),
    measures = as.character(measure_ids)
  ))
}

#' Parse DSD List from API Response
#'
#' @description
#' Parses the API response containing Data Structure Definitions (DSDs) and extracts a list of DSD IDs in the format "agencyId:id(version)".
#'
#' @param api_response A list representing the parsed JSON response from the API containing DSD information.
#'
#' @return A character vector of DSD IDs in the format "agencyId:id(version)".
#'
#' @examples
#' \dontrun{
#' # Assuming you have the JSON response as a string
#' json_string <- '{"DataStructure": [...]}'
#' api_response <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
#' dsd_ids <- parse_dsd_list(api_response)
#' }
#'
#' @importFrom stringr str_c
#' @export
parse_dsd_list <- function(api_response) {
  if (is.null(api_response$DataStructure) || length(api_response$DataStructure) == 0) {
    stop("Invalid API response: 'DataStructure' element is missing or empty.")
  }

  data_structures <- api_response$DataStructure
  dsd_ids <- vapply(data_structures, function(dsd) {
    agency_id <- dsd$agencyId
    id <- dsd$id
    version <- dsd$version
    if (is.null(agency_id) || is.null(id) || is.null(version)) {
      NA_character_
    } else {
      # Construct the DSD ID in the format "agencyId:id(version)"
      stringr::str_c(agency_id, ":", id, "(", version, ")")
    }
  }, FUN.VALUE = character(1))


  # Remove any NA values in case of missing data
  dsd_ids <- dsd_ids[!is.na(dsd_ids)]

  return(as.character(dsd_ids))
}

#' Handles logic for creating mapping tables according to the mapping type
#'
#' @description This function adds a new table or update an existing one in the `full_data` list based on the specified table type.
#'
#' @param full_data A named list representing the full dataset. This is where the new table will be added.
#' @param table_name A string specifying the name of the new table to be created.
#' @param table_type A string indicating the type of the table to be created.
#'   Must be one of `"Implicit"`, `"Fixed"`, or `"Mapping"`. Defaults to `"Implicit"`.
#'
#' @details
#' - If `table_type` is `"Fixed"`, the new table is a data frame with one column: `FIXED`.
#' - If `table_type` is `"Mapping"`, the new table is a data frame with two columns: `SOURCE` and `TARGET`.
#' - If `table_type` is `"Implicit"`, the function returns the `full_data` unchanged.
#'
#' @return The modified `full_data` list with the new table added, or the original `full_data` if `table_type` is `"Implicit"`.
#'
#' @examples
#' # Example full dataset
#' full_data <- list()
#'
#' # Create a Fixed table
#' full_data <- create_mapping_table(full_data, "FixedTable", "Fixed")
#'
#' # Create a Mapping table
#' full_data <- create_mapping_table(full_data, "MappingTable", "Mapping")
#'
#' # No table is added for "Implicit"
#' full_data <- create_mapping_table(full_data, "NoTable", "Implicit")
#'
#' @export
create_mapping_table <- function(full_data,
                                 table_name,
                                 table_type = c("Implicit", "Fixed", "Mapping")) {
  # check that only valid table types are being passed
  valid_table_types <- eval(formals(create_mapping_table)$table_type)
  match.arg(table_type, valid_table_types)
  if (table_type == "Fixed") {
    full_data[[table_name]] <- data.frame(FIXED = character(0),
                                          stringsAsFactors = FALSE)
    return(full_data)
  } else if (table_type == "Mapping") {
    full_data[[table_name]] <- data.frame(SOURCE = character(0),
                                          TARGET = character(0),
                                          stringsAsFactors = FALSE)
    return(full_data)
  } else {
    return(full_data)
  }
}

