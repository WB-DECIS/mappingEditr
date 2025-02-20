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
#' @param populated_df A data.frame containing a TARGET codelist IDs and LABELs.
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
                                 table_type = c("Implicit", "Fixed", "Mapping"),
                                 populated_df = NULL) {
  # check that only valid table types are being passed
  valid_table_types <- eval(formals(create_mapping_table)$table_type)
  match.arg(table_type, valid_table_types)
  if (table_type == "Fixed") {
    if (is.null(populated_df)) {
      new_df <- append_empty_row(input_df = data.frame(FIXED = character(0),
                                                       LABEL = character(0),
                                                       stringsAsFactors = FALSE))
      full_data$representation[[table_name]] <- new_df

      return(full_data)
    } else {
      full_data$representation[[table_name]] <- populated_df
      return(full_data)
    }
  } else if (table_type == "Mapping") {
    if (is.null(populated_df)) {
      new_df <- append_empty_row(input_df = data.frame(SOURCE = character(0),
                                                       TARGET = character(0),
                                                       LABEL = character(0),
                                                       IS_REGEX = FALSE,
                                                       stringsAsFactors = FALSE))
      full_data$representation[[table_name]] <- new_df

      return(full_data)
    } else {
      full_data$representation[[table_name]] <- populated_df
      return(full_data)
    }
  } else {
    return(full_data)
  }
}

#' Select correct table
#'
#' This function retrieves a specific mapping element from a JSON-like data
#' structure. It allows selecting either a `components` table or a table
#' from the `representation` field within the JSON data.
#'
#' @param json_data A function or reactive expression returning a list
#'   containing JSON data. The JSON data should have the structure:
#'   - `components`: A named element representing the components table.
#'   - `representation`: A list containing representation tables.
#' @param table_name A character string specifying the table to retrieve.
#'   - If `"components"`, the function returns the `components` element.
#'   - Otherwise, it retrieves the table from the `representation` field.
#' @return The specified table data, or `NULL` if the table does not exist.
#' @examples
#' json_data <- function() {
#'   list(
#'     components = list(mpg = "mpg", cyl = "cyl"),
#'     representation = list(
#'       FREQ = list(freq1 = "value1"),
#'       REF_AREA = list(ref1 = "value2")
#'     )
#'   )
#' }
#'
#' # Retrieve the components table
#' select_mapping_element(json_data, "components")
#'
#' # Retrieve a representation table
#' select_mapping_element(json_data, "FREQ")
#'
#' @export
select_correct_table <- function(json_data, table_name) {
  if (table_name == "components") {
    table_data <- json_data[[table_name]]
  } else {
    table_data <- json_data$representation[[table_name]]
  }
  return(table_data)
}

#' Update Correct Table in JSON Data
#'
#' @description Updates a specific table within a JSON-like object based on the provided table name and updated data.
#'
#' @param json_data A list representing the JSON-like object containing the data to be updated.
#' @param table_name A character string specifying the name of the table to update.
#'        If the table name is `"components"`, the function updates the `component` field;
#'        otherwise, it updates the `representation` field.
#' @param updated_data The new data to assign to the specified table. It should match the structure
#'        expected by the targeted table within the JSON-like object.
#'
#' @return The updated `json_data` object with the specified table updated with the `updated_data`.
#'
#' @details This function is designed to handle updates to specific parts of a JSON-like object. The
#'          structure of the `json_data` is assumed to have either a `component` or `representation`
#'          field that stores the tables being updated. The choice of which field to update depends
#'          on the value of `table_name`.
#'
#' @examples
#' # Example JSON-like data
#' json_data <- list(
#'   component = list(components = data.frame(id = 1, value = "A")),
#'   representation = list(other_table = data.frame(id = 2, value = "B"))
#' )
#'
#' # Updated data for the "components" table
#' updated_data <- data.frame(id = 3, value = "C")
#'
#' # Update the "components" table
#' updated_json <- update_correct_table(json_data, "components", updated_data)
#'
#' # Check the updated JSON
#' print(updated_json$component$components)
#'
#' @export
update_correct_table <- function(json_data, table_name, updated_data) {
  if (table_name == "components") {
    json_data$components <- updated_data
  } else {
    json_data$representation[[table_name]] <- updated_data
  }
  return(json_data)
}

#' Append an empty row to a data frame
#'
#' This function takes an input data frame and appends a new row containing
#' \code{NA} values for each of its columns. This can be useful for initializing
#' a new entry in a table or ensuring that a minimum row structure is maintained.
#'
#' @param input_df A data frame. The data frame to which an empty row should be appended.
#'
#' @return A data frame with one additional row, where the new row's values are \code{NA}.
#'
#' @examples
#' # Example usage:
#' df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
#' df_updated <- append_empty_row(df)
#' df_updated
#'
#' @export
append_empty_row <- function(input_df) {
  # Create a new row with NA values for each column
  new_row <- as.data.frame(lapply(input_df, function(x) NA), stringsAsFactors = FALSE)

  # Bind the new row to the original data frame
  updated_df <- rbind(input_df, new_row)

  return(updated_df)
}

#' Parse SDMX Codelist
#'
#' This function takes a parsed JSON object (as a list) representing an SDMX codelist
#' structure and extracts the `items.id` and `items.names.value` fields. It returns
#' a two-column data frame with columns `ID` and `LABEL`.
#'
#' @param json A list representing the parsed JSON structure of an SDMX codelist.
#'
#' @return A data frame with two columns: `ID` and `LABEL`.
#'
#' @examples
#' # Assuming 'json' is the parsed JSON structure shown in the prompt:
#' # json <- jsonlite::fromJSON("path_to_json_file.json", simplifyVector = FALSE)
#' df <- parse_sdmx_cl(json)
#' head(df)
#'
#' @export
parse_sdmx_cl <- function(json) {
  # Extract the first codelist. If there's more than one, adjust accordingly.
  codelist <- json$Codelist[[1]]

  # Extract items
  items <- codelist$items

  # Extract IDs
  ids <- vapply(items, function(x) x$id, FUN.VALUE = character(1))

  # Extract Labels (assuming 'names' is always at least one element and 'value' is present)
  labels <- vapply(items, function(x) x$names[[1]]$value, FUN.VALUE = character(1))

  # Create a two-column data frame
  df <- data.frame(
    ID = ids,
    LABEL = labels,
    stringsAsFactors = FALSE
  )

  # Sort the dataframe alphabetically by ID
  df <- df[order(df$ID), ]

  return(df)
}

#' Fetch and Parse SDMX Codelist
#'
#' This function retrieves a codelist from the SDMX API, parses it, and returns a data frame
#' containing the codelist's IDs and labels.
#'
#' @param table_name The name of the table to derive the codelist ID.
#' @param concepts_to_cl A lookup table linking concepts to their corresponding codelists
#' @param instance_url A function or string representing the base URL of the API endpoint.
#'
#' @return A data frame with two columns: `ID` and `LABEL`. Returns `NULL` if an error occurs.
#'
#' @examples
#' # Example usage
#' instance_url <- function() "https://example.com/"
#' df <- fetch_cl("UNIT_MEASURE", instance_url)
#' head(df)
#'
#' @export
fetch_cl <- function(table_name, concepts_to_cl, instance_url) {
  # Initialize the result
  codelist_df <- NULL
  # retrieve correct codelist ID
  cl_id <- concepts_to_cl$codelist_id[concepts_to_cl$concept_id == table_name]
  cl_id <- parse_dsd_id(cl_id)

  tryCatch({
    # Construct the API URL
    api_base_url <- instance_url
    api_url <- paste0(
      api_base_url,
      "FMR/ws/public/sdmxapi/rest/codelist/",
      cl_id,
      "?format=fusion-json"
    )

    # Make the API call
    response <- httr::GET(api_url)

    # Check if the response is successful
    if (httr::status_code(response) == 200) {
      # Parse the response content
      response_content <- httr::content(response, as = "parsed", type = "application/json")
      codelist_df <- parse_sdmx_cl(response_content)
    } else {
      stop("Failed to fetch codelist. Status code: ", httr::status_code(response))
    }
  }, error = function(e) {
    # Handle errors and show a notification in Shiny
    shiny::showNotification(paste("Error:", e$message), type = "error")
  })

  # Return the codelist or NULL if an error occurred
  return(codelist_df)
}
