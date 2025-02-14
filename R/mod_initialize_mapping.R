# mod_initialize_mapp.R

#' Initialize map Module UI
#'
#' @description A Shiny module UI for initializing a mapping from a DSD.
#'
#' @param id Module ID
#'
#' @return An action button to initialize a new mapping.
#' @export
initialize_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(
    inputId = ns("initialize_map"),
    label = "Initialize new mapping",
    class = "btn-primary"
  )
}


#' Initialize Lookup Module Server
#'
#' @description A Shiny module server for initializing a new lookup from a selected DSD.
#'
#' @param id Module ID
#' @param selected_instance_url A reactive expression that returns the base URL for API calls.
#' @param selected_dsd_id A reactive expression that returns the selected DSD ID.
#' @param json_data A reactive expression that returns the mapping structure.
#'
#' @return None.
#' @export
initialize_map_server <- function(id,
                                  selected_instance_url,
                                  selected_dsd_id,
                                  json_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    concepts_to_cl <- shiny::reactiveVal(NULL)

    # Observe the initialize lookup button
    shiny::observeEvent(input$initialize_map, {
      shiny::req(json_data)
      shiny::req(selected_instance_url)
      shiny::req(selected_dsd_id)
      # Check if a DSD ID is selected
      dsd_id <- selected_dsd_id()
      if (is.null(dsd_id) || dsd_id == "") {
        shiny::showNotification("Please select a DSD.", type = "warning")
        return()
      }

      # Show a loading message
      shiny::showNotification("Fetching DSD information...", type = "message")

      # Fetch the DSD information via API call
      dsd_info <- NULL
      tryCatch({
        # Construct the API URL (replace with the actual API endpoint)
        parsed_dsd_id <- parse_dsd_id(dsd_id)
        api_base_url <- selected_instance_url()
        api_url <- paste0(api_base_url,
                          "FMR/sdmx/v2/structure/datastructure/",
                          parsed_dsd_id,
                          "?format=fusion-json")
        # Make the API call
        response <- httr::GET(api_url)

        # Check if the response is successful
        if (httr::status_code(response) == 200) {
          # Parse the response content
          dsd_info <- httr::content(response, as = "parsed", type = "application/json")
        } else {
          stop("Failed to fetch DSD information. Status code: ", httr::status_code(response))
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })

      if (is.null(dsd_info)) {
        # If dsd_info is NULL, an error occurred
        return()
      }

      # Create json_data structure ----
      ## Process the DSD information to create empty json_data structure ----
      concepts <- extract_concepts_from_dsd(dsd_info)
      all_concept_ids <- c(concepts$dimensions, concepts$attributes, concepts$measures)

      new_json_data <- list(components = vector("list", length = 1),
                            representation = vector("list", length = length(all_concept_ids)))

      ## Fill out json_data structure with DSD information ----
      ### Add components ----
      components_df <- data.frame(SOURCE = NA,
                                  TARGET = all_concept_ids,
                                  stringsAsFactors = FALSE)
      new_json_data$components <- components_df
      # Create empty data frames for each concept
      names(new_json_data$representation) <- all_concept_ids

      # Update the json_data reactiveVal
      json_data(new_json_data)

      # Create concepts to codelist lookup ----
      new_concepts_to_cl <- create_concepts_to_cl_lkup(dsd_info)
      concepts_to_cl(new_concepts_to_cl)

      # Notify the user
      shiny::showNotification("Lookup initialized successfully!", type = "message")
    })
    return(concepts_to_cl = concepts_to_cl)
  })
}


#' Create a concept-to-codelist lookup dataframe from an SDMX DSD response
#'
#' This function parses an SDMX Data Structure Definition (DSD) response,
#' extracting all concepts (dimensions, attributes, and measures) and
#' their associated codelists (if any).
#'
#' @param resp A parsed R list representing an SDMX DSD response, similar
#'   to the format returned by `rsdmx` or equivalent SDMX APIs.
#'
#' @return A `data.frame` with two columns:
#'   \describe{
#'     \item{concept_id}{Extracted concept ID.}
#'     \item{codelist_id}{Associated codelist ID, or `NA` if none.}
#'   }
#'
#' @details
#' The function assumes that the input `resp` contains a structure like:
#' \code{resp$DataStructure[[1]]}, which has lists of `dimensions`,
#' `attributes`, and `measures`, each with `concept` and optionally
#' `representation$representation` fields. The `concept` is a URN from which
#' the actual concept ID is extracted, and the `codelist` (if present)
#' is parsed from the `representation` URN.
#'
#' @examples
#' # Assuming `resp` is the parsed SDMX DSD response as shown in the user's example:
#' # result <- create_concepts_to_cl_lkup(resp)
#' # head(result)
#'
#' @export
create_concepts_to_cl_lkup <- function(resp) {
  ds <- resp$DataStructure[[1]]

  get_concept_id <- function(concept_urn) {
    # Extract the concept ID from the last part of the URN
    parts <- strsplit(concept_urn, "\\.")[[1]]
    concept_id <- parts[length(parts)]
    concept_id
  }

  get_codelist_id <- function(rep_urn) {
    # Extract the codelist ID from the representation URN if present
    if (!grepl("Codelist=", rep_urn)) {
      return(NA_character_)
    }
    codelist_part <- sub(".*Codelist=", "", rep_urn)
    codelist_part
  }

  # Extract dimensions
  dims <- ds$dimensionList$dimensions
  dim_df <- do.call(rbind, lapply(dims, function(d) {
    concept_id <- get_concept_id(d$concept)
    if (!is.null(d$representation$representation)) {
      codelist_id <- get_codelist_id(d$representation$representation)
    } else {
      codelist_id <- NA_character_
    }
    data.frame(concept_id = concept_id, codelist_id = codelist_id, stringsAsFactors = FALSE)
  }))

  # Extract attributes
  attrs <- ds$attributeList$attributes
  attr_df <- do.call(rbind, lapply(attrs, function(a) {
    concept_id <- get_concept_id(a$concept)
    if (!is.null(a$representation$representation)) {
      codelist_id <- get_codelist_id(a$representation$representation)
    } else {
      codelist_id <- NA_character_
    }
    data.frame(concept_id = concept_id, codelist_id = codelist_id, stringsAsFactors = FALSE)
  }))

  # Extract measures
  meas <- ds$measures
  meas_df <- do.call(rbind, lapply(meas, function(m) {
    concept_id <- get_concept_id(m$concept)
    if (!is.null(m$representation$representation)) {
      codelist_id <- get_codelist_id(m$representation$representation)
    } else {
      codelist_id <- NA_character_
    }
    data.frame(concept_id = concept_id, codelist_id = codelist_id, stringsAsFactors = FALSE)
  }))

  all_concepts_df <- rbind(dim_df, attr_df, meas_df)
  all_concepts_df <- all_concepts_df[!is.na(all_concepts_df$codelist_id), ]
  all_concepts_df
}
