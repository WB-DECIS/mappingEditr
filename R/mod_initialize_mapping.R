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
#'
#' @return None.
#' @export
initialize_map_server <- function(id,
                                  selected_instance_url,
                                  selected_dsd_id) {
  shiny::moduleServer(id, function(input, output, session) {
    json_data <- shiny::reactiveVal(NULL)
    ns <- session$ns

    # Observe the initialize lookup button
    shiny::observeEvent(input$initialize_map, {
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

      # Process the DSD information to create empty json_data structure
      concepts <- extract_concepts_from_dsd(dsd_info)
      all_concept_ids <- c(concepts$dimensions, concepts$attributes, concepts$measures)

      new_json_data <- list(components = vector("list", length = 1),
                            representation = vector("list", length = length(all_concept_ids)))

      # Fill out json_data structure with DSD information
      ## Add components
      components_df <- data.frame(source = NA,
                                  target = all_concept_ids,
                                  stringsAsFactors = FALSE)
      new_json_data$components <- components_df
      # Create empty data frames for each concept
      names(new_json_data$representation) <- all_concept_ids

      # Update the json_data reactiveVal
      json_data <- json_data(new_json_data)

      # Notify the user
      shiny::showNotification("Lookup initialized successfully!", type = "message")
    })
    return(json_data)
  })
}
