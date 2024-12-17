# mod_select_dsd.R

#' DSD Selector Module UI
#'
#' @description A Shiny module UI for selecting a DSD from the available DSDs.
#'
#' @param id Module ID
#'
#' @return A `selectInput` UI element for selecting a DSD.
#' @export
select_dsd_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectInput(
    inputId = ns("dsd_choice"),
    label = "Select DSD:",
    choices = NULL  # Choices will be populated in the server
  )
}

# mod_select_dsd.R

#' DSD Selector Module Server
#'
#' @description A Shiny module server for fetching available DSDs and handling DSD selection.
#'
#' @param id Module ID
#' @param selected_instance_url A reactive expression that returns the base URL for API calls.
#'
#' @return A list containing a reactive expression `selected_dsd_id` that returns the selected DSD ID.
#' @export
select_dsd_server <- function(id, selected_instance_url) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the list of DSD IDs
    dsd_list <- shiny::reactiveVal(NULL)

    # Function to fetch DSD list from the API
    fetch_dsd_list <- function(api_url) {
      response <- httr::GET(api_url)
      if (httr::status_code(response) == 200) {
        dsd_info <- httr::content(response, as = "parsed", type = "application/json")
        return(dsd_info)
      } else {
        stop("Failed to fetch DSD list. Status code: ", httr::status_code(response))
      }
    }

    # Observe changes in selected_instance_url to fetch DSDs
    shiny::observeEvent(selected_instance_url(), {
      shiny::req(selected_instance_url())
      api_base_url <- selected_instance_url()
      api_url <- paste0(api_base_url, "FMR/sdmx/v2/structure/datastructure/*/*/*?format=fusion-json&detail=allstubs&includeMetadata=false&includeAllAnnotations=false")

      # Show a loading message
      shiny::showNotification("Fetching DSD list...", type = "message")

      # Fetch the DSD list via API call
      tryCatch({
        dsd_info <- fetch_dsd_list(api_url)
        if (!is.null(dsd_info) && length(dsd_info) > 0) {
          # Use parse_dsd_list to extract DSD IDs
          dsd_ids <- parse_dsd_list(dsd_info)
          dsd_list(dsd_ids)
          shiny::updateSelectInput(session, "dsd_choice", choices = dsd_ids)
        } else {
          shiny::showNotification("No DSDs found.", type = "warning")
          dsd_list(NULL)
          shiny::updateSelectInput(session, "dsd_choice", choices = NULL)
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
        dsd_list(NULL)
        shiny::updateSelectInput(session, "dsd_choice", choices = NULL)
      })
    }, ignoreNULL = TRUE)

    # Reactive expression to return the selected DSD ID
    selected_dsd_id <- shiny::reactive({
      input$dsd_choice
    })

    # Return the selected DSD ID
    return(list(
      selected_dsd_id = selected_dsd_id
    ))
  })
}



