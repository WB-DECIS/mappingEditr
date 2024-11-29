# mod_select_fmr.R

#' Instance Selector Module UI
#'
#' @description A Shiny module UI for selecting the FMR instance (DEV, QA, PROD).
#'
#' @param id Module ID
#'
#' @return A `selectInput` UI element for selecting an instance.
#' @export
select_fmr_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectInput(
    inputId = ns("instance_choice"),
    label = "Select FMR instance:",
    choices = c("FMR QA", "FMR PROD"),
    selected = "FMR QA"  # Default selection is PROD
  )
}

#' Instance Selector Module Server
#'
#' @description A Shiny module server for handling instance selection and providing the corresponding URL.
#'
#' @param id Module ID
#'
#' @return A list containing a reactive expression `selected_instance_url` that returns the selected instance URL.
#' @export
select_fmr_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Mapping from instance names to URLs
    instance_url_map <- list(
      "FMR QA" = "https://fmrqa.worldbank.org/",
      "FMR PROD" = "https://fmr.worldbank.org/"
    )

    # Reactive expression to get the selected URL
    selected_instance_url <- shiny::reactive({
      instance_url_map[[input$instance_choice]]
    })

    # Return the reactive expression so other modules can use it
    return(list(
      selected_instance_url = selected_instance_url
    ))
  })
}
