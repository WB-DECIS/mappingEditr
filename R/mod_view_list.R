# UI Function for the module
view_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyTree::shinyTree(ns("tree")) # Collapsible tree
  )
}

# Server Function for the module
view_list_server <- function(id, json_data) {
  moduleServer(id, function(input, output, session) {
    # Reactive JSON structure for visualization
    json_structure <- reactive({
      if (!is.null(json_data())) {
        tryCatch({
          # Parse the JSON file into a list
          parsed_json <- jsonlite::fromJSON(json_data(), simplifyVector = FALSE)
          parsed_json # Return the parsed JSON
        }, error = function(e) {
          NULL # Return NULL if parsing fails
        })
      } else {
        NULL
      }
    })

    # Render the shinyTree output
    output$tree <- shinyTree::renderTree({
      json_structure()
    })
  })
}
