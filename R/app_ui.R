#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shiny::titlePanel("JSON Editor"),
      bslib::card(
        bslib::card_header("Load and Edit JSON"),
        bslib::card_body(
          file_loader_ui("file_loader"),
          table_manager_ui("table_manager"),
          table_selector_ui("table_selector"),
          row_manager_ui("row_manager"),
          table_editor_ui("table_editor"),
          shiny::div(
            class = "d-flex justify-content-between mt-3",
            download_ui("download")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mappingEditr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
