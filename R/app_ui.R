app_ui <- function(request) {
  tagList(
    # Add custom CSS for dynamic sidebar width
    tags$style(
      HTML("
        .bslib-sidebar {
          width: auto !important; /* Allow the sidebar to adjust width */
          min-width: 300px;       /* Minimum width to maintain usability */
          max-width: 400px;       /* Maximum width for long content */
        }
        .bslib-sidebar .card {
          overflow: auto;         /* Ensure content inside the sidebar scrolls if needed */
        }
      ")
    ),
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Application UI logic
    bslib::page_sidebar(
      theme = bslib::bs_theme(version = 5, bootswatch = "zephyr"), # Using a modern bootswatch theme
      title = "Master Lookup Editor",
      bslib::layout_columns(
        widths = c(6, 6), # Adjust widths as needed (here it's a 50-50 split)
        select_fmr_ui("fmr_selector"),
        select_dsd_ui("select_dsd")
      ),
      # Sidebar: Group related actions and organize sections
      sidebar = bslib::sidebar(
        title = "Action Panel",

        # File Management Section
        bslib::card(
          bslib::card_header("File Management"),
          bslib::card_body(
            initialize_map_ui("initialize_map"),
            file_loader_ui("file_loader"),
            download_ui("download")
          )
        ),

        # Table Operations Section
        bslib::card(
          bslib::card_header("Table Operations"),
          bslib::card_body(
            select_table_ui("table_selector"),
            add_table_ui("add_table"),
            delete_table_ui("delete_table")
          )
        ),

        # Row Operations Section
        bslib::card(
          bslib::card_header("Row Operations"),
          bslib::card_body(
            add_row_ui("add_row"),
            delete_row_ui("delete_row")
          )
        )
      ),

      # Main Content Area with Tabs
      bslib::navset_tab(
        bslib::nav_panel(
          "Edit JSON", # Main editing panel
          bslib::card(
            bslib::card_header("Table Viewer"),
            bslib::card_body(
              edit_table_ui("table_editor"),
              define_mapping_type_ui("table_definition")
            )
          )
        ),
        bslib::nav_panel(
          "JSON Overview", # New tab for JSON overview
          view_list_ui("view_list") # Insert the module's UI
        )
      )

      # # Main Content Area
      # bslib::card(
      #   bslib::card_header("Table Viewer"),
      #   bslib::card_body(
      #     # Adding instructions or status messages above the table
      #     bslib::card_body(
      #       tags$p("Use the controls in the sidebar to upload a JSON file, edit the table, and download the updated file."),
      #       tags$hr() # Divider
      #     ),
      #     table_editor_ui("table_editor")
      #   )
      # )
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
