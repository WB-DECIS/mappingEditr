app_ui <- function(request) {
  tagList(
    # Custom CSS for dynamic/resizable sidebar width
    tags$style(
      HTML("
        .bslib-sidebar {
          width: auto !important;
          min-width: 300px;       /* Minimum width to maintain usability */
          max-width: 600px;       /* Increased maximum width for long content */
        }
        .bslib-sidebar .card {
          overflow: auto;         /* Scroll content if needed */
        }
        /* Adjust the main panel */
        #mainPanel {
          margin-left: 320px;      /* Reduce default empty space */
          max-width: calc(100% - 320px); /* Prevent excessive empty space */
          transition: margin-left 0.2s ease, max-width 0.2s ease;
        }
      ")
    ),

    # Add external resources (jQuery UI)
    golem_add_external_resources(),

    # Initialize the resizable functionality on the sidebar using jQuery UI.
    # Also update the mainPanel's margin to prevent overlap.
    tags$script(
      HTML("
        $(document).ready(function(){
          $('#sidebar').resizable({
            handles: 'e',
            minWidth: 300,
            maxWidth: 600,
            resize: function(event, ui) {
              var newWidth = ui.size.width;
              // Reduce the empty space while keeping things proportional
              $('#mainPanel').css({
                'margin-left': newWidth + 'px',
                'max-width': 'calc(100% - ' + newWidth + 'px)'
              });
            },
            stop: function(event, ui) {
              var newWidth = ui.size.width;
              $('#mainPanel').css({
                'margin-left': newWidth + 'px',
                'max-width': 'calc(100% - ' + newWidth + 'px)'
              });
            }
          });
          // Set initial margin based on the sidebar’s current width
          $('#mainPanel').css({
            'margin-left': $('#sidebar').width() + 'px',
            'max-width': 'calc(100% - ' + $('#sidebar').width() + 'px)'
          });
        });
      ")
    ),

    # Application UI logic
    bslib::page_sidebar(
      theme = bslib::bs_theme(version = 5, bootswatch = "zephyr"),
      title = "Master Lookup Editor",

      # Sidebar with an id ("sidebar") for the resizable functionality
      sidebar = bslib::sidebar(
        title = "Action Panel",
        id = "sidebar",

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

      # Everything else goes in mainPanel so it can shift as a single block
      div(
        id = "mainPanel",

        # Top row of UI elements
        bslib::layout_columns(
          widths = c(6, 6),
          select_fmr_ui("fmr_selector"),
          select_dsd_ui("select_dsd")
        ),

        # Main Content Area with Tabs
        bslib::navset_tab(
          bslib::nav_panel(
            "Edit JSON",
            bslib::card(
              bslib::card_header("Table Viewer"),
              bslib::card_body(
                edit_table_ui("table_editor")
              )
            )
          ),
          bslib::nav_panel(
            "JSON Overview",
            view_list_ui("view_list")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external resources inside the Shiny application.
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "mappingEditr"
    ),
    # Include jQuery UI (CSS and JS) for resizable functionality
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
  )
}
