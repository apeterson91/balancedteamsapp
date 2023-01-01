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
    tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # Your application UI logic
      navbarPage(
        theme = shinythemes::shinytheme("cosmo"),
        title = "Generate Balanced Teams Tool",
        id = "nav",
        tabPanel(
          "Analysis",
          mod_teamgen_ui("teamgen_ui")
        ),
        tabPanel(
          "About",
          mod_about_ui("AboutPage")
        ),
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
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'balancedteamsapp'
    ),
    tags$script(src = "inst/app/www/script.js")
  )
}

