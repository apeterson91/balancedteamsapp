#' teamgen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_teamgen_ui <- function(id) {
  ns <- NS(id, "teamgen_ui")
  tagList(
    sidebarLayout(
      sidebarPanel(
        tags$a(
          id = NS(id, "downloadit"), class = paste("btn btn-default shiny-download-link", "btn-primary"),
          href = "", target = "_blank", style = "width: 70%", icon("download"),
          "Player Template"
        ),
        fileInput(
          inputId = NS(id, "upload"), label = "Upload Player Data",
          accept = ".csv"
        ),
        br(),
        sliderInput(
          inputId = NS(id, "num_teams"),
          label = "Number of Teams",
          min = 2,
          max = 15,
          value = 7
        ),
        radioButtons(
          inputId = NS(id, "stratify"),
          label = "Stratify Team Balance",
          c("No" = FALSE, "Yes" = TRUE)
        ),
        tags$a(
          id = NS(id, "downloadgit"),
          class = paste("btn btn-default shiny-download-link", "btn-primary"),
          href = "", target = "_blank", style = "width: 70%", icon("download"),
          "Generated Teams"
        ),
      ),
      mainPanel(
        tableOutput(NS(id, "summary_table")),
        br(),
        dataTableOutput(NS(id, "generated_table"))
      )
    )
  )
}

#' teamgen Server Functions
#'
#' @importFrom utils read.csv write.table
#' @noRd
mod_teamgen_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$downloadit <- downloadHandler(
      filename = function() {"PlayerTemplate.csv"},
      content = function(file) {
        write.table(
          x = balancedteams::stratified_dataset, file = file, quote = TRUE, sep = ",",
          row.names = FALSE, col.names = TRUE)
         }
       )

    output$downloadgit <- downloadHandler(
      filename = function() {"GeneratedTeams.csv"},
      content = function(file) {
        write.table(
          x = gtdf(), file = file, quote = TRUE, sep = ",",
          row.names = FALSE, col.names = TRUE)
         }
       )

    indf <- reactive({
      file <- input$upload
      req(file)
      ext <- tools::file_ext(file$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
      indf <- readr::read_csv(file$datapath)
    })

    team_id <- group_id <- score <- group_score <- NULL

    nt <- reactive({input$num_teams})

    stratify_opt <- reactive({as.logical(input$stratify)})

    gtdf <-
      reactive({
        req(input$upload)
        balancedteams::GenerateBalancedTeams(indf(),
                                             num_teams = nt(),
                                             stratify = stratify_opt()) # %>%
        # dplyr::arrange(team_id, player_id, player_score)
      })

    st <- reactive({
      if(stratify_opt()) {
        gtdf() %>%
        dplyr::group_by(team_id) %>%
        dplyr::summarize(`Mean Score` = mean(player_score),
                         `Mean Strata Score` = mean(player_score * strata),
                         `# Players` = dplyr::n(),
                         `# Strata Players` = sum(strata)) %>%
        dplyr::rename(Team = team_id)

      } else {
        gtdf() %>%
        dplyr::group_by(team_id) %>%
        dplyr::summarize(`Mean Score` = mean(player_score),
                         `# Players` = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::rename(`Team` = team_id)
      }
    })

    output$summary_table <- renderTable({st()})

    output$generated_table <- renderDataTable({gtdf()})
  })
}