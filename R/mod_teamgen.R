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
          inputId = NS(id, "avit"), label = "Upload Player Data"
        ),
        br(),
        sliderInput(
          inputId = NS(id, "num_teams"),
          label = "Number of Teams",
          min = 2,
          max = 15,
          value = 7
        ),
        actionButton(NS(id, "do"), "Generate Teams"),
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
          x = balancedteams::mens_team, file = file, quote = TRUE, sep = ",",
          row.names = FALSE, col.names = TRUE)
         }
       )

    output$downloadgit <- downloadHandler(
      filename = function() {"GeneratedTeams.csv"},
      content = function(file) {
        write.table(
          x = indf, file = file, quote = TRUE, sep = ",",
          row.names = FALSE, col.names = TRUE)
         }
       )

    observeEvent(input$do, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Teams Generated')
    })

    indf <- reactive({
      file <- input$avit
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      indf <- read.csv(file$datapath, header = input$header)
    })

    team_id <- group_id <- group_score <- NULL

    gtdf <-
      reactive({
        balancedteams::GenerateBalancedTeams(indf$group_id,
                                             indf$group_score,
                                             num_teams = 7) %>%
        dplyr::arrange(team_id, group_id, group_score)
      })

    st <- reactive({
      indf %>%
      dplyr::group_by(team_id) %>%
      dplyr::summarize(`Mean Score` = mean(group_score),
                       `# Players` = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::rename(`Team` = team_id)
    })

    output$summary_table <- renderTable(st)

    output$generated_table <- renderDataTable(gtdf)
  })
}