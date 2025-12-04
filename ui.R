library(shiny)

games_file <- "games_history.csv"  # file where all games are stored

ui <- fluidPage(
  titlePanel("Basketball Game Tracker"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # --- TAB 1: New Game ----------------------------------------------------
    tabPanel(
      "New Game",
      sidebarLayout(
        sidebarPanel(
          dateInput("game_date", "Game date:", value = Sys.Date()),
          textInput("tournament", "Tournament:", ""),
          textInput("opponent", "Opponent:", ""),
          tags$hr(),
          
          h4("Record actions"),
          actionButton("ft_made", "FT Made"),
          actionButton("ft_missed", "FT Missed"),
          br(),
          br(),
          actionButton("two_made", "2P Made"),
          actionButton("two_missed", "2P Missed"),
          br(),
          br(),
          actionButton("three_made", "3P Made"),
          actionButton("three_missed", "3P Missed"),
          br(),
          br(),
          actionButton("oreb", "Off Reb"),
          actionButton("dreb", "Def Reb"),
          br(),
          br(),
          actionButton("assist", "Assist"),
          actionButton("steal", "Steal"),
          actionButton("blocks", "Block"),
          br(),
          br(),
          actionButton("turnover", "Turnover"),
          tags$hr(),
          
          actionButton("reset", "Reset current game"),
          actionButton("save_game", "Save game")
        ),
        mainPanel(
          fluidRow(
            h3("Current game stats"),
            column(4,
              tableOutput("count_table")
            ),
            column(4,
              tableOutput("pct_table")
            ),
            column(4,
                   tableOutput("summary_table"),
                   br(),
                   textOutput("eff_out")
            ),
          )
        )
      )
    ),
    
    # --- TAB 2: Game History ------------------------------------------------
    tabPanel(
      "Game History",
      
      # Game selector
      fluidRow(
        column(
          width = 4,
          uiOutput("game_select_ui")
        )
      ),
      
      tags$hr(),
      
      # Three-column layout matching "New Game"
      fluidRow(
        h3(textOutput("selected_game_title")),
        
        column(
          width = 4,
          tableOutput("count_table_hist")
        ),
        
        column(
          width = 4,
          tableOutput("pct_table_hist")
        ),
        
        column(
          width = 4,
          tableOutput("summary_table_hist"),
          br(),
          textOutput("eff_out_hist")
        )
      ),
      
      tags$hr(),
      
      # All saved games list
      fluidRow(
        column(
          width = 12,
          h4("All saved games"),
          tableOutput("games_history")
        )
      )
    )
  )
)
