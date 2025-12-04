library(shiny)

games_file <- "games_history.csv"  # file where all games are stored

server <- function(input, output, session) {
  
  # --- raw counters ----------------------------------------------------------
  vals <- reactiveValues(
    ft_made = 0, ft_missed = 0,
    two_made = 0, two_missed = 0,
    three_made = 0, three_missed = 0,
    turnover = 0, steal = 0,
    oreb = 0, dreb = 0,
    assist = 0, blocks = 0
  )
  
  observeEvent(input$ft_made,      { vals$ft_made      <- vals$ft_made      + 1 })
  observeEvent(input$ft_missed,    { vals$ft_missed    <- vals$ft_missed    + 1 })
  observeEvent(input$two_made,     { vals$two_made     <- vals$two_made     + 1 })
  observeEvent(input$two_missed,   { vals$two_missed   <- vals$two_missed   + 1 })
  observeEvent(input$three_made,   { vals$three_made   <- vals$three_made   + 1 })
  observeEvent(input$three_missed, { vals$three_missed <- vals$three_missed + 1 })
  observeEvent(input$turnover,     { vals$turnover     <- vals$turnover     + 1 })
  observeEvent(input$steal,        { vals$steal        <- vals$steal        + 1 })
  observeEvent(input$oreb,         { vals$oreb         <- vals$oreb         + 1 })
  observeEvent(input$dreb,         { vals$dreb         <- vals$dreb         + 1 })
  observeEvent(input$assist,       { vals$assist       <- vals$assist       + 1 })
  observeEvent(input$blocks,       { vals$blocks       <- vals$blocks       + 1 })
  
  # reset current game
  observeEvent(input$reset, {
    vals$ft_made      <- 0
    vals$ft_missed    <- 0
    vals$two_made     <- 0
    vals$two_missed   <- 0
    vals$three_made   <- 0
    vals$three_missed <- 0
    vals$turnover     <- 0
    vals$steal        <- 0
    vals$oreb         <- 0
    vals$dreb         <- 0
    vals$assist       <- 0
    vals$blocks       <- 0
  })
  
  # --- derived stats for current game ---------------------------------------
  stats <- reactive({
    ftMade    <- vals$ft_made
    ftMiss    <- vals$ft_missed
    twoMade   <- vals$two_made
    twoMiss   <- vals$two_missed
    threeMade <- vals$three_made
    threeMiss <- vals$three_missed
    tovs      <- vals$turnover
    steals    <- vals$steal
    oreb      <- vals$oreb
    dreb      <- vals$dreb
    assists   <- vals$assist
    blocks    <- vals$blocks
    
    reb   <- oreb + dreb
    fgMade   <- twoMade + threeMade
    fgMiss   <- twoMiss + threeMiss
    fga      <- fgMade + fgMiss
    fta      <- ftMade + ftMiss
    points   <- 2 * twoMade + 3 * threeMade + ftMade
    
    ft_pct    <- if (fta > 0) 100 * ftMade / fta else NA
    two_pct   <- if ((twoMade + twoMiss) > 0) 100 * twoMade / (twoMade + twoMiss) else NA
    three_pct <- if ((threeMade + threeMiss) > 0) 100 * threeMade / (threeMade + threeMiss) else NA
    fg_pct    <- if (fga > 0) 100 * fgMade / fga else NA
    
    efficiency <- (points + reb + assists + steals + blocks) -
      ((fga - fgMade) + (fta - ftMade) + tovs)
    
    list(
      counts = c(
        "FT Made"           = ftMade,
        "FT Missed"         = ftMiss,
        "2PM Made"          = twoMade,
        "2PM Missed"        = twoMiss,
        "3PM Made"          = threeMade,
        "3PM Missed"        = threeMiss,
        "Turnover"          = tovs,
        "Steal"             = steals,
        "Offensive Rebound" = oreb,
        "Defensive Rebound" = dreb,
        "Assist"            = assists,
        "Blocks"            = blocks
      ),
      pct = c(
        "FT%" = ft_pct,
        "2P%" = two_pct,
        "3P%" = three_pct,
        "FG%" = fg_pct
      ),
      summary = c(
        "Reb"      = reb,
        "Assist"   = assists,
        "Steal"    = steals,
        "Turnover" = tovs,
        "FGA"      = fga,
        "FGMade"   = fgMade,
        "FGMissed" = fgMiss,
        "Blocks"   = blocks,
        "Points"   = points
      ),
      efficiency = efficiency
    )
  })
  
  # --- helper: current game row as a data.frame -----------------------------
  current_game_row <- reactive({
    s <- stats()
    data.frame(
      Date       = as.character(input$game_date),
      Tournament = input$tournament,
      Opponent   = input$opponent,
      FT_Made      = s$counts["FT Made"],
      FT_Missed    = s$counts["FT Missed"],
      TwoPM_Made   = s$counts["2PM Made"],
      TwoPM_Missed = s$counts["2PM Missed"],
      ThreePM_Made   = s$counts["3PM Made"],
      ThreePM_Missed = s$counts["3PM Missed"],
      Oreb       = s$counts["Offensive Rebound"],   # NEW: store Oreb
      Dreb       = s$counts["Defensive Rebound"],   # NEW: store Dreb
      Reb        = s$summary["Reb"],
      Assist     = s$summary["Assist"],
      Steal      = s$summary["Steal"],
      Turnover   = s$summary["Turnover"],
      Blocks     = s$summary["Blocks"],
      FGA        = s$summary["FGA"],
      FGMade     = s$summary["FGMade"],
      FGMissed   = s$summary["FGMissed"],
      Points     = s$summary["Points"],
      FT_pct     = s$pct["FT%"],
      TwoP_pct   = s$pct["2P%"],
      ThreeP_pct = s$pct["3P%"],
      FG_pct     = s$pct["FG%"],
      Efficiency = s$efficiency,
      stringsAsFactors = FALSE
    )
  })
  
  # --- save game to CSV ------------------------------------------------------
  observeEvent(input$save_game, {
    # Require basic info before saving
    req(input$game_date, input$tournament, input$opponent)
    
    new_row <- current_game_row()
    
    if (file.exists(games_file)) {
      old <- read.csv(games_file, stringsAsFactors = FALSE)
      all_games <- rbind(old, new_row)
    } else {
      all_games <- new_row
    }
    
    write.csv(all_games, games_file, row.names = FALSE)
  })
  
  # --- load games history for display ---------------------------------------
  games_history <- reactive({
    if (file.exists(games_file)) {
      read.csv(games_file, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  # --- outputs: current game (tab 1) ----------------------------------------
  output$count_table <- renderTable({
    s <- stats()
    data.frame(
      Actions = names(s$counts),
      Counts  = as.integer(s$counts),
      row.names = NULL
    )
  })
  
  output$pct_table <- renderTable({
    s <- stats()
    pct <- s$pct
    data.frame(
      Stat    = names(pct),
      Percent = sprintf("%.2f", pct),
      row.names = NULL
    )
  })
  
  output$summary_table <- renderTable({
    s <- stats()
    data.frame(
      Stat  = names(s$summary),
      Value = as.integer(s$summary),
      row.names = NULL
    )
  })
  
  output$eff_out <- renderText({
    s <- stats()
    paste("Efficiency:", s$efficiency)
  })
  
  # --- outputs: game selector + per-game stats (tab 2) ----------------------
  
  # Dropdown of games
  output$game_select_ui <- renderUI({
    gh <- games_history()
    if (is.null(gh) || nrow(gh) == 0) {
      helpText("No games saved yet.")
    } else {
      choices <- setNames(
        object = seq_len(nrow(gh)),
        nm = paste0(gh$Date, " - ", gh$Tournament, " vs ", gh$Opponent)
      )
      selectInput("selected_game", "Select a game:", choices = choices)
    }
  })
  
  # Row corresponding to selected game
  selected_game_row <- reactive({
    gh <- games_history()
    req(gh, nrow(gh) > 0, input$selected_game)
    gh[as.integer(input$selected_game), , drop = FALSE]
  })
  
  # Convert selected row back into the same "stats"-like structure
  selected_stats <- reactive({
    row <- selected_game_row()
    
    counts <- c(
      "FT Made"           = row$FT_Made,
      "FT Missed"         = row$FT_Missed,
      "2PM Made"          = row$TwoPM_Made,
      "2PM Missed"        = row$TwoPM_Missed,
      "3PM Made"          = row$ThreePM_Made,
      "3PM Missed"        = row$ThreePM_Missed,
      "Turnover"          = row$Turnover,
      "Steal"             = row$Steal,
      "Offensive Rebound" = row$Oreb,
      "Defensive Rebound" = row$Dreb,
      "Assist"            = row$Assist,
      "Blocks"            = row$Blocks
    )
    
    pct <- c(
      "FT%" = row$FT_pct,
      "2P%" = row$TwoP_pct,
      "3P%" = row$ThreeP_pct,
      "FG%" = row$FG_pct
    )
    
    summary <- c(
      "Reb"      = row$Reb,
      "Assist"   = row$Assist,
      "Steal"    = row$Steal,
      "Turnover" = row$Turnover,
      "FGA"      = row$FGA,
      "FGMade"   = row$FGMade,
      "FGMissed" = row$FGMissed,
      "Blocks"   = row$Blocks,
      "Points"   = row$Points
    )
    
    list(
      counts = counts,
      pct = pct,
      summary = summary,
      efficiency = row$Efficiency
    )
  })
  
  # Title above history stats
  output$selected_game_title <- renderText({
    gh_row <- selected_game_row()
    paste0(gh_row$Date, " - ", gh_row$Tournament, " vs ", gh_row$Opponent)
  })
  
  # Tables for selected game (same format as current game)
  output$count_table_hist <- renderTable({
    s <- selected_stats()
    data.frame(
      Actions = names(s$counts),
      Counts  = as.integer(s$counts),
      row.names = NULL
    )
  })
  
  output$pct_table_hist <- renderTable({
    s <- selected_stats()
    pct <- s$pct
    data.frame(
      Stat    = names(pct),
      Percent = sprintf("%.2f", pct),
      row.names = NULL
    )
  })
  
  output$summary_table_hist <- renderTable({
    s <- selected_stats()
    data.frame(
      Stat  = names(s$summary),
      Value = as.integer(s$summary),
      row.names = NULL
    )
  })
  
  output$eff_out_hist <- renderText({
    s <- selected_stats()
    paste("Efficiency:", s$efficiency)
  })
  
  # Raw history table at bottom of Game History tab
  output$games_history <- renderTable({
    games_history()
  })
}