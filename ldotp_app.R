# Required packages
require(data.table)
require(tidyverse)
require(stringr)
require(formattable)
require(googledrive)
require(googlesheets4)
require(gargle)
require(reactable)
require(htmltools)

# system git IDs
system('git config user.email "richardplank@hotmail.com"')
system('git config user.name "richardplank"')

# De-authorize public sheet
gs4_deauth()

# sheet URLs
sheet_url <- "https://docs.google.com/spreadsheets/d/1eR_PDGFaZDEUcpilFB_qXmDxUlrd9bosxLUkXIePIPc/edit?gid=1305347185#gid=1305347185"

## Define functions

# --- 1. Scoring Function ---
calculate_points <- function(pred, home_real, away_real) {
  # Fix: If goals haven't been entered yet (NA), return 0 points immediately
  if (is.na(home_real) || is.na(away_real)) {
    return(0)
  }
  
  # Split the "2 - 1" string into numeric values
  p <- as.numeric(unlist(strsplit(as.character(pred), " - ")))
  
  # Safety check: if prediction is missing, return 0
  if (length(p) < 2 || any(is.na(p))) {
    return(0)
  }
  
  p_h <- p[1]
  p_a <- p[2]
  
  # Logic: 3 for exact score, 1 for correct result
  if (p_h == home_real && p_a == away_real) {
    return(3)
  } else if (sign(p_h - p_a) == sign(home_real - away_real)) {
    return(1)
  } else {
    return(0)
  }
}

# --- 2. Enhanced Standings Function with Tie-Breakers ---
get_standings <- function(table_base, preds, fixtures) {
  
  # A. Calculate Weekly Points and track if they were a "No-Show"
  weekly_scores <- preds |>
    inner_join(fixtures, by = "FixNum") |>
    rowwise() |>
    mutate(Pts = calculate_points(Pred, HomeGoals, AwayGoals)) |>
    group_by(Div, Predder) |>
    summarise(
      WeeklyPts = sum(Pts, na.rm = TRUE), 
      # TRUE if every prediction for this person was NA
      Missing = all(is.na(Pred)), 
      .groups = "drop"
    )
  
  # B. GD Logic Calculation
  gd_logic <- weekly_scores |>
    group_by(Div) |>
    mutate(
      IsMax = WeeklyPts == max(WeeklyPts),
      # Lowest score among everyone in the division
      IsMin = WeeklyPts == min(WeeklyPts)
    ) |>
    ungroup() |>
    mutate(
      GD_Change = case_when(
        IsMax ~ 1,
        (IsMin | Missing) ~ -1,
        TRUE ~ 0
      ),
      Won_Change = if_else(IsMax, 1, 0)
    )
  
  # C. Final Calculation & Sorting
  final_table <- table_base |>
    inner_join(gd_logic, by = c("Div", "Predder")) |>
    mutate(
      LiveTot = TotPoints + WeeklyPts,
      LiveGD  = GD + GD_Change,
      LiveWon = Won + Won_Change
    ) |>
    group_by(Div) |>
    arrange(desc(LiveTot), desc(LiveGD), desc(LiveWon), Predder) |>
    mutate(LiveRank = row_number()) |>
    ungroup()
  
  # Return a list so you can inspect the logic parts
  return(list(
    table = final_table,
    weekly = weekly_scores,
    logic = gd_logic
  ))
}

# --- 3. Change Detection with Color & Alphabetical Check ---
check_major_changes <- function(live_st, scenario_st) {
  
  # 1. Join the two states
  comp <- live_st |>
    select(Div, Predder, OldRank = LiveRank, OldTot = LiveTot, OldGD = LiveGD, OldWon = LiveWon) |>
    inner_join(
      scenario_st |> select(Predder, NewRank = LiveRank, NewTot = LiveTot, NewGD = LiveGD, NewWon = LiveWon),  
      by = "Predder"
    )
  
  # 2. Internal Function to identify exactly WHY a rank swap happened
  get_tie_suffix <- function(p_div, p_rank, scenario_data, live_data) {
    new_holder <- scenario_data[scenario_data$Div == p_div & scenario_data$LiveRank == p_rank, ]
    old_holder <- live_data[live_data$Div == p_div & live_data$LiveRank == p_rank, ]
    
    if(nrow(new_holder) == 0 || nrow(old_holder) == 0) return("")
    
    challenger <- new_holder
    defender   <- scenario_data[scenario_data$Div == p_div & scenario_data$LiveRank == (p_rank + 1), ]
    
    if(nrow(defender) == 0) return("")
    
    # Sequential Tie-break check
    if (challenger$LiveTot == defender$LiveTot) {
      if (challenger$LiveGD == defender$LiveGD) {
        if (challenger$LiveWon == defender$LiveWon) {
          return(" (only on alphabetical order)")
        } else {
          return(" (on Won)")
        }
      } else {
        return(" (on GD)")
      }
    }
    return("")
  } # End of get_tie_suffix
  
  # 3. Detect Changes and Apply Formatting
  changes <- comp |>
    rowwise() |>
    mutate(
      # Determine which rank to investigate for a tie
      suffix = case_when(
        NewRank < OldRank ~ get_tie_suffix(Div, NewRank, scenario_st, live_st),
        NewRank > OldRank ~ get_tie_suffix(Div, OldRank, scenario_st, live_st),
        TRUE ~ ""
      ),
      change_html = case_when(
        OldRank > 1 & NewRank == 1 ~ paste0("<span class='up'>", Predder, " is Champion!", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        OldRank == 1 & NewRank > 1 ~ paste0("<span class='down'>", Predder, " off the top", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        
        # Div 2 Promotion
        (Div == 2 & OldRank > 3 & NewRank <= 3) ~ paste0("<span class='up'>", Predder, " goes up", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        (Div == 2 & OldRank <= 3 & NewRank > 3) ~ paste0("<span class='down'>", Predder, " stays down", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        
        # Div 3 Promotion
        (Div == 3 & OldRank > 4 & NewRank <= 4) ~ paste0("<span class='up'>", Predder, " goes up", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        (Div == 3 & OldRank <= 4 & NewRank > 4) ~ paste0("<span class='down'>", Predder, " stays down", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        
        # Div 4 Promotion
        (Div == 4 & OldRank > 5 & NewRank <= 5) ~ paste0("<span class='up'>", Predder, " goes up", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        (Div == 4 & OldRank <= 5 & NewRank > 5) ~ paste0("<span class='down'>", Predder, " stays down", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        
        # Relegation
        (Div <= 3 & OldRank < 15 & NewRank >= 15) ~ paste0("<span class='down'>", Predder, " goes down", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        (Div <= 3 & OldRank >= 15 & NewRank < 15) ~ paste0("<span class='up'>", Predder, " stays up", if(suffix != "") paste0("<br><span class='suffix'>", suffix, "</span>") else "",         "</span>"),
        TRUE ~ NA_character_
      )
    ) |>
    ungroup() |>
    filter(!is.na(change_html)) |>
    pull(change_html)
  
  if (length(changes) == 0) return("-") else return(paste(changes, collapse = "<br>"))
}

# Define the page palette
my_theme <- reactableTheme(
  backgroundColor = "#1e1f21",       # Dark charcoal background
  borderColor = "#f0f0f0",           # Off-white lines/borders
  stripedColor = "#2a2b2d",         # Slightly lighter dark for rows
  headerStyle = list(
    backgroundColor = "#1e1f21",
    color = "#ffdc55",               # Mustard yellow for column headers
    borderBottom = "2px solid #f0f0f0"
  ),
  cellStyle = list(
    color = "#f0f0f0",               # Off-white text for body
    borderBottom = "1px solid #f0f0f0"
  )
)

while(TRUE){
#for(i in 1:1){
  message(paste("Updating at", Sys.time()))
  
  fixture_data <- read_sheet(sheet_url, sheet = "Fixtures")
  predictions_data <- read_sheet(sheet_url, sheet = "Preds", range = "Preds!B:F")
  table_data <- read_sheet(sheet_url, sheet = "PenultimateTable")
  
  division_reports <- list()
  
  # 1. Get the "Live" state
  live_full_data <- get_standings(table_data, predictions_data, fixture_data)
  live_standings <- live_full_data$table 
  
  for (d in 1:4) {
    div_live <- filter(live_standings, Div == d)
    div_report <- data.frame()
    
    for (i in 1:nrow(fixture_data)) {
      curr_fix <- fixture_data[i, ]
      status <- curr_fix$Status  # TKO, Live, or FT
      
      # Determine text for the side columns
      if (status == "FT") {
        h1_text <- "<span style='opacity:0.5; font-size:10px;'>FULL TIME</span>"
        a1_text <- "<span style='opacity:0.5; font-size:10px;'>FULL TIME</span>"
        fix_display <- paste0(curr_fix$HomeTeam, " ", curr_fix$HomeGoals, "-", curr_fix$AwayGoals, " ", curr_fix$AwayTeam)
        
      } else if (status == "TKO") {
        h1_text <- "-"
        a1_text <- "-"
        # Show "v" instead of "0-0" for games yet to start
        fix_display <- paste0(curr_fix$HomeTeam, " v ", curr_fix$AwayTeam)
        
      } else {
        # LIVE LOGIC: Calculate scenarios as normal
        fix_h1 <- fixture_data
        fix_h1$HomeGoals[i] <- fix_h1$HomeGoals[i] + 1
        st_h1  <- filter(get_standings(table_data, predictions_data, fix_h1)$table, Div == d)
        
        fix_a1 <- fixture_data
        fix_a1$AwayGoals[i] <- fix_a1$AwayGoals[i] + 1
        st_a1  <- filter(get_standings(table_data, predictions_data, fix_a1)$table, Div == d)
        
        h1_text <- check_major_changes(div_live, st_h1)
        a1_text <- check_major_changes(div_live, st_a1)
        fix_display <- paste0(curr_fix$HomeTeam, " ", curr_fix$HomeGoals, "-", curr_fix$AwayGoals, " ", curr_fix$AwayTeam)
      }
      
      div_report <- rbind(div_report, data.frame(
        `Home +1 Change` = h1_text,
        Fixture = fix_display,
        `Away +1 Change` = a1_text,
        Status = status, # Kept for row styling
        check.names = FALSE
      ))
    }
    division_reports[[paste0("Div", d)]] <- div_report
  }
  
  # --- Updated Reactable Function with Row Styling ---
  create_scenario_table <- function(data) {
    reactable(
      data,
      pagination = FALSE,
      highlight = TRUE,
      theme = my_theme, 
      # Logic to dim finished games
      rowStyle = function(index) {
        if (data$Status[index] == "FT") {
          list(background = "#141415", opacity = 0.7) 
        }
      },
      columns = list(
        Status = colDef(show = FALSE), # Hide the status helper column
        `Home +1 Change` = colDef(
          html = TRUE, 
          name = "If Home Score..",
          align = "left",
          minWidth = 110,
          style = list(background = "inherit", whiteSpace = "nowrap", fontSize = "12px")
        ),
        Fixture = colDef(
          name = "Score", 
          align = "center",
          minWidth = 85,
          maxWidth = 100, 
          style = list(
            background = "#252628", color = "#ffffff", fontWeight = "bold", 
            fontSize = "12px", whiteSpace = "nowrap",
            borderLeft = "1px solid #f0f0f0", borderRight = "1px solid #f0f0f0"
          )
        ),
        `Away +1 Change` = colDef(
          html = TRUE, 
          name = "If Away Score..", 
          align = "right",
          minWidth = 110,
          style = list(background = "inherit", whiteSpace = "nowrap", fontSize = "12px")
        )
      )
    )
  }
  # --- 3. Build the HTML Page Structure ---
  # We use tags$div and tags$h2 to create a clean, modern layout
  page <- tags$html(
    tags$head(
      tags$link(href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap", rel="stylesheet"),
      tags$meta(attributes = list("http-equiv" = "refresh", "content" = "60")),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$title("Live Prediction Scenarios"),
      tags$style(HTML("
          /* Desktop / General Styles */
          body { 
            font-family: 'Montserrat', sans-serif; 
            background-color: #1e1f21; 
            color: #f0f0f0; 
            padding: 20px; 
          }
          .container { max-width: 900px; margin: auto; }
          
          h1 { 
            text-align: center; 
            color: #ffdc55; 
            font-weight: 800; 
            text-transform: uppercase;
            letter-spacing: 1px;
            font-size: 2em;
          }
          
          .footnote {
            text-align: center;
            color: #ffdc55;
            font-size: 0.85em;
            font-weight: 600;
            margin-top: 30px;
            padding: 20px;
            letter-spacing: 1px;
            opacity: 0.9;
          }
          .footnote a { color: #ffdc55; text-decoration: underline; }
          
          h2 { 
            color: #ffdc55; 
            border-left: 5px solid #ffdc55; 
            padding-left: 15px; 
            text-transform: uppercase;
            font-size: 1.5em;
          }
        
          .up { 
            color: #afffba; 
            font-weight: bold; 
            display: block; 
            line-height: 1.0;  /* Reduced from 1.1 */
            margin-bottom: 0px; /* Removed the 5px gap */
          }
          .down { 
            color: #ffbaba; 
            font-weight: bold; 
            display: block; 
            line-height: 1.0;  /* Reduced from 1.1 */
            margin-bottom: 0px; /* Removed the 5px gap */
          }
          .suffix { 
            display: block; 
            font-size: 0.75em; 
            font-weight: 400; 
            font-style: italic; 
            opacity: 0.7; 
            line-height: 1.0; 
            margin-top: 2px;   /* Small controlled gap just for the suffix */
          }
        
          /* Mobile Styles (Portrait Mode) */
          @media (max-width: 600px) {
            body { padding: 10px; }
            .container { width: 100%; }
            h1 { font-size: 1.6em; }
            h2 { font-size: 1.3em; padding-left: 10px; }
            
            /* INCREASED font size for mobile readability */
            .rt-td { 
              padding: 6px 0px !important; /* Taller rows for easier reading */
              font-size: 12px;   /* Larger text */
            }
            .rt-th {
              font-size: 11px !important;
            }
          }
    "))
    ),
    tags$body(
      tags$div(class = "container",
               # Title and Timestamp
               tags$h1("Last Day of The Preds"),
               tags$p(class = "update-time", 
                      style = "text-align: center; font-size: 0.9em; opacity: 0.8;", 
                      paste("Last Updated:", format(Sys.time(), "%H:%M %d %b %Y"))),
               
               # Division 1
               tags$div(class = "div-section",
                        tags$h2("Division 1"),
                        create_scenario_table(division_reports$Div1)
               ),
               
               # Division 2
               tags$div(class = "div-section",
                        tags$h2("Division 2"),
                        create_scenario_table(division_reports$Div2)
               ),
               
               # Division 3
               tags$div(class = "div-section",
                        tags$h2("Division 3"),
                        create_scenario_table(division_reports$Div3)
               ),
               
               # Division 4
               tags$div(class = "div-section",
                        tags$h2("Division 4"),
                        create_scenario_table(division_reports$Div4)
               ),
               
               # The Single Footnote at the very bottom
               tags$div(class = "footnote",
                        style = "text-align: center; color: #ffdc55; font-size: 0.8em; font-weight: 600; margin-top: 40px; padding: 20px; letter-spacing: 1px;",
                        "DON'T FORGET ... TO ALWAYS REFER TO ",
                        tags$a(href = "https://preds.co.uk", style = "color: #ffdc55; text-decoration: underline;", "PREDS.CO.UK"),
                        " FOR OFFICIAL RANKINGS"
               )
      )
    )
  )
  
  # --- 4. Save to File ---
  htmltools::save_html(page, file = "live.html", libdir = "lib")

  try({
    # The "." tells Git to look at EVERYTHING in the folder (html and lib)
    system("git add .")

    # Commit only if there are changes (avoids errors if nothing changed)
    system('git commit -m "Auto-update scores" --no-verify')

    # Push using our authenticated remote
    system("git push origin main --quiet")
  }, silent = FALSE)

  message(paste("Successfully updated at", Sys.time()))

  # check the sheet every 60 seconds
  Sys.sleep(90)

}


# eyeballing
# div1 <- division_reports$Div1
# div2 <- division_reports$Div2
# div3 <- division_reports$Div3
# div4 <- division_reports$Div4

