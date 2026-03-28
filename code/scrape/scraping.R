library(rvest)
library(lubridate)
library(fs)
library(assertthat)
library(httr)
library(tictoc)
library(dplyr)
library(chromote)
library(R.utils)
library(openxlsx)
options(chromote.timeout = 30)
extract_data <- function(league,timezone,b,runstats,debug=FALSE){
  local_date <- as.Date(as.POSIXlt(Sys.time(), tz = timezone))
  link <- paste0("https://www.betexplorer.com", league, "fixtures")
  headers <- add_headers(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/58.0.3029.110 Safari/537.3"
  )
  
  page <- fetch_league_page(link, headers, b)
  
  odds_section <- html_nodes(page, ".table-main__odds")
  
  buttons <- html_nodes(odds_section, "button")
  
  # Step 4: Extract the actual odds (text content of buttons)
  odds <- html_attr(buttons, "data-odd")
  
  if(length(odds)>0){
    dates_section <- html_nodes(page, ".table-main__datetime")
    dates <- html_text(dates_section)
    if(length(dates)>0){
      print("Matches to scrape")
      for(i in 1:length(dates)){
        if(nchar(dates[i])==1){
          dates[i] <- dates[i-1]
        }
      }
      normalize_dates <- function(dates) {
      today <- local_date
      
      # Loop through each entry
      adjusted_dates <- sapply(dates, function(entry) {
        if (grepl("^Today\\s", entry, ignore.case = TRUE)) {
          time_part <- sub("^Today\\s+", "", entry, ignore.case = TRUE)
          date_part <- format(today, "%d.%m.")
          paste0(date_part, " ", time_part)
          
        } else if (grepl("^Tomorrow\\s", entry, ignore.case = TRUE)) {
          time_part <- sub("^Tomorrow\\s+", "", entry, ignore.case = TRUE)
          date_part <- format(today + 1, "%d.%m.")
          paste0(date_part, " ", time_part)
          
        } else {
          # Already in desired format
          entry
        }
      }, USE.NAMES = FALSE)
      
      return(adjusted_dates)
    }
    dates <- normalize_dates(dates)
    
    matches_section <- html_nodes(page,".h-text-left")
    
    match_links <- html_nodes(page, 'a.in-match') %>% html_text(trim=TRUE)
    
    a_node <- html_node(matches_section, "a.in-match")
    team_names_mat <- matrix(nrow=length(dates),ncol=15)
    j <- 1
    for(i in 1:length(a_node)){
      if(length(a_node[[i]])>0){
        team_names <- html_nodes(a_node[[i]],"span") %>% html_text(trim = TRUE)
        link  <- paste0("https://www.betexplorer.com",html_attr(a_node[[i]],"href"))
        team_names_mat[j,1] <- team_names[1]
        team_names_mat[j,2] <- team_names[2]
        team_names_mat[j,3] <- link
        
        j <- j+1
      }
    }
    oddsmatuse <- matrix(odds,nrow=length(odds)/3,ncol=3,byrow=TRUE)
    
    waittimes <- c(4,8,12)
    
    base_year <- format(Sys.Date(), "%Y")
    parsed_dates <- as.POSIXct(
      ifelse(grepl("\\d{4}", dates), 
             dates, 
             paste0(dates, base_year)),
      format = ifelse(grepl("\\d{4}", dates),
                      "%d.%m.%Y %H:%M",  # has year
                      "%d.%m. %H:%M%Y")  # no year
    )
    ordered_indices <- order(parsed_dates)
    x <- nrow(oddsmatuse)
    first_x_indices <- ordered_indices[1:x]
    
    matchmatuse <- team_names_mat[first_x_indices, ]
    matchmatuse <- as.matrix(matchmatuse)
    if(ncol(matchmatuse)==1){
      matchmatuse <- t(as.matrix(matchmatuse))
    }
    for (i in seq_len(nrow(matchmatuse))) {
      
      success <- FALSE
      attempts <- 0
      
      while (!success && attempts < 3) {  # Retry up to 3 times
        attempts <- attempts + 1
        tryCatch({
          Sys.sleep(abs(waittimes[attempts]+rnorm(1,sd=0.5)))  # small delay between requests
          print(paste0("Scraping game ",matchmatuse[i,1],"-",matchmatuse[i,2]))
          runstats$scrapecount <- runstats$scrapecount+1
          if (runstats$consecutive_hits > 10) {
            cat("Cooling down after success streak\n")
            Sys.sleep(runif(1, 10, 20))
            runstats$consecutive_hits <- 0
          }
          if(runstats$scrapecount%%20==0||runstats$consecutive_fails>=5){
            cat("🔄 Resetting browser session\n")
            b$close()
            Sys.sleep(runif(1, 5, 10))
            b <- ChromoteSession$new()
            
            b$Network$setUserAgentOverride(
              userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
            )
          }
          if(debug==FALSE){
            matchdata <- extract_match_info(matchmatuse[i,3],b)
          }else{
            matchdata <-  extract_match_info_debug(matchmatuse[i,3],b)
          }
          
          if(matchdata$status=="finished"){
            print("Game has finished")
            success <- TRUE
          }
          if(matchdata$status=="oddsincomplete"){
            print("Some odds are missing")
            success <- TRUE
          }
          
          matchdata <- matchdata$return
          odds <- matchdata$odds
          sd <- matchdata$sd
          nleaders <- matchdata$nleaders
          leaders <- matchdata$leaders
          matchmatuse[i,4:6] <- odds
          matchmatuse[i,7:9] <- sd
          matchmatuse[i,10:12] <- nleaders
          matchmatuse[i,13:15] <- leaders
          success <- TRUE
        }, error = function(e) {
          cat(paste0("⚠️ Error in game ", i, ": ", e$message, "\n"))
          cat("⏳ Retrying this game in 10 seconds...\n")
          Sys.sleep(10)
        })
      }
      
      if (!success) {
        runstats$games_timed_out <- runstats$games_timed_out + 1
        cat(paste0("❌ Skipping game ", i, " after 5 failed attempts.\n"))
        runstats$consecutive_fails <- runstats$consecutive_fails+1
        runstats$consecutive_hits <- 0
      }
      if(success){
        runstats$consecutive_fails <- 0
        runstats$consecutive_hits <- runstats$consecutive_hits+1
      }
      if (runif(1) < 0.1) {
        print("Redirecting to main page")
        b$Page$navigate("https://www.betexplorer.com/")
        Sys.sleep(runif(1, 3, 6))
      }
    }
    
    ids <- which(!is.na(matchmatuse[,4]))
    matchmatuse <- matchmatuse[ids,]
    if(NROW(matchmatuse)>0){
      datesuse <- dates[first_x_indices]
      day_month <- sub("(\\d{2}\\.\\d{2})\\..*", "\\1", datesuse)
      # Explanation:
      # (\\d{2}\\.\\d{2}) captures "dd.mm"
      # \\..* matches from the dot after month until end (including time), and replaces it with just the day-month
      
      # Step 2: Get today's date and extract current year, month
      today <- Sys.Date()
      current_year <- format(today, "%Y")
      current_month <- as.numeric(format(today, "%m"))
      
      # Step 3: Parse extracted day and month into date object (no year yet)
      dates_parsed <- as.Date(paste0(day_month, ".", current_year), format = "%d.%m.%Y")
      
      # Step 4: Adjust year for Jan/Feb matches when today is Nov/Dec
      # Define a function to adjust year accordingly
      adjust_year <- function(date_vec, today) {
        yr <- as.numeric(format(today, "%Y"))
        mo_today <- as.numeric(format(today, "%m"))
        
        # Extract month of each date
        mo_date <- as.numeric(format(date_vec, "%m"))
        
        # Logical vector for dates needing year increment
        add_year <- mo_today %in% c(11,12) & mo_date %in% c(1,2)
        
        # Convert to POSIXlt for easier manipulation
        date_lt <- as.POSIXlt(date_vec)
        
        # Add 1 to the year component for those dates
        date_lt$year[add_year] <- date_lt$year[add_year] + 1
        
        # Convert back to Date
        adjusted_dates <- as.Date(date_lt)
        
        return(adjusted_dates)
      }
      
      # Step 5: Apply adjustment
      final_dates <- adjust_year(dates_parsed, today)
      
      dif <- final_dates-local_date
      final_dates <- final_dates[ids]
      dif <- dif[ids]
      
      matchmatuse_mat <- matrix(matchmatuse, ncol =15)
      
      matuse <- data.frame(Date = final_dates, dif = dif, matchmatuse = matchmatuse_mat, stringsAsFactors = FALSE)
      matuse$id <- paste0(matuse$Date,"_",matuse$matchmatuse.1,"_",matuse$matchmatuse.2)
      matuse <- matuse[matuse$dif<21,]
      return(list(result=matuse,browser=b,runstats=runstats))
    }
    return(list(result=matchmatuse,browser=b,runstats=runstats))
    }
  }
}

extract_match_info <- function(link, b) {
  b$Page$navigate(link)
  Sys.sleep(10)
  
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  odds_nodes <- html_nodes(page, "a.archiveOdds")
  bookie_names <- html_nodes(page, "a.in-bookmaker-logo-link")
  bookie_names <- html_text(bookie_names,trim=TRUE)
  status <- page %>%
    html_element("#js-eventstage") %>%
    html_text()
  status <- ifelse(is.na(status),"not finished",status)
  
  if(status=="Finished"){
    return(list(status="finished",return=NA))
  }
  
  if(length(odds_nodes)==0){
    stop("No odds returned")
  }
  
  if(length(odds_nodes)%%3!=0){
    return(list(status="oddsincomplete",return=NA))
  }

  
  text <- odds_nodes %>% html_text(trim = TRUE)
  home <- as.numeric(text[seq(1, length(text), by = 3)])
  draw <- as.numeric(text[seq(2, length(text), by = 3)])
  away <- as.numeric(text[seq(3, length(text), by = 3)])
  homebest <- max(home)
  drawbest <- max(draw)
  awaybest <- max(away)
  homen <- length(home[home==max(home)])/2
  drawn <- length(draw[draw==max(draw)])/2
  awayn <- length(away[away==max(away)])/2
  
  homeleader <- ifelse(homen==1,bookie_names[which(home==max(home))],NA)
  drawleader <- ifelse(drawn==1,bookie_names[which(draw==max(draw))],NA)
  awayleader <- ifelse(awayn==1,bookie_names[which(away==max(away))],NA)
  return(list(status="succeeded",return=list(odds=c(max(home),max(draw),max(away)),sd=c(sd(home),sd(draw),sd(away)),nleaders=c(homen,drawn,awayn),leaders=c(homeleader,drawleader,awayleader))))
  #return(c(sd(home), sd(draw), sd(away)))
}

extract_leagues <- function(){
  base_url <- "https://www.betexplorer.com/football/"
  page <- read_html(base_url)
  country_nodes <- html_nodes(page, "a")
  country_links <- html_attr(country_nodes, "href")
  country_links <- country_links[grepl("^/football/[^/]+/$", country_links)]
  country_links <- country_links[9:length(country_links)]
  country_urls <- paste0("https://www.betexplorer.com", country_links)
  
  league_urls <- c()
  
  for (url in country_urls) {
    Sys.sleep(1)  # Be respectful with delays
    cat("Scraping:", url, "\n")
    
    country_page <- tryCatch(read_html(url), error = function(e) NULL)
    
    if (!is.null(country_page)) {
      league_nodes <- html_nodes(country_page, "a")
      league_links <- html_attr(league_nodes, "href")
      
      # Extract only valid league URLs (2-level deep, like /football/england/premier-league/)
      league_links <- league_links[grepl("^/football/[^/]+/[^/]+/$", league_links)]
      
      league_urls <- c(league_urls, league_links)
    }
  }
  
  country_tags <- sub("^/football/([^/]+)/.*$", "\\1", league_urls)
  
  # Combine into data.frame for easier handling
  df <- data.frame(country = country_tags, url = league_urls, stringsAsFactors = FALSE)
  
  # Keep top 2 leagues per country
  library(dplyr)
  top_leagues <- df %>%
    group_by(country) %>%
    slice_head(n = 2) %>%
    ungroup()
  
  # Extract final URLs
  final_league_urls <- top_leagues$url
  filtered_urls <- final_league_urls[!grepl("\\d{4}", final_league_urls)]
  return(final_league_urls)
}

write_league <- function(league,timezone,b,version,runstats,debug=FALSE){
  result <- extract_data(league,timezone,b,runstats,debug)
  data <- result$result
  if(nrow(data)>0&&length(data)>0){
    b <- result$browser
    runstats <- result$runstats
    runstats$leagues_scraped <- runstats$leagues_scraped+1
    runstats$games_scraped_total <- runstats$games_scraped_total+nrow(data)
    trimmed <- sub("^/football/", "", league)
    trimmed <- sub("/$", "", trimmed)
    
    # Split by "/"
    parts <- strsplit(trimmed, "/")[[1]]
    
    # Capitalize the first letter of the first part
    parts[1] <- paste0(toupper(substring(parts[1], 1, 1)), substring(parts[1], 2))
    
    # Join with underscore
    name <- paste(parts, collapse = "_")
    if(file_exists(paste0("data/new/",version,"/",name,".csv"))){
      fulldata <- read.csv(paste0("data/new/",version,"/",name,".csv"))
      numrow <- nrow(fulldata)
      numcol <- ncol(fulldata)
      for(i in 1:nrow(data)){
        row <- data[i,]
        if(row$id %in% fulldata$id){
          runstats$games_updated_existing <- runstats$games_updated_existing+1
          colname1 <- paste0("home_odds_l",row$dif)
          colname2 <- paste0("draw_odds_l",row$dif)
          colname3 <- paste0("away_odds_l",row$dif)
          colname4 <- paste0("home_sd_l",row$dif)
          colname5 <- paste0("draw_sd_l",row$dif)
          colname6 <- paste0("away_sd_l",row$dif)
          colname7 <- paste0("home_nleaders_l",row$dif)
          colname8 <- paste0("draw_nleaders_l",row$dif)
          colname9 <- paste0("away_nleaders_l",row$dif)
          colname10 <- paste0("home_leader_l",row$dif)
          colname11 <- paste0("draw_leader_l",row$dif)
          colname12 <- paste0("away_leader_l",row$dif)
          newcols <- setdiff(c(colname1,colname2,colname3,colname4,colname5,colname6,colname7,colname8,colname9,colname10,colname11,colname12),colnames(fulldata))
          for(col in newcols){
            fulldata[[col]] <- NA
          }
          fulldata[[colname1]][which(fulldata$id==row$id)] <- row$matchmatuse.4
          fulldata[[colname2]][which(fulldata$id==row$id)] <- row$matchmatuse.5
          fulldata[[colname3]][which(fulldata$id==row$id)] <- row$matchmatuse.6
          fulldata[[colname4]][which(fulldata$id==row$id)] <- row$matchmatuse.7
          fulldata[[colname5]][which(fulldata$id==row$id)] <- row$matchmatuse.8
          fulldata[[colname6]][which(fulldata$id==row$id)] <- row$matchmatuse.9
          fulldata[[colname7]][which(fulldata$id==row$id)] <- row$matchmatuse.10
          fulldata[[colname8]][which(fulldata$id==row$id)] <- row$matchmatuse.11
          fulldata[[colname9]][which(fulldata$id==row$id)] <- row$matchmatuse.12
          fulldata[[colname10]][which(fulldata$id==row$id)] <- row$matchmatuse.13
          fulldata[[colname11]][which(fulldata$id==row$id)] <- row$matchmatuse.14
          fulldata[[colname12]][which(fulldata$id==row$id)] <- row$matchmatuse.15
        }else{
          runstats$games_new_added <- runstats$games_new_added+1
          new_data <- as.data.frame(lapply(fulldata, function(x) rep(NA,1)))
          new_data$id <- row$id
          id <- data$id[i]
          colname1 <- paste0("home_odds_l",row$dif)
          colname2 <- paste0("draw_odds_l",row$dif)
          colname3 <- paste0("away_odds_l",row$dif)
          colname4 <- paste0("home_sd_l",row$dif)
          colname5 <- paste0("draw_sd_l",row$dif)
          colname6 <- paste0("away_sd_l",row$dif)
          colname7 <- paste0("home_nleaders_l",row$dif)
          colname8 <- paste0("draw_nleaders_l",row$dif)
          colname9 <- paste0("away_nleaders_l",row$dif)
          colname10 <- paste0("home_leader_l",row$dif)
          colname11 <- paste0("draw_leader_l",row$dif)
          colname12 <- paste0("away_leader_l",row$dif)
          newcols <- setdiff(c(colname1,colname2,colname3,colname4,colname5,colname6,colname7,colname8,colname9,colname10,colname11,colname12),colnames(fulldata))
          for(col in newcols){
            fulldata[[col]] <- NA
            new_data[[col]] <- NA
          }
          new_data[[colname1]][which(new_data$id==row$id)] <- row$matchmatuse.4
          new_data[[colname2]][which(new_data$id==row$id)] <- row$matchmatuse.5
          new_data[[colname3]][which(new_data$id==row$id)] <- row$matchmatuse.6
          new_data[[colname4]][which(new_data$id==row$id)] <- row$matchmatuse.7
          new_data[[colname5]][which(new_data$id==row$id)] <- row$matchmatuse.8
          new_data[[colname6]][which(new_data$id==row$id)] <- row$matchmatuse.9
          new_data[[colname7]][which(new_data$id==row$id)] <- row$matchmatuse.10
          new_data[[colname8]][which(new_data$id==row$id)] <- row$matchmatuse.11
          new_data[[colname9]][which(new_data$id==row$id)] <- row$matchmatuse.12
          new_data[[colname10]][which(new_data$id==row$id)] <- row$matchmatuse.13
          new_data[[colname11]][which(new_data$id==row$id)] <- row$matchmatuse.14
          new_data[[colname12]][which(new_data$id==row$id)] <- row$matchmatuse.15
          fulldata <- rbind(fulldata,new_data)
        }
      }
      errortext <- run_fatal_checks(fulldata)
      if(length(errortext)>1){
        runstats$fatal_fails <- runstats$fatal_fails+1
        runstats$error_log <- c(runstats$error_log,errortext)
      }
      numrow2 <- nrow(fulldata)
      if(numrow>numrow2){
        errortext <- paste0("Number of rows has declined for league ",name)
        runstats$fatal_fails <- runstats$fatal_fails+1
        runstats$error_log <- c(runstats$error_log,errortext)
      }
      if(length(errortext)==1){
        write.csv(fulldata,paste0("data/new/",version,"/",name,".csv"),row.names=FALSE)
        runstats$filename <- paste0("data/new/",version,"/",name,".csv")
      }
      runstats$total_rows <- runstats$total_rows+nrow(fulldata)
    }else{
      runstats$leagues_added <- runstats$leagues_added+1
      exampledata <- read.csv(paste0("data/new/",version,"/hold.csv"))
      numcol <- ncol(exampledata)
      new_data <- as.data.frame(lapply(exampledata, function(x) rep(NA, nrow(data))))
      new_data$id <- data$id
      for(i in 1:nrow(data)){
        runstats$games_new_added <- runstats$games_new_added+1
        id <- data$id[i]
        row <- data[i,]
        colname1 <- paste0("home_odds_l",row$dif)
        colname2 <- paste0("draw_odds_l",row$dif)
        colname3 <- paste0("away_odds_l",row$dif)
        colname4 <- paste0("home_sd_l",row$dif)
        colname5 <- paste0("draw_sd_l",row$dif)
        colname6 <- paste0("away_sd_l",row$dif)
        colname7 <- paste0("home_nleaders_l",row$dif)
        colname8 <- paste0("draw_nleaders_l",row$dif)
        colname9 <- paste0("away_nleaders_l",row$dif)
        colname10 <- paste0("home_leader_l",row$dif)
        colname11 <- paste0("draw_leader_l",row$dif)
        colname12 <- paste0("away_leader_l",row$dif)
        new_data[[colname1]][i] <- row$matchmatuse.4
        new_data[[colname2]][i] <- row$matchmatuse.5
        new_data[[colname3]][i] <- row$matchmatuse.6
        new_data[[colname4]][i] <- row$matchmatuse.7
        new_data[[colname5]][i] <- row$matchmatuse.8
        new_data[[colname6]][i] <- row$matchmatuse.9
        new_data[[colname7]][i] <- row$matchmatuse.10
        new_data[[colname8]][i] <- row$matchmatuse.11
        new_data[[colname9]][i] <- row$matchmatuse.12
        new_data[[colname10]][i] <- row$matchmatuse.13
        new_data[[colname11]][i] <- row$matchmatuse.14
        new_data[[colname12]][i] <- row$matchmatuse.15
      }
      errortext <- run_fatal_checks(new_data)
      if(length(errortext)>1){
        runstats$fatal_fails <- runstats$fatal_fails+1
        runstats$error_log <- c(runstats$error_log,errortext)
      }
      if(length(errortext)==1){
        write.csv(new_data,paste0("data/new/",version,"/",name,".csv"),row.names=FALSE)
        runstats$filename <- paste0("data/new/",version,"/",name,".csv")
      }
      runstats$total_rows <- runstats$total_rows+nrow(new_data)
    }
  }
  return(list(runstats=runstats,browser=b))
}

add_results <- function(league,b,version,runstats){
  link <- paste0("https://www.betexplorer.com", league, "results")
  headers <- add_headers("User-Agent" = "...")
  
  page <- fetch_league_page(link, headers, b)
  result_section <- html_nodes(page, ".h-text-center")
  results <- html_text(result_section)
  results <- results[!(results %in%  c("1","2","X"))]
  
  if(length(results)>0){
    teamname_section <- html_nodes(page,".h-text-left")
    a_node <- html_node(teamname_section, "a.in-match")
    team_names_mat <- matrix(nrow=length(results),ncol=2)
    j <- 1
    for(i in 1:length(a_node)){
      if(length(a_node[[i]])>0){
        team_names <- html_nodes(a_node[[i]],"span") %>% html_text(trim = TRUE)
        team_names_mat[j,1] <- team_names[1]
        team_names_mat[j,2] <- team_names[2]
        j <- j+1
      }
    }
    
    dates_section <- html_nodes(page, ".h-text-right.h-text-no-wrap")
    dates <- html_text(dates_section)
    
    current_date <- as.Date(format(Sys.time(), tz = "Europe/London"))
    
    dates_standardized <- sapply(dates, function(d) {
      d <- tolower(trimws(d))  # Remove whitespace and make lowercase
      
      if (d == "today") {
        return(format(current_date, "%Y-%m-%d"))
        
      } else if (d == "yesterday") {
        return(format(current_date - 1, "%Y-%m-%d"))
        
      } else if (grepl("^\\d{2}\\.\\d{2}\\.$", d)) {
        # Format like "16.05." — add current year
        parsed <- dmy(paste0(d, year(current_date)))
        return(format(parsed, "%Y-%m-%d"))
        
      } else if (grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", d)) {
        # Format like "30.12.2024"
        parsed <- dmy(d)
        return(format(parsed, "%Y-%m-%d"))
        
      } else {
        # Unknown format — return NA
        return(NA)
      }
    })
    
    resultmat <- data.frame(Date = dates_standardized,  team_names = team_names_mat,  score=results)
    resultmat$id <- paste0(resultmat$Date,"_",resultmat$team_names.1,"_",resultmat$team_names.2)
    
    for(rown in 1:nrow(resultmat)){
      if(substr(resultmat$score[rown], nchar(resultmat$score[rown]) - 2, nchar(resultmat$score[rown])) == "AfP"){
        team1 <- resultmat$team_names.1[rown]
        team2 <- resultmat$team_names.2[rown]
        prevgame <- resultmat[which(resultmat$team_names.1==team2&resultmat$team_names.2==team1),]
        if(nrow(prevgame)>0){
          resultmat$score[rown] <- prevgame$score
        }else{
          resultmat$score[rown] <- "X"
        }
      }
    }
    
    resultmat$score_result <- with(resultmat, ifelse(
      is.na(score), NA,
      ifelse(
        as.numeric(sub(":.*", "", score)) > as.numeric(sub(".*:", "", score)), 1,
        ifelse(
          as.numeric(sub(":.*", "", score)) < as.numeric(sub(".*:", "", score)), 2,
          "X"
        )
      )
    ))
    
    resultmat <- resultmat[!is.na(resultmat$score_result),]
    
    trimmed <- sub("^/football/", "", league)
    trimmed <- sub("/$", "", trimmed)
    
    # Split by "/"
    parts <- strsplit(trimmed, "/")[[1]]
    
    # Capitalize the first letter of the first part
    parts[1] <- paste0(toupper(substring(parts[1], 1, 1)), substring(parts[1], 2))
    
    # Join with underscore
    name <- paste(parts, collapse = "_")
    if(file_exists(paste0("data/new/",version,"/",name,".csv"))){
      fulldata <- read.csv(paste0("data/new/",version,"/",name,".csv"))
      numcol <- ncol(fulldata)
      idsnoresults <- fulldata$id[is.na(fulldata$result)]
      update <- intersect(idsnoresults,resultmat$id)
      for(useid in update){
        runstats$games_score_added <- runstats$games_score_added+1 
        fulldata$result[fulldata$id==useid] <- resultmat$score_result[resultmat$id==useid]
        if(runif(1)<0.05){
          runstats$game_checks[runstats$game_check_count,] <- c(useid,resultmat$score_result[resultmat$id==useid])
          runstats$game_check_count <- runstats$game_check_count+1
        }
      }
      assert_that(ncol(fulldata)==numcol,msg=paste0("Adding results has changed the number of columns for league ",league))
      write.csv(fulldata,paste0("data/new/",version,"/",name,".csv"),row.names=FALSE)
    }
  }
  return(runstats)
}

fetch_league_page <- function(link, headers, b) {
  # Try httr first
  res <- try(httr::GET(link, headers), silent = TRUE)
  
  if (inherits(res, "try-error") || res$status_code == 429) {
    cat("⚠️ GET failed or rate-limited, falling back to Chromote...\n")
    
    b$Page$navigate(link)
    Sys.sleep(5)
    
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    return(read_html(html))
  }
  
  stop_for_status(res)
  return(read_html(res))
}

loop_over_leagues <- function(v,debug=FALSE,start = 1){
  start_time <- Sys.time()
  runstats <- list("leagues_scraped"=0,"leagues_added"=0,"games_scraped_total"=0,"games_new_added"=0,"games_updated_existing"=0,"games_score_added"=0,"total_rows"=0,"game_checks"=matrix(nrow=0,ncol=2),"game_check_count"=1,"games_timed_out"=0,"filename"="","fatal_fails"=0,"error_log"=c(),"consecutive_hits"=0,"consecutive_fails"=0,"scrapecount"=0)
  leaguelist <- read.csv("data/hold/leaguelist.csv")
  leagues <- leaguelist[, 2]
  timezones <- leaguelist[, 3]
  
  # create ONE Chromote session for the whole run
  b <- init_browser()
  b <- warmup_session(b)
  commit_sha <- Sys.getenv("GITHUB_SHA")
  date <- Sys.Date()
  
  if (!dir.exists(paste0("data/new/",v))) {
    dir.create(paste0("data/new/",v), recursive = TRUE)
    hold <- read.csv("data/new/hold.csv")
    write.csv(hold,paste0("data/new/",v,"/hold.csv"))
  }
  
  for (i in start:nrow(leaguelist)) {
    league <- leagues[i]
    timezone <- timezones[i]
    success <- FALSE
    delay <- 15+rnorm(1,sd=0.5)   # base retry delay
    
    while (!success) {
      cat(paste0("Processing league ", league, " (row ", i, ")\n"))
      
      tryCatch({
        Sys.sleep(5)+rnorm(1,sd=0.5)  # polite pause
        result <- write_league(league, timezone, b,v,runstats,debug)  # pass Chromote session
        runstats <- result$runstats
        b <- result$browser
        runstats <- add_results(league,b,v,runstats)
        success <- TRUE
        delay <- 15  # reset delay on success
      }, error = function(e) {
        if (grepl("429", e$message)) {
          cat("⚠️ Rate limit error: ", e$message, "\n")
          cat("⏳ Waiting ", delay, "s before retrying...\n")
          Sys.sleep(delay + runif(1, 0, 5))
          delay <- min(delay * 2, 300)  # escalate
        } else {
          cat("⚠️ Error: ", e$message, "\n")
          cat("⏳ Retrying in 30s...\n")
          Sys.sleep(30)
        }
      })
    }
    #check_file_structure(runstats$filename,v)
  }
  end_time <- Sys.time()
  time_elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))
  wb <- loadWorkbook("data/log/scraper_log.xlsx")
  logdata <- read.xlsx("data/log/scraper_log.xlsx", sheet = "Summary")
  newrow <- data.frame(
    date = date,
    github_SHA = commit_sha,
    script_version = v,
    time_elapsed = time_elapsed,
    leagues_scraped = runstats$leagues_scraped,
    leagues_added = runstats$leagues_added,
    games_scraped_total = runstats$games_scraped_total,
    games_new_added = runstats$games_new_added,
    games_updated_existing = runstats$games_updated_existing,
    games_score_added = runstats$games_score_added,
    total_rows = runstats$total_rows,
    fatal_fails = runstats$fatal_fails,
    stringsAsFactors = FALSE
  )
  logdata <- rbind(logdata,newrow)
  writeData(wb, sheet = "Summary", logdata, withFilter = FALSE)
  
  gamechecks <- read.xlsx("data/log/scraper_log.xlsx", sheet="Manual_check")
  newdata <- runstats$game_checks
  gamechecks <- rbind(gamechecks,newdata)
  writeData(wb,sheet="Manual_check", gamechecks, withFilter=FALSE)
  
  saveWorkbook(wb, "data/log/scraper_log.xlsx", overwrite = TRUE)
}

write_to_train_test <- function(){
  temptrain <- read.csv("data/new/hold.csv")
  temptrain$league <- 0
  temptrain <- temptrain[,c(1,ncol(temptrain),2:(ncol(temptrain)-1))]
  temptest <- temptrain
    
  leaguelist <- read.csv("data/hold/leaguelist.csv")
  leagues <- leaguelist[, 2]
  timezones <- leaguelist[, 3]
  
  for (i in 1:nrow(leaguelist)) {
    league <- leagues[i]
    trimmed <- sub("^/football/", "", league)
    trimmed <- sub("/$", "", trimmed)
    
    # Split by "/"
    parts <- strsplit(trimmed, "/")[[1]]
    
    # Capitalize the first letter of the first part
    parts[1] <- paste0(toupper(substring(parts[1], 1, 1)), substring(parts[1], 2))
    
    # Join with underscore
    name <- paste(parts, collapse = "_")
    if(file.exists(paste0("data/new/",name,".csv"))){
      tempdata <- read.csv(paste0("data/new/",name,".csv"))
      tempdata$league <- name
      tempdata <- tempdata[,c(1,ncol(tempdata),2:(ncol(tempdata)-1))]
      finished <- tempdata[!is.na(tempdata$result),]
      finished <- finished[,1:ncol(temptrain)]
      ongoing <- tempdata[is.na(tempdata$result),]
      ongoing <- ongoing[,1:ncol(temptrain)]
      temptrain <- rbind(temptrain,finished)
      temptest <- rbind(temptest,ongoing)  
    }
  }
  temptrain <- temptrain[nchar(temptrain$id)>2,]
  temptest <- temptest[nchar(temptest$id)>2,]
  temptrain_formatted <- convert_data_to_model_format(temptrain,return=TRUE,write=FALSE)
  temptest_formatted <- convert_data_to_model_format(temptest,return=TRUE,write=FALSE)
  temptest_formatted <- temptest_formatted %>%
    group_by(id, outcome) %>%
    slice_min(order_by = daysout, n = 1, with_ties = FALSE) %>%
    ungroup()
  temptrain_formatted$saveid <- paste0(temptrain_formatted$id,"-",temptrain_formatted$daysout,"-",temptrain_formatted$outcome)
  train_file <- "data/model/train.rds"
  
  train <- temptrain_formatted
  
  saveRDS(train, file = "data/model/train.rds")
  saveRDS(temptest_formatted,file = "data/model/test.rds")
}

convert_data_to_model_format <- function(rawdata,return=FALSE,write=TRUE){
  allgames <- matrix(ncol=10)
  allgames <- data.frame(allgames)
  colnames(allgames) <- c("id","league","daysout","outcome","odds_history","sd_history","final_result","payoff","odds","ndays")
  for(i in 1:nrow(rawdata)){
    row <- rawdata[i,]
    days <- 21-which(!is.na(row[3:23]))
    league <- row$league
    id  <- row$id
    result  <- row$result
    gamemat <- matrix(ncol=10)
    gamemat <- data.frame(gamemat)
    colnames(gamemat) <- colnames(allgames)
    for(j in days){
      homeodds <- row[1,23-j]
      drawodds <- row[1,44-j]
      awayodds <- row[1,65-j]
      oddsvechome <- c(as.numeric(row[ , 3:(23 - j)]),rep(NA,j))
      oddsvecdraw <- c(as.numeric(row[ , 24:(44 - j)]),rep(NA,j))
      oddsvecaway <- c(as.numeric(row[ , 45:(65 - j)]),rep(NA,j))
      sdvechome <- c(as.numeric(row[ , 67:(87 - j )]),rep(NA,j))
      sdvecdraw <- c(as.numeric(row[ , 88:(108 - j)]),rep(NA,j))
      sdvecaway <- c(as.numeric(row[ , 109:(129 - j)]),rep(NA,j))
      minimat <- matrix(ncol=10,nrow=3)
      minimat  <- data.frame(minimat)
      colnames(minimat) <- colnames(gamemat)
      minimat$id <- rep(id,3)
      minimat$league <- rep(league,3)
      minimat$daysout <- rep(j,3)
      minimat$outcome <- rep(result,3)
      minimat$odds_history <- list(oddsvechome,oddsvecdraw,oddsvecaway)
      minimat$sd_history <- list(sdvechome,sdvecdraw,sdvecaway)
      minimat$final_result <- c("1","2","X")
      homepay <- ifelse(result=="1",homeodds-1,-1)
      drawpay <- ifelse(result=="X",drawodds-1,-1)
      awaypay <- ifelse(result=="2",awayodds-1,-1)
      minimat$payoff <- c(homepay,drawpay,awaypay)
      minimat$odds <- c(homeodds,drawodds,awayodds)
      minimat$ndays <- c(sum(!is.na(oddsvechome)),sum(!is.na(oddsvecdraw)),sum(!is.na(oddsvecaway)))
      gamemat <- rbind(gamemat,minimat)
    }
    allgames  <- rbind(allgames,gamemat)
  }
  allgames <- allgames[!is.na(allgames$id),]
  if(return==TRUE){
    return(allgames)
  }
  if(write==TRUE){
    write.csv(allgames,"data/dump/modeldata.csv")
  }
}

add_new_columns <- function(colnames){
  files <- list.files("data/new", full.names = TRUE)
  for (f in files) {
    df <- read.csv(f)
    
    df <- df %>%
      mutate(across(all_of(new_cols), ~ NA))
    
    write.csv(df, f, row.names = FALSE)
  }
}

#Write second level checks for after write and make sure some scores are randomly emailed for manual checking
run_fatal_checks <- function(fulldata){
  test1  <- compare_n_days(fulldata)
  test2 <- leader_and_nleaders_only_on_same_days(fulldata)
  test3 <- check_sum_probs(fulldata)
  test4 <- check_unique_ids(fulldata)
  text <- paste0(test1,"\n",test2,"\n",test3,"\n",test4)
  if(length(text)>1){
    return(paste0("Fatal error found in the data, see details below \n",text))
  }else{
    return("")
  }
}

compare_n_days <- function(fulldata){
  failedgames <-  c()
  oddscolumns <- fulldata[,grep("odds", names(fulldata))]
  sdcolumns <- fulldata[,grep("odds", names(fulldata))]
  nleadercolumns <- fulldata[,grep("nleaders", names(fulldata))]
  for(i in 1:nrow(fulldata)){
    nodds <- sum(!(is.na(oddscolumns[i,])))
    nsd <- sum(!(is.na(sdcolumns[i,])))
    nnleaders <- sum(!is.na(nleadercolumns[i,]))
    if(!(nodds==nsd & nodds==nnleaders)){
      failedgames <- c(failedgames,fulldata$id[i])
    }
  }
  if(length(failedgames)>0){
    return(paste0("Check for same number of values per column failed for game(s):",failedgames))
  }else{
    return(NULL)
  }
}

leader_and_nleaders_only_on_same_days <- function(fulldata){
  nleadercolumns <- fulldata[,grep("nleaders", names(fulldata))]
  leadercolumns <- fulldata[,grep("leader_", names(fulldata))]
  failedgames <- c()
  for(i in 1:nrow(fulldata)){
    ids <- which(!is.na(leadercolumns[i,]))
    nleaders <- as.numeric(nleadercolumns[i,ids])
    if(sum(nleaders!=1)>0){
      failedgames <- c(failedgames,fulldata$id[i])
    }
  }
  if(length(failedgames)>0){
    return(paste0("Check that leading bookie should only be present on days with only one leading bookie failed for game(s):",failedgames))
  }else{
    return(NULL)
  }
}

check_sum_probs <- function(fulldata){
  failedgames <-  c()
  oddscolumns <- fulldata[,grep("odds", names(fulldata))]
  for(i in 1:nrow(fulldata)){
    odds <- oddscolumns[i,]
    odds <- odds[!is.na(odds)]
    ndays <- length(odds)/3
    if(ndays>0){
      for(j  in 1:ndays){
        dayodds <- odds[c(j,(ndays+j),(2*ndays+j))]
        probs <- 1/as.numeric(dayodds)
        if(!(sum(probs)>0.9 & sum(probs)<1.1)){
          failedgames <- c(failedgames,fulldata$id[i])
        }
      }
    }
  }
  failedgames <- unique(failedgames)
  if(length(failedgames)>0){
    return(paste0("Check for sum of probabilities each day failed for game(s):",failedgames))
  }else{
    return(NULL)
  }
}

check_unique_ids <- function(fulldata){
  ids <- fulldata$id
  if(length(unique(fulldata$id[duplicated(fulldata$id)]))>0){
    return(paste0("Check for only unique ids failed, following game(s) have duplicates:",unique(fulldata$id[duplicated(fulldata$id)])))
  }else{
    return(NULL)
  }
}

check_file_structure <- function(league,v){
  data <- read.csv(league)
  example <- read.csv(paste0("data/new/",v,"/hold.csv"))
  if(length(setdiff(colnames(data),colnames(example)))>0){
    stop(paste0("A fatal change in the file structure has been detected in league ",league))
  }
}

init_browser <- function() {
  b <- ChromoteSession$new()
  
  b$Network$enable()
  b$Page$enable()
  b$Runtime$enable()
  
  # --- Realistic User Agent ---
  b$Network$setUserAgentOverride(
    userAgent = paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/120.0.0.0 Safari/537.36"
    )
  )
  
  # --- Screen / device ---
  b$Emulation$setDeviceMetricsOverride(
    width = sample(c(1366, 1920), 1),
    height = sample(c(768, 1080), 1),
    deviceScaleFactor = 1,
    mobile = FALSE
  )
  
  # --- Timezone (important!) ---
  b$Emulation$setTimezoneOverride("Europe/Amsterdam")
  
  # --- Extra headers ---
  b$Network$setExtraHTTPHeaders(list(
    "Accept-Language" = "en-US,en;q=0.9"
  ))
  
  # --- Stealth script ---
  b$Page$addScriptToEvaluateOnNewDocument("
    Object.defineProperty(navigator, 'webdriver', { get: () => undefined });

    window.chrome = { runtime: {} };

    Object.defineProperty(navigator, 'languages', {
      get: () => ['en-US', 'en']
    });

    Object.defineProperty(navigator, 'plugins', {
      get: () => [1, 2, 3, 4, 5]
    });

    Object.defineProperty(navigator, 'hardwareConcurrency', {
      get: () => 4
    });

    Object.defineProperty(navigator, 'deviceMemory', {
      get: () => 8
    });
  ")
  
  return(b)
}

warmup_session <- function(b) {
  b$Page$navigate("https://www.betexplorer.com/")
  b$Page$loadEventFired(wait_ = TRUE)
  
  Sys.sleep(runif(1, 3, 6))
  
  # Accept cookies if present
  b$Runtime$evaluate("
    let btn = document.querySelector('#onetrust-accept-btn-handler');
    if (btn) btn.click();
  ")
  
  Sys.sleep(runif(1, 1, 3))
  
  # Random scroll
  b$Runtime$evaluate(sprintf("
    window.scrollTo(0, %d);
  ", sample(200:1000, 1)))
  
  Sys.sleep(runif(1, 2, 4))
  return(b)
}

extract_match_info_debug <- function(link, b, i = NA, attempt = NA) {
  
  cat("\n============================\n")
  cat("URL:", link, "\n")
  cat("Attempt:", attempt, "\n")
  
  start_time <- Sys.time()
  
  b$Page$navigate(link)
  
  # ✅ Wait for odds to load instead of fixed sleep
  wait_for_odds <- function(timeout = 15) {
    start <- Sys.time()
    
    while (as.numeric(Sys.time() - start, units = "secs") < timeout) {
      
      res <- b$Runtime$evaluate(
        "document.querySelectorAll('a.archiveOdds').length"
      )$result$value
      
      if (!is.null(res) && res > 0) {
        return(TRUE)
      }
      
      Sys.sleep(1)
    }
    
    return(FALSE)
  }
  
  odds_loaded <- wait_for_odds(timeout = 15)
  
  if (!odds_loaded) {
    cat("⚠️ Odds did NOT load within timeout\n")
  } else {
    cat("✅ Odds detected in DOM\n")
  }
  
  # Now extract HTML AFTER waiting
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  
  load_time <- Sys.time() - start_time
  cat("Load time:", load_time, "\n")
  cat("HTML size:", nchar(html), "\n")
  
  # Save HTML for inspection
  fname <- paste0("debug_page_", i, "_attempt_", attempt, ".html")
  writeLines(html, fname)
  cat("Saved HTML to:", fname, "\n")
  
  # Detect blocking / Cloudflare
  cat("Contains 'cf-':", grepl("cf-", html), "\n")
  cat("Contains 'cloudflare':", grepl("cloudflare", html, ignore.case = TRUE), "\n")
  cat("Contains 'attention':", grepl("attention", html, ignore.case = TRUE), "\n")
  
  page <- read_html(html)
  
  odds_nodes <- html_nodes(page, "a.archiveOdds")
  cat("Odds nodes found:", length(odds_nodes), "\n")
  
  eventstage_nodes <- html_nodes(page, "#js-eventstage")
  cat("Has js-eventstage:", length(eventstage_nodes), "\n")
  
  # Print status if available
  status <- NA
  if (length(eventstage_nodes) > 0) {
    status <- html_text(eventstage_nodes)
    cat("Game status:", status, "\n")
  }
  
  return(list(
    html_size = nchar(html),
    odds_nodes = length(odds_nodes),
    has_eventstage = length(eventstage_nodes),
    status = status,
    odds_loaded = odds_loaded
  ))
}