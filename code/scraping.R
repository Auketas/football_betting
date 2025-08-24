library(rvest)
library(lubridate)
library(fs)
library(assertthat)
library(httr)
library(tictoc)
library(dplyr)
library(chromote)
library(R.utils)
extract_data <- function(league,timezone,b){
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
    team_names_mat <- matrix(nrow=length(dates),ncol=6)
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
          Sys.sleep(2)  # small delay between requests
          print(paste0("Scraping game ",matchmatuse[i,1],"-",matchmatuse[i,2]))
          matchmatuse[i, 4:6] <- extract_sd(matchmatuse[i, 3],b)
          success <- TRUE
        }, error = function(e) {
          cat(paste0("⚠️ Error in game ", i, ": ", e$message, "\n"))
          cat("⏳ Retrying this game in 10 seconds...\n")
          Sys.sleep(10)
        })
      }
      
      if (!success) {
        cat(paste0("❌ Skipping game ", i, " after 3 failed attempts.\n"))
      }
    }
    
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
    
    matchmatuse_mat <- matrix(matchmatuse, ncol = 6)
    
    matuse <- data.frame(Date = final_dates, dif = dif, matchmatuse = matchmatuse_mat, oddsmatuse = oddsmatuse, stringsAsFactors = FALSE)
    matuse$id <- paste0(matuse$Date,"_",matuse$matchmatuse.1,"_",matuse$matchmatuse.2)
    matuse <- matuse[matuse$dif<21,]
    return(matuse)
    }
  }
}

extract_sd <- function(link, b) {
  b$Page$navigate(link)
  Sys.sleep(5)
  
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  odds_nodes <- html_nodes(page, "a.archiveOdds")
  if (length(odds_nodes) == 0) {
    return(c(NA, NA, NA))
  }
  
  text <- odds_nodes %>% html_text(trim = TRUE)
  home <- as.numeric(text[seq(1, length(text), by = 3)])
  draw <- as.numeric(text[seq(2, length(text), by = 3)])
  away <- as.numeric(text[seq(3, length(text), by = 3)])
  
  return(c(sd(home), sd(draw), sd(away)))
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

write_league <- function(league,timezone,b){
  data <- extract_data(league,timezone,b)
  if(nrow(data)>0&&length(data)>0){
    trimmed <- sub("^/football/", "", league)
    trimmed <- sub("/$", "", trimmed)
    
    # Split by "/"
    parts <- strsplit(trimmed, "/")[[1]]
    
    # Capitalize the first letter of the first part
    parts[1] <- paste0(toupper(substring(parts[1], 1, 1)), substring(parts[1], 2))
    
    # Join with underscore
    name <- paste(parts, collapse = "_")
    if(file_exists(paste0("data/new/",name,".csv"))){
      fulldata <- read.csv(paste0("data/new/",name,".csv"))
      numcol <- ncol(fulldata)
      for(i in 1:nrow(data)){
        row <- data[i,]
        if(row$id %in% fulldata$id){
          colname1 <- paste0("home_odds_l",row$dif)
          colname2 <- paste0("draw_odds_l",row$dif)
          colname3 <- paste0("away_odds_l",row$dif)
          fulldata[[colname1]][which(fulldata$id==row$id)] <- row$oddsmatuse.1
          fulldata[[colname2]][which(fulldata$id==row$id)] <- row$oddsmatuse.2
          fulldata[[colname3]][which(fulldata$id==row$id)] <- row$oddsmatuse.3
          
        }else{
          new_data <- as.data.frame(lapply(fulldata, function(x) rep(NA,nrow(data))))
          new_data$id <- data$id
          for(i in 1:nrow(data)){
            id <- data$id[i]
            colname1 <- paste0("home_odds_l",data$dif[i])
            colname2 <- paste0("draw_odds_l",data$dif[i])
            colname3 <- paste0("away_odds_l",data$dif[i])
            colname4 <- paste0("home_sd_l",data$dif[i])
            colname5 <- paste0("draw_sd_l",data$dif[i])
            colname6 <- paste0("away_sd_l",data$dif[i])
            new_data[[colname1]][i] <- data$oddsmatuse.1[i]
            new_data[[colname2]][i] <- data$oddsmatuse.2[i]
            new_data[[colname3]][i] <- data$oddsmatuse.3[i]
            new_data[[colname4]][i] <- data$matchmatuse.4[i]
            new_data[[colname5]][i] <- data$matchmatuse.5[i]
            new_data[[colname6]][i] <- data$matchmatuse.6[i]
          }
          fulldata <- rbind(fulldata,new_data)
        }
      }
      assert_that(ncol(fulldata)==numcol,msg=paste0("Adding data has changed the number of columns for league ",league))
      write.csv(fulldata,paste0("data/new/",name,".csv"),row.names=FALSE)
    }else{
      exampledata <- read.csv(paste0("data/new/hold.csv"))
      numcol <- ncol(exampledata)
      new_data <- as.data.frame(lapply(exampledata, function(x) rep(NA, nrow(data))))
      new_data$id <- data$id
      for(i in 1:nrow(data)){
        id <- data$id[i]
        colname1 <- paste0("home_odds_l",data$dif[i])
        colname2 <- paste0("draw_odds_l",data$dif[i])
        colname3 <- paste0("away_odds_l",data$dif[i])
        colname4 <- paste0("home_sd_l",data$dif[i])
        colname5 <- paste0("draw_sd_l",data$dif[i])
        colname6 <- paste0("away_sd_l",data$dif[i])
        new_data[[colname1]][i] <- data$oddsmatuse.1[i]
        new_data[[colname2]][i] <- data$oddsmatuse.2[i]
        new_data[[colname3]][i] <- data$oddsmatuse.3[i]
        new_data[[colname4]][i] <- data$matchmatuse.4[i]
        new_data[[colname5]][i] <- data$matchmatuse.5[i]
        new_data[[colname6]][i] <- data$matchmatuse.6[i]
      }
      assert_that(ncol(new_data)==numcol,msg=paste0("Adding data has changed the number of columns for league ",league))
      write.csv(new_data,paste0("data/new/",name,".csv"),row.names=FALSE)
    }
  }
}

add_results <- function(league,b){
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
    
    
    trimmed <- sub("^/football/", "", league)
    trimmed <- sub("/$", "", trimmed)
    
    # Split by "/"
    parts <- strsplit(trimmed, "/")[[1]]
    
    # Capitalize the first letter of the first part
    parts[1] <- paste0(toupper(substring(parts[1], 1, 1)), substring(parts[1], 2))
    
    # Join with underscore
    name <- paste(parts, collapse = "_")
    if(file_exists(paste0("data/new/",name,".csv"))){
      fulldata <- read.csv(paste0("data/new/",name,".csv"))
      numcol <- ncol(fulldata)
      idsnoresults <- fulldata$id[is.na(fulldata$result)]
      update <- intersect(idsnoresults,resultmat$id)
      for(useid in update){
        fulldata$result[fulldata$id==useid] <- resultmat$score_result[resultmat$id==useid]
      }
      assert_that(ncol(fulldata)==numcol,msg=paste0("Adding results has changed the number of columns for league ",league))
      write.csv(fulldata,paste0("data/new/",name,".csv"),row.names=FALSE)
    }
  }
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

loop_over_leagues <- function(start = 1) {
  leaguelist <- read.csv("data/hold/leaguelist.csv")
  leagues <- leaguelist[, 2]
  timezones <- leaguelist[, 3]
  
  # create ONE Chromote session for the whole run
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  
  for (i in start:nrow(leaguelist)) {
    league <- leagues[i]
    timezone <- timezones[i]
    success <- FALSE
    delay <- 15   # base retry delay
    
    while (!success) {
      cat(paste0("Processing league ", league, " (row ", i, ")\n"))
      
      tryCatch({
        Sys.sleep(5)  # polite pause
        write_league(league, timezone, b)  # pass Chromote session
        add_results(league,b)
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
  }
}

write_to_train_test <- function(debug=FALSE){
  temptrain <- read.csv("data/new/hold.csv")
  temptrain$league <- 0
  temptrain <- temptrain[,c(1,ncol(temptrain),2:(ncol(temptrain)-1))]
  temptest <- temptrain
    
  leaguelist <- read.csv("data/hold/leaguelist.csv")
  leagues <- leaguelist[, 2]
  timezones <- leaguelist[, 3]
  
  for (i in 1:nrow(leaguelist)) {
    print(i)
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
      print(paste0("looking at ",name))
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
  print("league loop done")
  temptrain <- temptrain[nchar(temptrain$id)>2,]
  temptest <- temptest[nchar(temptest$id)>2,]
  temptrain_formatted <- convert_data_to_model_format(temptrain,return=TRUE,write=FALSE)
  temptest_formatted <- convert_data_to_model_format(temptest,return=TRUE,write=FALSE)
  temptest_formatted <- temptest_formatted %>%
    group_by(id, outcome) %>%
    slice_min(order_by = daysout, n = 1, with_ties = FALSE) %>%
    ungroup()
  print("1")
  temptrain_formatted$saveid <- paste0(temptrain_formatted$id,"-",temptrain_formatted$daysout,"-",temptrain_formatted$outcome)
  train_file <- "data/model/train.rds"
  print("2")
  if (file.exists(train_file)) {
    print("file exists")
    train <- readRDS(train_file)
    train <- rbind(temptrain_formatted, train)
    train <- train[!duplicated(train$saveid), ]
  } else {
    train <- temptrain_formatted
  }
  saveRDS(train, file = "data/model/train.rds")
  saveRDS(temptest_formatted,file = "data/model/test.rds")
}

convert_data_to_model_format <- function(rawdata,return=FALSE,write=TRUE){
  allgames <- matrix(ncol=10)
  allgames <- data.frame(allgames)
  colnames(allgames) <- c("id","league","daysout","outcome","odds_history","sd_history","final_result","payoff","odds","ndays")
  for(i in 1:nrow(rawdata)){
    print(paste0("conversion line ",i))
    row <- rawdata[i,]
    days <- 21-which(!is.na(row[3:23]))
    print(days)
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
    print(ncol(allgames))
    print(ncol(gamemat))
    allgames  <- rbind(allgames,gamemat)
  }
  print("loop done")
  allgames <- allgames[!is.na(allgames$id),]
  print(paste0("nrow allgames ",nrow(allgames)))
  print(paste0("ncol allgames ",ncol(allgames)))
  if(return==TRUE){
    return(allgames)
  }
  if(write==TRUE){
    write.csv(allgames,"/data/dump/modeldata.csv")
  }
}




