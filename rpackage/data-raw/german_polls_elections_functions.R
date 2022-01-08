
get_german_polls_data <- function() {
  main_page <- "http://www.wahlrecht.de/umfragen/"

  # Get name of each institute from the table at the mainpage
  institutes <- xml2::read_html(main_page)
  institutes <- rvest::html_nodes(institutes, "th.in a")
  institutes <- rvest::html_text(institutes)

  # get link to each institute's page from the table at the mainpage
  links <- xml2::read_html(main_page)
  links <- rvest::html_nodes(links, "th.in a")
  links <- xml2::xml_attr(links, "href")

  # combine 'main_page' and 'links' to valid urls
  urls <- paste0(main_page, links)
  complete_url_list <- list()
  for(i in seq_along(urls)){
    all_urls <- xml2::read_html(urls[i])
    all_urls <- rvest::html_nodes(all_urls, "p.navi a")
    all_urls <- xml2::xml_attr(all_urls, "href")
    if(length(all_urls) > 0){
      complete_url_list[[i]] <- data.frame(institutes = institutes[i], url = c(urls[i], paste0(main_page, all_urls)), stringsAsFactors = FALSE)
    } else {
      complete_url_list[[i]] <- data.frame(institutes = institutes[i], url = urls[i], stringsAsFactors = FALSE)
    }
  }
  url_df <- do.call(rbind, complete_url_list)
  # Can remove stimmung etc

  # read all tables into list 'raw'
  # raw is then a list containing for each institute another list
  # with a data.frame for each time period
  # due to website structure every second element in the second level list is NULL
  raw_list <- list()
  for(i in seq_along(url_df$url)){
    raw_list[[i]] <- XML::readHTMLTable(url_df$url[i], stringsAsFactors = FALSE)
    raw_list[[i]] <- raw_list[[i]][[2]]
    colnames(raw_list[[i]])[1] <- "Datum"
    raw_list[[i]]$institute <- url_df$institutes[i]
    raw_list[[i]]$url <- url_df$url[i]
  }
  # Small fixes
  colnames(raw_list[[33]]) <-
    c("Datum", " ", 'CDU/CSU', 'SPD', "GRÜNE", 'FDP', 'PDS', 'Sonstige', " ", " ")

  ###------------------------------------------ PREPARE FOR THE MERGE ------------------------------------------###

  poll_data <- dplyr::bind_rows(raw_list)
  # solve encoding issue in header
  Encoding(names(poll_data)) <- 'UTF-8'

  ###------------------------------------------ CLEANING ------------------------------------------###
  # remove row(s) where 'Bundestagswahl' occurs
  omit <- poll_data$Befragte == 'Bundestagswahl' & !is.na(poll_data$Befragte)
  poll_data <- poll_data[!omit, ]
  omit <- poll_data$Befragte == 'Wahl 1998' & !is.na(poll_data$Befragte)
  poll_data <- poll_data[!omit, ]

  # Will be computed later
  poll_data$Sonstige <- NULL

  # set each party's column to numeric and divide by 100
  columns_to_clean  <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD", "PDS", "REP", "Linke.PDS", "PIRATEN", "Rechte", "Nichtwähler/Unentschl.", "REP/DVU", "CDU", "Unent-schlossene", "Nicht-wähler", "Nichtwähler/Unentschlos.")
  for(j in seq_along(columns_to_clean)){
    cn <- columns_to_clean[j]
    poll_data[, cn] <- gsub(x = poll_data[, cn], pattern = "%", replacement = "")
    poll_data[, cn] <- gsub(x = poll_data[, cn], pattern = ",", replacement = ".")
    poll_data[, cn] <- suppressWarnings(as.numeric(poll_data[, cn])) / 100
  }

  # the column 'Befragte' includes lots of non-numeric chars like '~', '?', '.', ...
  poll_data$Befragte <- as.numeric(gsub(x = poll_data$Befragte, pattern = "\\D",  replacement = ""))

  # Datum
  poll_data$Datum <- as.Date(gsub(x = poll_data$Datum, pattern = "[*]", replacement = ""), format = "%d.%m.%Y")

  # Fix periods
  poll_data$Zeitraum <- gsub(x = poll_data$Zeitraum, pattern = ",", replacement = ".")
  poll_data$Zeitraum <- gsub(x = poll_data$Zeitraum, pattern = "bis", replacement = "–")
  poll_data$Zeitraum <- gsub(x = poll_data$Zeitraum, pattern = " ", replacement = "")
  poll_data$Zeitraum <- gsub(x = poll_data$Zeitraum, pattern = "-", replacement = "–")
  date_list <- strsplit(poll_data$Zeitraum, "–")
  poll_data$from <- as.Date("1900-01-01")
  poll_data$to <- as.Date("1900-01-01")
  for(i in 1:nrow(poll_data)){
    poll_data$from[i] <- as.Date(paste0(date_list[[i]][1], lubridate::year(poll_data$Datum[i])), format = "%d.%m.%Y")
    poll_data$to[i] <- as.Date(paste0(date_list[[i]][2], lubridate::year(poll_data$Datum[i])), format = "%d.%m.%Y")
  }
  # Correct when month equals 12 and Datum is smaller
  from_correct <- poll_data$Datum < poll_data$from & lubridate::month(poll_data$from) == 12 & lubridate::month(poll_data$Datum) == 1
  from_correct[is.na(from_correct)] <- FALSE
  lubridate::year(poll_data$from[from_correct]) <- lubridate::year(poll_data$from[from_correct]) - 1L
  to_correct <- poll_data$Datum < poll_data$to & lubridate::month(poll_data$to) == 12 & lubridate::month(poll_data$Datum) == 1
  to_correct[is.na(to_correct)] <- FALSE
  lubridate::year(poll_data$to[to_correct]) <- lubridate::year(poll_data$to[to_correct]) - 1L

  # Add pds to linke and sum irrelevant parties up to 'Sonstige'
  poll_data$LINKE <- pmax(poll_data$LINKE, poll_data$Linke.PDS, poll_data$PDS, na.rm = TRUE)
  poll_data$Linke.PDS <- NULL
  poll_data$PDS <- NULL

  poll_data[,"Nichtwähler/Unentschl."] <- pmax(poll_data[,"Nichtwähler/Unentschl."],
                                               poll_data[,"Unent-schlossene"],
                                               poll_data[,"Nicht-wähler"],
                                               poll_data[,"Nichtwähler/Unentschlos."],
                                               na.rm = TRUE)
  poll_data[,"Unent-schlossene"] <- NULL
  poll_data[,"Nicht-wähler"] <- NULL
  poll_data[,"Nichtwähler/Unentschlos."] <- NULL

  columns_to_sum  <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD")
  poll_data$Sonstige <- 1 - rowSums(poll_data[, columns_to_sum], na.rm = TRUE)
  poll_data$Institut <- poll_data$institute

  # select relevant variables
  columns_to_keep  <- c("Institut", "Datum", "CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD", "Sonstige", "Befragte", "Nichtwähler/Unentschl.", "from", "to" )
  poll_data <- poll_data[, columns_to_keep]
  poll_data <- dplyr::filter(poll_data, !is.na(Datum), !is.na(Institut), !is.na(SPD))

  return(poll_data)
}

