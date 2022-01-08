

get_wikipedia_spanish_polls_data <- function(wiki_url, xpath, column_name, party_columns, digits = 1) {
  checkmate::assert_string(wiki_url)
  checkmate::assert_string(wiki_table_xpath)
  checkmate::assert_character(column_name)
  checkmate::assert_integerish(party_columns, lower = 1, upper = length(column_name))

  raw_polls <- xml2::read_html(wiki_url)
  raw_polls <- rvest::html_nodes(raw_polls, xpath=wiki_table_xpath)
  raw_polls <- rvest::html_table(raw_polls, fill = TRUE, trim = TRUE, header = NA)[[1]]

  polls <- raw_polls
  if(all(unname(unlist(raw_polls[1,1:4])) == colnames(raw_polls)[1:4])){
    polls <- polls[-1,]
  }

  polls <- polls[, 1:length(column_name)]
  colnames(polls) <- column_name

  # Remove footnotes
  polls$house <- gsub(x = polls$house, pattern = "\\[.+", replace = "")

  # Split up into comissioner and polling house (keep house)
  polls$house <- unlist(lapply(strsplit(polls$house, split = "/"), FUN = function(x) {ifelse(length(x) != 0, x[[1]], "")}))

  # Remove all , from Sample size
  polls$n <- gsub(x = polls$n, pattern = ",", replace = "")
  polls$n <- suppressWarnings(as.numeric(polls$n))

  # Change all ? to NA in col 2:4
  polls$turnout <- suppressWarnings(as.numeric(polls$turnout))

  # Remove filling rows
  fill_rows <- polls$house == "" & polls$fieldwork_date == "" & is.na(polls$turnout) & is.na(polls$n)
  polls <- polls[!fill_rows,]

  # Parse fieldate to to and from
  polls$fieldwork_date <- gsub(x = polls$fieldwork_date, pattern = ",", replacement = ".")
  polls$from <- as.Date("1900-01-01")
  polls$to <- as.Date("1900-01-01")
  for(i in 1:nrow(polls)){
    fd <- parse_date(d = polls$fieldwork_date[i], info = polls$house[i])
    polls$from[i] <- fd["from"]
    polls$to[i] <- fd["to"]
  }

  # To numeric (the columns with parties), first remove numbers after digit decimals
  for(j in party_columns){
    polls[, j] <- stringr:::str_extract(polls[, j], pattern = paste0("[0-9]+\\.[0-9]{", digits,"}"))
    polls[, j] <- as.numeric(polls[, j])/100
  }

  # Remove general elections
  polls <- polls[!grepl(x = polls$house, pattern = "general election"),]
  polls <- polls[!grepl(x = polls$house, pattern = "EP election"),]

  return(polls)
}


parse_date <- function(d, info){
  checkmate::assert_string(d)
  checkmate::assert_string(info)

  month_abbr <- adapop:::month_abbr_en()

  ds <- strsplit(d, split = "–")[[1]]
  ds <- stringr::str_trim(ds)

  if(length(ds) == 0){
    warning("'", info, "' is missing date (", d, ")", call. = FALSE)
    return(c(from = as.Date(NA), to = as.Date(NA)))
  }
  if(length(ds) == 1){
    message("'", info, "' has only one date (", d, ")")
    ds[2] <- ds[1]
  }
  if(length(ds) == 2){
    to <- strsplit(ds[2], " ")[[1]]
    if(length(to) == 3){
      to_num <- suppressWarnings(as.numeric(to))
      month_num <- which(to[2] == month_abbr)
      if(length(month_num) != 1) warning("'", info, "' is not correct (", d, ")", call. = FALSE)
      to_num[2] <- month_num
    } else {
      warning("'", info, "' is not correct (", d, ")", call. = FALSE)
      return(c(from = as.Date(NA), to = as.Date(NA)))
    }
    from <- strsplit(ds[1], " ")[[1]]
    from_num <- suppressWarnings(as.numeric(from))
    if(length(from) >= 2){
      month_num <- which(from[2] == month_abbr)
      if(length(month_num) != 1) warning("'", info, "' is not correct (", d, ")", call. = FALSE)
      from_num[2] <- month_num
      if(length(from) == 2)  from_num <- c(from_num, to_num[3])
    } else if (length(from) == 1){
      from_num <- c(from_num, to_num[2:3])
    }
    dats <- c(from=as.Date(paste0(as.character(from_num), collapse = "-"), "%d-%m-%Y"),
              to=as.Date(paste0(as.character(to_num), collapse = "-"), "%d-%m-%Y"))
    return(dats)
  } else {
    warning("'", info, "' is not correct (", d, ")", call. = FALSE)
    return(c(from = as.Date(NA), to = as.Date(NA)))
  }

}
