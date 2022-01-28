source("data-raw/spanish_polls_elections_functions.R")
pls <- list()

# Current polls
wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PSOE", "PP", "VOX", "UP", "Cs", "ERC", "Mas Pais", "JxCat", "PNV", "EH Bildu", "CUP", "CC", "BNG", "NS", "PRC")
party_columns <- 5:length(column_name)
pls[[1]] <- get_wikipedia_spanish_polls_data(wiki_url, wiki_table_xpath, column_name, party_columns)


wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PSOE", "PP", "Cs", "UP", "VOX", "ERC", "JxCat", "PNV", "EH Bildu", "Compromis", "CC", "NS", "PRC", "Mas Pais", "CUP")
party_columns <- 5:length(column_name)
pls[[2]] <- get_wikipedia_spanish_polls_data(wiki_url, xpath = wiki_table_xpath, column_name, party_columns)


wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PP", "PSOE", "UP", "Cs", "ERC", "PDeCAT", "PNV", "PACMA", "EH Bildu", "CC", "VOX", "Compromis", "JxCat", "NS")
party_columns <- 5:length(column_name)
pls[[3]] <- get_wikipedia_spanish_polls_data(wiki_url, xpath = wiki_table_xpath, column_name, party_columns)


wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2016_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PP", "PSOE", "Podemos", "Cs", "IU-UPeC", "ERC", "DiL/CDC", "PNV", "PACMA", "EH Bildu", "CC", "UP")
party_columns <- 5:length(column_name)
pls[[4]] <- get_wikipedia_spanish_polls_data(wiki_url, xpath = wiki_table_xpath, column_name, party_columns)

# 2015
wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PP", "PSOE", "IU-UPeC", "UPyD", "CiU", "Amaiur/EH Bildu", "PNV", "ERC", "BNG", "CC", "Compromis", "Cs", "Podemos", "DiL/CDC")
party_columns <- 5:length(column_name)
pls[[5]] <- get_wikipedia_spanish_polls_data(wiki_url, xpath = wiki_table_xpath, column_name, party_columns)

# 2011
wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PSOE", "PP", "IU", "CiU", "PNV", "UPyD", "ERC", "BNG", "CC", "Amaiur")
party_columns <- 5:length(column_name)
pls[[6]] <- get_wikipedia_spanish_polls_data(wiki_url, xpath = wiki_table_xpath, column_name, party_columns)
pls[[6]] <- pls[[6]][-nrow(pls[[6]]),] # Remove the last line

# 2008
wiki_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2008_Spanish_general_election"
wiki_table_xpath <- '//*[@id="mw-content-text"]/div/table[1]'
column_name <- c("house", "fieldwork_date", "n", "turnout", "PSOE", "PP", "IU", "CiU", "ERC", "PNV", "CC", "BNG", "UPyD")
party_columns <- 5:length(column_name)
pls[[7]] <- get_wikipedia_spanish_polls_data(wiki_url, xpath = wiki_table_xpath, column_name, party_columns)


spanish_polls <- dplyr::bind_rows(pls)
spanish_polls$PublYearMonth <- paste0(lubridate::year(spanish_polls$to), "-", tolower(ada:::month_abbr_en())[lubridate::month(spanish_polls$to)])
spanish_polls$Company <- as.factor(spanish_polls$house)

spanish_polls$Uncertain <- NA
spanish_polls$n <- as.integer(spanish_polls$n)
spanish_polls$PublDate <- NA
spanish_polls$collectPeriodFrom <- spanish_polls$from
spanish_polls$collectPeriodTo <- spanish_polls$to
spanish_polls$approxPeriod <- FALSE

spanish_polls$UP <- rowSums(spanish_polls[, c("UP", "Podemos", "IU-UPeC", "IU")], na.rm = TRUE)

spanish_polls <- spanish_polls[, c("PublYearMonth", "Company", "PSOE", "PP", "VOX", "UP", "Cs", "ERC", "CC", "Uncertain", "n", "PublDate", "collectPeriodFrom", "collectPeriodTo", "approxPeriod", "house")]
spanish_polls <- spanish_polls[order(spanish_polls$collectPeriodTo, decreasing = TRUE),]

write.csv(spanish_polls, file = paste0("data-raw/files/spanish_polls.csv"), row.names = FALSE)
usethis::use_data(spanish_polls, overwrite = TRUE)

# Curate data
# data(spanish_polls)
## Houses with more than 40 polls:
data(house_map)
houses <- house_map[house_map$country == "Spain",]
house_names <- houses$house_name
spanish_polls$PublDate <- as.Date(spanish_polls$collectPeriodTo) + 1L
to_rm <- spanish_polls$collectPeriodFrom > spanish_polls$collectPeriodTo
to_rm[is.na(to_rm)] <- FALSE
if(any(to_rm)){
  message("poll no ", paste0(which(to_rm), collapse = ", "), " has been removed due to incorrect period.")
  spanish_polls <- spanish_polls[!to_rm, ]
}

# Remove houses with fewer than 40 polls
spanish_polls <- spanish_polls[spanish_polls$house %in% house_names,]

# Set 0 values to NA
parties <- c("PSOE", "PP", "VOX", "UP", "Cs", "ERC", "CC")
for(i in seq_along(parties)){
  err <- !is.na(spanish_polls[[parties[i]]]) & spanish_polls[[parties[i]]] < 0.0001
  spanish_polls[[parties[i]]][err] <- NA
  if(any(err)) print(parties[i])
}

spanish_polls_curated <- spanish_polls

write.csv(spanish_polls_curated, file = paste0("data-raw/files/spanish_polls_curated.csv"), row.names = FALSE)
usethis::use_data(spanish_polls_curated, overwrite = TRUE)


spanish_elections <- data.frame(rbind(c(6792199, 5047040, 3656979, 3119364, 1650318, 880734,  124289),
                                      c(7513142, 4373653, 2688092, 3751145, 4155665, 1024628, 137664),
                                      c(5443846, 7941236, 47182,   5087538, 3141570, 639652,   78253),
                                      c(5545315, 7236965, 58114,   5212711 + 926783, 3514528, 604285, 81917),
                                      c(7003511, 10866566, NA, 1686040, NA, 256985, 143881),
                                      c(11289335, 10278010, NA, 969946, NA, 298139, 174629)))
colnames(spanish_elections) <- c("PSOE", "PP", "VOX", "UP", "Cs", "ERC", "CC" )
elec_date <- c(as.Date('2019-11-10'),
               as.Date('2019-04-28'),
               as.Date('2016-06-26'),
               as.Date('2015-12-20'),
               as.Date('2011-11-20'),
               as.Date('2008-03-09'))
elec_counts <- c(24258228, 26201371, 24053755, 25211313, 24348886, 25734863)
for(i in 1:nrow(spanish_elections)){
  spanish_elections[i,] <- spanish_elections[i,] / elec_counts[i]
}
spanish_elections[is.na(spanish_elections)] <- 0


spanish_elections <-
  dplyr::bind_cols(dplyr::tibble(PublYearMonth = c("2019-nov","2019-apr","2016-jun", "2015-dec", "2011-nov", "2008-mar"),
                                 Company = rep(as.character(NA), length(elec_counts))),
                   spanish_elections,
                   tibble::tibble(Uncertain = rep(as.numeric(NA), length(elec_counts)),
                                 n = elec_counts,
                                 PublDate = elec_date,
                                 collectPeriodFrom = elec_date,
                                 collectPeriodTo = elec_date,
                                 approxPeriod = FALSE,
                                 house = "Election"))
usethis::use_data(spanish_elections, overwrite = TRUE)
write.csv(spanish_elections, file = paste0("data-raw/files/spanish_elections.csv"), row.names = FALSE)

