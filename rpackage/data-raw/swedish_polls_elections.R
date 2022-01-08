library(SwedishPolls)
swe_polls <- SwedishPolls::get_polls()
swedish_polls <- swe_polls
usethis::use_data(swedish_polls, overwrite = TRUE)
write.csv(swedish_polls, file = "data-raw/files/swedish_polls.csv", row.names = FALSE)

# Curation
swe_polls <- curate_swedish_polls(swe_polls)
swedish_polls_curated <- swe_polls

## code to prepare `test_x` dataset goes here
usethis::use_data(swedish_polls_curated, overwrite = TRUE)
write.csv(swedish_polls_curated, file = "data-raw/files/swedish_polls_curated.csv", row.names = FALSE)


swedish_elections <- read.csv(file = "data-raw/files/swedish_elections.csv")
swedish_elections <- tibble::as_tibble(swedish_elections)
swedish_elections$PublDate <- as.Date(swedish_elections$PublDate)
swedish_elections$collectPeriodFrom <- as.Date(swedish_elections$collectPeriodFrom)
swedish_elections$collectPeriodTo <- as.Date(swedish_elections$collectPeriodTo)
swedish_elections$Company <- as.character(swedish_elections$Company)
usethis::use_data(swedish_elections, overwrite = TRUE)
write.csv(swedish_elections, file = "data-raw/files/swedish_elections.csv", row.names = FALSE)

