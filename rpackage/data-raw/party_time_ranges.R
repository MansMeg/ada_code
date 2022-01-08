swedish_party_time_ranges <- NULL
usethis::use_data(swedish_party_time_ranges, overwrite = TRUE)

german_party_time_ranges <-
  list(AfD = list(from = as.Date("2013-02-06"))) # Founded
usethis::use_data(german_party_time_ranges, overwrite = TRUE)

spanish_party_time_ranges <-
  list(VOX = list(from = as.Date("2016-07-01")), # First poll
       Cs = list(from = as.Date("2013-01-01")) # Went national)
       )
usethis::use_data(spanish_party_time_ranges, overwrite = TRUE)

