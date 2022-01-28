# Run in rpackage
source("data-raw/german_polls_elections_functions.R")

german_polls <- get_german_polls_data()

# Get all in same format/colnames
german_polls$PublYearMonth <- paste0(lubridate::year(german_polls$Datum), "-", tolower(ada:::month_abbr_en())[lubridate::month(german_polls$Datum)])
german_polls$Company <-  as.factor(german_polls$Institut)
german_polls$Uncertain <- german_polls$`Nichtwähler/Unentschl.`
german_polls$n <- as.integer(german_polls$Befragte)
german_polls$PublDate <- german_polls$Datum
german_polls$collectPeriodFrom <- german_polls$from
german_polls$collectPeriodTo <- german_polls$to
german_polls$approxPeriod <- FALSE
german_polls$house <- as.factor(german_polls$Institut)
german_polls$Sonstige <- NULL

german_polls <- german_polls[, c("PublYearMonth", "Company", "CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD", "Uncertain", "n", "PublDate", "collectPeriodFrom", "collectPeriodTo", "approxPeriod", "house")]
german_polls <- german_polls[order(german_polls$PublDate, decreasing = TRUE),]

# Change variables names
colnames(german_polls)[which(colnames(german_polls) == "GRÜNE")] <- "GRUNE"

## code to prepare `german_polls` dataset goes here
write.csv(german_polls, file = "data-raw/files/german_polls.csv", row.names = FALSE)
usethis::use_data(german_polls, overwrite = TRUE)

# Curate data
# data(german_polls)

## Manual curating of individual observations
# This is set to 15?? on the homepage, rounding down
german_polls$n[german_polls$Company=="Allensbach" & german_polls$PublDate == "2013-03-20"] <- 1500

## General curation
to_rm <- german_polls$collectPeriodFrom > german_polls$collectPeriodTo
to_rm[is.na(to_rm)] <- FALSE
if(any(to_rm)){
  message("poll no ", paste0(which(to_rm), collapse = ", "), " has been removed due to incorrect period.")
  german_polls <- german_polls[!to_rm, ]
}

german_polls_curated <- german_polls
usethis::use_data(german_polls_curated, overwrite = TRUE)
write.csv(german_polls_curated, file = "data-raw/files/german_polls_curated.csv", row.names = FALSE)


# german_elections <- data.frame(rbind(c(15390950 + 3889990, 18129100, 2538913, 2208531, 3764168, NA),
#                                      c(13856674 + 3191000, 12079758, 3977125, 4076496, 4791124, NA),
#                                      c(16233642 + 3544079, 12843458, 3180299, 1028645, 3585178, 810915),
#                                      c(14030751 + 3255487, 11429231, 3717922, 3249238, 3966637, 5317499)))
german_elections <- data.frame(rbind(c(14004908+3324480, 20181269, 3301624, 3080955, 2515454, NA),
                                     c(14167561+4315080, 18488668, 4110355, 3538815, 1916702, NA),
                                     c(16631049, 16194665, 3838326, 4648144, 4118194, NA),
                                     c(14658515, 9990488, 4643272, 6316080, 5155933, NA),
                                     c(18165446, 11252215, 3694057, 2083533, 3755699, 2056985),
                                     c(15317344, 9539381, 4158400, 4999449, 4297270, 5878115)))
colnames(german_elections) <- c("CDU/CSU", "SPD", "GRUNE", "FDP", "LINKE" , "AfD")
elec_date <- c(as.Date('1998-09-27'),
               as.Date('2002-09-22'),
               as.Date('2005-09-18'),
               as.Date('2009-09-27'),
               as.Date('2013-09-22'),
               as.Date('2017-09-24'))
elec_counts <- c(49947087-638575,48582761-586281, 48044134 - 756146, 44005575 - 634385, 44309925 - 583069, 46976341 - 460849)
for(i in 1:nrow(german_elections)){
  german_elections[i,] <- german_elections[i,] / elec_counts[i]
}
german_elections[is.na(german_elections)] <- 0


german_elections <-
  dplyr::bind_cols(tibble::tibble(PublYearMonth = c("1998-sep", "2002-sep", "2005-sep","2009-sep","2013-sep", "2017-sep"),
                              Company = rep(as.character(NA), length(elec_counts))),
                   german_elections,
                   tibble::tibble(Uncertain = rep(as.numeric(NA), length(elec_counts)),
                              n = elec_counts,
                              PublDate = elec_date,
                              collectPeriodFrom = elec_date,
                              collectPeriodTo = elec_date,
                              approxPeriod = FALSE,
                              house = "Election"))
usethis::use_data(german_elections, overwrite = TRUE)
write.csv(german_elections, file = "data-raw/files/german_elections.csv", row.names = FALSE)
