library(ada)
data("swe_polls")
pd <- polls_data(y = swe_polls[,3:11]/100,
                 house = swe_polls$Company,
                 publish_date = swe_polls$PublDate,
                 start_date = swe_polls$collectPeriodFrom,
                 end_date = swe_polls$collectPeriodTo,
                 n = swe_polls$n)
pd <- subset_dates(x = pd, from = "2010-01-01", to = "2019-12-31")
pd_test <- pd[complete_poll_info(pd)]
usethis::use_data(pd_test)
