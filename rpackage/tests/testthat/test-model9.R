library(adapop)
library(rstan)
if(0){
  data("x_test_multi")
  txdf <- as.data.frame(x_test_multi$dynamic_trend)
  colnames(txdf) <- c(paste0("x", 1:3))
  data("pd_test")

  data("pd_test")

  time_scale <- "week"
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)

  sd <- stan_polls_data(x = spd,
                        time_scale = time_scale,
                        y_name = c("x1", "x2", "x3"),
                        model = "model9",
                        known_state = known_state)

  pop9 <- poll_of_polls(y = c("x1", "x2", "x3"),
                         model = "model9",
                         polls_data = spd,
                         time_scale = time_scale,
                         known_state = known_state,
                         #                                 slow_scales = as.Date(c("2010-01-01", "2011-01-01")),
                         warmup = 2000,
                         iter = 2500,
                         chains = 2)
  pop6 <- poll_of_polls(y = c("x1", "x2", "x3"),
                        model = "model6",
                        polls_data = spd,
                        time_scale = time_scale,
                        known_state = known_state,
                        #                                 slow_scales = as.Date(c("2010-01-01", "2011-01-01")),
                        warmup = 2000,
                        iter = 2500,
                        chains = 2)
  p1 <-  plot(pop9,"x1") + geom_pop_line(pop9, txdf$x1,color='blue')
  p2 <-  plot(pop9,"x2") + geom_pop_line(pop9, txdf$x2,color='blue')
  p3 <-  plot(pop9,"x3") + geom_pop_line(pop9, txdf$x3,color='blue')
  print(ggarrange(p1,p2,p3))
  p4 <-  plot(pop6,"x1") + geom_pop_line(pop6, txdf$x1,color='blue')
  p5 <-  plot(pop6,"x2") + geom_pop_line(pop6, txdf$x2,color='blue')
  p6 <-  plot(pop6,"x3") + geom_pop_line(pop6, txdf$x3,color='blue')
  print(ggarrange(p1,p2,p3,p4,p5,p6),nrow=2)
  #geom_pop_line(pop6, txdf$x3,color='red')
}
if(0){
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2019-12-31")
parties <- c("S","M", "MP","V")
data("swedish_elections")
data("swedish_polls_curated")

propotion.of.parties <- function(y, parties){
  sum.party <- rowSums(y[,parties], na.rm=T)
  for(i in 1:dim(y)[1])
    y[i,parties] <-  y[i,parties]/sum.party[i]
  return(y)
}
#making the parties sum to one
swedish_polls_curated <- swedish_polls_curated[swedish_polls_curated$Company=="Sifo",]
swedish_polls_curated <- propotion.of.parties(swedish_polls_curated, parties)
swedish_elections     <- propotion.of.parties(swedish_elections, parties)
#cov(diff(as.matrix(swedish_polls_curated[,parties])),use = "pairwise.complete.obs")
pd <- polls_data(y = swedish_polls_curated[,parties],
                 house = swedish_polls_curated$Company,
                 publish_date = swedish_polls_curated$PublDate,
                 start_date = swedish_polls_curated$collectPeriodFrom,
                 end_date = swedish_polls_curated$collectPeriodTo,
                 n = swedish_polls_curated$n)
pd <- subset_dates(pd, from = start_date, to = end_date)
pd <- pd[complete_poll_info(pd)]

ed <- swedish_elections
ed$date <- ed$PublDate
ed<- remove_known_states(pd, ed)


time_scale <- "week"
set.seed(4711)



sd_ <- stan_polls_data(x = pd,
                      time_scale = time_scale,
                      y_name = parties,
                      model = "model6",
                      known_state = ed)

pop9 <- poll_of_polls(y = parties,
                      model = "model9",
                      polls_data = pd,
                      time_scale = time_scale,
                      known_state = ed,
                      hyper_parameters = list(sigma_kappa_hyper = 0.001),
                      warmup = 2000,
                      iter = 2500,
                      chains = 2)
Sigma_X <- extract(pop9$stan_fit, par= 'Sigma_X')$Sigma_X
MSigma_X <- apply(Sigma_X, c(2,3), mean)

print(diag(sqrt(1/diag(MSigma_X) ) )%*%MSigma_X%*%diag(sqrt(1/diag(MSigma_X) ) ))
print(cor(pd$y[,c("S","M","MP","V")]))
pop6 <- poll_of_polls(y = parties,
                        model = "model6",
                        polls_data = pd,
                        time_scale = time_scale,
                        known_state = ed,
                        hyper_parameters = list(sigma_kappa_hyper = 0.001),
                        warmup = 2000,
                        iter = 2500,
                        chains = 2)

p1 <-  plot(pop6,"S") #+ geom_pop_line(pop8a, pd$y,color='blue')

p2 <-  plot(pop9,"S") #+ geom_pop_line(pop8a, pd$y,color='blue')
}
