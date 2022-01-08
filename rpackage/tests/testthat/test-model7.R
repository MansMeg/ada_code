##
# test run
#
#
##
skip("Currently we skip this code when testing. Needs to be implemented as test suite")

library(adapop)
#
data("x_test")
txdf <- as.data.frame(x_test[3:4])
colnames(txdf) <- paste0("x", 3:length(x_test))
data("pd_test")

time_scale <- "week"
set.seed(4711)

spd <- simulate_polls(x = txdf,
                      pd = pd_test,
                      npolls = 150,
                      time_scale = time_scale,
                      start_date = "2010-01-01")
true_idx <- c(1, 25, 44, 72, 100)
known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
known_state <- cbind(known_state, txdf[true_idx,])
pop7 <- poll_of_polls(y = c("x3", "x4"),
 model = "model7",
 polls_data = spd,
 time_scale = time_scale,
 known_state = known_state,
 warmup = 1000,
 iter = 1500,
 chains = 2)

pop7 <- poll_of_polls(y = c("x3", "x4"),
                      model = "model7",
                      polls_data = spd,
                      time_scale = time_scale,
                      known_state = known_state,
                      warmup = 1000,
                      iter = 1500,
                      chains = 2)
pop6 <- poll_of_polls(y = c("x3", "x4"),
                      model = "model6",
                      polls_data = spd,
                      time_scale = time_scale,
                      known_state = known_state,
                      warmup = 1000,
                      iter = 1500,
                      chains = 2)

plot(pop7,"x3")+ geom_pop_line(pop7, txdf$x3,color='blue') + geom_pop_line(pop6, txdf$x3,color='red')
#
#' returns samples generated to predicit y
#' @param  poll_obj object generating preidiciton
#' @param  time     time point of the predicition (deafult all)
#' @return Y      - (SxTxP) time points, samples, dimesnion
y_prediction <- function(poll_obj, time=NULL){
        return(extract(poll_obj$stan_fit,'y_pred')[[1]])
}
#'
#'
#' @param y   - (T x 1) observations
#' @param y_p - (T x S) simulation from the predictive distribution
CRPS <- function(y, y_p){
        T <- dim(y_p)[2]
        S <- dim(y_p)[1]
        res <- matrix(0, T, 2)
        for(t in 1:T){
                E_y_Y <- mean(abs(y[t]-y_p[t,]))
                E_Y_Y <- mean(abs(y_p[t,]-sample(y_p[t,],replace=F)))
                res[t,] <- c(0.5*E_Y_Y-E_y_Y,
                             -E_y_Y/E_Y_Y - 0.5 * log(E_Y_Y)
                        )
        }
        return(res)
}
y_prediction <- function(poll_obj, time=NULL){
        if(class(pop7)[1] %in% c("pop_model7")){
                Y_p <-  extract(pop7$stan_fit,'y_pred')[[1]]
        }
        return(Y_p)
}
model_eval_pop <- function(poll_obj, time=NULL){

        object <- list()
        Y_p <- y_prediction(poll_obj, time=NULL)
        Y_t <- poll_obj$polls_data$y
        P <- length(poll_obj$y)
        T <- dim(Y_t)[1]
        object$CRPS <- matrix(nrow=T,ncol=P)
        object$SCRPS <- matrix(nrow=T,ncol=P)
        for(p in 2:(P+1)){
                res_p <- CRPS(y = poll_obj$polls_data$y[[c(p)]],
                              y_p = Y_p[,,p-1])
                print(p)
                object$CRPS[,p-1]  <- res_p[,1]
                object$SCRPS[,p-1] <- res_p[,2]
        }
        class(object) <- c("poll_of_polls_model_eval")
        return(object)
}
model_eval_pop(pop7)

