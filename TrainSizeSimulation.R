generate_diff_data <- function(nsamples1, nsamples2 = 200) {
  train <- data.frame(replicate(10,runif(nsamples1)))
  test <- data.frame(replicate(10,runif(nsamples2)))
  noise <- rnorm(nsamples1)
  return(list(train, test, noise))
}


N<- 20
size_vec <- c(10,50,100,200,500,1000,2000,5000,10000)
RMSE_rf_size <- array(0, dim=c(N,9))

RMSE_poly_size <- array(0, c(N,9))
CI_poly_size <- array(0, c(N,9))
covg_poly_size <- array(0, c(N,9))

RMSE_exp_size <- array(0, c(N,9))
CI_exp_size <- array(0, c(N,9))
covg_exp_size <- array(0, c(N,9))

RMSE_cu_size <- array(0, c(N,9))
CI_cu_size <- array(0, c(N,9))
covg_cu_size <- array(0, c(N,9))


for (iter in seq(1,N,1)) {
  for (i in seq(1,9,1)) {
    size <- size_vec[i]
    eta <- 1
    list[train,test,noise] <- generate_diff_data(size)
    y <- func(train, eta = 1) + noise
    rf<- randomForest(x=train, y=y)
    pred <- predict(rf, test)
    y_actual <- func(test, eta=eta)
    RMSE_rf_size[iter,i] <- calc_rmse(y_actual, pred)

    bart_machine <- bartMachine(train, y, prior_name = "poly_splits", beta=2)
    pred <- predict(bart_machine, test)
    RMSE_poly_size[iter,i] <- calc_rmse(y_actual, pred)
    CI <- calc_credible_intervals(bart_machine, test)
    CI_poly_size[iter,i] <- mean(CI[,2])-mean(CI[,1])
    covg_poly_size[iter,i] <- get_coverage(CI, y_actual)
    
    bart_machine <- bartMachine(train, y, prior_name = "exp_splits", Gamma=5)
    pred <- predict(bart_machine, test)
    RMSE_exp_size[iter, i] <- calc_rmse(y_actual, pred)
    CI <- calc_credible_intervals(bart_machine, test)
    CI_exp_size[iter,i] <- mean(CI[,2])-mean(CI[,1])
    covg_exp_size[iter,i] <- get_coverage(CI, y_actual)
    
    bart_machine <- bartMachine(train, y, prior_name = "cond_unif", lam=0.01)
    pred <- predict(bart_machine, test)
    RMSE_cu_size[iter, i] <- calc_rmse(y_actual, pred)
    CI <- calc_credible_intervals(bart_machine, test)
    CI_cu_size[iter,i] <- mean(CI[,2])-mean(CI[,1])
    covg_cu_size[iter,i] <- get_coverage(CI, y_actual)
  }
}

save(RMSE_rf_size, file= "RMSE_rf_size.Rdata")
save(RMSE_poly_size, file = "RMSE_poly_size.Rdata")
save(CI_poly_size, file= "CI_poly_size.Rdata")
save(covg_poly_size, file="covg_poly_size.Rdata")
save(RMSE_exp_size, file = "RMSE_exp_size.Rdata")
save(CI_exp_size, file= "CI_exp_size.Rdata")
save(covg_exp_size, file="covg_exp_size.Rdata")
save(RMSE_cu_size, file="RMSE_cu_size.Rdata")
save(CI_cu_size, file= "CI_cu_size.Rdata")
save(covg_cu_size, file="covg_cu_size.Rdata")

#RMSE plot
plot(size_vec, seq(0.5,4.5,0.5), type = "n", xlab =  expression(n), ylab = "RMSE")
for (iter in seq(1,20,1)) {
  points(size_vec, RMSE_rf_size[iter,],col="black")
  points(size_vec,RMSE_poly_size[iter,],col="red")
  points(size_vec,RMSE_cu_size[iter,],col="green")
  points(size_vec,RMSE_exp_size[iter,],col="blue")
}
legend(0, 0.8, legend=c(expression(paste(beta, "=2")),expression(paste(lambda, "= 0.01")),expression(paste(Gamma, "=5")), "random forest"),
       col=c("red", "green", "blue", "black"), cex=0.7, text.width = 0.15, lty = 1)


#covg plot
plot(size_vec, seq(9,100,11), type = "n", xlab =  expression(n), ylab = "% Coverage of Credible Interval")
for (iter in seq(1,20,1)) {
points(size_vec,100*covg_poly_size[iter,],col="red")
points(size_vec,100*covg_cu_size[iter,],col="green")
points(size_vec,100*covg_exp_size[iter,],col="blue")
}
legend(0, 2, legend=c(expression(paste(beta, "=2")),expression(paste(lambda, "= 0.01")),expression(paste(Gamma, "=5"))),
       col=c("red", "green", "blue"), cex=0.7, text.width = 0.15, lty = 1)

#CI width plot
plot(size_vec, seq(2,6,0.5), type = "n", xlab =  expression(n), ylab = "Width of Credible Interval")
for (iter in seq(1,20,1)) {  
  points(size_vec,CI_poly_size[iter,],col="red")
  points(size_vec,CI_cu_size[iter,],col="green")
  points(size_vec,CI_exp_size[iter,],col="blue")
}
legend(0, 2, legend=c(expression(paste(beta, "=2")),expression(paste(lambda, "= 0.01")),expression(paste(Gamma, "=5"))),
       col=c("red", "green", "blue"), cex=0.7, text.width = 0.15, lty = 1)