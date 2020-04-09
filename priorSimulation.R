options(java.parameters = "-Xmx5000m")
library("bartMachine")
set_bart_machine_num_cores(1)
library(randomForest)
library(gsubfn) 
library(ggplot2)

func <- function(X, eta = 0.1) {
  5*sin(pi*X[, 1]*X[, 2])+8*((X[, 3]-0.5)**2)+5*X[, 1]*X[, 2]*X[, 3]+6*exp(X[, 4]*X[, 5])+7*X[, 5]**eta
}

generate_data <- function(nsamples = 200) {
  train <- data.frame(replicate(10,runif(nsamples)))
  test <- data.frame(replicate(10,runif(nsamples)))
  noise <- rnorm(nsamples)
  return(list(train, test, noise))
}

generate_diff_data <- function(nsamples1, nsamples2 = 200) {
  train <- data.frame(replicate(10,runif(nsamples1)))
  test <- data.frame(replicate(10,runif(nsamples2)))
  noise <- rnorm(nsamples1)
  return(list(train, test, noise))
}

calc_rmse <- function(y_actual, y_pred) {
  sqrt(mean((y_actual-y_pred)**2))
}

get_coverage <- function(CI, y) {
  #CI is df of credible intervals/ prediction intervals
  y_in_ppi = y >= CI[,1] & y <= CI[,2]
  prop_ys_in_ppi = sum(y_in_ppi) / length(y_in_ppi)
}

RMSE_rf <- array(0, dim=c(N,10))

RMSE_poly <- array(0, c(N,3,10))
CI_poly <- array(0, c(N,3,10))
covg_poly <- array(0, c(N,3,10))

RMSE_exp <- array(0, c(N,3,10))
CI_exp <- array(0, c(N,3,10))
covg_exp <- array(0, c(N,3,10))

RMSE_cu <- array(0, c(N,3,10))
CI_cu <- array(0, c(N,3,10))
covg_cu <- array(0, c(N,3,10))

N <- 20
beta_vec <- c(1,2,3)
gamma_vec <- c(3, 5, 10)
lam_vec <- c(0.01, 0.1, 0.5)
for (iter in seq(1,N,1)) {
  list[train,test,noise] <- generate_data()
  for (i in seq(1,10,1)){
    eta = i/10
    y <- func(train, eta = eta) + noise
    rf<- randomForest(x=train, y=y)
    pred <- predict(rf, test)
    y_actual <- func(test, eta=eta)
    RMSE_rf[iter,i] <- calc_rmse(y_actual, pred)
    for (j in seq(1,3,1)) {
      b <- beta_vec[j]
      g <- gamma_vec[j]
      lam <- lam_vec[j]
      
      bart_machine <- bartMachine(train, y, prior_name = "poly_splits", beta=b)
      pred <- predict(bart_machine, test)
      RMSE_poly[iter,j,i] <- calc_rmse(y_actual, pred)
      CI <- calc_credible_intervals(bart_machine, test)
      CI_poly[iter,j,i] <- 2*(mean(CI[,2])-mean(CI[,1]))/(mean(CI[,2])+mean(CI[,1]))
      covg_poly[iter,j,i] <- get_coverage(CI, y_actual)
      
      bart_machine <- bartMachine(train, y, prior_name = "exp_splits", Gamma=g)
      pred <- predict(bart_machine, test)
      RMSE_exp[iter,j, i] <- calc_rmse(y_actual, pred)
      CI <- calc_credible_intervals(bart_machine, test)
      CI_exp[iter,j,i] <- 2*(mean(CI[,2])-mean(CI[,1]))/(mean(CI[,2])+mean(CI[,1]))
      covg_exp[iter,j,i] <- get_coverage(CI, y_actual)
      
      bart_machine <- bartMachine(train, y, prior_name = "cond_unif", lam=lam)
      pred <- predict(bart_machine, test)
      RMSE_cu[iter,j, i] <- calc_rmse(y_actual, pred)
      CI <- calc_credible_intervals(bart_machine, test)
      CI_cu[iter,j,i] <- 2*(mean(CI[,2])-mean(CI[,1]))/(mean(CI[,2])+mean(CI[,1]))
      covg_cu[iter,j,i] <- get_coverage(CI, y_actual)
    }
  }
}


save(RMSE_rf, file= "RMSE_rf.Rdata")
save(RMSE_poly, file = "RMSE_poly.Rdata")
save(CI_poly, file= "CI_poly.Rdata")
save(covg_poly, file="covg_poly.Rdata")
save(RMSE_exp, file = "RMSE_exp.Rdata")
save(CI_exp, file= "CI_exp.Rdata")
save(covg_exp, file="covg_exp.Rdata")
save(RMSE_cu, file="RMSE_cu.Rdata")
save(CI_cu, file= "CI_cu.Rdata")
save(covg_cu, file="covg_cu.Rdata")


#beta plot
par(mfrow=c(2,2))
x <- seq(0.1,1,0.1)
plot(seq(0,1,0.1), seq(0.5,2.6,0.21), type = "n", xlab =  expression(eta), ylab = "RMSE")
for (iter in seq(1,20,1)) {
  points(x,RMSE_poly[iter,2,],col="red")
  points(x,RMSE_rf[iter,],col="black")
}
legend(0, 0.8, legend=c(expression(paste(beta, "=2")),"random forest"),
       col=c("red", "black"), cex=0.7, text.width = 0.15, lty = 1)

#Gamma plot
x <- seq(0.1,1,0.1)
plot(seq(0,1,0.1), seq(0.5,2.6,0.21), type = "n", xlab =  expression(eta), ylab = "RMSE")
for (iter in seq(1,20,1)) {
  points(x,RMSE_exp[iter,2,],col="red")
  points(x,RMSE_rf[iter,],col="black")
}
legend(0, 0.8, legend=c(expression(paste(Gamma, "=5")),"random forest"),
       col=c("red", "black"), cex=0.7, text.width = 0.15, lty = 1)

#cu plot
x <- seq(0.1,1,0.1)
plot(seq(0,1,0.1), seq(0.5,2.6,0.21), type = "n", xlab =  expression(eta), ylab = "RMSE")
for (iter in seq(1,20,1)) {
  points(x,RMSE_cu[iter,1,],col="red")
  points(x,RMSE_rf[iter,],col="black")
}
legend(0, 0.8, legend=c(expression(paste(lambda, "=0.01")),"random forest"),
       col=c("red", "black"), cex=0.7, text.width = 0.15, lty = 1)

#mean RMSE plot
x <- seq(0.1,1,0.1)
plot(seq(0,1,0.1), seq(0.5,2.6,0.21), type = "n", xlab =  expression(eta), ylab = "mean RMSE")
for (i in seq(1,9,1)) {
  points(x[i],mean(RMSE_rf[,i]),col="black")
  points(x[i],mean(RMSE_cu[,1,i]),col="red")
  points(x[i],mean(RMSE_poly[,1,i]),col="blue")
  points(x[i],mean(RMSE_exp[,1,i]),col="green")
}
legend(0, 2.5, legend=c("random forest",expression(paste(lambda, "=0.01")),expression(paste(beta, "=2")),expression(paste(Gamma, "=5"))),
       col=c("black","red", "blue", "green"), cex=0.7, text.width = 0.15, lty = 1)


#CI covg plot and width plot
par(mfrow=c(1,2))
x <- seq(0.1,1,0.1)
plot(seq(0,1,0.1), seq(60,100,4), type = "n", xlab =  expression(eta), ylab = "% Coverage of 95% Credible Interval")
for (iter in seq(1,20,1)) {
  points(x,100*covg_poly[iter,2,],col="red")
  points(x,100*covg_cu[iter,1,],col="green")
  points(x,100*covg_exp[iter,2,],col="blue")
}
legend(0, 70, legend=c(expression(paste(beta, "=2")),expression(paste(lambda, "= 0.01")),expression(paste(Gamma, "=5"))),
       col=c("red", "green", "blue"), cex=0.7, text.width = 0.15, lty = 1)

x <- seq(0.1,1,0.1)
plot(seq(0,1,0.1), seq(1.5,5.5,0.4), type = "n", xlab =  expression(eta), ylab = "Normalised width of 95% Credible Interval")
for (iter in seq(1,20,1)) {
points(x,CI_poly[iter,2,],col="red")
points(x,CI_cu[iter,1,],col="green")
points(x,CI_exp[iter,2,],col="blue")
}
legend(0, 2.5, legend=c(expression(paste(beta, "=2")),expression(paste(lambda, "= 0.01")),expression(paste(Gamma, "=5"))),
      col=c("red", "green", "blue"), cex=0.7, text.width = 0.15, lty = 1)

#load("C:/Users/Asus 2/bartMachine/bartMachine/RMSE_poly.Rdata")
#load("C:/Users/Asus 2/bartMachine/bartMachine/RMSE_rf.Rdata")