library(devtools)
install_github("swager/randomForestCI")

library(randomForest)
library(randomForestCI)

get_rf_CI <- function(rf,test){
  infjack_rf = randomForestInfJack(rf, test, calibrate = TRUE)
  l = dim(infjack_rf)[1]
  CI <- array(0,c(l,2))
  CI[,1] <- infjack_rf$y.hat-1.96*sqrt(infjack_rf$var.hat)
  CI[,2] <- infjack_rf$y.hat+1.96*sqrt(infjack_rf$var.hat)
  return(CI)
}


###specific example of difference
list[train,test,noise] <- generate_diff_data(200)
y <- func(train, eta=0.5) + noise
y_actual <- func(test, eta=0.5)

rf = randomForest(train, y, keep.inbag = TRUE)
rf_CI <- get_rf_CI(rf, test)
bart_model <- bartMachine(train, y, prior_name="poly_splits",beta=2)
bart_CI <- calc_credible_intervals(bart_model, test)

par(mfrow=c(1,2))
plot(seq(7,25,1), seq(7,25,1), type = "n", xlab =  "Actual response", ylab = "Predicted Response")
for (i in seq(1,length(y_actual),1)) {
  segments(y_actual[i], rf_CI[i,1], y_actual[i], rf_CI[i,2] , col = "black",pch=19)	
  }
abline(a=0, b=1, col='red')
plot(seq(7,27,1), seq(7,27,1), type = "n", xlab =  "Actual response", ylab = "Predicted Response")
for (i in seq(1,length(y_actual),1)) {
  segments(y_actual[i], bart_CI[i,1],y_actual[i],bart_CI[i,2],col="black",pch=19, lwd=0.6)
}
abline(a=0, b=1, col='red')


###now repeat 20 times for different sized datasets and plot coverage and normalized width
###rf CI require O(n) bootstrap samples to stabilise the error which is bad
N<- 20
size_vec <- c(10,50,100,200,500,1000,2000)
bart_CI_width <- array(0,c(N,7))
bart_CI_covg <- array(0,c(N,7))
rf_CI_width <- array(0,c(N,7))
rf_CI_covg <- array(0,c(N,7))
for (iter in seq(1,N,1)) {
  for (i in seq(1,length(size_vec),1)) {
    size <- size_vec[i]
    eta <- 1/2
    list[train,test,noise] <- generate_diff_data(size)
    y <- func(train, eta = eta) + noise
    y_actual <- func(test, eta=eta)
    rf<- randomForest(x=train, y=y, keep.inbag=TRUE)
    rf_CI <- get_rf_CI(rf, test)
    rf_CI_width[iter,i] <- 2*(mean(rf_CI[,2]-rf_CI[,1]))/(mean(rf_CI[,2]+rf_CI[,1]))
    rf_CI_covg[iter,i] <- get_coverage(rf_CI,y_actual)
    
    bart_machine <- bartMachine(train, y)
    bart_CI <- calc_credible_intervals(bart_machine, test)
    bart_CI_width[iter,i] <- 2*(mean(bart_CI[,2]-bart_CI[,1]))/(mean(bart_CI[,2]+bart_CI[,1]))
    bart_CI_covg[iter,i] <- get_coverage(bart_CI,y_actual)
  }
}


par(mfrow=c(1,2))
plot(size_vec, seq(0,0.72,0.12), type = "n", xlab =  "training size", ylab = "Normalised width of 95% Credible Interval")
for (i in seq(1,length(size_vec),1)) {
  points(size_vec[i],mean(bart_CI_width[,i]),col="red")
  points(size_vec[i],mean(rf_CI_width[,i]),col="black")
}
legend(0, 2.5, legend=c("random forest",expression(paste(beta, "=2"))),
       col=c("black","red"), cex=0.7, text.width = 0.15, lty = 1)
plot(size_vec, seq(0.1,1,0.15), type = "n", xlab =  "training size", ylab = "Coverage of 95% Credible Interval")
for (i in seq(1,length(size_vec),1)) {
  points(size_vec[i],mean(bart_CI_covg[,i]),col="red")
  points(size_vec[i],mean(rf_CI_covg[,i]),col="black")
}
legend(0, 2.5, legend=c("random forest",expression(paste(beta, "=2"))),
       col=c("black","red"), cex=0.7, text.width = 0.15, lty = 1)