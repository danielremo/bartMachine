options(java.parameters = "-Xmx5000m")
library("bartMachine")
set_bart_machine_num_cores(1)
args(bartMachine)


func <- function(X, eta = 0.1) {
  5*sin(pi*X[, 1]*X[, 2])+8*((X[, 3]-0.5)**2)+5*X[, 1]*X[, 2]*X[, 3]+6*exp(X[, 4]*X[, 5])+X[, 5]**eta
}

X <- data.frame(replicate(10,runif(50)))
y <- func(X, eta=0.1)+rnorm(50)
bartMachine(X, y, prior_name = "cond_unif", c = 0.5)

"""
RMSE <- matrix(0, 3, 10)
for (eta in seq(0.1,1,0.1)){
  for (beta in c(1,2,3){
    bart_machine <- bartMachine(X, y, beta=beta)
  })
}

devtools::install_github("danielremo/bartMachine/bartMachine")



"""