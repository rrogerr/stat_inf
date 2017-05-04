# How sample mean and variance converge to population mean and 
# variance respectively.

# die roll, mu = 3.5, sd = 1.707...
nosim <- 10000          #number of simulations
n <- 10                 #number of measurements per simulation 
m <- matrix(sample(1:6, nosim*n, replace = TRUE), nosim)

mean(apply(m, 1, mean))
sd(apply(m, 1, mean))*sqrt(10)