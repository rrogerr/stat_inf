# Law of Large Numbers

n <- 100

p <- cumsum(sample(0:1, n, replace = TRUE))/(1:100)

df <- data.frame(n = 1:n, p)

library(ggplot2)

pl <- ggplot(df, aes(n, p)) + geom_line()

pl