library(UsingR)

data(father.son)

x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n*B, replace = TRUE),B,n)

medians <- apply(resamples, 1, median)

df <- data.frame(x = medians)

library(ggplot2)

ggplot(df, aes(x = df$x)) + geom_histogram()

# bootstrapped quantiles
quantile(medians, c(.025, .975))
