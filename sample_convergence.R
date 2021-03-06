# How sample mean and variance converge to population mean and 
# variance respectively.

# die roll, mu = 3.5, sd = 1.707...
df <- data.frame(m = vector(), sd = vector(), n = vector())

nosim <- 1000

for(n in c(5, 10, 50, 100)){
        m <- matrix(sample(1:6, nosim*n, replace = TRUE), nosim)
                
        sd <- apply(m, 1, sd)
        m <- apply(m, 1, mean)
        n1 <- rep(n, times = length(m))
                
        df1 <- data.frame(m, sd, n1)
        df <- rbind(df, df1)
}



library(ggplot2)
h <- ggplot(df, aes(df$m)) + geom_histogram(binwidth = 0.2) +
        facet_grid(n1 ~ ., labeller = label_both)

df2 <- data.frame(m = vector(), sd = vector(), nosim = vector())

n <- 10

for(nosim in c(50, 100, 150)){
        m <- matrix(sample(1:6, nosim*n, replace = TRUE), nosim)
        
        sd <- apply(m, 1, sd)
        m <- apply(m, 1, mean)
        nosim1 <- rep(nosim, times = length(m))
        
        df1 <- data.frame(m, sd, nosim1)
        df2 <- rbind(df2, df1)
}

h1 <- ggplot(df2, aes(df2$m)) + geom_histogram(binwidth = 0.2) +
        facet_grid(nosim1 ~ ., labeller = label_both)

h1