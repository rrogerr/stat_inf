# "generate 40 exponentials" refers to generating 
# 40 random numbers from an exponential distribution.
# we are going to generate 40 exponentials a 1000 times

nosim <- 1000
lambda <- 0.2
n <- 40

m <- matrix(rexp(nosim*n, rate = lambda), nosim)
mn <- apply(m, 1, mean)

# equal to the theoretical standard deviation of the population
sd(mn)*sqrt(40)

df <- data.frame(mean = mn, exponential = m[,1])

library(reshape2)

df <- melt(df)

library(ggplot2)

# comparing theoretical vs. experimental means
a <- ggplot(df, aes(x = value)) + 
        geom_histogram(data = subset(df, df$variable == "mean"), binwidth = 0.1) + 
        geom_vline(aes(xintercept = mean(mn), colour = "sample mean")) + 
        geom_vline(aes(xintercept = 1/lambda, color = "population mean"))
a

# comparing sample sd vs. theoretical sd
b <- ggplot(df, aes(x = value)) + 
        geom_histogram(data = subset(df, df$variable == "mean"), binwidth = 0.1) + 
        geom_vline(aes(xintercept = mean(mn) - sd(mn), colour = "sample sd")) + 
        geom_vline(aes(xintercept = mean(mn) + sd(mn), colour = "sample sd")) +
        geom_vline(aes(xintercept = 1/lambda - (1/lambda)/sqrt(n), color = "population sd")) +
        geom_vline(aes(xintercept = 1/lambda + (1/lambda)/sqrt(n), color = "population sd"))
b

# comparing distribution of exponentials vs. means of exponentials
h <- ggplot(df, aes(x = value)) + 
        geom_histogram(data = subset(df, df$variable == "mean"), binwidth = 0.1, fill = "red") +
        geom_histogram(data = subset(df, df$variable == "exponential"), binwidth = 0.1, fill = "blue") 
h


#################################################################
#################################################################
#################################################################

# Tooth Growth

data("ToothGrowth")

qplot(supp,len,data=ToothGrowth, facets=~dose, 
      main="Tooth Growth by delivery method and dose (mg/day)",
      xlab="Delivery Method", ylab="Tooth length")+
        facet_grid(~dose, labeller = "label_value")+ 
        geom_boxplot(aes(fill = supp)) + 
        scale_fill_discrete(name = "Delivery Method",
                            breaks = c("OJ","VC"),
                            labels = c("Orange Juice", "Vitamine C"))

a <- subset(ToothGrowth, supp == "VC" & dose == 2)$len
b <- subset(ToothGrowth, supp == "OJ" & dose == 2)$len

t.test(subset(ToothGrowth, supp == "OJ" & dose == 0.5)$len, 
       subset(ToothGrowth, supp == "VC" & dose == 0.5)$len)

t.test(subset(ToothGrowth, supp == "OJ" & dose == 1)$len, 
       subset(ToothGrowth, supp == "VC" & dose == 1)$len)

t.test(subset(ToothGrowth, supp == "OJ" & dose == 2)$len, 
       subset(ToothGrowth, supp == "VC" & dose == 2)$len)

power.t.test(n = 10, delta = mean(b) - mean(a),
             sd = sqrt((var(a)+var(b))/2), alternative = "one.sided", type = "two.sample")