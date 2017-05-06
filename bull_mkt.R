# Three investors with different priors
# are betting that a market exhibits
# a bullish behavior. The market is indeed
# bullish (goes up 60% of the time).
# All their probabilities converge to 
# 1 using Bayes Rule.

# the following line allowed me to save
# a seed that makes the probabilities to
# look bouncy
# rs <- .Random.seed

f <- 1 # number of times the market is up
x <- c(0.1, 0.5, 0.9) # prior probability
pr <- matrix(data = x, nrow = 1)
.Random.seed <- rs

price <- 50
st <- vector()

for(t in 1:200){
        # 1 represents "market up", 0 represents "market down"
        s <- sample(c(0,1), 1, prob = c(0.4,0.6), replace = TRUE)
        f <- f + s
        
        x <- ((f/t)*x)/(((f/t)*x) + 0.49*(1-x))
        
        pr <- rbind(pr, x)
        
        price <- price + 2*(s-1/2)
        st <- c(st, price)
}

row.names(pr) <- NULL
pr <- as.data.frame(pr)

png("/home/rogelio/Desktop/datasciencecoursera/lol.png")
par(mfrow = c(2,1))
with(pr, plot(V1, type = "p", col = "black",
             ylab = "probabilities", xlab = "days"))
with(pr, lines(V2, col = "red", type = "p"))
with(pr, lines(V3, col = "blue", type = "p"))
legend("bottomright", lty = c(1,1,1), col = c("black","red","blue"), 
       legend = c("P = 0.1", "P = 0.5", "P = 0.9"))

plot(st, type = "l", ylab = "price", xlab = "days")
dev.off()