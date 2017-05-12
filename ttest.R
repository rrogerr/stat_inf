# sleep data
# extra sleep hours with and 
# without a certain substance

data("sleep")

g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]

dif <- g1 - g2
mn <- mean(dif)
s <- sd(dif)
n <- 10

mn + c(-1,1)*qt(.975, n-1)*s/sqrt(n)
t.test(dif)
t.test(g1, g2, paired = TRUE)

###########################################################
###########################################################

# Two unmatched randomized samples.
# Systolic blood pressure of 8 oral contraceptive users
# vs. 21 controls

# users mean and standard deviation (mmHg)
c <- 132.86
sdc <- 15.34

# controls mean and standard deviation (mmHg)
p <- 127.44
sdp <- 18.23

# pooled variance estimate
sd <- sqrt((7*15.34^2+20*18.23^2)/(8+21-2))

# 95% confidence interval contains 0
c-p + c(-1,1)*qt(.975,27)*sd*(1/8+1/21)^.5