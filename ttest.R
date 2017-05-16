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

###########################################################
###########################################################

# In a study of emergency room waiting times, investigators 
# consider a new and the standard triage systems. To test 
# the systems, administrators selected 20 nights and 
# randomly assigned the new triage system to be used on 10 
# nights and the standard system on the remaining 10 
# nights. They calculated the nightly median waiting time 
# (MWT) to see a physician. The average MWT for the new 
# system was 3 hours with a variance of 0.60 while the 
# average MWT for the old system was 5 hours with a 
# variance of 0.68. Consider the 95% confidence interval 
# estimate for the differences of the mean MWT associated 
# with the new system. Assume a constant variance. What is 
# the interval? Subtract in this order (New System - Old System).

# new system
nm <- 3
nv <- 0.6
nn <- 10 # degrees of freedom + 1

# oldsystem
om <- 5
ov <- 0.68
on <- 10

sd <- sqrt(((nn-1)*0.6 + (on-1)*0.68)/(on+nn-2))

nm - om + c(-1,1)*qt(.975, on + nn - 2)*sd*(1/nn + 1/on)^.5

###########################################################
###########################################################

# Suppose that 18 obese subjects were randomized, 9 each, 
# to a new diet pill and a placebo. Subjects’ body mass 
# indices (BMIs) were measured at a baseline and again after 
# having received the treatment or placebo for four weeks. 
# The average difference from follow-up to the baseline 
# (followup - baseline) was −3 kg/m2 for the treated group 
# and 1 kg/m2 for the placebo group. The corresponding 
# standard deviations of the differences was 1.5 kg/m2 for 
# the treatment group and 1.8 kg/m2 for the placebo group. 
# Does the change in BMI over the four week period appear 
# to differ between the treated and placebo groups? Assuming 
# normality of the underlying data and a common population 
# variance, calculate the relevant *90%* t confidence 
# interval. Subtract in the order of (Treated - Placebo) 
# with the smaller (more negative) number first.

# treated
tm <- -3
tsd <- 1.5
tn <- 9 # degrees of freedom + 1

# placebo
pm <- 1
psd <- 1.8
pn <- 9

tm - pm + c(-1,1)*qt(.95, pn + tn - 2)*((psd^2)/pn + (tsd^2)/tn)^.5