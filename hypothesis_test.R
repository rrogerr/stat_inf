# A respiratory disturbance index (RDI) greater than 30 events/hour is 
# considered a symptom of severe sleep disordered breathing (SDB).
#
# A sample of 100 overweight subjects gave an average RDI of 32 with
# standard deviation 10.
#
# H0: The mean RDI of the population of all overweight people is
#     mu = 30.
# Ha: The mean RDI of the population of all overweight people is 
#     mu > 30

# We assume mu = 30 (H0) and calculate the 95% quantile. If X = 32
# lies above that quantile, we reject H0.

(32 - 30)/(10/sqrt(100)) > qnorm(.95, mean = 0, 10/sqrt(100))

# Assume that the mean and the standard deviation shown above were 
# obtained from a sample of 16 subjects. In that case it's better
# to use the quantiles from the t-distribution.

(32-30)/(10/sqrt(16)) > qt(.95, 15)

# We can't reject the null hypothesis under that assumption.



# Example taken from here:
# https://onlinecourses.science.psu.edu/statprogram/node/139

# An engineer measured the hardness of 25 pieces of ductile iron 
# that were subcritically annealed. The resulting data is:

hard <- c(170, 167, 174, 179, 179, 156,	163, 156, 187, 156, 
          183, 179, 174, 179, 170, 156,	187, 179, 183, 174, 
          187, 167, 159, 170, 179)

# The engineer hypothesized that the mean of all such ductile
# iron pieces is greater than 170 (Ha).

mu <- mean(hard)
s <- sd(hard)

(mu-170)/(s/sqrt(25)) > qt(.95, df = 24)

# There is not sufficient evidence to adopt (Ha).