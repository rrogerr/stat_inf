# Permutation tests in R

data("InsectSprays")

a <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- a$count
group <- as.character(a$spray)

# We assume that there is no difference between the effects of
# sprays B or C, in other words the labels B or C have no effect
# on the count:
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])

# with all the labels in place

observedStat <- testStat(y, group)

# now we are going to randomize to eliminate the association
# of each label with its counts.
permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))

# 
observedStat
mean(permutations > observedStat)
