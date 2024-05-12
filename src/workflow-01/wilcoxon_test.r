# Sample data
group1 <- c(58729000, 59593000, 59959000, 60253000, 59429000)
group2 <- c(57925000, 58127000, 58175000, 58513000, 58145000)

# Perform the Wilcoxon rank sum test
test <- wilcox.test(group1, group2)

# Print the results
print(test)