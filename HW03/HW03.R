########## Problem 1

##### Part a

### Load in the data

data_1 <- read.csv("~/Documents/STAT616/HW03/tire.csv")
data_1 <- data_1[,-1]
value <- c(data_1[,1], data_1[,2], data_1[,3], data_1[,4], data_1[,5])
type <- c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10))
data_1 <- data.frame(type = type, value = value)

### Build the regression model

lm_1 <- lm(value ~ type, data = data_1)
summary(lm_1)
anova(lm_1)

### Summary

# Based on the F-statistic of 7.8145 (which is greater than one), I can state that there is a statistically significant difference between the tire brands.

# The assumptions I used are the errors are iid Normal(0, sigma squared) and Equality of Variances.
# Show assumptions with QQ Plot and Bartlett test for equal variance

##### Part b

data_1a <- data_1[1:10, 2]
data_1b <- data_1[11:20, 2]
data_1c <- data_1[21:30, 2]
data_1d <- data_1[31:40, 2]
data_1e <- data_1[41:50, 2]

t.test(data_1a)
t.test(data_1b)
t.test(data_1c)
t.test(data_1d)
t.test(data_1e)

### Construct the ANOVA model

anova_1 <- aov(value ~ type, data = data_1)

### Construct the Tukey confidence intervals

TukeyHSD(anova_1)

### Construct the Bonferroni confidence intervals

confint(anova_1)

##### Part c



########## Problem 2

##### Part a

# t ^ bt

##### Part b

# 


























