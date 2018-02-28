########## Problem 1



##### Part a

### Load in the data

data_1 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/tire.csv")
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

# The assumptions I used are: (1) Errors are iid Normal(0, sigma-squared) and (2) the variances are equal for all tire brands.

### Test my assumptions

resids_1 <- residuals(lm_1)

qqnorm(resids_1)
qqline(resids_1)

bartlett.test(value ~ type, data = data_1)



##### Part b

### Construct the ANOVA model

anova_1 <- aov(value ~ type, data = data_1)

### Calculate the mean values per factor level

mean_vals_1 <- tapply(data_1$value, data_1$type, mean)

### Construct the Tukey confidence intervals

TukeyHSD(anova_1)

### Construct the Bonferroni confidence intervals

confint(anova_1)



##### Part c

# TBD



########## Problem 2



##### Part a (from HW #1, Problem 6)

# n = bt = # of observations
# (tn)! / (n!)^t



##### Part b

# (bt)! because there are bt total "slots" open for assignment



##### Part c

# ((bt)!)^2 because we are basically doing the same assignment twice, one for each factor



########## Problem 3




























