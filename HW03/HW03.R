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

# The assumptions I used are IID, Homogeneity and Normality of Errors.

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



########## Problem 2

##### Part a

# t ^ bt

##### Part b

# 


























