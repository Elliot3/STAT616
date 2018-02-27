########## Problem 1

### Load in the data

data_1 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/tire.csv")
data_1 <- data_1[,-1]

##### Part a

### Build the regression model, regression summary and ANOVA table

lm_1 <- lm(A ~ ., data = data_1)
summary(lm_1)
anova(lm_1)

### Summary

# Because all of the F-statistics in the ANOVA table are less then one, we can 
