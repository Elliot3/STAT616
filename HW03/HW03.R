library(pairwiseCI)



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



### Standard Confidence Intervals

### 1A - Confidence Interval

data_1A <- data_1[1:10,]
n <- length(data_1A$value)
mean_1A <- mean(data_1A$value)
sd_1A <- sd(data_1A$value)

error_1A <- qt(0.975, df = n - 1)*((sd_1A)/sqrt(n))

left_1A <- mean_1A - error_1A
right_1A <- mean_1A + error_1A

### 1B - Confidence Interval

data_1B <- data_1[11:20,]
n <- length(data_1B$value)
mean_1B <- mean(data_1B$value)
sd_1B <- sd(data_1B$value)

error_1B <- qt(0.975, df = n - 1)*((sd_1B)/sqrt(n))

left_1B <- mean_1B - error_1B
right_1B <- mean_1B + error_1B

### 1C - Confidence Interval

data_1C <- data_1[21:30,]
n <- length(data_1C$value)
mean_1C <- mean(data_1C$value)
sd_1C <- sd(data_1C$value)

error_1C <- qt(0.975, df = n - 1)*((sd_1C)/sqrt(n))

left_1C <- mean_1C - error_1C
right_1C <- mean_1C + error_1C

### 1D - Confidence Interval

data_1D <- data_1[31:40,]
n <- length(data_1D$value)
mean_1D <- mean(data_1D$value)
sd_1D <- sd(data_1D$value)

error_1D <- qt(0.975, df = n - 1)*((sd_1D)/sqrt(n))

left_1D <- mean_1D - error_1D
right_1D <- mean_1D + error_1D

### 1E - Confidence Interval

data_1E <- data_1[41:50,]
n <- length(data_1E$value)
mean_1E <- mean(data_1E$value)
sd_1E <- sd(data_1E$value)

error_1E <- qt(0.975, df = n - 1)*((sd_1E)/sqrt(n))

left_1E <- mean_1E - error_1E
right_1E <- mean_1E + error_1E



### Bonferroni Confidence Intervals

### 1A - Confidence Interval

error_1A_bon <- qt(0.995, df = n - 1)*((sd_1A)/sqrt(n))

left_1A_bon <- mean_1A - error_1A_bon
right_1A_bon <- mean_1A + error_1A_bon

### 1B - Confidence Interval

error_1B_bon <- qt(0.995, df = n - 1)*((sd_1B)/sqrt(n))

left_1B_bon <- mean_1B - error_1B_bon
right_1B_bon <- mean_1B + error_1B_bon

### 1C - Confidence Interval

error_1C_bon <- qt(0.995, df = n - 1)*((sd_1C)/sqrt(n))

left_1C_bon <- mean_1C - error_1C_bon
right_1C_bon <- mean_1C + error_1C_bon

### 1D - Confidence Interval

error_1D_bon <- qt(0.995, df = n - 1)*((sd_1D)/sqrt(n))

left_1D_bon <- mean_1D - error_1D_bon
right_1D_bon <- mean_1D + error_1D_bon

### 1E - Confidence Interval

error_1E_bon <- qt(0.995, df = n - 1)*((sd_1E)/sqrt(n))

left_1E_bon <- mean_1E - error_1E_bon
right_1E_bon <- mean_1E + error_1E_bon

# The Bonferroni confidence intervals are much more conservative than our traditional t-statistic confidence intervals (ie its a wider range). I prefer
# the traditional confidence interval due to the fact that with Bonferroni we are less likely to reject Ho which means we would be more likely to commit
# a Type II error.



##### Part c



### Construct the ANOVA model

anova_1 <- aov(value ~ type, data = data_1)

### Construct the Tukey confidence intervals

TukeyHSD(anova_1)

### Construct the Bonferroni confidence intervals

pairwiseCI(value ~ type, data = data_1, conf.level = 0.99)

# The Bonferroni confidence intervals are much more conservative than our traditional t-statistic confidence intervals (ie its a wider range). I prefer
# the traditional confidence interval due to the fact that with Bonferroni we are less likely to reject Ho which means we would be more likely to commit
# a Type II error.



########## Problem 2



##### Part a (from HW #1, Problem 6)

# n = bt = # of observations
# (tn)! / (n!)^t



##### Part b

# (bt)! because there are bt total "slots" open for assignment



##### Part c

# ((bt)!)^2 because we are basically doing the same assignment twice, one for each factor



########## Problem 3

# t = 5
# b = 4
# n = bt = 20

# 1) For each column (block), starting with the first, randomly permute t_1 to t_5 (representing treatments) without replacement and assign them to the cells in the order specified.
# 2) Now I have a 5x4 table with the treatment types assigned to each cell
# 3) For our first data row (t_1 to t_5), randomly select a block and enter the data into each cell matching the treatment designated from step 1
# 4) Complete this process until all cells have data in them



########## Problem 4

# No. Due to the fact that observational studies are not randomized due to self-selection, a Complete Randomized Block design cannot be observational.



########## Problem 5

library(lme4)

### Load in the data

data_5 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/blood.csv")

##### Part a

temp_day <- rep(1:15, times = 3)
temp_doc <- rep(1:3, each = 15)
temp_vals <- c(data_5[,2], data_5[,3], data_5[,4])

data_5a <- data.frame(day = temp_day, doctor = temp_doc, value = temp_vals)

### Build the regression model

# lm_5a <- lm(value ~ (1|doctor), data = data_5a)
# summary(lm_5a)
# anova(lm_5a)



########## Problem 6



##### Part a

# Completely Randomized Block. No, because if there goal of the study is simply to "test three training methods in statistical sampling for auditing" then I don't
# think that blocking by time elapsed since college graduation is a very useful blocking feature. If they were testing explicitly how the different training options
# were affected by time since graduation, then I would think this is a good design. As it stands right now, the blocks seem arbitrary.



##### Part b

# Y_ij = Y_bar_.. + (Y_bar_i. - Y_bar_..) + (Y_bar_.j - Y_bar_..) + (Y_ij - Y_bar_i. - Y_bar_.j + Y_bar_..)



##### Part c

data_6 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/audit_b10.csv")






















