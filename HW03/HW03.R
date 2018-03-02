library(pairwiseCI)
library(lme4)



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
lm_1_summ <- summary(lm_1)
lm_1_anova <- anova(lm_1)

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
c(left_1A, right_1A)

### 1B - Confidence Interval

data_1B <- data_1[11:20,]
n <- length(data_1B$value)
mean_1B <- mean(data_1B$value)
sd_1B <- sd(data_1B$value)

error_1B <- qt(0.975, df = n - 1)*((sd_1B)/sqrt(n))

left_1B <- mean_1B - error_1B
right_1B <- mean_1B + error_1B
c(left_1B, right_1B)

### 1C - Confidence Interval

data_1C <- data_1[21:30,]
n <- length(data_1C$value)
mean_1C <- mean(data_1C$value)
sd_1C <- sd(data_1C$value)

error_1C <- qt(0.975, df = n - 1)*((sd_1C)/sqrt(n))

left_1C <- mean_1C - error_1C
right_1C <- mean_1C + error_1C
c(left_1C, right_1C)

### 1D - Confidence Interval

data_1D <- data_1[31:40,]
n <- length(data_1D$value)
mean_1D <- mean(data_1D$value)
sd_1D <- sd(data_1D$value)

error_1D <- qt(0.975, df = n - 1)*((sd_1D)/sqrt(n))

left_1D <- mean_1D - error_1D
right_1D <- mean_1D + error_1D
c(left_1D, right_1D)

### 1E - Confidence Interval

data_1E <- data_1[41:50,]
n <- length(data_1E$value)
mean_1E <- mean(data_1E$value)
sd_1E <- sd(data_1E$value)

error_1E <- qt(0.975, df = n - 1)*((sd_1E)/sqrt(n))

left_1E <- mean_1E - error_1E
right_1E <- mean_1E + error_1E
c(left_1E, right_1E)



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



# See hand-written...



########## Problem 3

# t = 5
# b = 4
# n = bt = 20

# 1) For each column (block), starting with the first, randomly permute t_1 to t_5 (representing treatments) without replacement and assign them to the cells in the order specified.
# 2) Now I have a 5x4 table with the treatment types assigned to each cell randomly. The rows represent the subjects and the columns represent the blocks.



########## Problem 4

# No. Due to the fact that observational studies are not randomized due to self-selection, a Complete Randomized Block design cannot be observational.



########## Problem 5



### Load in the data

data_5 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/blood.csv")

##### Part a

temp_dev <- rep(c("dev1", "dev2", "dev3"), each = 15)
temp_vals <- c(data_5[,2], data_5[,3], data_5[,4])

data_5a <- data.frame(device = temp_dev, value = temp_vals)

t_val <- abs(qt(0.025, 2))
std_err <- 0.014

### Build the regression model

lm_5a <- lm(value ~ device, data = data_5a)
summary(lm_5a)
anova(lm_5a)



##### Part b - Do the same stuff for Part b as Part a

temp_doc <- rep(c("doc1", "doc2", "doc3"), each = 15)
temp_vals <- c(data_5[,5], data_5[,6], data_5[,7])

data_5b <- data.frame(doctor = temp_doc, value = temp_vals)

### Build the regression model

lm_5b <- lm(value ~ doctor, data = data_5b)
summary(lm_5b)
anova(lm_5b)

##### Part c

# Finish up later


########## Problem 6



##### Part a

# Completely Randomized Block. Yes, because I would expect a different results on the training for recent graduates vs. those that have had several years elapse
# since they graduates. I think that time since graduation is a good blocking methodology because I would expect more experienced auditors to get better scores.



##### Part b

# Y_ij = Y_bar_.. + (Y_bar_i. - Y_bar_..) + (Y_bar_.j - Y_bar_..) + (Y_ij - Y_bar_i. - Y_bar_.j + Y_bar_..)
# Y_ij = mu + alpha_i + Beta_j + epsilon_ij



##### Part c

data_6 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/audit_b10.csv")

data_6$block <- as.factor(data_6$block)
data_6$method <- as.factor(data_6$method)

lm_6c <- lm(score ~ block + method, data = data_6)
summary(lm_6c)
anova(lm_6c)



##### Part d

# Since the p-value for the blocks is 0.0001, we can reject the Ho that the blocks are all the same. So yes, blocking was needed.



##### Part e

# Based on the p-value for the methods is significantly less than 0, we may reject the Ho at the methods are all the same.

anova_6c <- aov(score ~ method, data = data_6)
TukeyHSD(anova_6c)

# The assumptions I used are: (1) Errors are iid Normal(0, sigma-squared) and (2) the variances are equal for all tire brands.

### Test my assumptions

resids_6 <- residuals(lm_6c)

qqnorm(resids_6)
qqline(resids_6)

bartlett.test(score ~ method, data = data_6)



##### Part f

# Finish up later




##### Part g

# Finish up later




########## Problem 7

data_7 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW03/resistor.csv")

data_7$plate <- as.factor(data_7$plate)
data_7$shape <- rep(c("A", "B", "C", "D"), each = 4)
data_7$shape <- as.factor(data_7$shape)

lm_7 <- lm(noise ~ plate + shape, data = data_7)
summary(lm_7)
anova(lm_7)

anova_7 <- aov(noise ~ plate + shape, data = data_7)
TukeyHSD(anova_7)



########## STAT616 - Problem 3

temp_block <- c("1-1", "2-1","3-1", "4-1", "5-1", "1-2", "2-2", "3-2", "4-2")
temp_aarau <- c(0.772, 0.744, 0.767, 0.745, 0.725, 0.844, 0.831, 0.867, 0.859)
temp_karlsruhe <- c(1.186, 1.151, 1.322, 1.339, 1.200, 1.402, 1.365, 1.537, 1.559)
temp_lehigh <- c(1.061, 0.992, 1.063, 1.062, 1.065, 1.178, 1.037, 1.086, 1.052)
temp_cardiff <- c(1.025, 0.905, 0.930, 0.899, 0.871, 1.004, 0.853, 0.858, 0.805)

data_3 <- data.frame(blocks = temp_block, aarau = temp_aarau, karlsruhe = temp_karlsruhe,
                     lehigh = temp_lehigh, cardiff = temp_cardiff)

temp_blocks <- rep(data_3[,1], times = 4)
temp_labs <- rep(names(data_3[-1]), each = 9)
temp_vals <- c(data_3[,2], data_3[,3], data_3[,4], data_3[,5])

data_3 <- data.frame(block = temp_blocks, type = temp_labs, value = temp_vals)

##### Part 1 - contr.treatment (default)



### Build the regression model - contr.treatment (Dummy Coding) - alpha1 = 0

lm_3a <- lm(value ~ block + type, data = data_3)
summary(lm_3a)
anova(lm_3a)

##### Part 2 - contr.sum (Zero Sum Coding) - alpha1 + alpha2 + ... = 0



((0.744+1.151+.992+.905) / 4) - ((0.772+1.186+1.061+1.025) / 4)






