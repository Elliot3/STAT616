########## Workspace Prep ##########

## Load in the necessary libraries

library(lme4)



########## Problem 1 ##########



## Load in the data

data_1 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW04/blood.csv")

##### Part a #####

data_1a <- setNames(data.frame(matrix(NA, ncol = 2, nrow = 45)), c("doctor", "value"))
data_1a$doctor <- c(rep("doc1", times = 15), rep("doc2", times = 15), rep("doc3", times = 15))
data_1a$value <- c(data_1$doc1, data_1$doc2, data_1$doc3)

mu_doc <- mean(data_1a$value)

lm_1a <- lm(value ~ doctor, data = data_1a)
anova(lm_1a)
summary(lm_1a)

sigma_alpha_1a <- (248.163 - 1.784) / 15
sigma_epsilon_1a <- 1.784
se_mu_hat_1a <- sqrt((sigma_epsilon_1a + (15 * sigma_alpha_1a)) / 45)
ci_mu_hat_1a <- c(mu_doc - qt(0.975, df = 2) * se_mu_hat_1a, mu_doc + qt(0.975, df = 2) * se_mu_hat_1a)

##### Part b #####

lm_1b <- lmer(value ~ (1|doctor), data = data_1a, REML = FALSE)
summary(lm_1b)

sigma_alpha_1b <- 10.911
sigma_epsilon_1b <- 1.784
se_mu_hat_1b <- sqrt((sigma_epsilon_1b + (15 * sigma_alpha_1b)) / 45)
ci_mu_hat_1b <- c(mu_doc - qt(0.975, df = 2) * se_mu_hat_1b, mu_doc + qt(0.975, df = 2) * se_mu_hat_1b)

##### Part c #####

lm_1c <- lmer(value ~ (1|doctor), data = data_1a, REML = TRUE)
summary(lm_1c)

sigma_alpha_1c <- 16.425
sigma_epsilon_1c <- 1.784
se_mu_hat_1c <- sqrt((sigma_epsilon_1c + (15 * sigma_alpha_1c)) / 45)
ci_mu_hat_1c <- c(mu_doc - qt(0.975, df = 2) * se_mu_hat_1c, mu_doc + qt(0.975, df = 2) * se_mu_hat_1c)

##### Part d #####

# Results for 1a and 1c almost identical
# 1a and 1c are biased, 1b is unbiased
# 1b the CI is much tighter (higer accuracy) then other two
# Prefer 1b for the above reasons

##### Part e #####

data_1e <- setNames(data.frame(matrix(NA, ncol = 2, nrow = 45)), c("device", "value"))
data_1e$device <- c(rep("dev1", times = 15), rep("dev2", times = 15), rep("dev3", times = 15))
data_1e$value <- c(data_1$dev1, data_1$dev2, data_1$dev3)

mu_dev <- mean(data_1e$value)

## ANOVA

lm_1e_anova <- lm(value ~ device, data = data_1e)
anova(lm_1e_anova)
summary(lm_1e_anova)

sigma_alpha_1e_anova <- (0.007 - 88.082) / 15
sigma_alpha_1e_anova <- 0
sigma_epsilon_1e_anova <- 88.082
se_mu_hat_1e_anova <- sqrt((sigma_epsilon_1e_anova + (15 * sigma_alpha_1e_anova)) / 45)
ci_mu_hat_1e_anova <- c(mu_dev - qt(0.975, df = 42) * se_mu_hat_1e_anova, mu_dev + qt(0.975, df = 42) * se_mu_hat_1e_anova)

## ML

lm_1e_ml <- lmer(value ~ (1|device), data = data_1e, REML = FALSE)
summary(lm_1e_ml)

sigma_alpha_1e_ml <- 0
sigma_epsilon_1e_ml <- 82.21
se_mu_hat_1e_ml <- sqrt((sigma_epsilon_1e_ml + (15 * sigma_alpha_1e_ml)) / 45)
ci_mu_hat_1e_ml <- c(mu_dev - qt(0.975, df = 2) * se_mu_hat_1e_ml, mu_dev + qt(0.975, df = 2) * se_mu_hat_1e_ml)

## REML

lm_1e_reml <- lmer(value ~ (1|device), data = data_1e, REML = TRUE)
summary(lm_1e_reml)

sigma_alpha_1e_reml <- 0
sigma_epsilon_1e_reml <- 84.08
se_mu_hat_1e_reml <- sqrt((sigma_epsilon_1e_reml + (15 * sigma_alpha_1e_reml)) / 45)
ci_mu_hat_1e_reml <- c(mu_dev - qt(0.975, df = 2) * se_mu_hat_1e_reml, mu_dev + qt(0.975, df = 2) * se_mu_hat_1e_reml)



########## Problem 2 ##########



## Load in the data

data_2 <- read.table("~/Documents/Rice_University/Spring_2018/STAT616/HW04/moth.txt", header = TRUE)

## Interaction effects model

lm_interact_2 <- lm(count ~ location + trap + (location * trap), data = data_2)
summary(lm_interact_2)
anova(lm_interact_2)
confint(lm_interact_2)

## Diagnostics

plot(resid(lm_interact_2), xlab = "Index", ylab = "Residual Value", main = "Homoscedasticity of Residuals")
abline(h = 0, col = "red")

qqnorm(resid(lm_interact_2), main = "Residual QQ Plot")
qqline(resid(lm_interact_2), col = "red")

## Summary

# Blah blah
# Blah blah
# Blah blah

## No interaction effects model

lm_no_interact_2 <- lm(count ~ location + trap, data = data_2)
summary(lm_no_interact_2)
anova(lm_no_interact_2)
confint(lm_no_interact_2)

## Diagnostics

plot(resid(lm_no_interact_2), xlab = "Index", ylab = "Residual Value", main = "Homoscedasticity of Residuals")
abline(h = 0, col = "red")

qqnorm(resid(lm_no_interact_2), main = "Residual QQ Plot")
qqline(resid(lm_no_interact_2), col = "red")

## Summary

# Blah blah
# Blah blah
# Blah blah



########## Problem 3 ##########



## Load in the data

data_3 <- read.table("~/Documents/Rice_University/Spring_2018/STAT616/HW04/thick_guage.txt", header = TRUE)
value <- c(data_3[,2], data_3[,3], data_3[,4], data_3[,5], data_3[,6], data_3[,7])
oper <- c(rep("one", times = 20), rep("two", times = 20), rep("three", times = 20))
part <- rep(data_3[,1], times = 6)

df_3 <- data.frame(part, oper, value)

##### Part a #####

## format(scientific_value, scientific = FALSE)

lm_3a <- lm(value ~ oper + part + (oper * part), data = df_3)
summary(lm_3a)
anova(lm_3a)

sigma_epsilon_3a <- 0.0000022797
sigma_oper_part_3a <- (0.0000010066 - sigma_epsilon_3a) / 10
sigma_oper_part_3a <- 0
sigma_part_3a <- (0.0000005838 - sigma_oper_part_3a) / (10 * 3)
sigma_oper_3a <- (0.0000046167 - sigma_oper_part_3a) / (10 * 10)

# We can get valid estimates from the ANOVA table because we have balance!

##### Part b #####

## ML

lm_3b_ml <- lmer(value ~ (1|oper) + (1|part) + (1|(oper:part)), data = df_3, REML = FALSE)
summary(lm_3b_ml)

sigma_epsilon_3b_ml <- 0.000658
sigma_oper_part_3b_ml <- (0.0000000007321 - sigma_epsilon_3b_ml) / 10
sigma_oper_part_3b_ml <- 0
sigma_part_3b_ml <- (0.001312 - sigma_oper_part_3b_ml) / (10 * 3)
sigma_oper_3b_ml <- (0.0004251 - sigma_oper_part_3b_ml) / (10 * 10)

## REML

lm_3b_reml <- lmer(value ~ (1|oper) + (1|part) + (1|(oper:part)), data = df_3, REML = TRUE)
summary(lm_3b_reml)

sigma_epsilon_3b_reml <- 0.0006578
sigma_oper_part_3b_reml <- (0 - sigma_epsilon_3b_reml) / 10
sigma_oper_part_3b_reml <- 0
sigma_part_3b_reml <- (0.0013679 - sigma_oper_part_3b_reml) / (10 * 3)
sigma_oper_3b_reml <- (0.0004574 - sigma_oper_part_3b_reml) / (10 * 10)

## Comparisons

# The variance of the interact effect are 0 for all methods
# The sigma estimate for oper and part is close between ML and REML but much smaller for ANOVA
# Again, we know the ANOVA and ML are biased, while REML is unbiased

##### Part c #####

# According to the ANOVA table from Part a, there are no significant sources of variation; all F values are ~1.



########## Problem 4 ##########



## Load in the data

data_4 <- read.table("~/Documents/Rice_University/Spring_2018/STAT616/HW04/bloodpressure.txt", header = TRUE)

df_4 <- setNames(data.frame(matrix(NA, ncol = 2, nrow = 20)), c("treatment", "decrease"))
df_4$treatment <- c(rep("A", times = 10), rep("B", 10))
df_4$decrease <- c(data_4[, 3], data_4[, 6])

##### Part a #####

lm_4a <- lm(decrease ~ treatment, data = df_4)
anova(lm_4a)
summary(lm_4a)

# Due the p-value of 0.2025, I will not reject my null hypothesis that two samples are equal.







