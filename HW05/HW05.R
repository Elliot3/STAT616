########## Problem 1 ##########





## Load in the data

data <- c(2, 5, 5, 5, 2, 4, 4, 9, 5, 10, 6, 8, 7, 5, 4, 5, 3, 5, 6, 6, 3, 5, 2, 7, 10, 7, 6)

## Load in functions and packages

std_err <- function(x) sd(x)/sqrt(length(x))

## Set some data parameters

B <- 500
n <- length(data)

##### Part a #####

sd(data)
sqrt(mean(data))

# Poisson distribution has one parameter, theta, which represents both the EX and VarX



##### Part b #####



## Containers for statistics

sim_SD_1b <- numeric()
sim_sqrt_1b <- numeric()

## Perform the simulation

for (i in 1:B) {
    
    temp <- rpois(n = n, lambda = 5)
    sim_SD_1b[i] <- sd(temp)
    sim_sqrt_1b[i] <- sqrt(mean(temp))
    
}

## Harvest the standard errors of the results

se_SD_1b <- std_err(sim_SD_1b)
se_sqrt_1b <- std_err(sim_sqrt_1b)

# I prefer sqrt(mean(X)) method because it has a tighter bound and thus is more accurate/precise



##### Part c #####



## Containers for statistics

sim_SD_1c <- numeric()
sim_sqrt_1c <- numeric()

## Perform the simulation

for (i in 1:B) {
    
    temp <- sample(x = data, size = n, replace = TRUE)
    sim_SD_1c[i] <- sd(temp)
    sim_sqrt_1c[i] <- sqrt(mean(temp))
    
}

## Harvest the standard errors of the results

se_SD_1c <- std_err(sim_SD_1c)
se_sqrt_1c <- std_err(sim_sqrt_1c)

# I prefer sqrt(mean(X)) method because it has a tighter bound and thus is more accurate/precise



##### Part d #####



# The parametric SEs are larger, implying a larger spread. I prefer the parametric method because it
# is representative of the true distribution, not a sample-based estimate of the true distribution.





########## Problem 2 ##########





## Load in the data

data_2 <- read.csv("~/Documents/Rice_University/Spring_2018/STAT616/HW05/ncschools.csv")

## Remove schools where no SATs were taken

data_2_final <- data_2[data_2$pctsat != 0, ]
data_2_final <- data_2_final[data_2_final$category == "H", ]

## Set some data parameters

B <- 500
n <- dim(data_2_final)[1]



##### Part a #####



## Correlation between pctsat and avesat

cor(data_2_final$avesat, data_2_final$pctsat)

# There is approximately no relationship between pcstat and avesat, only 0.12

## Create a containers for the results

output_2a <- setNames(data.frame(matrix(NA, ncol = 2, nrow = n)), c("avesat", "pctsat"))
sim_corr_2a <- numeric()

## Perform the simulation

for (i in 1:B) {
    
    rand_inds <- sample(x = 1:n, size = n, replace = TRUE)
    
    for (j in 1:length(rand_inds)) {
        
        temp <- data_2_final[rand_inds[j], 8:9]
        output_2a[j, ] <- temp
        
    }
    
    sim_corr_2a[i] <- cor(output_2a$avesat, output_2a$pctsat)
    
}

## Harvest the standard errors of the results

se_cc_2a <- std_err(sim_corr_2a)



##### Part b #####



## Update the dataset

data_2b <- data_2_final[data_2_final$grade == "A", ]

## Set some data parameters

B <- 500
n <- dim(data_2b)[1]

## Correlation between pctsat and avesat

cor(data_2b$avesat, data_2b$pctsat)

# There is a marginally positive relationship between pcstat and avesat of 0.29

## Create a containers for the results

output_2b <- setNames(data.frame(matrix(NA, ncol = 2, nrow = n)), c("avesat", "pctsat"))
sim_corr_2b <- numeric()

## Perform the simulation

for (i in 1:B) {
    
    rand_inds <- sample(x = 1:n, size = n, replace = TRUE)
    
    for (j in 1:length(rand_inds)) {
        
        temp <- data_2b[rand_inds[j], 8:9]
        output_2b[j, ] <- temp
        
    }
    
    sim_corr_2b[i] <- cor(output_2b$avesat, output_2b$pctsat)
    
}

## Harvest the standard errors of the results

se_cc_2b <- std_err(sim_corr_2b)



##### Part c #####



## Update the dataset

data_2c <- data_2_final[data_2_final$grade == "C", ]

## Set some data parameters

B <- 500
n <- dim(data_2c)[1]

## Correlation between pctsat and avesat

cor(data_2c$avesat, data_2c$pctsat)

# There is a marginally negative relationship between pcstat and avesat of -0.12

## Create a containers for the results

output_2c <- setNames(data.frame(matrix(NA, ncol = 2, nrow = n)), c("avesat", "pctsat"))
sim_corr_2c <- numeric()

## Perform the simulation

for (i in 1:B) {
    
    rand_inds <- sample(x = 1:n, size = n, replace = TRUE)
    
    for (j in 1:length(rand_inds)) {
        
        temp <- data_2c[rand_inds[j], 8:9]
        output_2c[j, ] <- temp
        
    }
    
    sim_corr_2c[i] <- cor(output_2c$avesat, output_2c$pctsat)
    
}

## Harvest the standard errors of the results

se_cc_2c <- std_err(sim_corr_2c)



##### Part d #####



# The standard errors for the correlation coefficients are very similar with the A schools
# at 0.005 and the B schools at 0.004 showing that they have similar variation.

# I think the correlation signs are different because in C schools, we may infer that most
# of the students are not good students, so if more students take the exam, the average grade
# will drop. With the A schools we have the opposite affect.



##### Part e #####














