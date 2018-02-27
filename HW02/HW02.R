

# Problem 2

## Part b

options(scipen = 999)

values <- c(1,3,5,9,5,5,5,6,6,3,3,3,0,6,14,10,18)
labels <- c("A","A","A","B","B","B","B","C","C","C","D","D","D","D","E","E","E")

data <- data.frame(labels, values)

aov_out <- aov(values ~ labels, data = data)
summary(aov_out)

f_stat <- as.vector(summary.lm(aov_out)$fstatistic["value"])



f_stats <- numeric()
n <- 5000

for (i in 1:n) {
    
    labels_temp <- sample(labels)
    data_temp <- data.frame(labels_temp, values)
    aov_out_temp <- aov(values ~ labels_temp, data = data_temp)
    f_stats[i] <- as.vector(summary.lm(aov_out_temp)$fstatistic["value"])
    
}

count_greater <- sum(f_stats >= f_stat)

count_greater/n

## Part c

### Check for error normality

qqnorm(residuals(aov_out))
qqline(residuals(aov_out))

hist(residuals(aov_out))

### Check residuals for equal variance

plot(residuals(aov_out))
abline(h = 0, col = "red")

## Part e

### Compare B and C

t.test(x = data[4:7,2], y = data[8:10,2])

### Compare B and E

t.test(x = data[4:7,2], y = data[14:17,2])




# STAT616 - Problem 1

values <- c(1,2,3,4,60000001,60000000)
labels <- c("A","A","B","B","C","C")

data <- data.frame(labels, values)

aov_out <- aov(values ~ labels, data = data)
summary(aov_out)

f_stat <- as.vector(summary.lm(aov_out)$fstatistic["value"])



f_stats <- numeric()
n <- 5000

for (i in 1:n) {
    
    labels_temp <- sample(labels)
    data_temp <- data.frame(labels_temp, values)
    aov_out_temp <- aov(values ~ labels_temp, data = data_temp)
    f_stats[i] <- as.vector(summary.lm(aov_out_temp)$fstatistic["value"])
    
}

count_greater <- sum(f_stats >= f_stat)

count_greater/n