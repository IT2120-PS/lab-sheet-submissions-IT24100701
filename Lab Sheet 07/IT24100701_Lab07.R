setwd("C:\\Users\\USER\\Desktop\\IT24100701")

a <- 0  
b <- 40  
x1 <- 10
x2 <- 25

prob1 <- (x2 - x1) / (b - a)
cat("Exercise 1: Probability =", prob1, "\n")

lambda <- 1/3
time <- 2

prob2 <- pexp(time, rate = lambda)   
cat("Exercise 2: Probability =", prob2, "\n")


mean_iq <- 100
sd_iq <- 15


prob3_i <- 1 - pnorm(130, mean = mean_iq, sd = sd_iq)
cat("Exercise 3 (i): Probability IQ > 130 =", prob3_i, "\n")


iq_95 <- qnorm(0.95, mean = mean_iq, sd = sd_iq)
cat("Exercise 3 (ii): 95th Percentile IQ =", iq_95, "\n")
