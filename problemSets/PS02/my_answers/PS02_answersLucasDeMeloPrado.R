###############
# Problem Set 2
###############

# Load libraries
library(tidyverse)

############
# Question 1
############

# Import data:
notStopped <- c(14, 7)
bribeRequest <- c(6, 7)
warning <- c(7, 1)

# Create contingency table:
data <- as.data.frame(cbind(notStopped, bribeRequest, warning),
                      row.names= c("upperClass", "lowerClass"))

# Table of expected frequencies (fe):
notStopped_fe <- c(rowSums(data)*colSums(data)[1]/sum(data))
bribeRequest_fe <- c(rowSums(data)*colSums(data)[2]/sum(data))
warning_fe <- c(rowSums(data)*colSums(data)[3]/sum(data))

data_fe <- as.data.frame(cbind(notStopped_fe, bribeRequest_fe, warning_fe),
                    row.names = c("upperClass", "lowerClass"))

# Chi-squared test statistics:
X2 <- sum(((data - data_fe)^2)/data_fe)

# P-value:
df <- (nrow(data)-1)*(ncol(data)-1)
p <- pchisq(X2, df, lower.tail = FALSE)

# Checking:
chisq.test(data)

# Standardized residuals:
notStopped_res <- round(c((data$notStopped - data_fe$notStopped_fe)/
                      sqrt(data_fe$notStopped_fe*
                          (1-(rowSums(data)/sum(data)))*
                          (1-(colSums(data)[1]/sum(data))))),
                      digits = 2)
bribeRequest_res <- round(c((data$bribeRequest-data_fe$bribeRequest_fe)/
                        sqrt(data_fe$bribeRequest_fe*
                           (1-(rowSums(data)/sum(data)))*
                           (1-(colSums(data)[2]/sum(data))))),
                        digits = 2)
warning_res <- round(c((data$warning - data_fe$warning_fe)/
                   sqrt(data_fe$warning_fe*
                          (1-(rowSums(data)/sum(data)))*
                          (1-(colSums(data)[3]/sum(data))))),
                   digits = 2)

data_res <- as.data.frame(cbind(notStopped_res, bribeRequest_res, warning_res),
                          row.names = c("upperClass", "lowerClass"))

# Checking:
str(chisq.test(data))

############
# Question 2
############

# Import data:
women <- read_csv("women.csv")
x <- women$reserved
y <- women$water

# Linear function:
b <- sum((x - mean(x))*(y - mean(y)))/sum((x - mean(x))^2)
a <- mean(y) - b*mean(x)
y_hat <- a+b*x

# Standard deviation and standard error of beta:
s_beta <- sqrt((sum((y-y_hat)^2))/(length(y)-2))
se_beta <- s_beta/sqrt(sum((x-mean(x))^2))

# T-test:
t <- (b-0)/se_beta

# P-value (two-tail):
p <- 2*pt(t, df=length(y)-2, lower.tail=FALSE)

# Checking:
summary(lm(y~x))

# Standard deviations of x and y:
s_x <- sqrt(sum((x-mean(x))^2)/(length(x)-1))
s_y <- sqrt(sum((y-mean(y))^2)/(length(y)-1))

# Correlation:
r <- (s_x/s_y)*b

# Checking:
cor(x, y)