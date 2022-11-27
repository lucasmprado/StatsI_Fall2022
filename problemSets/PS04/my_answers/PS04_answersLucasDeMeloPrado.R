###############
# PROBLEM SET 4
###############

## QUESTION 1

# Dataset:
install.packages("car")
library("car")
library("stargazer")
data(Prestige)
help(Prestige)

# Create new variable professional
unique(Prestige$type)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Multivariate linear model
mod <- lm(prestige ~ income + professional + income:professional,
          data = Prestige)
stargazer(mod)

## QUESTION 2

# P-value for 2-a
2*pt(2.625, df = 131 - 3, lower.tail = F)

# P-value for 2-b
2*pt(3.231, df = 131 - 3, lower.tail = F)
