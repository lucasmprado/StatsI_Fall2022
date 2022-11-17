# Load packages
library("tidyverse")
library("broom")
library("stargazer")

# Import data:
dat <- read.csv("C:/Users/lucas/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")

# Exploring data:
dim(dat)
str(dat)
head(dat)
tail(dat)
summary(dat)

############
# QUESTION 1
############

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is difflog:
lm_q1 <- lm(voteshare ~ difflog, data = dat)
summary(lm_q1)
stargazer(lm_q1, title="The effect of the difference in campaign spending
          between incumbent and challenger on the incumbent's vote share",
          column.labels = "Incumbent's vote share",
          covariate.labels = "Difference in campaign spending")

# 2. Make a scatterplot of the two variables and add the regression line:
png("lm_q1.png", 640, 480)
dat %>%
  ggplot(aes(difflog, voteshare)) +
  geom_point(color="gray35") +
  geom_smooth(method = "lm", color = "red") +
  xlab("Difference in campaign spending between incumbent and challenger") +
  ylab("Incumbent's vote share") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))
dev.off()

# 3. Save the residuals of the model in a separate object:
lm_q1_res <- lm_q1$residuals

############
# QUESTION 2
############

# 1. Run a regression where the outcome variable is presvote and the explanatory variable is difflog:
lm_q2 <- lm(presvote ~ difflog, data=dat)
summary(lm_q2)
stargazer(lm_q2, title="The effect of the difference between incumbent's
          and challenger's spending on the vote share of the presidential
          candidate of the incumbent's party",
          column.labels = "Vote share of the presidential candidate of
          the incumbent's party",
          covariate.labels = "Difference in campaign spending")

# 2. Make a scatterplot of the two variables and add the regression line:
png("lm_q2.png", 640, 480)
dat %>%
  ggplot(aes(difflog, presvote)) +
  geom_point(color="gray35") +
  geom_smooth(method = "lm", color = "red") +
  xlab("Difference in campaign spending between incumbent and challenger") +
  ylab("Vote share of the presidential candidate of the incumbent's party") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))
dev.off()

# 3. Save the residuals of the model in a separate object:
lm_q2_res <- lm_q2$residuals

############
# QUESTION 3
############

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is presvote:
lm_q3 <- lm(voteshare ~ presvote, data=dat)
summary(lm_q3)
stargazer(lm_q3, title="The association between the vote share of the
          presidential candidate of the incumbent's party and the
          incumbent's electoral success",
          column.labels = "Incumbent's vote share",
          covariate.labels = "Vote share of the presidential candidate of
          the incumbent's party")

# 2. Make a scatterplot of the two variables and add the regression line:
png("lm_q3.png", 640, 480)
dat %>%
  ggplot(aes(presvote, voteshare)) +
  geom_point(color="gray35") +
  geom_smooth(method = "lm", color = "red") +
  xlab("Vote share of the presidential candidate of the incumbent's party") +
  ylab("Incumbent's vote share") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))
dev.off()

############
# QUESTION 4
############

# 1. Run a regression where the outcome variable is the residuals from Question 1
# and the explanatory variable is the residuals from Question 2:
lm_q4 <- lm(lm_q1_res ~ lm_q2_res)
summary(lm_q4)
stargazer(lm_q4, title="The association between the residuals from
          Question 1 and Question 2",
          column.labels = "Residuals from Question 1",
          covariate.labels = "Residuals from Question 2")

# 2. Make a scatterplot of the two residuals and add the regression line:
png("lm_q4.png", 640, 480)
dat %>%
  ggplot(aes(lm_q2_res, lm_q1_res)) +
  geom_point(color="gray35") +
  geom_smooth(method = "lm", color = "red") +
  xlab("Residuals from Question 2") +
  ylab("Residuals from Question 1") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16))
dev.off()

############
# QUESTION 5
############

# 1. Run a regression where the outcome variable is the incumbent’s voteshare
# and the explanatory variables are difflog and presvote:
lm_q5 <- lm(voteshare ~ difflog + presvote, data = dat)
summary(lm_q5)
stargazer(lm_q1, lm_q3, lm_q5, title="The effect of the difference
          between incumbent's and challenger's spending and the
          president's popularity on the incumbent's vote share",
          covariate.labels = c("Difference in campaign spending",
                               "Vote share of the presidential candidate
                               of the incumbent's party"))
