#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse"),  pkgTest)

# set working directory
setwd("C:/Users/lucas/Documents/GitHub/StatsI_Fall2022/problemSets/PS01")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)

# Sample mean:
yBar <- mean(y)

# Standard deviation:
s <- sqrt(sum((y-yBar)^2)/(length(y)-1))

# Standard error:
se <- s/sqrt(length(y))

# Z-score:
z90 <- qnorm((1-0.9)/2, lower.tail = FALSE)

# Confidence interval lower value:
CIlower <- yBar-z90*se

# Confidence interval upper value:
CIupper <- yBar+z90*se

# Confidence interval:
CI <- c(CIlower, CIupper)

# Null hypothesis:
mu <- 100

# T-test for alpha = 0.05:
t <- (yBar - mu)/se

# P-value (right tail):
p <- pt(t, df = length(y)-1, lower.tail = FALSE)

#####################
# Problem 2
#####################

expenditure <- read.table("C:/Users/lucas/Documents/GitHub/StatsI_Fall2022/datasets/expenditure.txt",
                          header=T)

expenditure$RegionFactor <- as.factor(expenditure$Region)
levels(expenditure$RegionFactor) <- c("Northeast", "North Central", "South", "West")

# (1) Scatterplot Y-X1:
ggplot(data=expenditure, mapping = aes(x = X1, y = Y)) +
  geom_point(color="blue", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita expenditure on housing assistance by
per capita personal income in the 50 U.S. states") +
  xlab("Per capita personal income") +
  ylab("Per capita expenditure on housing assistance")

# (2) Scatterplot Y-X2:
ggplot(data=expenditure, mapping = aes(x = X2, y = Y)) +
  geom_point(color="gold3", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita expenditure on housing assistance by
number of financially insecure residents (per 100,000) in the 50 U.S. states") +
  xlab("Financially insecure residents (per 100,000)") +
  ylab("Per capita expenditure on housing assistance")

# (3) Scatterplot Y-X3:
ggplot(data=expenditure, mapping = aes(x = X3, y = Y)) +
  geom_point(color="red", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita expenditure on housing assistance by
number of residents in urban areas (per 1,000) in the 50 U.S. states") +
  xlab("Residents in urban areas (per 1,000)") +
  ylab("Per capita expenditure on housing assistance")

# (4) Scatterplot X1-X2:
ggplot(data=expenditure, mapping = aes(x = X2, y = X1)) +
  geom_point(color="green3", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita personal income by
number of financially insecure residents (per 100,000) in the 50 U.S. states") +
  xlab("Financially insecure residents (per 100,000)") +
  ylab("Per capita personal income")

# (5) Scatterplot X1-X3:
ggplot(data=expenditure, mapping = aes(x = X3, y = X1)) +
  geom_point(color="purple", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita personal income by
number of residents in urban areas (per 1,000) in the 50 U.S. states") +
  xlab("Residents in urban areas (per 1,000)") +
  ylab("Per capita personal income")

# (6) Scatterplot X2-X3:
ggplot(data=expenditure, mapping = aes(x = X3, y = X2)) +
  geom_point(color="orange", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Number of financially insecure residents (per 100,000) by
number of residents in urban areas (per 1,000) in the 50 U.S. states") +
  xlab("Residents in urban areas (per 1,000)") +
  ylab("Financially insecure residents (per 100,000)")

# (7) Boxplot Y by Region:
ggplot(data=expenditure, mapping = aes(x = RegionFactor, y = Y, fill=RegionFactor)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita expenditure on housing assistance by U.S. region") +
  xlab("U.S. region") +
  ylab("Per capita expenditure on housing assistance") +
  guides(fill=F)

# (8) Scatterplot Y-X1:
ggplot(data=expenditure, mapping = aes(x = X1, y = Y)) +
  geom_point(color="blue", size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) +
  ggtitle("Per capita expenditure on housing assistance by
per capita personal income in the 50 U.S. states") +
  xlab("Per capita personal income") +
  ylab("Per capita expenditure on housing assistance")
  
# (9) Scatterplot Y-X1-Region:
ggplot(data=expenditure, mapping = aes(x = X1, y = Y, group = RegionFactor)) +
  geom_point(size=5, aes(shape = RegionFactor, color = RegionFactor)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14), legend.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom") +
  ggtitle("Per capita expenditure on housing assistance by
per capita personal income in the 50 U.S. states classified by region") +
  xlab("Per capita personal income") +
  ylab("Per capita expenditure on housing assistance") +
  labs(color = "Region", shape = "Region")
