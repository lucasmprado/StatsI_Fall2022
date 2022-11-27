##############################
# Final model script: Team _ #
##############################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
library("tidyverse")
library("stargazer")

# Load training data
train <- readRDS("data/train.rds")

# Data transformation
ZipCode_group <- train %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipCode_group = ntile(cumul_count, 5))
  
train$Bedrooms1 <- cut(train$Bedrooms,
                      breaks = c(0, 1, 2, 3, 4, 5, max(train$Bedrooms)))

train$Bathrooms1 <- cut(train$Bathrooms,
                       breaks = c(0, 1, 2, 3, 4, 5, max(train$Bathrooms)))

plot(Bedrooms1 ~ Bathrooms1, data = train)

# Model
mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipCode, 
          data = train,
          na.action = na.omit)

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipCode + ZipCode:SqFtTotLiving, 
           data = train,
           na.action = na.omit)

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipCode + Bathrooms1 + Bedrooms1 + Bathrooms1:Bedrooms1, 
          data = train,
          na.action = na.omit)

summary(mod)

stargazer(mod1, type = "text")
stargazer(mod2, type = "text")
stargazer(mod4, type = "text")

########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
test <- test %>% 
  # I will copy/paste here the code you use above
  
# Run model on test data
test$prediction <- predict(mod, newdata = test)

# Calculate RMSE
test$residuals <- test$AdjSalePrice - test$prediction
(rmse <- sqrt(mean(test$residuals^2)))

# Calculate R^2
mean_y <- mean(test$AdjSalePrice)
tss <- sum((test$AdjSalePrice - mean_y)^2)
rss <- sum((test$AdjSalePrice - test$prediction)^2)
(r_sq <- 1 - (rss/tss))