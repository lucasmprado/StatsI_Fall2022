##############################
# Final model script: Team _ #
##############################

### Note: do not run the model; just add the code

# Load any packages here

# Load data
dat <- readRDS("data/test.rds")

# Data transformation
dat <- dat %>% 
  # Add here any code necessary to transform variables

# Model
mod <- lm(# your model formula here
          , 
          data = dat,
          na.action = na.omit)
