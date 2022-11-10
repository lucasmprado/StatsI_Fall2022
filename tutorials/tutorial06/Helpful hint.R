#######
# HINTS
#######

library(tidyverse)

dat <- read.csv("movies.csv") # You need to edit this for your system

## How to select Warner films
# There is more than one Warner Bros. studio
sort(unique(dat$studio))

# The grepl() function can be used to filter character strings based on
# a word within the string.
dat %>%
  filter(grepl("Warner|WARNER", studio)) %>%
  summarise(c_score = mean(critics_score))

# We can see that none of the Warner films have won best picture
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_win == "yes") %>%
  select(title)

# But they did get two nominations...
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_nom == "yes") %>%
  select(title, thtr_rel_year)


#######################
# PREPARING THE DATASET
#######################

# Convert factors:
char_vecs <- sapply(dat, is.character) # Make an index object for subsetting character vectors
unique <- Map(length, lapply(dat[,char_vecs], unique)) # Count the unique values in each character vector
factors <- names(dat[,char_vecs][,unique <= 11 & unique > 2]) # Subset character vectors with more than 2 and fewer than 12 unique values
dat[,factors] <- lapply(dat[,factors], as.factor) # Coerce these to factor

# Relevel factors with implicit order:
unique(dat$mpaa_rating)
dat$mpaa_rating <- factor(dat$mpaa_rating, 
                          levels = c("G", "PG", "PG-13",
                                     "R", "NC-17", "Unrated"))

unique(dat$critics_rating)
dat$critics_rating <- factor(dat$critics_rating, 
                             levels = c("Rotten", "Fresh",
                                        "Certified Fresh"))

unique(dat$audience_rating)
dat$audience_rating <- factor(dat$audience_rating, 
                              levels = c("Spilled", "Upright"))

# Convert logicals:
logical <- names(dat[,char_vecs][,unique == 2]) # Make an index object for logicals
logical <- logical[-1] # Drop one column (audience_rating) which is not logical
dat[,logical] <- ifelse(dat[,logical] == "no", FALSE, TRUE) # Convert "yes"/"no" to TRUE/FALSE
dat[,logical] <- lapply(dat[,logical], as.logical) # Coerce to logical

####################
# EXPLORING THE DATA
####################

# Number of Warner movies in the dataset: 71
dat %>%
  filter(grepl("Warner|WARNER", studio)) %>%
  summarise(count=n())

# All Warner movies:
dat %>%
  filter(grepl("Warner|WARNER", studio)) %>%
  select(title, best_pic_nom, best_pic_win)

# Warner movies that won the best picture award:
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_win == T) %>%
  select(title)

# Warner movies that were nominated to best picture award:
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_nom == T) %>%
  select(title)

# Warner movies audience scores and mean:
dat%>%
  filter(grepl("Warner|WARNER", studio)) %>%
  select(title, audience_score)

warner <- subset(dat, grepl("Warner|WARNER", studio))

mean(warner$audience_score) # 62.70423
mean(dat$audience_score) # 62.36252

# Warner movies critic scores and mean:
dat%>%
  filter(grepl("Warner|WARNER", studio)) %>%
  select(title, critics_score)

mean(warner$critics_score) # 53.71831
mean(dat$critics_score) # 57.68817

#####################################
# BAR CHARTS
# (best picture winners and nominees)
#####################################

# Subsetting for best-picture winners and nominees:
winners <- subset(dat, best_pic_win==T)
nominees <- subset(dat, best_pic_nom==T)

# Winners and nominees by genre:
barplot(table(winners$best_pic_win, winners$genre))
barplot(table(nominees$best_pic_nom, nominees$genre))

# Winners and nominess by release month:
barplot(table(winners$best_pic_win, winners$thtr_rel_month))
barplot(table(nominees$best_pic_nom, nominees$thtr_rel_month))

# Winners and nominees by best actor:
barplot(table(winners$best_pic_win, winners$best_actor_win))
barplot(table(nominees$best_pic_nom, nominees$best_actor_win))

# Winners and nominees by best actress:
barplot(table(winners$best_pic_win, winners$best_actress_win))
barplot(table(nominees$best_pic_nom, nominees$best_actress_win))

# Winners and nominees by best director:
barplot(table(winners$best_pic_win, winners$best_dir_win))
barplot(table(nominees$best_pic_nom, nominees$best_dir_win))


##################
# CHI-SQUARED TEST
##################

# Genre:
chisq.test(dat$best_pic_win, dat$genre) # p = 0.97
chisq.test(dat$best_pic_nom, dat$genre) # p = 0.09

# Release month:
chisq.test(dat$best_pic_win, dat$thtr_rel_month) # p = 0.29
chisq.test(dat$best_pic_nom, dat$thtr_rel_month) ### p = 2.348e-05

# Best actor:
chisq.test(dat$best_pic_win, dat$best_actor_win) # p = 0.58
chisq.test(dat$best_pic_nom, dat$best_actor_win) ### p = 8.13e-05

# Best actress:
chisq.test(dat$best_pic_win, dat$best_actress_win) ### p = 0.0009
chisq.test(dat$best_pic_nom, dat$best_actress_win) ### p = 1.02e-06

# Best director:
chisq.test(dat$best_pic_win, dat$best_dir_win) ### p = 1.28e-14
chisq.test(dat$best_pic_nom, dat$best_dir_win) ### p = 0.0004


######################################
# WHAT I ACTUALLY TRIED DOING IN CLASS
######################################

plot(movies$best_pic_win~movies$actor1)

dat$best_pic_win <- as.logical(dat$best)

plot(dat$best_pic_win~dat$studio)

dat %>%
  filter(best_pic_win == "yes") %>%
  group_by(studio) %>%
  summarise(count = n())

dat %>%
  filter(best_pic_win == "yes") %>%
  group_by(audience_rating) %>%
  summarise(count=n())

dat %>%
  filter(best_pic_nom == "yes") %>%
  group_by(audience_score) %>%
  summarise(count=n())

dat %>%
  filter(best_pic_win == "yes") %>%
  group_by(genre) %>%
  summarise(count=n())

with(dat, plot(critics_score~audience_score))

dat %>%
  filter(best_pic_nom == "yes") %>%
  summarise(count=n())

dat %>%
  filter(best_pic_nom == "yes") %>%
  filter(best_actor_win == "yes") %>%
  summarise(count=n())

dat %>%
  filter(best_pic_nom == "yes") %>%
  filter(best_actress_win == "yes") %>%
  summarise(count=n())

dat %>%
  filter(best_pic_nom == "yes") %>%
  filter(best_dir_win == "yes") %>%
  summarise(count=n())

dat %>%
  filter(best_pic_win == "yes") %>%
  filter(best_actor_win == "yes") %>%
  summarise(count=n())

dat %>%
  filter(best_pic_win == "yes") %>%
  filter(best_actress_win == "yes") %>%
  summarise(count=n())

dat %>%
  filter(best_pic_win == "yes") %>%
  filter(best_dir_win == "yes") %>%
  summarise(count=n()) %>%
  ggplot(aes(best_pic_win, best_dir_win))

picWin <- subset(dat, dat$best_pic_win %in% c("yes"))
picNom <- subset(dat, dat$best_pic_nom %in% c("yes"))
actorWin <- subset(dat, dat$best_actor_win %in% c("yes"))

plot(picWin~actorWin)

dat %>%
  select(best_dir_win) %>% # just keep the two relevant cols
    group_by(best_actor_win) %>% # perform a nested grouping operation (release month, then T/F horror)
  summarise(n = n()) %>% # get a raw count for each group
  pivot_wider(names_from = horror, values_from = n) %>% # change the shape of our data
  ungroup() %>%
  mutate(All = round(`FALSE` / sum(`FALSE`), 3), # calculate proportions for all films
         Horror = `TRUE` / sum(`TRUE`, na.rm = TRUE)) %>% # calculate proportions for horror films
  select(thtr_rel_month, All, Horror) %>% # drop all other columns
  pivot_longer(cols = c("All", "Horror"), names_to = "film_type") %>% # change the shape again!
  mutate(month = factor(month.abb[thtr_rel_month], levels = month.abb)) %>% # create a factor for months
  ggplot(aes(month, value)) + # plot the data
  geom_col(aes(fill = film_type), position = "dodge") +
  labs(title = "Proportion of Theatrical Releases by Month", y = "proportion") 

chisq.test(x = dat$genre, y = dat$best_pic_win)
chisq.test(x = dat$genre, y = dat$best_pic_nom)
chisq.test(x = dat$best_dir_win, y = dat$best_pic_win)
chisq.test(x = dat$best_dir_win, y = dat$best_pic_nom)
chisq.test(x = dat$best_actor_win, y = dat$best_pic_win)
chisq.test(x = dat$best_actor_win, y = dat$best_pic_win)
