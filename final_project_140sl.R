# load packages
library(readr)
library(stringi)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)

# load in data
listings.gz <- read_csv(file = "listings.csv.gz")
listings.gz <- listings_csv

# clean listings.gz to use only relevant variables
list2 <- listings.gz[ , c(49:50, 52:59, 80)]

# clean amenities variable
list2$amenities <- stri_replace_all_regex(str = list2$amenities, pattern = "\\{", replacement = "")
list2$amenities <- stri_replace_all_regex(str = list2$amenities, pattern = "\\}", replacement = "")
list2$amenities <- stri_replace_all_regex(str = list2$amenities, pattern = "\"", replacement = "")

# use complete case
list2$amenities <- ifelse(list2$amenities == "", NA, list2$amenities)
list3 <- list2[complete.cases(list2), ]

# linear regression on attributes
fit <- lm(review_scores_rating ~ property_type + room_type + accommodates +
            bathrooms + bedrooms + beds + bed_type, data = list3)
summary(fit)

# dataframes of significant attributes
attributes <- c("property type: Bungalow", "property type: Condominium", "property type: Guesthouse",
                "property type: House", "property type: Loft", "property type: Townhouse", 
                "room type: Private Room", "room type: Shared Room", "accommodates", "bedrooms", 
                "beds", "bed type: Futon", "bed type: Real Bed")
estimates <- c(2.33567, 2.11278, 3.35298, 1.50701, 2.65329, 1.30589, -0.56145, -4.06204, -0.33131,
               0.60600, -0.22101, -1.67170, -2.06934)

# create one df of significant attributes
df <- as.data.frame(attributes, estimates, stringsAsFactors = FALSE)
df2 <- as.data.frame(estimates)
df <- cbind(df, df2)

# one for positive, one for negative
df_pos <- df[df$estimates > 0, ]
df_neg <- df[df$estimates < 0, ]

# make bar plot of positive attributes
p <- ggplot(data = df_pos, aes(x = reorder(attributes, estimates), y = estimates)) + geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ylab("Change in Review Score") +
  xlab("Host's Attributes") + ggtitle("Attributes of Hosts that Significantly and Positively \nAffect Review Score") +
  theme(plot.title = element_text(hjust = 0.5))
p

# make bar plot of negative attributes
p <- ggplot(data = df_neg2, aes(x = reorder(attributes, -estimates), y = estimates)) + geom_bar(stat = "identity", fill = "maroon") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ylab("Change in Review Score") +
  xlab("Host's Attributes") + ggtitle("Attributes of Hosts that Significantly and Negatively \nAffect Review Score") +
  theme(plot.title = element_text(hjust = 0.5))
p

# gave df IDs
list3$id <- 1:nrow(list3)

# split attributes' list into multiple variables
dat <- with(list3, strsplit(amenities, ','))
df2 <- data.frame(id = factor(rep(list3$id, times = lengths(dat)),
                              levels = list3$id), amenities = unlist(dat))
df3 <- as.data.frame(cbind(id = list3$id, table(df2$id, df2$amenities)))

# get new df of just reviews
reviews <- as.data.frame(list3$review_scores_rating)

# merge amenities with reviews
amenities <- cbind(df3, reviews)

# clean amenities
names(amenities)[94] <- "review_scores_rating"
amenities <- amenities[ , -1]

# fit linear regression on amenities
fit2 <- lm(review_scores_rating ~ ., data = amenities)
summary(fit2)

# dataframes of significant amenities
amen <- c("24-hour check-in", "Cable TV", "Children's books and toys", "Dryer", "Elevator in building", 
          "First aid kit", "Free parking on premises", "Game console", "Hair dryer", 
          "Hangers", "Heating", "Hot tub", "Indoor fireplace", "Laptop friendly workspace", 
          "Pets allowed", "Pets live on this property", "Pool", "Private entrance", "Safety card", 
          "Shampoo", "Smoking allowed", "TV", "Washer")
values <- c(-0.56896, 0.47176, 2.00865, 1.20878, -0.39856, 0.80373, 0.62664, -2.29282, 0.54309, 
            0.46016, 0.89403, -0.68816, 0.54685, 0.75075, -0.65192, 1.56824, 0.34171, 0.50053, 
            -0.70855, 0.29488, -0.82392, 0.57804, -0.74120)

# create only 1 df of significant amenities
df_amen <- as.data.frame(amen, stringsAsFactors = FALSE)
df2_amen <- as.data.frame(values)
df_amen <- cbind(df_amen, df2_amen)

# create pos and neg df
amen_pos <- df_amen[df_amen$values > 0, ]
amen_neg <- df_amen[df_amen$values < 0, ]

# make bar plot of positive amenities
p <- ggplot(data = amen_pos, aes(x = reorder(amen, values), y = values)) + geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ylab("Change in Review Score") +
  xlab("Host's Amenities") + ggtitle("Amenities of Hosts that Significantly and Positively \nAffect Review Score") +
  theme(plot.title = element_text(hjust = 0.5))
p

# make bar plot of negative amenities
p <- ggplot(data = amen_neg, aes(x = reorder(amen, -values), y = values)) + geom_bar(stat = "identity", fill = "maroon") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ylab("Change in Review Score") +
  xlab("Host's Amenities") + ggtitle("Amenities of Hosts that Significantly and Negatively \nAffect Review Score") +
  theme(plot.title = element_text(hjust = 0.5))
p