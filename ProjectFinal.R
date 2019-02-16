#import libraries
library(dplyr)
library(imputeTS)
library(tidyverse)
library(ggplot2)
library(reshape)
library(GGally)
library(forecast)


#import the dataset to the workspace
dataset.df <- read.csv("googleplaystore.csv")

#Clean data, convert from factor to num type
data.clean <- dataset.df %>%
  mutate(
    # Eliminate some characters to transform Installs to numeric
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    # Eliminate M to transform Size to numeric
    Size = gsub("M", "", Size),
    # Replace cells with k to 0 since it is < 1MB
    Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
    # Transform reviews to numeric
    Reviews = as.numeric(Reviews),
    # Remove currency symbol from Price, change it to numeric
    Price = as.numeric(gsub("\\$", "", as.character(Price))),
    # Replace "Varies with device" to NA since it is unknown
    Min.Android.Ver = gsub("Varies with device", NA, Android.Ver),
    # Keep only version number to 1 decimal
    Min.Android.Ver = as.numeric(substr(Min.Android.Ver, start = 1, stop = 3)),
    # Drop old Android version column
    Android.Ver = NULL
  )

#check if their are any duplicate records. 
nrow(data.clean %>% distinct())

#Omit duplicate records
data.clean <- data.clean %>% distinct()

#Replace NA or missing values with mean
data.clean$Rating <- na.mean(data.clean$Rating)
data.clean$Size <- na.mean(data.clean$Size)

#check the missing values
sum(is.na(data.clean$Reviews))

#Descriptive statistics
summary(data.clean)

#BoxPlot
ggplot(data.clean,aes(x=Content.Rating, y=log10(Installs))) + scale_y_continuous("Installs, log10-scaling") +
  geom_boxplot(outlier.colour = "red")+
  geom_point()
ggplot(data.clean,aes(x=Type, y=log10(Installs))) + scale_y_continuous("Type, log10-scaling") +
  geom_boxplot(outlier.colour = "red")+
  geom_point()


#copy dataframe and standardize on that dataframe
data.final <- data.clean

#create dummy variable for Type i.e. free or paid
dummy_type <- as.data.frame(model.matrix(~ 0 + Type, data=data.final))
dummy_Content.Rating <- as.data.frame(model.matrix(~ 0 + Content.Rating, data=data.final))
dummy_Category <- as.data.frame(model.matrix(~ 0 + Category, data=data.final))
data.final <- cbind(data.final[,-7], dummy_type[,])
data.final <- cbind(data.final[,-8], dummy_Content.Rating[,])
data.final <- cbind(data.final[,-2], dummy_Category[,])
data.final <- data.final[,-12]#drop one dummy type
data.final <- data.final[,-12]#drop one dummy content rating
data.final <- data.final[,-34]#drop one dummy category

#standardize data
options(scipen=999, digits = 5)
#data.final[,c(3,4,5,6,7,12,13,14,15,16,17)] <- scale(data.final[,c(3,4,5,6,7,12,13,14,15,16,17)])

#choose data for Linear modelling
data.final.new <- data.final[-c(1,8,9,10)]
data.final.new <- data.final.new[-c(6)]

#Heat map between various predictors and target variable
cor.mat <- round(cor(data.final.new),2)
heatmap(as.matrix(cor.mat),Colv = NA,Rowv = NA)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))

# select variables for regression
selected.var <- data.final.new

# partition data
set.seed(3)  # set seed for reproducing the partition
numberOfRows <- nrow(selected.var)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- selected.var[train.index, ]
valid.df <- selected.var[-train.index, ]

# use lm() to run a linear regression of Ratings on all predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
data.final.lm <- lm(Rating ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(data.final.lm)


# use predict() to make predictions on a new set. 
data.final.lm.pred <- predict(data.final.lm, valid.df)
options(scipen=999, digits = 5)
some.residuals <- valid.df$Rating[1:20] - data.final.lm.pred[1:20]
data.frame("Predicted" = data.final.lm.pred[1:20], "Actual" = valid.df$Rating[1:20],
           "Residual" = some.residuals)

#accurcy of the model with all the predictors
accuracy(data.final.lm.pred, valid.df$Rating)
plot(data.final.lm)
plot(data.final.lm.pred, valid.df$Rating,xlab = "Actual", ylab = "Predcited")


# use step() to run stepwise regression.
# model - 1 Using backward regression
data.final.lm.backward <- step(data.final.lm, direction = "backward")
summary(data.final.lm.backward)  # Which variables were dropped?
data.final.lm.backward.pred <- predict(data.final.lm.backward, valid.df)
accuracy(data.final.lm.backward.pred, valid.df$Rating)

# model 1 - using forward regression
# create model with no predictors
data.final.lm.null <- lm(Rating~1, data = train.df)

# use step() to run forward regression.
data.final.lm.forward <- step(data.final.lm.null, scope=list(lower=data.final.lm.null, upper=data.final.lm), direction = "forward")
summary(data.final.lm.forward)  # Which variables were added?
data.final.lm.forward.pred <- predict(data.final.lm.forward, valid.df)
accuracy(data.final.lm.forward.pred, valid.df$Rating)

# model 1 - using stepwise
# use step() to run stepwise regression.
data.final.lm.stepwise <- step(data.final.lm, direction = "both")
summary(data.final.lm.stepwise)  # Which variables were dropped/added?
data.final.lm.stepwise.pred <- predict(data.final.lm.stepwise, valid.df)
accuracy(data.final.lm.step.pred, valid.df$Rating)

# coefficient
round(coefficients(data.final.lm.stepwise),5)



#Second model Reviews+Size+CategoryGame
# use lm() to run a linear regression of Ratings on all predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
data.final.lm2 <- lm(Rating ~  Reviews+Size+CategoryFAMILY+Price+Content.RatingTeen+TypeFree, data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(data.final.lm2)


# use predict() to make predictions on a new set. 
data.final.lm.pred2 <- predict(data.final.lm2, valid.df)
options(scipen=999, digits = 5)
some.residuals <- valid.df$Rating[1:20] - data.final.lm.pred2[1:20]
data.frame("Predicted" = data.final.lm.pred2[1:20], "Actual" = valid.df$Rating[1:20],
           "Residual" = some.residuals)

#accurcy of the model with all the predictors
accuracy(data.final.lm.pred2, valid.df$Rating)


plot(data.final.lm2)
plot(data.final.lm.pred2, valid.df$Rating,xlab = "Actual", ylab = "Predcited")
    



# use step() to run stepwise regression.
# model - 2 Using backward regression
data.final.lm.backward2 <- step(data.final.lm2, direction = "backward")
summary(data.final.lm.backward2)  # Which variables were dropped?
data.final.lm.backward2.pred <- predict(data.final.lm.backward2, valid.df)
accuracy(data.final.lm.backward2.pred, valid.df$Rating)



# model 2 - using stepwise
# use step() to run stepwise regression.
data.final.lm.stepwise2 <- step(data.final.lm2, direction = "both")
summary(data.final.lm.stepwise2)  # Which variables were dropped/added?
data.final.lm.stepwise2.pred <- predict(data.final.lm.stepwise2, valid.df)
accuracy(data.final.lm.stepwise2.pred, valid.df$Rating)



#Third model Reviews+Size+CategoryGame+CategorySPORTS+Price+CategoryTravel_and_local
# use lm() to run a linear regression of Ratings on all predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
data.final.lm3 <- lm(Rating ~  Reviews+Size+CategoryGAME+Price+Content.RatingTeen+TypeFree, data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(data.final.lm3)


# use predict() to make predictions on a new set. 
data.final.lm.pred3 <- predict(data.final.lm3, valid.df)
options(scipen=999, digits = 5)
some.residuals <- valid.df$Rating[1:20] - data.final.lm.pred[1:20]
data.frame("Predicted" = data.final.lm.pred3[1:20], "Actual" = valid.df$Rating[1:20],
           "Residual" = some.residuals)

#accurcy of the model with all the predictors
accuracy(data.final.lm.pred3, valid.df$Rating)
coefficients(data.final.lm3)

plot(data.final.lm3)
#plot(data.final.lm.pred2, valid.df$Rating, xlab = "Actual", ylab = "Predcited")


# use step() to run stepwise regression.
# model - 3 Using backward regression
data.final.lm.backward3 <- step(data.final.lm3, direction = "backward")
summary(data.final.lm.backward3)  # Which variables were dropped?
data.final.lm.backward3.pred <- predict(data.final.lm.backward3, valid.df)
accuracy(data.final.lm.backward3.pred, valid.df$Rating)


# model 3 - using stepwise
# use step() to run stepwise regression.
data.final.lm.stepwise3 <- step(data.final.lm3, direction = "both")
summary(data.final.lm.stepwise3)  # Which variables were dropped/added?
data.final.lm.stepwise3.pred <- predict(data.final.lm.stepwise3, valid.df)
accuracy(data.final.lm.stepwise3.pred, valid.df$Rating)


#Fourth Model
#Third model Reviews+Size+CategoryGame+CategorySPORTS+Price+CategoryTravel_and_local
# use lm() to run a linear regression of Ratings on all predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
data.final.lm4 <- lm(Rating ~  Reviews+Size+CategoryGAME+Price+Content.RatingEveryone+TypeFree, data = train.df)

#CategoryFAMILY+CategoryART_AND_DESIGN
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(data.final.lm4)


# use predict() to make predictions on a new set. 
data.final.lm.pred4 <- predict(data.final.lm4, valid.df)
options(scipen=999, digits = 5)
some.residuals <- valid.df$Rating[1:20] - data.final.lm.pred[1:20]
data.frame("Predicted" = data.final.lm.pred4[1:20], "Actual" = valid.df$Rating[1:20],
           "Residual" = some.residuals)

#accurcay of the model with all the predictors
accuracy(data.final.lm.pred4, valid.df$Rating)
plot(data.final.lm4)
plot(data.final.lm.pred4, valid.df$Rating, xlab = "Actual", ylab = "Predcited")


# use step() to run stepwise regression.
# model - 4 Using backward regression
data.final.lm.backward4 <- step(data.final.lm4, direction = "backward")
summary(data.final.lm.backward4)  # Which variables were dropped?
data.final.lm.backward4.pred <- predict(data.final.lm.backward4, valid.df)
accuracy(data.final.lm.backward4.pred, valid.df$Rating)


# model 4 - using stepwise
# use step() to run stepwise regression.
data.final.lm.stepwise4 <- step(data.final.lm4, direction = "both")
summary(data.final.lm.stepwise4)  # Which variables were dropped/added?
data.final.lm.stepwise4.pred <- predict(data.final.lm.stepwise4, valid.df)
accuracy(data.final.lm.stepwise4.pred, valid.df$Rating)
