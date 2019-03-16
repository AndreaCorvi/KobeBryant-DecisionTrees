# Classification with decision trees on "Kobe Bryant Shot Selection" dataset #

# Packages needed
library(ggplot2)
library(readr)
library(plyr)
library(dplyr)
library(tibble)
library(magrittr)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(printr)
library(caret)
library(gridExtra)
library(scales)
library(pROC)

# Dataset upload
Project_data <- read.csv("~/Desktop/AC/NewProject/KobeData.csv")
data <- Project_data

# Remove NAs 
data = data %>% 
  na.omit()

# Check type of data for analysis
unique(data$action_type)
unique(data$combined_shot_type)
is.factor(data$action_type)
is.factor(data$shot_zone_range)
is.factor(data$shot_zone_basic)
is.factor(data$shot_zone_area)
is.factor(data$opponent)
is.factor(data$home_away)
is.factor(data$game_month)
is.numeric(data$time_remaining)
is.numeric(data$shot_distance)
is.factor(data$season)
data$shot_made_flag = as.factor(data$shot_made_flag)
is.factor(data$shot_made_flag)
levels(data$shot_made_flag) # Change name of factors in "shot_made_flag"
levels(data$shot_made_flag) = c("Miss","Made")
is.numeric(data$game_year)
unique(data$action_type)

## Gross %s
plyr::count(data$home_away) 
count <- sum(data$playoffs=="1") 
count
# Away Gross % = 43.64%
# Home Gross % = 45.65%
# <24s % = 31.97%
# >24s % = 45.42%
# <60s % = 38.05%
# >60%s = 45.54%
# Overall = 44.62%


count <- sum(data$shot_made_flag=="Made" & data$time_remaining < 60)
count
count <- sum(data$shot_made_flag=="Made")


### Data Visualization ###

# Set color palette
colScale <- scale_colour_manual(name = "colors",values = mypalette)
mypalette= c("#ffa153","#189cff")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

# Plot with made and missed shots
total.shoots = ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=shot_made_flag))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_manual(name = "colors",values = mypalette)+
  ggtitle("Total Shots - Kobe Bryant")

total.shoots

# Plot of made and missed shots divided per season
seasons.shots = ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=shot_made_flag))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_manual(name = "colors",values = mypalette)+
  ggtitle("Total Shots - Kobe Bryant - Seasons")+
  facet_wrap(~season)

seasons.shots   

# Plot of shooting range
range.shoots= ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=shot_zone_range))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_manual(values = cbPalette)+
  ggtitle("Range - Kobe Bryant")

range.shoots
# Plot of court zones
basic.shoots= ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=shot_zone_basic))+
  geom_point(alpha=0.7, size=0.9)+
  scale_color_manual(values = cbPalette)+
  ggtitle("Area - Kobe Bryant")

basic.shoots

# Plot of shot types
shot.type = ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=combined_shot_type))+
  geom_point(alpha=0.7, size=0.4)+
  scale_color_manual(name = "colors",values = cbPalette)+
  ggtitle("Shots Type")

shot.type

#Plot of action types
action.type = ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=action_type))+
  geom_point(alpha=0.7, size=0.5)+
  ggtitle("Action Types")

action.type

# Plot of shots made and missed in the last 24 seconds
time.shot = ggplot2::ggplot(data, aes(x=loc_x, y=loc_y, color=shot_made_flag))+
  geom_point(data = data[which(data$time_remaining < 24),], alpha=0.7, size=0.9)+
  scale_color_manual(name = "colors",values = mypalette)+
  ggtitle("Last Minute Shots - Kobe Bryant")

time.shot

# Plot of made and missed shots versus every opponent
opponent.shot = ggplot2::ggplot(data, aes(x=opponent, fill=shot_made_flag))+
  geom_bar(alpha=0.7, size=0.9)+
  scale_color_manual(name = "colors",values = mypalette)+
  ggtitle("Total Shots - Kobe Bryant")

opponent.shot

#### Data Analysis ###

# Dropping unuseful columns
names(data)
sub.data =  data %>% 
  dplyr::select(-game_event_id,-game_id,-minutes_remaining,-lon,-lat)

# Understanding dataset variable behaviour
plyr::count(sub.data$shot_made_flag)

# Setting data types
std_loc_x = scale(data$loc_x, scale = TRUE, center = TRUE)
std_loc_y = scale(data$loc_y, scale = TRUE,center = TRUE)
angle = std_loc_x/std_loc_y
  
sub.data <- sub.data %>% 
  mutate(angle)
# Angle isn't actually used later but could be helpful for other works

names(sub.data)

# Split data into training set and test set
set.seed(12345)
train.indices <- createDataPartition(sub.data$shot_made_flag,p=0.8,list=FALSE)
data.train <- sub.data[train.indices, ]   # Train data
data.test  <- sub.data[-train.indices, ]  # Test data

# Training
rpmodel <- rpart(shot_made_flag~action_type+shot_distance+home_away+season+game_year+
                   opponent+time_remaining+shot_zone_basic+shot_zone_range+shot_zone_area+game_month+playoffs,data=data.train,
                 control=rpart.control(minsplit=1, minbucket=1, cp=0.001, xval = 10),parms=list(split="gini"))
printcp(rpmodel)
plotcp(rpmodel)
# Test
rpresults <- rpart.predict(rpmodel, newdata=data.test, type=c("class"))
# Confusion Matrix
rpconfMat= table(rpresults, data.test$shot_made_flag)
addmargins(rpconfMat)
rpaccuracy <- sum(diag(rpconfMat))/sum(rpconfMat)
rpaccuracy
t.error= 1-rpaccuracy
t.error
summary(rpresults)
summary(rpmodel)
precision = 1025/2293
precision
recall = 1025/1375
recall
f1 = 2*((precision*recall)/(precision+recall))
f1
# Plots
rpart.plot::rpart.plot(rpmodel,1)
rpart.plot::rpart.plot(rpmodel)

## Roc
pred.prob = predict(rpmodel, data.test, type="prob")
auc = (auc(data.test$shot_made_flag,pred.prob[,2]))
plot(pROC::plot.roc(data.test$shot_made_flag,pred.prob[,2]),print.auc=TRUE,legacy.axis=TRUE, width = 3, col="blue")
auc
pred.prob
summary(pred.prob)
