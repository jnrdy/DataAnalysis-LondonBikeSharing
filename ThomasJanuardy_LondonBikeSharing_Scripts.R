#Thomas Januardy 00000046001
#Group B5

#Dataset: London Bike Sharing

#---library---
library(readr) #read.csv
library(dplyr) #function %>%
library(ggplot2) #plotting
library(Amelia) #missmap
library(caTools) #set.seed
library(e1071) #svm
library(rpart) #decision tree
library(party) #decision tree
library(rpart.plot) #decision tree
library(caret) #confusion matrix
library(lubridate) #timestamp (datetime)

#---read data---
london <- read.csv("B5_ThomasJanuardy_00000046001.csv")
#View(london)
+
str(london)

london <- london %>% select(cnt, t1, t2, hum, timestamp, season, weather_code,
                            wind_speed, is_weekend, is_holiday)

#---change column names---
london <- london %>% rename("Rentals" = "cnt",
                    "Temperature" = "t1",
                    "Feels-Like Temperature" = "t2",
                    "Humidity" = "hum",
                    "Timestamp" = "timestamp",
                    "Season" = 'season',
                    'Weather' = 'weather_code',
                    'Windspeed' = 'wind_speed',
                    'Workday' = 'is_weekend',
                    'Holiday' = 'is_holiday')

#---check missmap---
missmap(london)
#No missing data

#---convert "timestamp" to datetime variable type---
london$Timestamp <- as_datetime(london$Timestamp)


#---convert "season" to factor variable---
london$Season <- factor(
  london$Season, levels = c(0,1,2,3),
  labels = c('Spring', 'Summer', 'Fall','Winter'),
  ordered = TRUE)

#---visualization: Rentals---
ggplot(london, aes(Rentals))+ 
  geom_histogram(fill = 'orange', colour="black")+
  labs(y="Count", title = "London Bike Rentals Plot")

#---Visualization: Rentals by Season plot---
col <- c("olivedrab3", 'yellow', 'orange', 'grey50')

options(repr.plot.width=10, repr.plot.height=8)
london %>% group_by(Season) %>%
  summarise(n = n(), rent = sum(Rentals)) %>%
  ggplot(aes(Season, rent, fill = Season)) + 
  geom_bar(stat = "identity", show.legend = F, color = 'black') +
  theme_bw(base_size = 16) + scale_fill_manual(values = col) +
  labs(title = "Bike Rentals by Season", x = "", y = "Total Rentals") +
  scale_y_continuous(labels = scales::label_comma())

#---visualization: Rentals by Season ---
ggplot(london, aes(Rentals, fill = Season))+
  geom_histogram(colour="black")+
  facet_wrap(~ Season)

#---visualization: Rentals by Season Boxplot ---
ggplot(london, aes(Rentals, fill = Season))+
  geom_boxplot()+
  facet_wrap(~ Season)

#---visualization: Rentals by Season (with Temperatures)---
col <- c("olivedrab3", 'yellow', 'orange', 'grey50')

ggplot(london, aes(Temperature, Rentals, color = Season)) +
  geom_jitter(width = 0.25) + scale_color_manual(values = col) +
  labs(y="Total Rentals", title = "Rentals with Temperature by Season") +
  facet_grid(.~Season) + theme_bw(base_size = 12)


#---Algorithm 1: Support Vector Machine---

#splitting

set.seed(46001)
sample <- sample(nrow(london), 0.6*nrow(london))

training <- london[sample,]
testing <- london[-sample,]

nrow(training)
nrow(testing)

#fitting SVM

londonSVM = svm(formula = Season ~ .,
                 data = training,
                 type = 'C-classification',
                 kernel = 'linear')

summary(londonSVM)

#predicting (training)
londonSVM_pred <- predict(londonSVM, training)
londonSVM_pred

#confusion matrix
confusionMatrix(londonSVM_pred, training$Season)

#predicting (testing)
londonSVM_pred2 <- predict(londonSVM, testing)
londonSVM_pred2

#confusion matrix
confusionMatrix(londonSVM_pred2, testing$Season)


#---Algorithm 2: Decision Tree---

#splitting
set.seed(46001)
sample2 <- sample(nrow(london), 0.6*nrow(london))

training2 <- london[sample2,]
testing2 <- london[-sample2,]

nrow(training2)
nrow(testing2)



#decision tree: rpart
london_rpart <- rpart(Season~., data=training2)
rpart.plot(london_rpart, box.palette="RdBu", cex = 0.6)
print(london_rpart)

#predicting and confusion matrix (training)
predict_rpart <- predict(london_rpart,training,type="class")
table_rpart <- table(predict_rpart,training$Season)

caret::confusionMatrix(table_rpart)

#predicting and confusion matrix (testing)
predict_rpart2 <- predict(london_rpart,testing,type="class")
table_rpart2 <- table(predict_rpart2,testing$Season)

caret::confusionMatrix(table_rpart2)











