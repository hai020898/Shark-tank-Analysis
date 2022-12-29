attach(shark_tank)
require(dplyr)
library(car)
library(ggplot2)
library(boot)
library(psych)
library(corrplot)
library(methods)
library(tidyverse)
require(lmtest)
require(plm)
library(splines)
library(caTools)
library(data.table)
library(stargazer)
install.packages("e1071")
library(e1071) 
library(ggplot2)


install.packages('tree')
install.packages('rpart.plot')
library(tree)
library(rpart)
library(rpart.plot)
install.packages('randomForest')
library(randomForest)
install.packages("formattable")
library(formattable)
install.packages("rms")
require(rms)
install.packages('gbm')
library(gbm)

install.packages('MASS')
install.packages('klaR')
library(MASS)
library(klaR)
library(caret)

#########Data description

##Quantitative variables

#askedFor
summary(askedFor)
boxplot(askedFor, col="light blue")
hist(askedFor, col="light blue")
skewness(askedFor)
nrow(shark_tank[shark_tank$askedFor <=500000,])
nrow(shark_tank[shark_tank$askedFor >1000000,])
#exchangeForStake

summary(exchangeForStake)
boxplot(exchangeForStake, col="light blue")
hist(exchangeForStake, col="light blue")
skewness(exchangeForStake)

#valuation

summary(valuation)
boxplot(valuation, col="light blue")
hist(valuation, col="light blue")
skewness(exchangeForStake)
nrow(shark_tank[shark_tank$exchangeForStake <= 20,])
nrow(shark_tank[shark_tank$exchangeForStake > 20 & shark_tank$exchangeForStake <= 40 ,])
nrow(shark_tank[shark_tank$valuation <=5000000,])



#Quantitative variables summary
df <- data.frame(
  Variable = c("askedFor","exchangeForStake","valuation"),
  Minimum = c(10000,3.00,40000),
  Median = c(150000,15.00,100000),
  Mean = c(258491,
           17.54,
           2165615),
  
  Maximum = c(5000000,
              100.00,
              30000000),
  Skewness = c("Left",
               "Left",
               "Left"),
  stringsAsFactors = FALSE)



formattable(df, list(
  #r_squared = color_tile("light blue", "green"),
  Skewness= formatter("span", style = x ~ ifelse(x == "Left",
                                                 style(color = "purple", font.weight = "bold"), NA))
))

#Correlation matrix for quantitative variables
numeric = shark_tank[,c('askedFor','exchangeForStake','valuation' )]
cormat <- round(cor(numeric),3)
stargazer(cormat, title='Correlation matrix', type='html')

#Preprocessing data

attach(shark_tank)

#dropped irrelevant column
shark_tank = shark_tank%>%dplyr::select(-c("description", "entrepreneurs", "website", "title"))

#create state column
shark_tank$state = str_sub(shark_tank$location, -2)

#create region column
shark_tank = shark_tank %>% mutate(region =
                         case_when(state %in% c('WA','OR','ID','MT','WY','CO','UT','NV','CA','AK','HI','AZ','NM')~'West',
                                   state %in% c('ND','MN','WI','MI','SD','IA','IL','IN','OH','NE','KS','MO')~'Midwest',
                                   state %in% c('ME','NH','VT','MA','RI','CT','NY','NJ','PA')~'Northeast',
                                   state %in% c('TX','OK','AR','LA','KY','TN','MS','AL','GA','FL','SC','NC','VA','WV','DC','MD','DE')~'South'))

#Drop location column

shark_tank = shark_tank%>%dplyr::select(-c("location"))

attach(shark_tank)

#Drop episode-season column

shark_tank = shark_tank%>%dplyr::select(-c("episode.season"))
attach(shark_tank)

#The shark variable
shark_tank$barbara = 0
shark_tank$lori = 0
shark_tank$robert = 0
shark_tank$kevin_o = 0
shark_tank$steve= 0
shark_tank$daymond = 0
shark_tank$jeff = 0
shark_tank$mark = 0
shark_tank$kevin_h = 0
shark_tank$john = 0
shark_tank$nick = 0

shark_tank$barbara = ifelse(shark_tank$shark1 == 'Barbara Corcoran'|shark_tank$shark2 == 'Barbara Corcoran',1,0)
shark_tank$lori = ifelse(shark_tank$shark1 == 'Lori Greiner',1,0)
shark_tank$robert = ifelse(shark_tank$shark2 == 'Robert Herjavec'|shark_tank$shark3 == 'Robert Herjavec',1,0)
shark_tank$kevin_o = ifelse(shark_tank$shark2 == "Kevin O'Leary"|shark_tank$shark2 == "Kevin O'Leary"|shark_tank$shark3 == "Kevin O'Leary",1,0)
shark_tank$steve = ifelse(shark_tank$shark2 == 'Steve Tisch',1,0)
shark_tank$daymond = ifelse(shark_tank$shark3 == 'Daymond John'|shark_tank$shark4 == 'Daymond John'|shark_tank$shark5 == 'Daymond John',1,0)
shark_tank$jeff = ifelse(shark_tank$shark4 == 'Jeff Foxworthy',1,0)
shark_tank$mark = ifelse(shark_tank$shark4 == 'Mark Cuban'|shark_tank$shark5 == 'Mark Cuban',1,0)
shark_tank$kevin_h = ifelse(shark_tank$shark5 == 'Kevin Harrington',1,0)
shark_tank$john = ifelse(shark_tank$shark5 == 'John Paul DeJoria',1,0)
shark_tank$nick = ifelse(shark_tank$shark5 == 'Nick Woodman',1,0)

attach(shark_tank)

#Drop other irrelevant columnas
shark_tank = shark_tank%>%dplyr::select(-c("shark1",'shark2','shark3','shark4','shark5','state','episode','season','valuation'))




shark_tank = shark_tank %>%
  dplyr::group_by(category)%>%
  dplyr::mutate(category = ifelse(n()<24, 'other', category))

attach(shark_tank)



#Dummify variables
shark_tank$Multiple.Entreprenuers = as.factor(shark_tank$Multiple.Entreprenuers)
shark_tank$deal=as.factor(shark_tank$deal)
shark_tank$category = as.factor(shark_tank$category)
shark_tank$region = as.factor(shark_tank$region)

attach(shark_tank)

##Split train set and test set


##################Model Testing###########################

#RandomForest for Feature Selection
myforest = randomForest(deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+barbara+lori+robert+kevin_o+steve+daymond+jeff+mark+kevin_h+john+nick,
                        ntree=500, data=shark_tank, importance=TRUE, na.action=na.omit)


importance(myforest)
varImpPlot(myforest)

stargazer(importance(myforest), title='Variable importance', type = 'html')

##Eliminate variables with negative Mean Decrease Accuracy: barbara, lori, kevin_o, steve, daymond, jeff, john

shark_tank = shark_tank%>%dplyr::select(-c("barbara",'lori','kevin_o','steve','daymond','jeff','john'))


index <- sample(nrow(shark_tank),nrow(shark_tank)*0.70)
shark_tank_train = shark_tank[index,]
shark_tank_test = shark_tank[-index,]
attach(shark_tank_train)

set.seed(1)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf_default = train(deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+robert+kevin_h+nick,
                   data = shark_tank_train,
                   method='rf',
                   metric='Accuracy',
                   trControl = trControl)
#Find the best mtry
set.seed(1)
tuneGrid <- expand.grid(.mtry = c(1: 10))


rf_mtry = train(deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+robert+kevin_h+nick,
                   data = shark_tank_train,
                   method='rf',
                   metric='Accuracy',
                   tuneGrid = tuneGrid,
                   trControl = trControl,
                   importance = TRUE,
                   nodesize = 14,
                   ntree=300)

print(rf_mtry)

best_mtry = rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy)

#Find the best maxnode

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 30)) {
  set.seed(1)
  rf_maxnode <- train(deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+robert+kevin_h+nick,
                      data = shark_tank_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#Find the best ntree
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(1)
  rf_maxtrees <- train(deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+robert+kevin_h+nick,
                       data = shark_tank_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 20,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#Train the model with mtry=1, ntree=1000, maxnodes=20

fit_rf <- train(deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+robert+kevin_h+nick,
                shark_tank_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 1000,
                maxnodes = 20)

#Making prediction 

prediction = predict(fit_rf, shark_tank_test)
confusionMatrix(prediction, shark_tank_test$deal)



#Boosted forest

shark_tank1 = shark_tank
shark_tank1$deal=ifelse(shark_tank1$deal =='FALSE', 0, 1)

shark_tank1_train = shark_tank_train
shark_tank1_test = shark_tank_test

shark_tank1_train$deal=ifelse(shark_tank_train$deal =='FALSE', 0, 1)
shark_tank1_test$deal=ifelse(shark_tank_test$deal =='FALSE', 0, 1)

attach(shark_tank_train)

#create hyperparameter grid

hyper_grid <- expand.grid(
  
  interaction.depth = c(1:10),
  n.trees = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000), 
  optimal_trees = list(),               # a place to dump results
  min_RMSE = list()                     # a place to dump results
)

trees = c(1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,20000)
depth = c(1:10)
combo = list()
accuracy = list()
for (a in trees){
  for (b in depth){
    combo = append(combo, list(list(a,b)))
    # reproducibility
    set.seed(1)
    
    # train model
    gbm_tune <- gbm(
      formula = deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+robert+kevin_h+nick,
      distribution = "bernoulli",
      data = shark_tank1_train,
      n.trees = a,
      interaction.depth = b,
      cv=10,
      n.cores = NULL, # will use all cores by default
      verbose = FALSE
    )
    gbm.test = predict(gbm_tune, newdata=shark_tank1_test, type='response', n.trees = a)
    gbm_tune.class=ifelse(gbm.test<0.5,0,1)
    accuracy_score = confusionMatrix(as.factor(gbm_tune.class), as.factor(shark_tank1_test$deal))$overall['Accuracy']
    accuracy = append(accuracy, accuracy_score)
  }
}
combo[which.min(accuracy)]
accuracy[which.min(accuracy)]

gbm_tune <- gbm(
  formula = deal~category+askedFor+exchangeForStake+Multiple.Entreprenuers+region+mark+kevin_h,
  distribution = "bernoulli",
  data = shark_tank1_train,
  n.trees = 2500,
  interaction.depth = 2,
  cv=10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
gbm.test = predict(gbm_tune, newdata=shark_tank1_test, type='response', n.trees = 2500)
gbm_tune.class=ifelse(gbm.test<0.5,0,1)
accuracy_score = confusionMatrix(as.factor(gbm_tune.class), as.factor(shark_tank1_test$deal))$overall['Accuracy']

cm = confusionMatrix(as.factor(gbm_tune.class), as.factor(shark_tank1_test$deal))

##Draw confusion matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(cm)

##Make some prediction

predict(gbm_tune,data.frame(category='other', askedFor = 10000000, exchangeForStake=15, Multiple.Entreprenuers= FALSE, region ='West',robert=1, mark = 1, kevin_h=1,nick=0 ),type='response', n.trees = 2500)

test_data = data.frame(category = c('Specialty Food', 'Novelties'),
                       askedFor = c(100000, 200000),
                       exchangeForStake = c(10,15),
                       Multiple.Entreprenuers = c('FALSE','FALSE'),
                       region = c('West','Midwest'),
                       mark = c(1,0),
                       kevin_h = c(0,1))
test_data$category = as.factor(test_data$category)
test_data$Multiple.Entreprenuers = as.factor(test_data$Multiple.Entreprenuers)
test_data$region = as.factor(test_data$region)

predict(gbm_tune,test_data,type='response', n.trees = 2500)

test_data[1,]


####Gender classification model

myforest_producer = randomForest(region~deal+category+askedFor+exchangeForStake+Multiple.Entreprenuers+barbara+lori+robert+kevin_o+steve+daymond+jeff+mark+kevin_h+john+nick,
                                 ntree=500, data=shark_tank, importance=TRUE, na.action=na.omit)
importance(myforest_producer)
varImpPlot(myforest_producer)

stargazer(importance(myforest_producer), title='Variable importance', type = 'html')

#Eliminate Multiple.Entreprenuers, barbara, lori, robert, steve, daymond, john, kevin_o
mylda = lda(region~deal+category+askedFor+exchangeForStake+Multiple.Entreprenuers+jeff+mark+nick, data=shark_tank1_train)


pred.shark_tank1_train = predict(mylda, shark_tank1_train)$class
pred.shark_tank1_test = predict(mylda, shark_tank1_test)$class
mean(pred.shark_tank1_train == shark_tank1_train$region)
mean(pred.shark_tank1_test == shark_tank1_test$region)



myqda = qda(region~deal+category+askedFor+exchangeForStake+Multiple.Entreprenuers+jeff+mark+nick, data=shark_tank1_train)

pred.shark_tank1_train = predict(myqda, shark_tank1_train)$class
pred.shark_tank1_test = predict(myqda, shark_tank1_test)$class
mean(pred.shark_tank1_train == shark_tank1_train$region)
mean(pred.shark_tank1_test == shark_tank1_test$region)


