library(caret) ### caret library has the confusion matrix
library(forecast) ### forecast library has the prediction errors
library(rpart) 
library(rpart.plot) ### rpart and rpart.plot libraries are for the tree analysis
library(randomForest)
library(e1071)

movies <- read.csv('/Users/kalyanrohithtg/Documents/spring/machinelearning/project/movies/Movies.csv')
View(movies)


train_index<-sample(rownames(movies), dim(movies)[1]*0.6)
valid_index<-setdiff(rownames(movies),train_index)
train_data<-movies[train_index, ]
valid_data<-movies[valid_index, ]

mytree<- rpart(Trailer_Views ~ Marketing_Expense + Production_Expense + Budget + Movie_Length + Lead_.Actor_Rating + Lead_Actress_Rating + Director_Rating + Producer_Rating + X3D_Available + Twitter_Hashtags + Genre + Avg_Age_Actors, data = train_data, method = "anova")

prp(mytree)

predicted_values <- predict(mytree, newdata=valid_data)

accuracy(predicted_values, valid_data$Trailer_Views)


# support vector regression
svm1 <- svm(Trailer_Views ~ Marketing_Expense + Production_Expense + Budget + Movie_Length + Lead_.Actor_Rating + Lead_Actress_Rating + Director_Rating + Producer_Rating + X3D_Available + Twitter_Hashtags + Genre + Avg_Age_Actors, data = train_data)


prediction1<-predict(svm1, valid_data)

# RMSE and other measures
accuracy(prediction1, valid_data$Trailer_Views)


myforest <- randomForest(Trailer_Views ~ Marketing_Expense + Production_Expense + Budget + Movie_Length + Lead_.Actor_Rating + Lead_Actress_Rating + Director_Rating + Producer_Rating + X3D_Available + Twitter_Hashtags + Genre + Avg_Age_Actors, data = train_data) 

### Predict using the validation data
predicted_values_forest <- predict(myforest, newdata=valid_data)

### Prediction performance 
accuracy(predicted_values_forest, valid_data$Trailer_Views)
