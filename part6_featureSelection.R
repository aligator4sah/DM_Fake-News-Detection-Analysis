#part67- feature selection

##remove redundant features
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# calculate correlation matrix
#get df from part 2
dataset8 = df[c("author","language","site_url","country","likes","shares","spam_score","Gave_Fake",
                "text_contains_keywords","title_contains_keywords","main_img_url","type")]

dataset8$language<- as.numeric(as.factor(dataset8$language))
dataset8$author<- as.numeric(as.factor(dataset8$author))
dataset8$site_url<- as.numeric(as.factor(dataset8$site_url))
dataset8$country<- as.numeric(as.factor(dataset8$country))
dataset8$main_img_url<- as.numeric(as.factor(dataset8$main_img_url))
str(dataset8)

correlationMatrix <- cor(dataset8[,-12])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

################################################################################
#Rank features by importance
set.seed(7)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(type~., data=dataset8, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

###################################################################################
#Feature selection
# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(dataset8[,1:11], dataset8[,12], sizes=c(1:11), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
