

# PROJECT - CREDIT CARD ANALYSIS

#load the data

cc=read.csv(file.choose(),header = TRUE)

# Data Exploration

dim(cc)
head(cc,3)

names(cc)

table(cc$Class)

summary(cc$Amount)
var(cc$Amount)
sd(cc$Amount)


# Data Manipulation----- we will scale our data using the scale() function. 
#   We will apply this to the amount component of our creditcard_data amount. 
#   Scaling is also known as feature standardization. With the help of scaling,
#   the data is structured according to a specified range. Therefore, 
#   there are no extreme values in our 
#   dataset that might interfere with the functioning of our model

cc$Amount=scale(cc$Amount)
summary(cc$Amount)

new_data=cc[,-1]
head(new_data,2)



# Data Modeling------ we will split our dataset into training set
#                     as well as test set with a split ratio of 0.80.


library('caTools')

set.seed(123)
split=sample.split(new_data$Class,SplitRatio = .80)
train=subset(new_data,split==T)
test=subset(new_data,split==F)

dim(new_data)
dim(train)
dim(test)

# Fitting Logistic Regression Model

logistic=glm(Class~.,data = train,family = 'binomial')
summary(logistic)

plot(logistic)

pred_class=predict(logistic,test,probability=TRUE)
head(pred_class,3)

install.packages('pROC')
library('pROC')
ROC=roc(test$Class,pred_class,plot = TRUE,color='blue')


pred=predict(logistic,test,type = 'response')
test$pred=predict(logistic,test,type = 'response')
head(test)

test$pred=ifelse(test$pred>.5,1,0)
head(test)

tab=table(test$pred,test$Class)

table(test$pred)
table(test$Class)

accuracy=sum(diag(tab))/sum(tab)
accuracy



#  Fitting a Decision Tree Model------- 
#  Decision Trees to plot the outcomes of a decision.
#  These outcomes are basically a consequence through which
#  we can conclude as to what class the object belongs to.

# import libraries for decision tree

library(rpart)
library(rpart.plot)

dt=rpart(Class~.,data = train,method = 'class')
test$pred_dt=predict(dt,test,type='class')
head(test)

table(test$class)
table(test$pred_dt)
table(test$Class)

tab=table(test$Class,test$pred_dt)
accuracy_dt=sum(diag(tab))/sum(tab)
accuracy_dt

print(paste("The accuracy of the decison tree model is",accuracy_dt))

rpart.plot(dt)


# Artificial Neural Network

# Artificial Neural Networks are a type of machine learning algorithm 
# that are modeled after the human nervous system.
# The ANN models are able to learn the patterns using the historical data 
# and are able to perform classification on the input data.

#install packages and import library

install.packages('neuralnet')
library('neuralnet')

ann=neuralnet(Class~.,train,linear.output = FALSE)
plot(ann)

pred_ann=compute(ann,test)
result_ann=pred_ann$net.result
head(ann,10)

test$result_ann=ifelse(result_ann>.5,1,0)






#      Gradient Boosting (GBM)



library(gbm, quietly=TRUE)
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train, test)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train) / (nrow(train) + nrow(test))
  )
)
# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")



model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
#Plot the gbm model
plot(model_gbm)



gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)







 



