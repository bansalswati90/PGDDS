################### SVM Handwritten Digit Recogniser MNIST DATASET ##################

#####################################################################################

# 1. Business Understanding: 

#The objective is to correctly classify the handwritten digits(0-9) based on the pixel 
#values given as features.

#####################################################################################

# 2. Data Understanding: 

#The training set contains 60000 observations of 28x28 image data, and the test set 10000 observations
#Each row corresponds to an observation and each
#column corresponds to a pixel ranging from 0-255 in the image.
#0 correponds to white colour and 255 corresponds to black colour
#y(Label): Corresponds to number between 0 and 9, which is the digit written down.

#Number of Attributes: 785 (784 continuous, 1 nominal class label)

#3. Data Preparation: 

# setting the current working directory using setwd()

#Loading libraries
library(kernlab)
library(readr)
library(caret)
library(e1071)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#Loading Data

mnist_train <- read.csv("mnist_train.csv",stringsAsFactors = FALSE ,header = FALSE)

mnist_test <- read.csv("mnist_test.csv",stringsAsFactors = FALSE,header = FALSE)

#Naming the 1st column as label "Digit" as it contains the digit values which we need to recognise
#rest all columns are pixel values we will name it as Pixel_number
colnames(mnist_train)[1] <- c("Digit")
for (i in seq(2, ncol(mnist_train), by = 1)) {
  colnames(mnist_train)[i] <- paste("Pixel", as.character(i - 1), sep = "_")
}

colnames(mnist_test)[1] <- c("Digit")
for (i in seq(2, ncol(mnist_test), by = 1)) {
  colnames(mnist_test)[i] <- paste("Pixel", as.character(i - 1), sep = "_")
}

#Understanding Dimensions
dim(mnist_train)
#60000   785

dim(mnist_test)
#10000   785

#The data has 784 attributes
#Structure of the dataset
str(mnist_train)
str(mnist_test)

#printing first few rows
head(mnist_train)
head(mnist_test)

#Exploring the data
summary(mnist_train)
summary(mnist_test)

#checking missing/NA  valueS
sapply(mnist_train, function(x) sum(is.na(x)))
sapply(mnist_test, function(x) sum(is.na(x)))
#There are no missing values

#Remove duplicate values (if any) in the dataset
nrow(unique(mnist_train))  #60000
#The total number of rows in still 60000 indicating all values are unique in the dataset,
#there are no duplicated values.
nrow(unique(mnist_test))   #10000
#The total number of rows in still 10000 indicating all values are unique in the dataset, 
#there are no duplicated values.

#Check  missing values
sum(is.na(mnist_train)) #0 indicates no missing value
colSums(is.na(mnist_train))

sum(is.na(mnist_test))
colSums(is.na(mnist_test))
#0 , indicates there are no missing values in the dataset

#Checking NULL values
sum(is.null(mnist_train))
is.null(mnist_train)

sum(is.null(mnist_test))
is.null(mnist_test)
#There are no NULL values

#Checking blank values
sapply(mnist_train, function(x) length(which(x=="")))
sapply(mnist_test, function(x) length(which(x=="")))
#There are no blank values

#Converting our target class that is Digit to factor
mnist_train$Digit <- as.factor(mnist_train$Digit)
mnist_test$Digit <- as.factor(mnist_test$Digit)

#Columns having Zero-Varience i.e same values
#train_data <- mnist_train[sapply(mnist_train, function(x) length(unique(na.omit(x)))>1)]
#test_data <- mnist_test[sapply(mnist_test, function(x) length(unique(na.omit(x)))>1)]
#We do not remove columns which have only zero value,because it represent the position or pixel on the axis

#All 784 columns, except first column(that is the Digit columns have numbers 0-9), 
#are having values between 0 and 255
#it is the intensity of colour 0 represents white and 255 represents black
min_max_traindf <- data.frame(min = sapply(mnist_train[, -1], min),
                              max = sapply(mnist_train[, -1], max))
min_max_testdf <- data.frame(min = sapply(mnist_test[, -1], min),
                             max = sapply(mnist_test[, -1], max))
#We will divide all the pixels value by the highest pixel value of pixel i.e. 
#255 inorder to bring down the scale
#Pixel value ranges from 0-255 , 0 being the lowest and 255 being the highest
mnist_test[,-1] <- sapply(mnist_test[,-1], function(x) x/255)
mnist_train[,-1] <- sapply(mnist_train[,-1], function(x) x/255)
#Since we would be sampling small amount of data as compared to the whole train data , 
#scaling of data is better to avoid biasing of high or low values on the model
#kernels internally use dot product for computation and with higher values computation becomes more and more complex
#Now the range of pixel values after scaling would be from 0.00 to 1.00

#Outlier treatment is not required because 1st column has values from 0-9 , and rest 784 columns 
#have range of 0-255

#Plotting intensity of Digits in the given data
mnist_train1 <- mnist_train
#Taking the mean of each row in train dataset
mnist_train1$intensity <- apply(mnist_train[,-1], 1, mean)

intensitybydigit <- aggregate (intensity ~ Digit, data = mnist_train1, FUN = mean)

plotintensity <- ggplot(data=intensitybydigit, aes(x=Digit, y = intensity)) +
                 geom_bar(stat="identity")

plotintensity + scale_x_discrete(limits = 0:9) + 
  xlab("Digit Label") + ylab("Average Intensity") + theme_classic()
#Since the same digit can be written differently the intensity varies

#Each digit has a huge variation in the way it is written
#####################################################################################

#3. Model Building: 

#Taking only some random sample(3%) of the train data as the train data is huge 
#and it takes a lot of time to build model
set.seed(100)

train.indices = sample(1:nrow(mnist_train), 0.03*nrow(mnist_train))
train = mnist_train[train.indices, ]

#We will test on the whole test data
test = mnist_test

#4. Constructing Model
##############################################################################
#4.1 Using Linear Kernel- vanilladot
###############################################################################
#Performing vanilladot
Model_linear <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "vanilladot")

#Predicting the linear model on test data
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$Digit)

#Accuracy    : 90.15% with default hyperparameter C
#Kappa       : 0.942
#With good values of sensitivity and specificity
##############################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM 

# We will use the train function from caret package to perform crossvalidation
# We will use 5 fold crossvalidation with metric as Accuracy
trainControl <- trainControl(method="cv", number=5)

metric <- "Accuracy"

set.seed(10)

#Value of C from 1 to 10 in steps of 2
grid <- expand.grid(C=seq(1, 10, by=2))

# Performing 5-fold cross validation with metric used as Accuracy and range of C as 1 to 10
#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svmlinear <- train(Digit~., data=train, method="svmLinear", metric=metric, 
                       tuneGrid=grid, trControl=trainControl)


# Printing cross validation result for linear SVM
print(fit.svmlinear)
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was C = 1. 
#C  Accuracy   Kappa    
#1  0.9005426  0.8894517
#3  0.9005426  0.8894517
#5  0.9005426  0.8894517
#7  0.9005426  0.8894517
#9  0.9005426  0.8894517
# Accuracy - 90.05%

# Plotting "fit.svmlinear" results
plot(fit.svmlinear)

###############################################################################
# Validating the model after cross validation on test data

evaluate_linear_test<- predict(fit.svmlinear, test)
confusionMatrix(evaluate_linear_test, test$Digit)

#Accuracy    : 90.15% (For a linear model)
#Kappa       : 0.8905 
#Senstivity  : 90.02%
#Specificity : 98.90%

###############################################################################
#Non-linear model Kernel
###############################################################################
#4.2 Using polynomial Kernel- polydot
###############################################################################
#Using polynomial Kernel
Model_Poly <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "polydot")

# Predicting the model results on test data
Eval_Poly<- predict(Model_Poly, test)

#confusion matrix - Poly Kernel
confusionMatrix(Eval_Poly,test$Digit)

#Accuracy   : 90.15%
#Kappa      : 0.8905  
#The default hyperparameters are giving good accuracy
#with good values of sensitivity and specificity

#####################################################################
# Hyperparameter tuning and Cross Validation  - Polynomial - SVM 
#cross validation to pick the best value of sigma and C , so that we get the model
#which gives the optimal accuracy

#5 fold Cross validation
trainControl <- trainControl(method="cv", number=5)

metric <- "Accuracy"

set.seed(10)
grid <- expand.grid(.degree=seq(1,3,by=1), .scale=c(0.1,0.5,1,2), .C=c(0.1,0.5,1))

#Appling 5 Fold Cross validation on the various combination of degree scale and 
#C using train for metric Accuracy
fit.svmpoly <- train(Digit~., data=train, method="svmPoly", metric=metric, 
                     tuneGrid=grid, trControl=trainControl)

#Printing SVM polynomial
print(fit.svmpoly)
#Plotting SVM polynomial
plot(fit.svmpoly)

#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were degree = 2, scale = 2 and C = 0.1

#Accuracy - 92.11%
#Accuracy increases after tuning the parameter

###############################################################################
# Validating the model after cross validation on test data

evaluate_non_linear_poly <- predict(fit.svmpoly, test)
confusionMatrix(evaluate_non_linear_poly, test$Digit)

# Accuracy  : 93.29% (For a polynomial model)
# Kappa     : 0.9254
#Senstivity : 93.22%
#Specificity: 99.25%

###############################################################################
#4.3 Using RBF Kernel- rbfdot
###############################################################################
#Using RBF Kernel
Model_RBF <- ksvm(Digit~ ., data = train, scale = FALSE, kernel = "rbfdot")

# Predicting the model results 
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$Digit)

#Accuracy   : 93.01%
#Kappa      : 0.9223
#With good value of sensitivity and specificity
#The default hyperparameters , c and the sigma values RBF kernel is doing a very good job

################################################################################
#Hyperparameter tuning and Cross Validation  - RBF - SVM 

#cross validation to pick the best value of sigma and C , 
#so that we get the model which gives the optimal accuracy

trainControl <- trainControl(method="cv", number=5)

metric <- "Accuracy"

set.seed(10)
grid <- expand.grid(.sigma=c(0.025,0.05), .C=c(0.1,0.5,1,2,3))

#train to get the best fit radial vlues for Accuracy metric for 5 folds wit different values of C and sigma
fit.svmradial <- train(Digit~., data=train, method="svmRadial", metric=metric, 
                       tuneGrid=grid, trControl=trainControl)

#Printing the best fit values
print(fit.svmradial)
#sigma  C    Accuracy   Kappa    
#0.025  0.1  0.8283693  0.8091366
#0.025  0.5  0.9216724  0.9129342
#0.025  1.0  0.9311184  0.9234375
#0.025  2.0  0.9405598  0.9339315
#0.025  3.0  0.9400027  0.9333126
#0.050  0.1  0.3572183  0.2796022
#0.050  0.5  0.8916676  0.8796230
#0.050  1.0  0.9211245  0.9123410
#0.050  2.0  0.9233452  0.9148089
#0.050  3.0  0.9233452  0.9148089

#Plotting the best fit radial values
plot(fit.svmradial)
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were sigma = 0.025 and C = 2

#Accuracy : 94.05%
#The accuracy increases after tuning the parameter

###############################################################################
# Validating the model after cross validation on test data

evaluate_non_linear_rbf <- predict(fit.svmradial, test)
confusionMatrix(evaluate_non_linear_rbf, test$Digit)

# Accuracy   : 94.78% (for RBF)
# Kappa      : 0.942
#Senstivity  : 94.73%
#Specificity : 99.42%

##############################################################################
#(All the values achieved are with 3% train data and full test data , the values differ 
#when we take different percentage of each , to reduce the computational time we have used less samples)

#So the accuracy of model on the test data is
#1. Linear - 90.15% with hyperparameter C=1
#2. Polynomial - 93.29% with hyperparameter degree = 2, scale = 2 and C = 0.1
#3. RBF - 94.78% with hyperparameter C= 2 , sigma = 0.025

#If we are not scaling the pixel values , the accuracy achieved by 
#RBF kernel(11% with sigma = 0.05 and C = 0.1) is too low, the values are low because it is able to detect only digit 1
#due to biasing of extremely high or extremely low value of pixel values in the sample data
#Accuracy of linear(90.15% with C=1) and 
#Polynomial(93.32% with degree = 2, scale = 0.1 and C = 0.1) kernel models is better than the RBF
#In this case we can use nonlinear-Polynomial kernel to build the model

#Hence we conclude that this dataset required non-linearity 
#By bringing non-linear kernel we could do remarkably better than the linear kernel and the accuracy increased
#The order of complexity increases from linear kernel to RBF kernel.
#The hyperparameter sigma directly controls the amount of non-linearity in the decision boundary.
#The higher the sigma, the more nonlinear the boundary.
#C is the Cost hyperparameter more the value more is the complexity of model

#It depends on business and good knowlege of the domain, and also the computational time, hardware and cost at hand to select the best model
#Here models are giving a pretty decent accuracy ,sensitivity and specificity, and depending upon the business we can select any.
#RBF kernel is giving us the best accuracy(in case of scaling) and is doing better than the linear and polynomial kernel
#Polynomial kernel gives consistent results at both places
#The models are performing well on train and test data both , hence there is no overfitting or underfitting