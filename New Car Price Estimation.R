#Set the current working directory using setwd(path)

# Check if package is installed in the system. If not install automatically
list.of.packages <- c("car", "MASS", "tidyr","ggplot2","dplyr","reshape2","corrplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Loading Packages
library(car)
library(MASS)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)


#Reading CarPrice_Assignment.csv file using read.csv
raw.carprice <- read.csv("CarPrice_Assignment.csv" , stringsAsFactors = TRUE)

carprice <- raw.carprice

View(carprice)

#Dimension, structure and summary of carprice dataset
dim(carprice) #205 rows and 26 columns
str(carprice)
summary(carprice)

#Remove duplicate values (if any) in the dataset
nrow(unique(carprice))
#The total number of rows in still 205 indicating all values are unique in the dataset, there are no duplicated values.

#Check  missing values
sum(is.na(carprice))
colSums(is.na(carprice))
#0 , indicates there are no missing values in the dataset

#Checking NULL values
sum(is.null(carprice))
is.null(carprice)
#There are no NULL values

#Checking blank values
sapply(carprice, function(x) length(which(x=="")))

#splitting car Company as company name and car name
carprice <- separate(carprice,CarName,into=c("CompanyName" , "ModelName") , sep=" " , extra = "merge", fill = "right")

#Identifying any mis-spelt Car Company Name
unique(carprice$CompanyName)

carprice$CompanyName[carprice$CompanyName == "maxda"] = "mazda"
carprice$CompanyName[carprice$CompanyName == "Nissan"] = "nissan"
carprice$CompanyName[carprice$CompanyName == "porcshce"] = "porsche"
carprice$CompanyName[carprice$CompanyName == "toyouta"] = "toyota"
carprice$CompanyName[carprice$CompanyName == "vokswagen"] = "volkswagen"
carprice$CompanyName[carprice$CompanyName == "vw"] = "volkswagen"
#The actual company name is alfa-romeo not alfa-romero but keeping it as it is give in the dataset as it does not affect our analysis

length(unique(carprice$CompanyName))
#22 unique Car brands are present

str(carprice)

#Grouping company names as per the average price
Companyname_grouping <- (carprice %>%
                           group_by(CompanyName) %>%
                           summarise(Avg_Price = mean(price)) %>%
                           arrange(desc(Avg_Price)))
View(Companyname_grouping)

#Plotting graph to see the average price ranges
ggplot(Companyname_grouping,aes(x = reorder(CompanyName, -Avg_Price),y=Avg_Price)) +
      geom_bar(stat="identity") +
      ggtitle("Company Name vs Average Price")+
      labs(x="Company Name" , y="Average Price")


#Converting to factor
carprice$symboling <- as.factor(carprice$symboling)
carprice$CompanyName <- as.factor(carprice$CompanyName)

#We can bin the Company Names based on the average price of the cars into 3 bins
levels(carprice$CompanyName) <- list(
  'highend' = c('jaguar','buick','porsche','bmw'),
  'midend' = c('volvo','audi','mercury','alfa-romero','peugeot','saab'),
  'lowend' = c('mazda','nissan','volkswagen','toyota','renault','mitsubishi','isuzu','subaru','honda','plymouth','dodge','chevrolet')
  )
#now there are 3 levels highend, midend, lowend based on average price of car

####Outlier Treatment####
#The  outlier values can be best decided as per business requirements, below are the ones considered as per jump in values
#To check for outliers, we find out the quantile values at each 1% interval and wherever there is a high jump from one quantile to another, we cap/floor those values.

#For wheelbase
quantile(carprice$wheelbase, seq(0, 1, 0.01))
summary(carprice$wheelbase)
boxplot.stats(carprice$wheelbase)$out
boxplot(carprice$wheelbase)
#capping value above 99% as there is a huge jump
carprice$wheelbase[which(carprice$wheelbase > 115.544)] <- 115.544

#For carlength
quantile(carprice$carlength, seq(0, 1, 0.01))
summary(carprice$carlength)
boxplot.stats(carprice$carlength)$out
boxplot(carprice$carlength)
#capping value below 1% as there is a huge jump
carprice$carlength[which(carprice$carlength < 144.816)] <- 144.816

#For carwidth
quantile(carprice$carwidth, seq(0, 1, 0.01))
summary(carprice$carwidth)
boxplot.stats(carprice$carwidth)$out
boxplot(carprice$carwidth)
#capping value above 96% 
carprice$carwidth[which(carprice$carwidth > 70.852)] <- 70.852

#For carheight
quantile(carprice$carheight, seq(0, 1, 0.01))
summary(carprice$carheight)
boxplot.stats(carprice$carheight)$out
boxplot(carprice$carheight)
# Based on above boxplot and quantile results, there are no outliers for carheight
# Hence no need to treat outliers for this carheight variable

#For curbweight
quantile(carprice$curbweight, seq(0, 1, 0.01))
summary(carprice$curbweight)
boxplot.stats(carprice$curbweight)$out
boxplot(carprice$curbweight)
# Based on above boxplot and quantile results, there are no outliers for curbweight

#For enginesize
quantile(carprice$enginesize, seq(0, 1, 0.01))
summary(carprice$enginesize)
boxplot.stats(carprice$enginesize)$out
boxplot(carprice$enginesize)
#there is a jump from 95%
carprice$enginesize[which(carprice$enginesize > 201.20)] <- 201.20

#For boreratio
quantile(carprice$boreratio, seq(0, 1, 0.01))
summary(carprice$boreratio)
boxplot.stats(carprice$boreratio)$out
boxplot(carprice$boreratio)
# Based on above boxplot and quantile results, there are no outliers for boreratio

#For stroke
quantile(carprice$stroke, seq(0, 1, 0.01))
summary(carprice$stroke)
boxplot.stats(carprice$stroke)$out
boxplot(carprice$stroke)
#capping value below 7% and above 98%
carprice$stroke[which(carprice$stroke < 2.6512)] <- 2.6512
carprice$stroke[which(carprice$stroke > 3.8968)] <- 3.8968

#For compressionratio
quantile(carprice$compressionratio, seq(0, 1, 0.01))
summary(carprice$compressionratio)
boxplot.stats(carprice$compressionratio)$out
boxplot(carprice$compressionratio)
#capping value below 4% and above 90% since there is a huge jump
carprice$compressionratio[which(carprice$compressionratio > 10.9400)] <- 10.9400
carprice$compressionratio[which(carprice$compressionratio < 7.5000)] <- 7.5000

#For horsepower
quantile(carprice$horsepower, seq(0, 1, 0.01))
summary(carprice$horsepower)
boxplot.stats(carprice$horsepower)$out
boxplot(carprice$horsepower)
#capping value above 97% since there is a huge jump
carprice$horsepower[which(carprice$horsepower > 184.00)] <- 184.00

#For peakrpm
quantile(carprice$peakrpm, seq(0, 1, 0.01))
summary(carprice$peakrpm)
boxplot.stats(carprice$peakrpm)$out
boxplot(carprice$peakrpm)
#capping value above 99% since there is a huge jump
carprice$peakrpm[which(carprice$peakrpm > 6000)] <- 6000

#For citympg
quantile(carprice$citympg, seq(0, 1, 0.01))
summary(carprice$citympg)
boxplot.stats(carprice$citympg)$out
boxplot(carprice$citympg)
#capping value above 99% since there is a huge jump
carprice$citympg[which(carprice$citympg > 44.72)] <- 44.72

#For highwaympg
quantile(carprice$highwaympg, seq(0, 1, 0.01))
summary(carprice$highwaympg)
boxplot.stats(carprice$highwaympg)$out
boxplot(carprice$highwaympg)
#capping value above 98% since there is a huge jump
carprice$highwaympg[which(carprice$highwaympg > 46.92)] <- 46.92

####Dummy variable creation to convert the categorical variable to numerical####

####For 2 levels factor
summary(carprice$fueltype)
#diesel    gas 
#  20    185 
levels(carprice$fueltype) <- c(1,0)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]
summary(carprice$fueltype)
#it has been converted to numeric variable

#similarly for aspirarion
summary(carprice$aspiration)
#std turbo 
#168    37
levels(carprice$aspiration) <- c(1,0)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
summary(carprice$aspiration)

#similarly for doornumber
summary(carprice$doornumber)
#four  two 
#115   90 
levels(carprice$doornumber) <- c(1,0)
carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]
summary(carprice$doornumber)


#similarly for enginelocation
summary(carprice$enginelocation)
#front  rear 
#202     3 
levels(carprice$enginelocation) <- c(1,0)
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]
summary(carprice$enginelocation)

####For 3 levels factor
#for drivewheel which has 3 factors converting it into 2 dummy variables
summary(carprice$drivewheel)
#4wd fwd rwd 
#  9 120  76
dummy_1 <- data.frame(model.matrix( ~ drivewheel , data = carprice)) 
View(dummy_1)
dummy_1 <- dummy_1[, -1]

####For 5 levels factor
#for carbody which has 5 factors converting it into 4 dummy variables
summary(carprice$carbody)
#convertible     hardtop   hatchback       sedan       wagon 
#       6           8          70          96          25 
dummy_2 <- data.frame(model.matrix( ~ carbody , data = carprice)) 
View(dummy_2)
dummy_2 <- dummy_2[, -1]

####For 6 levels factor
#for symboling which has 6 factors converting it into 5 dummy variables
summary(carprice$symboling)
#-2 -1  0  1  2  3 
# 3 22 67 54 32 27 
dummy_3 <- data.frame(model.matrix( ~ symboling , data = carprice)) 
View(dummy_3)
dummy_3 <- dummy_3[, -1]

####For 7 levels factor
#for enginetype and cylindernumber which has 7 factors converting it into 6 dummy variables each
summary(carprice$enginetype)
#dohc dohcv     l   ohc  ohcf  ohcv rotor 
#12     1       12   148    15    13     4 
dummy_4 <- data.frame(model.matrix( ~ enginetype , data = carprice))
View(dummy_4)
dummy_4 <- dummy_4[, -1]


summary(carprice$cylindernumber)
#eight   five   four    six  three twelve    two 
#5        11    159     24      1      1      4 
dummy_5 <- data.frame(model.matrix( ~ cylindernumber, data = carprice))
View(dummy_5)
dummy_5 <- dummy_5[, -1]


####For 8 levels factor
#for fuelsystem which has 8 factors converting it into 7 dummy variables each
summary(carprice$fuelsystem)
#1bbl 2bbl 4bbl  idi  mfi mpfi spdi spfi 
# 11   66    3   20    1   94    9    1 
dummy_6 <- data.frame(model.matrix( ~ fuelsystem , data = carprice))
View(dummy_6)
dummy_6 <- dummy_6[, -1]


####For company name (22 companies-3 levels)
#for Company name (22 companies)we created 3 levels , hence there should be 2 dummy variables
summary(carprice$CompanyName)
#highend  midend  lowend 
#  24      39     142 
dummy_7 <- data.frame(model.matrix( ~ CompanyName , data = carprice))
View(dummy_7)
dummy_7 <- dummy_7[, -1]

#Combining the dummy variables created after removing the actual categorical columns
carprice_1 <-
  cbind(carprice[, -which(
    names(carprice) %in%  c(
      "drivewheel" ,
      "carbody" ,
      "symboling" ,
      "enginetype" ,
      "cylindernumber" ,
      "fuelsystem" ,
      "CompanyName"
    )
  )],
  dummy_1 ,
  dummy_2 ,
  dummy_3 ,
  dummy_4 ,
  dummy_5 ,
  dummy_6,
  dummy_7)
View(carprice_1)

#Removing ModelName(as given that we need to consider only company name as the independent variable for the model building) from the dataset as it is not required in the analysis
carprice_1 <- carprice_1[,-which(names(carprice_1) %in% c("ModelName"))] 
View(carprice_1)

#Building a correlation matrix between all the variables
correlation_matrix<-cor(carprice_1)
View(correlation_matrix)

# separate training and testing data into 70:30 respectively
#for Model building and Model evaluation
set.seed(100)

trainindices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))
train = carprice_1[trainindices,]
test = carprice_1[-trainindices,]

#Removing CarID(gives the unique Id number) from the train dataset and not from test dataset as it would be used for plotting graphs
train = train[,-which(names(train) %in% c("car_ID"))]

# Build model 1 containing all variables
model_1 <-lm(price ~ ., data = train)
summary(model_1)

#Using stepAIC
step <- stepAIC(model_1, direction = "both")

step

#call part of the output of step tells us what model we need to build, we input that to model_2
model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                  carwidth + curbweight + horsepower + drivewheelrwd + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + symboling0 + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetyperotor + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
                data = train)
summary(model_2)
sort(vif(model_2), decreasing = T)
#Adjusted R squared =0.9658

#carbodysedan,cylindernumberfour,curbweight,carbodyhatchback, CompanyNamelowend,carbodywagon,horsepower,carwidth has high VIF but low pvalue i.e. they are highly significant 
#and hence removing wheelbase VIF=6.956386 ,p-value=0.055134
model_3 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + horsepower + drivewheelrwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_3)
sort(vif(model_3), decreasing = T)
#Adjusted R squared =0.965

#cylindernumberfour,curbweight has high VIF but low pvalue i.e. they are highly significant 
#and hence removing carbodysedan VIF=12.485193 ,p-value=0.091757
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + horsepower + drivewheelrwd + carbodyhardtop + 
                carbodyhatchback + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_4)
sort(vif(model_4), decreasing = T)
#Adjusted R squared = 0.9645

#cylindernumberfour,curbweight,CompanyNamelowend , horsepower,cylindernumbersix,carwidth,CompanyNamemidend,cylindernumberfive has high VIF but low pvalue i.e. they are highly significant 
#and hence removing enginetypeohc VIF=5.241921 ,p-value=0.057330
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + horsepower + drivewheelrwd + carbodyhardtop + 
                carbodyhatchback + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_5)
sort(vif(model_5), decreasing = T)
#Adjusted R squared = 0.9637

#curbweight,cylindernumberfour,CompanyNamelowend , horsepower,cylindernumbersix,carwidth,CompanyNamemidend,cylindernumberfive has high VIF but low pvalue i.e. they are highly significant 
#and hence removing drivewheelrwd VIF=3.169688 ,p-value=0.322026
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + horsepower + carbodyhardtop + 
                carbodyhatchback + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_6)
sort(vif(model_6), decreasing = T)
#Adjusted R squared = 0.9637

#cylindernumberfour,curbweight,CompanyNamelowend , horsepower,carwidth,cylindernumbersix,CompanyNamemidend,cylindernumberfive,enginelocation,enginetyperotor,enginetypel has high VIF but low pvalue i.e. they are highly significant 
#VIF>2 has high significance , hence we will see cor(train$horsepower, train$curbweight)
# 0.7698322 , (as horsepower increases, curbweight increases) we will remove horsepower since it has higher p-value as compared to curbweight, and if curbweight is removed adjusted R squared reduces more
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + carbodyhardtop + 
                carbodyhatchback + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_7)
sort(vif(model_7), decreasing = T)
#Adjusted R squared = 0.9626


#cylindernumberfour,curbweight,CompanyNamelowend ,carwidth,cylindernumbersix,CompanyNamemidend,cylindernumberfive,enginelocation,enginetyperotor has high VIF but low pvalue i.e. they are highly significant 
#VIF>2 has high significance , hence we will see cor(train$carwidth, train$curbweight)
# 0.8825034 ,(this is evident because if width increases curbweight increases) we will remove carwidth since it has higher p-value as compared to curbweight, and if we remove curbweight adjusted R -squared reduced more
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                curbweight + carbodyhardtop + 
                carbodyhatchback + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_8)
sort(vif(model_8), decreasing = T)
#Adjusted R squared = 0.9565

#cylindernumberfour,CompanyNamelowend,cylindernumbersix,CompanyNamemidend,curbweight,cylindernumberfive,enginelocation,enginetyperotor has high VIF but low pvalue i.e. they are highly significant 
#VIF>2 has high significance , hence we will see multicollinearity if we see cor(train$CompanyNamelowend, train$curbweight)
# -0.7069605 , negative correlation is high, but if we remove any one of these from the model the Adjusted R squared reduces sharply, 
#hence we will start removing variables based on high p-value
#Removing carbodyhardtop , p-value=0.86134
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                curbweight + 
                carbodyhatchback + carbodywagon + symboling0 + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_9)
sort(vif(model_9), decreasing = T)
#Adjusted R squared = 0.9568

#Removing symboling0 , p-value=0.67597
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                curbweight + 
                carbodyhatchback + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
              data = train)
summary(model_10)
sort(vif(model_10), decreasing = T)
#Adjusted R squared = 0.9571

#Removing enginetypeohcf , p-value=0.65654
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 carbodyhatchback + carbodywagon + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_11)
sort(vif(model_11), decreasing = T)
#Adjusted R squared = 0.9573

#Removing carbodyhatchback , p-value=0.20037
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 carbodywagon + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_12)
sort(vif(model_12), decreasing = T)
#Adjusted R squared = 0.9571

#Removing aspiration , p-value=0.09410
model_13 <- lm(formula = price ~ enginelocation + 
                 curbweight + 
                 carbodywagon + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_13)
sort(vif(model_13), decreasing = T)
#Adjusted R squared = 0.9565

####Creating correlation matrix on this dataset to see multicollinearity between variables####
corrsubset_onmodel_13 <- subset(train,select=c(enginelocation, curbweight, carbodywagon, enginetypel,
                                               enginetyperotor , cylindernumberfive , cylindernumberfour , 
                                               cylindernumbersix , CompanyNamemidend , CompanyNamelowend,price))
cormat <- round(cor(corrsubset_onmodel_13), 2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

#Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())


#########################################################################################################
#Removing cylindernumbersix VIF=5.520754
#cor(train$cylindernumberfour,train$cylindernumbersix) #-0.6480336 , cylindernumbersix has high pvalue as compared to cylindernumberfour
model_14 <- lm(formula = price ~ enginelocation + 
                 curbweight + 
                 carbodywagon + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_14)
sort(vif(model_14), decreasing = T)
#Adjusted R squared = 0.9403

#Removing enginetypedohcv , p-value=0.85544
model_15 <- lm(formula = price ~ enginelocation + 
                 curbweight + 
                 carbodywagon + 
                 enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_15)
sort(vif(model_15), decreasing = T)
#Adjusted R squared = 0.9407

#Removing enginetyperotor , p-value=0.38918
model_16 <- lm(formula = price ~ enginelocation + 
                 curbweight + 
                 carbodywagon + 
                 enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_16)
sort(vif(model_16), decreasing = T)
#Adjusted R-squared 0.9409


#remove cylindernumberfive is correlated to cylindernumberfour and also if we remove it it does not have high impact on model
model_17 <- lm(formula = price ~ enginelocation + 
                 curbweight + 
                 carbodywagon + 
                 enginetypel + 
                 cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_17)
sort(vif(model_17), decreasing = T)
#Adjusted R squared = 0.934

#can remove enginelocation it has zero variance almost , only 3 values are present with rear engine and rest 202 with front engine
model_18 <- lm(formula = price ~  
                 curbweight + 
                 carbodywagon + 
                 enginetypel + 
                 cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_18)
sort(vif(model_18), decreasing = T)
#Adjusted R squared = 0.917

#can remove enginetypel has pvalue has only 2 stars( 0.00434 **) ,
#and see the impact we adjusted R -squared does not reduce much we can accept the model
model_19 <- lm(formula = price ~  
                 curbweight + 
                 carbodywagon + 
                 cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_19)
sort(vif(model_19), decreasing = T)
#Adjusted R squared = 0.9125 --We accept and move ahead

#can remove carbodywagon , since it has only 2 stars(0.00153 **)
#and see the impact we adjusted R -sqaured does not reduce much we can accept the model
model_20 <- lm(formula = price ~  
                 curbweight + 
                 cylindernumberfour + 
                 CompanyNamemidend + CompanyNamelowend, 
               data = train)
summary(model_20)
sort(vif(model_20), decreasing = T)
#Adjusted R squared = 0.9065 -- we accept and move ahead

#Since all the p-values are now highly significant , and on removal of any variable the Adjusted R squared is affected a lot.
#The model prediction changes a lot , so we go ahead with model_20 has final model

# Predict the car prices in the testing dataset
Predict_1 <- predict(model_20,test[,-which(names(test) == c("price"))])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared
#0.8665987

#Adjusted R squared = 0.9065 and rsqaured predicted from model is 0.8665987, we have a deviation of 4% which is acceptable

#Error = Actual - Predicted
test$error <-  test$price - test$test_price

# Plot errors for the price prediction
ggplot(test, aes(car_ID, error)) + geom_point() + 
  scale_x_continuous(name = "car ID") +
  scale_y_continuous(name = "Error") +
  geom_hline(yintercept = 0)
#The error is random , all data points are randomly distributed, it is a white noise
#Hence it shows there are no extra variables which can be introduced in the model which can make model better

# Plot - Actual vs Predicted Price
ggplot(test, aes(car_ID, price)) + geom_line(col="green4") +
  scale_x_continuous(name = "car ID") +
  scale_y_continuous(name = "price") +
  geom_line(aes(x=car_ID, y=test_price), col="red") + 
  ggtitle(label = " Plot - Actual vs Predicted Price")
# green line indicates the actual price , red line indicates the predicted price of the car
#Both the lines are highly overlapping which means the model is doing pretty well.

#set graphic output
par(mfrow=c(2,2))

#create residual plots
plot(model_20)

#adjusted Rsquaredvalue for model_20 = 0.9065
#Rsquared value of this model on test data =  0.8665987
#so this is the final accepted model for price prediction with only +/-4 deviation.

#Final price prediction can be done using the following :
# Price = 1.262e+04 + (5.782e+00 * curbweight) + (-3.292e+03 * cylindernumberfour) +
#         (-1.046e+04 * CompanyNamemidend) + (-1.385e+04 * CompanyNamelowend)     

#Main Factors that impact the pricing are
#1)curbweight - The weight of a car without occupants or baggage.It has positive 5.782 coefficient which implies more the weight , more is the price
#  curbweight also depends upon carwidth, carheight
#2)cylindernumberfour - number of cylinders in car. cylindernumberfour has negative -3292 coefficent which implies cars with 4 cylinders would be cheaper
#3)CompanyNamemidend - Company having midend segments car have high negative coefficients of -10460
#4)CompanyNamelowend - Company having lowend segments car have high negative coefficients of -13850
  # 3rd and 4th point implies that the car manufactured by midend and lowend segment companies are cheaper than the high end ones
