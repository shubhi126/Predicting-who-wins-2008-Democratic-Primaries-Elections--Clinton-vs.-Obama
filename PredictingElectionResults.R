########################################################
### Project: Predicting who wins 2008 Democratic Primaries - Clinton vs. Obama  By Shubham Choudhary
### Used Various classification models like Logistic Regression, K Nearest Neighbour
### and Clustering methods like K Means. Performed K Fold Cross Validation 
### of all models.
########################################################

#Calling out the function script I created in R

source("myfunctions.R")

# read data into R

election_data <- read.csv("ElectionDataAlone.csv")


# Next use the function summary to inspect the data

summary(election_data)

##############################################

# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# I use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]
summary(election_data_test)

election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)



####################### VISUALISATION #############################

plot(election_data_train$SpeakingNonEnglish, election_data_train$Homeowner, xlab = "Speaking Non-English %", ylab = "Home Ownership %")
t1 <- lm(Homeowner~ SpeakingNonEnglish, data = election_data_train)
summary(t1)
abline(76.262, -.331)



####################### MODELLING #############################
### MODEL 1 

# SIMPLE LINEAR REGRESSION WITH MOST VARIABLES AND 2 FOLD & 10 FOLD CROSS VALIDATION

### This will turn off warning messages
options(warn=-1)
############################
#we are setting the seed to be 1; so it is 
# easier to replicate.
set.seed(1)
election_data <- read.csv("ElectionDataAlone.csv")

a1<- election_data_train
a1 <- a1[complete.cases(a1), -4]
a1 <- a1[complete.cases(a1), -43]
a1 <- a1[complete.cases(a1), -41]
summary(a1)
# -1 above deletes the first column which is the customer id.
## there are 7043 complete observations
## in this class, to understand the concepts we will work with 3000
## observations sampled from that pool
a1 <- a1[sample(nrow(a1), 1736), ]
### and we will split in two groups. 
summary(a1)
nfold <- 2 
n <- nrow(a1) # the number of observations
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
summary(a1)

## We have 1500 observations in our first set of regressions

#the first model below might take a little bit of time of run; there are a lot of variables!

OOS <- data.frame(linear=NA,  null=NA) 

### Set the second part for testing (first for training)
k <- 2
### Set the other part for training (if not k)
train <- which(foldid!=k) # train on all but fold `k'
test  <- which(foldid==k) # test on fold k

model.linear <-glm(Obama_margin_percent~MalesPer100Females+AgeBelow35+Age35to65+Age65andAbove+Asian+AmericanIndian+Hawaiian+ Hispanic+HighSchool+Bachelors+Poverty+IncomeAbove75K+MedianIncome+AverageIncome+UnemployRate+ManfEmploy+SpeakingNonEnglish+Medicare+MedicareRate+SocialSecurity+SocialSecurityRate+RetiredWorkers+Disabilities+DisabilitiesRate+FarmArea+LandArea+PopDensity+Pop+Homeowner+Disabilities+Region+Black+White , data=a1, subset=train)
model.null <- glm(Obama_margin_percent~1, data=a1, subset=train)
summary(a1)

## get predictions: type=response so we have probabilities
pred.linear <- predict(model.linear, newdata=a1[-train,], type="response")
pred.null <- predict(model.null, newdata=a1[-train,], type="response")

## calculate and log R2

OOS$linear <- R2(y=a1$Obama_margin_percent[-train], pred=pred.linear)

#Null model (just intercept)
OOS$null <- R2(y=a1$Obama_margin_percent[-train], pred=pred.null)

### Lets list the results stored in the dataframe OOS
OOS 
###


### K-Fold cross validation
###
### Essentially the same code as before just that 
### we will use more folds and plot the uncertainty
### Number of OOS validation `folds'

### K Fold Cross Validation
###
### create a vector of fold memberships (random order)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(linear=rep(NA,nfold),  null=rep(NA,nfold)) 

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions and null model
  model.linear <-glm(Obama_margin_percent~MalesPer100Females+AgeBelow35+Age35to65+Age65andAbove+Asian+AmericanIndian+Hawaiian+ Hispanic+HighSchool+Bachelors+Poverty+IncomeAbove75K+MedianIncome+AverageIncome+UnemployRate+ManfEmploy+SpeakingNonEnglish+Medicare+MedicareRate+SocialSecurity+SocialSecurityRate+RetiredWorkers+Disabilities+DisabilitiesRate+FarmArea+LandArea+PopDensity+Pop+Homeowner+Disabilities+Region+Black+White , data=a1, subset=train)
  model.nulll <-glm(Obama_margin_percent~1, data=a1, subset=train)
  ## get predictions: type=response so we have probabilities
  pred.linear  <- predict(model.linear, newdata=a1[-train,], type="response")
  pred.null <- predict(model.nulll, newdata=a1[-train,], type="response")
  
  ## calculate and log R2
  
  
  OOS$linear[k] <- R2(y=a1$Obama_margin_percent[-train], pred=pred.linear)
  OOS$linear[k]
  #Null
  OOS$null[k] <- R2(y=a1$Obama_margin_percent[-train], pred=pred.null)
  OOS$null[k]
  
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"with R2 of",OOS$linear[k], "      And This is how we do a Cross Validation!"))
}
### Do not worry about the warning messages. 
### These are minor numerical issues in this case.
### 
### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=FALSE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
#if you put legend is TRUE in the previous command, you get the labels;
#but this seems to induce some formatting issues.


### we can plot a box blot 
### so see how OOS R2 fluctuates across fold
if (nfold >= 10){
  ### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"linear"
  ### Lets zoom in  to see better the performance of 
  ### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
  
}



### MODEL 2

#LINEAR REGRESSION WITH INTERACTION

### This will turn off warning messages
options(warn=-1)
############################
#we are setting the seed to be 1; so it is 
# easier to replicate.
set.seed(1)
election_data <- read.csv("ElectionDataAlone.csv")

a1<- election_data_train
a1 <- a1[complete.cases(a1), -4]
a1 <- a1[complete.cases(a1), -43]
a1 <- a1[complete.cases(a1), -41]
summary(a1)
# -1 above deletes the first column which is the customer id.
## there are 7043 complete observations
## in this class, to understand the concepts we will work with 3000
## observations sampled from that pool
a1 <- a1[sample(nrow(a1), 1736), ]
### and we will split in two groups. 
summary(a1)
nfold <- 2 
n <- nrow(a1) # the number of observations
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
summary(a1)

## We have 1500 observations in our first set of regressions
##
#the first model below might take a little bit of time of run; there are a lot of variables!

OOS <- data.frame(linear=NA,  null=NA) 

### Set the second part for testing (first for training)
k <- 2
### Set the other part for training (if not k)
train <- which(foldid!=k) # train on all but fold `k'
test  <- which(foldid==k) # test on fold k

model.linear <-glm(Obama_margin_percent~Age65andAbove+Asian+White+AmericanIndian+Hispanic+Bachelors+Poverty+IncomeAbove75K+MedianIncome+MedicareRate+SocialSecurityRate+DisabilitiesRate+PopDensity+log(FarmArea)+Homeowner+Region+Black+ElectionType*UnemployRate,data=a1,subset = train)
model.null <- glm(Obama_margin_percent~1, data=a1, subset=train)
summary(a1)

## get predictions: type=response so we have probabilities
pred.linear <- predict(model.linear, newdata=a1[-train,], type="response")
pred.null <- predict(model.null, newdata=a1[-train,], type="response")

## calculate and log R2

OOS$linear <- R2(y=a1$Obama_margin_percent[-train], pred=pred.linear)

#Null model (just intercept)
OOS$null <- R2(y=a1$Obama_margin_percent[-train], pred=pred.null)

### Lets list the results stored in the dataframe OOS
OOS 
###


### K-Fold cross validation
###
### Essentially the same code as before just that 
### we will use more folds and plot the uncertainty
### Number of OOS validation `folds'

### create a vector of fold memberships (random order)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(linear=rep(NA,nfold),  null=rep(NA,nfold)) 

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions and null model
  model.linear <-glm(Obama_margin_percent~MalesPer100Females+AgeBelow35+Age35to65+Age65andAbove+Asian+AmericanIndian+Hawaiian+ Hispanic+HighSchool+Bachelors+Poverty+IncomeAbove75K+MedianIncome+AverageIncome+UnemployRate+ManfEmploy+SpeakingNonEnglish+Medicare+MedicareRate+SocialSecurity+SocialSecurityRate+RetiredWorkers+Disabilities+DisabilitiesRate+FarmArea+LandArea+PopDensity+Pop+Homeowner+Disabilities+Region+Black+White , data=a1, subset=train)
  model.nulll <-glm(Obama_margin_percent~1, data=a1, subset=train)
  ## get predictions: type=response so we have probabilities
  pred.linear  <- predict(model.linear, newdata=a1[-train,], type="response")
  pred.null <- predict(model.nulll, newdata=a1[-train,], type="response")
  
  ## calculate and log R2
  OOS$linear[k] <- R2(y=a1$Obama_margin_percent[-train], pred=pred.linear)
  OOS$linear[k]
  #Null
  OOS$null[k] <- R2(y=a1$Obama_margin_percent[-train], pred=pred.null)
  OOS$null[k]
  
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"with R2 of",OOS$linear[k], "      And This is how we do a Cross Validation!"))
}
### Do not worry about the warning messages. 
### These are minor numerical issues in this case.
### 
### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=FALSE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
#if you put legend is TRUE in the previous command, you get the labels;
#but this seems to induce some formatting issues.

### we can plot a box blot 
### so see how OOS R2 fluctuates across fold
if (nfold >= 10){
  ### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"linear"
  ### Lets zoom in  to see better the performance of 
  ### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
  
}



#MODEL 3

# USING K NEAREST NEIGHBOURS TO PREDICT VALUES IN TEST DATA SET

###### k-NN ############################################################
############################################################################

installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)

#myvars <- c("Region","MalesPer100Females","AgeBelow35", "Age35to65", "Age65andAbove","White","Black","Asian","AmericanIndian","Hawaiian","Hispanic","HighSchool","Bachelors","Poverty","IncomeAbove75K","MedianIncome","AverageIncome","UnemployRate","ManfEmploy")         
###x<- election_data_train[myvars]
### summary(x)
###y<- election_data_test[myvars]
###is.na(y)

x<- cbind( election_data_train$Region , election_data_train$Age35to65, election_data_train$SocialSecurityRate , election_data_train$SpeakingNonEnglish,election_data_train$White , election_data_train$Hispanic,election_data_train$Poverty ,election_data_train$AmericanIndian , election_data_train$IncomeAbove75K,election_data_train$MedianIncome,election_data_train$UnemployRate , election_data_train$Bachelors,election_data_train$Medicare , election_data_train$RetiredWorkers)

g <- election_data_train$Obama_margin_percent

y<- cbind( election_data_test$Region , election_data_test$Age35to65, election_data_test$SocialSecurityRate , election_data_test$SpeakingNonEnglish,election_data_test$White , election_data_test$Hispanic,election_data_test$Poverty ,election_data_test$AmericanIndian , election_data_test$IncomeAbove75K,election_data_test$MedianIncome,election_data_test$UnemployRate , election_data_test$Bachelors,election_data_test$MedicareRate , election_data_test$RetiredWorkers)

mod15 <- knn(train=x, test=y, cl=g, k=15, prob=TRUE)
summary(mod15)
predictedvector <- mod15[]

election_data_test$predictedbyknn <- c(predictedvector)
length(election_data_test$predictedbyknn)



####################### CLUSTERING USING K MEANS #############################
#### Clustering 
#### kmean cluster based on Unemployment Rate and Average Income

names(election_data_train)

#### Let's calculte the percentage of votes distributed to Obama relative to Clinton
election_data_train$Obama_percent <- 100*election_data_train$Obama/(election_data_train$Obama + election_data_train$Clinton)

#### column 25 is Unemployment Rate, Column 26 is Average Income
km <- election_data_train[c(25, 26)]
km[,1] <- scale(km[,1])
km[,2] <- scale(km[,2])

#### everything will be in corn flower blue 
col <- "cornflowerblue"

Obama_kmeans <- kmeans(km,3)
colorcluster <- 2+Obama_kmeans$cluster
plot(km, col = colorcluster, xlab="Standardized Unemployment Rate (%)", ylab="Standardized Average Income ($)")
points(Obama_kmeans$centers, col = 2:4, pch = 8, cex = 2)

### Three clusters:
### low or medium Unemployment Rate, and low or medium Average Income;
### low or medium Unemployment Rate, and high Average Income;
### high Unemployment Rate, and low or medium Average Income.



####################### QUESTION 4 #############################
install.packages('stargazer')
library('stargazer')

### Finding impact of changing hispanic demographic
###
#### Model with 1771 controls to measure the impact of 10% larger Hispanic demographic

## Original Hispanic Population Regression
election_data_train$d <- election_data_train$Hispanic*1.05
x4 <- lm( Obama_margin_percent ~ .-d -b-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )

##Hispanic Population increases by 5%
stargazer(x4, type ="text")
x2 <- lm( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
stargazer(x2, type = "text")


## Original Black Population Regression
election_data_train$b <- election_data_train$Black*1.05
x5 <- lm( Obama_margin_percent ~ .-b-d-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )

##Black Population increases by 5%
stargazer(x5, type ="text")
x3 <- lm( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
stargazer(x3, type ="text")
