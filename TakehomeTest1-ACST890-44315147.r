#================================#
# ACST890 - Take Home Test 1     #
# Tran Nguyen Bao Dao - 44315147 #                 
#================================#

#Question 1
Price <- function(C,FaceValue,n,rate){
  if (length(rate)!=1){testr <- rep(0,length(rate))
  for (i in 2:length(rate)){
    testr[i] <- rate[i] < rate[i-1]}
  if (sum(testr) > 0) {
    cat('Notice: You have inputted some interest rate that is less than the previous rate', "\n")
  }} else {
    cat('Notice: You have inputted only 1 interest rate', "\n")  }
    r <- rep(0,n)
  m <- length(rate)
  testn <- m < n
  if (testn) {
    cat('Notice: You have inputted data interest rate that is less than number of our term "n"', "\n")
    r[1] <- rate[1] 
    for (j in m:n){r[j] <- rate[m]}} 
  else { r <- rate }
  P <- 0
  P1 <- 0
  for (i in 1:n){
    P1 <- C*exp(-r[i]*i) + P1
  }
  P <- P1 + FaceValue*exp(-r[n]*n)
  cat('Price of bond is: ',P, "\n")
}

# Noticed: Please input Interest rate for every 6 month in vector: 
Price( C = 1, FaceValue = 100, n = 5, rate = c(0.05,0.07,0.08,0.09,0.1))

#Question 3
#3a/ Read the dataset into R and name it dataset
cat('Copy singapore.economy.csv to folder: ',getwd(),'\n')
dataset <- read.csv("singapore.economy.csv")
head(dataset)

#3b/Exclude all records that contain NA
dataset <- na.omit(dataset)

#3c/Plot Singapore GDP against time
plot(dataset$time, dataset$gdp, xlab='Time', ylab = 'GDP (%)', main='Singapore GDP growth')

#3d/Calculate the mean and standard deviation of Singapore GDP for each period
GDP1 <- dataset[dataset$period == 1,]$gdp
GDP2 <- dataset[dataset$period == 2,]$gdp
GDP3 <- dataset[dataset$period == 3,]$gdp
stat.table <- matrix(c(mean(GDP1),sd(GDP1),mean(GDP2),sd(GDP2),mean(GDP3),sd(GDP3)), ncol =2, byrow = TRUE)
colnames(stat.table) <- c('Mean','Standard Deviation')
rownames(stat.table) <- c('Period 1','Period 2','Period 3')
print(stat.table)

#3e/ Scatterplot for every pair of variables in the dataset, except for time and period
datasetdf <- as.data.frame(dataset)
pairs(datasetdf[3:ncol(datasetdf)] , panel=panel.smooth) 
#We don't include column 1,2 as they are Time and Period 

#3f/ Simple linear regression with gdp and exp
lm.fit_f =lm(gdp~exp , data= dataset )
summary(lm.fit_f)
# The coefficient exp is significant to explain gdp. 
# Also the model is good prediction for F-statistic very large and p-value is significant, thus they strongly proved the relationship 
# However, the adjusted R-squared = 0.2879 < 0.5 This model is not exactly a good fit.

#3g/ Multiple regression model to predict gdp:
lm.fit_g <- lm(gdp~dataset$exp+dataset$epg+ dataset$hpr+ dataset$gdpus+ dataset$oil+ dataset$crd, data= dataset )
summary(lm.fit_g)
# The coefficients exp, wpg, hpr is significant for the model.
# Oil, gdpus, crd is insignificant coefficients for the model.
# The F-statistic states the fitness of data is lower in this model.
# However, the R squared is higher than the single regression of exp state a better fit, but still less than 0.5
lm.fit_g_adj <- lm(gdp~exp+epg+hpr, data= dataset) #remove insignificant variable out of model
summary(lm.fit_g_adj)
#After we remove Oil, gdpus, crd, the F-statistic of the later model have increased to 20.71
# The R squared is also increase to 0.3517, still <0.5.

#3h/ 
#Calculate the 5% quantile of gdp
quantile <- quantile(dataset$gdp,0.05)
cat('The 5% quantile of gdp is', print(quantile),'\n')
#create a factor vector for the economy state
testquantile <- dataset$gdp < quantile(dataset$gdp,0.05)
testquantile[testquantile == TRUE] = c('Crisis')
testquantile[testquantile == FALSE] = c('Normal')
state <- as.factor(testquantile)
print(state)
#Adding up State to dataset
dataset <- data.frame(dataset,state)
#Fit the logistic regression model
S_BCI<- glm( formula = state ~ bci, family = binomial , data = dataset[dataset$time<2008,])
summary(S_BCI)
#Compute the confusion matrix
glm.probs <- predict(S_BCI,dataset[dataset$time>=2008,] ,type ="response")
contrasts(state)
glm.pred=rep("Crisis",length(state)-length(dataset[dataset$time<2008,]$time))
glm.pred[glm.probs >.5]="Normal"
confusion_matrix <- table(glm.pred, dataset[dataset$time>=2008,]$state)
confusion_matrix
cat('Misclassification rate is',(1 - mean(glm.pred == dataset[dataset$time>=2008,]$state))*100,'%\n')
