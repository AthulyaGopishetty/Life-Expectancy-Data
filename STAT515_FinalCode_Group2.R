install.packages("randomForest")
install.packages("rpart.plot")
install.packages("ellipse")

library(ggplot2)
library(dplyr)
library(corrplot)
library(MASS)
library(ISLR)
library(lattice)
library(hexbin)
library(car)   # vif() and qqPlot functions
library(splines)
library(tree)	
library(rpart)# Popular decision tree algorithm
#library(rattle)				      	# Fancy tree plot
library(rpart.plot)			    	# Enhanced tree plots
library(RColorBrewer)			  	# Color selection for fancy tree plot
#library(party)					      # Alternative decision tree algorithm
#library(partykit)				      # Convert rpart object to BinaryTree
library(randomForest)


data = read.csv("LifeExpectancy.csv")
#####################################
# Data preprocessing
#####################################
summary(data)
#find the null value for each column
colSums(is.na(data))

# delete the null value for Life.expectancy first, because we will predict this column.
data <- data[!is.na(data$Life.expectancy), ]

#change the data type for developed column to numeric.
data$Status<-ifelse(data$Status=="Developed",1,0)

# Firstly, fill the null value for each column with the mean of each country
#Life.expectancy
data <- data %>%
  group_by(Country) %>%
  mutate(Life.expectancy = ifelse(is.na(Life.expectancy), mean(Life.expectancy, na.rm=TRUE), Life.expectancy))
#Adult.Mortality
data <- data %>%
  group_by(Country) %>%
  mutate(Adult.Mortality = ifelse(is.na(Adult.Mortality), mean(Adult.Mortality, na.rm=TRUE), Adult.Mortality))
#Alcohol
data <- data %>%
  group_by(Country) %>%
  mutate(Alcohol = ifelse(is.na(Alcohol), mean(Alcohol, na.rm=TRUE), Alcohol))
#Hepatitis.B
data <- data %>%
  group_by(Country) %>%
  mutate(Hepatitis.B = ifelse(is.na(Hepatitis.B), mean(Hepatitis.B, na.rm=TRUE), Hepatitis.B))
#Polio
data <- data %>%
  group_by(Country) %>%
  mutate(Polio = ifelse(is.na(Polio), mean(Polio, na.rm=TRUE), Polio))
#Total.expenditure
data <- data %>%
  group_by(Country) %>%
  mutate(Total.expenditure = ifelse(is.na(Total.expenditure), mean(Total.expenditure, na.rm=TRUE), Total.expenditure))
#Diphtheria
data <- data %>%
  group_by(Country) %>%
  mutate(Diphtheria = ifelse(is.na(Diphtheria), mean(Diphtheria, na.rm=TRUE), Diphtheria))
#GDP
data <- data %>%
  group_by(Country) %>%
  mutate(GDP = ifelse(is.na(GDP), mean(GDP, na.rm=TRUE), GDP))
# thinness 5-9 years
data <- data %>%
  group_by(Country) %>%
  mutate(thinness.5.9.years = ifelse(is.na(thinness.5.9.years), mean(thinness.5.9.years, na.rm=TRUE), thinness.5.9.years))
# thinness  1-19 years
data <- data %>%
  group_by(Country) %>%
  mutate(thinness..1.19.years = ifelse(is.na(thinness..1.19.years), mean(thinness..1.19.years, na.rm=TRUE), thinness..1.19.years))
#Income.composition.of.resources
data <- data %>%
  group_by(Country) %>%
  mutate(Income.composition.of.resources = ifelse(is.na(Income.composition.of.resources), mean(Income.composition.of.resources, na.rm=TRUE), Income.composition.of.resources))
#Schooling
data <- data %>%
  group_by(Country) %>%
  mutate(Schooling = ifelse(is.na(Schooling), mean(Schooling, na.rm=TRUE), Schooling))

# check the null value again.
colSums(is.na(data))

#There are still some null values exist, the next step we will
#fill the null value with the status of each country instead of deleting them.
#Schooling
data %>%group_by(Status) %>% summarize(mean(Schooling, na.rm=TRUE))

data <- data %>%
  group_by(Status) %>%
  mutate(Schooling = ifelse(is.na(Schooling), mean(Schooling, na.rm=TRUE), Schooling))
#Income.composition.of.resources
data <- data %>%
  group_by(Status) %>%
  mutate(Income.composition.of.resources = ifelse(is.na(Income.composition.of.resources), mean(Income.composition.of.resources, na.rm=TRUE), Income.composition.of.resources))
#thinness.5.9.years
data <- data %>%
  group_by(Status) %>%
  mutate(thinness.5.9.years = ifelse(is.na(thinness.5.9.years), mean(thinness.5.9.years, na.rm=TRUE), thinness.5.9.years))
#thinness..1.19.years
data <- data %>%
  group_by(Status) %>%
  mutate(thinness..1.19.years = ifelse(is.na(thinness..1.19.years), mean(thinness..1.19.years, na.rm=TRUE), thinness..1.19.years))
#GDP
data <- data %>%
  group_by(Status) %>%
  mutate(GDP = ifelse(is.na(GDP), mean(GDP, na.rm=TRUE), GDP))
#Population
data <- data %>%
  group_by(Status) %>%
  mutate(Population = ifelse(is.na(Population), mean(Population, na.rm=TRUE), Population))
#Hepatitis.B
data <- data %>%
  group_by(Status) %>%
  mutate(Hepatitis.B = ifelse(is.na(Hepatitis.B), mean(Hepatitis.B, na.rm=TRUE), Hepatitis.B))
#Alcohol
data <- data %>%
  group_by(Status) %>%
  mutate(Alcohol = ifelse(is.na(Alcohol), mean(Alcohol, na.rm=TRUE), Alcohol))
#Total.expenditure
data <- data %>%
  group_by(Status) %>%
  mutate(Total.expenditure = ifelse(is.na(Total.expenditure), mean(Total.expenditure, na.rm=TRUE), Total.expenditure))

#double check: it there any null value still in each column
colSums(is.na(data))
View(data)
dim(data)

# boxplot for the outliers of Life.expectancy
outliers <- boxplot(data$Life.expectancy, main = "Box plot for Life expectancy",
                    ylab = "Frequency", range =1.8)$out

data <-data[-which(data$Life.expectancy %in% outliers),]

##################################
#Correlation plot
##################################
# exclude non-numerical colums for correlation plot
#put the life.expectancy to the first column and then make the pie correlation chart.
data <- data[,c(4,2:3,5:21)]
corr <- cor(data,method = 'pearson')
corrplot(corr, method="pie")

#selected variables chart
offDiag <- function(x,y,...){
  panel.grid(h = -1,v = -1,...)
  panel.hexbinplot(x,y,xbins = 15,...,border = gray(.7),
                   trans = function(x)x^.5)
  panel.loess(x , y, ..., lwd=2,col='red')
}


onDiag <- function(x, ...){
  yrng <- current.panel.limits()$ylim
  d <- density(x, na.rm = TRUE)
  d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
  panel.lines(d,col = rgb(.83,.66,1),lwd = 2)
  diag.panel.splom(x, ...)
}
relations = data[,-c(3)]
splom(relations,as.matrix = TRUE,
      xlab = '',main = "Life Expectancy: Selected Variables",
      pscale = 0, varname.cex = 0.8,axis.text.cex = 0.6,
      axis.text.col = "purple",axis.text.font = 2,
      axis.line.tck = .5,
      panel = offDiag,
      diag.panel = onDiag
)

##########################################################
#linear regression
#########################################################
trainIndex  <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[trainIndex,]
test <- data[-trainIndex,]

# lm model with all columns
lm.all = lm(Life.expectancy ~ ., data=train)
summary(lm.all)
plot(lm.all)

p <- predict(lm.all, test[,c(2:20)])
error <- (p- test$Life.expectancy)
MSE1 <- sqrt(mean(error^2))

#reduce the column which is not significant linear association with life.expectancy
lm.fit1 <-  update(lm.all, ~.-Year-Alcohol-percentage.expenditure-Measles-Hepatitis.B-Total.expenditure-GDP-thinness.5.9.years-thinness..1.19.years-Population)
summary(lm.fit1)
plot(lm.fit1)

p <- predict(lm.fit1, test[,c(2:20)])
error <- (p- test$Life.expectancy)
MSE2 <- sqrt(mean(error^2))
###########################################
# fitting natural splines for linear model
###########################################
#fit Income.composition.of.resources
ggplot(data,aes(x = Income.composition.of.resources,y = Life.expectancy)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,formula = y~ns(x,4),
              color = "blue",fill = "cyan") +
  labs(
    x = "Income composition of resources",
    y = "Life expectancy",
    title = "Life Expectancy vs Income composition of resources")


#fit HIV.AIDS
ggplot(data,aes(x = HIV.AIDS,y = Life.expectancy)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "lm",formula = y~ns(x,6),
              color = "blue",fill = "cyan") +
  labs(
    x = "HIV.AIDS",
    y = "Life expectancy",
    title = "Life Expectancy vs HIV.AIDS")

#fit Adult.Mortality
ggplot(data,aes(x = Adult.Mortality,y = Life.expectancy)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "lm",formula = y~ns(x,3),
              color = "blue",fill = "cyan") +
  labs(
    x = "Adult.Mortality",
    y = "Life expectancy",
    title = "Life Expectancy vs Adult.Mortality")

# after fit the model as Multivariate Regression
lm.fit2 <- lm(Life.expectancy ~ Status+ns(Adult.Mortality,3)+infant.deaths+under.five.deaths+Polio+Diphtheria+ns(HIV.AIDS, 6)+ns(Income.composition.of.resources,4)+Schooling, data = train)
summary(lm.fit2)
plot(lm.fit2)

p <- predict(lm.fit2, test[,c(2:20)])
error <- (p- test$Life.expectancy)
MSE3 <- sqrt(mean(error^2))

################################################################
# Decision tree -category
#################################################################

## transfer Life.expectancy to label
labled <- data %>% 
  mutate(label = case_when(Life.expectancy <= 61.92 ~ 'Q1',
                           (Life.expectancy > 61.92 & Life.expectancy <=70.00) ~ 'Q2',
                           (Life.expectancy > 70.00 & Life.expectancy <=74.80)  ~ 'Q3',
                           (Life.expectancy > 74.80) ~ 'Q4',
  )
  )
dim(labled)
labled <- labled[,c(21,2:20)]
View(labled)

set.seed(1)
trainIndex  <- sample(1:nrow(labled), 0.8 * nrow(labled))
train <- labled[trainIndex,]
test <- labled[-trainIndex,]

# Decision tree model
tree <- rpart(label~., train, method = 'class')
rpart.plot(tree, extra = 1)

# Accuracy for decision tree -category
pred = predict(tree,test,type="class")
confMat <- table(test$label,pred)
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)

####################################################
#decision tree- continuous
#####################################################
# Here we fit a regression tree to the data set. First, we create a
# training set, and fit the tree to the training data.
set.seed(1)
train_data = sample(1:nrow(data), 0.8*nrow(data))
tree.data=tree(Life.expectancy~.,data,subset=train_data)
summary(tree.data)

# show the tree
plot(tree.data)
text(tree.data,pretty=0)

# mse value for unpruned tree
yhat=predict(tree.data,newdata=data[-train_data,])
data.test= data.matrix(data[-train_data,"Life.expectancy"])
plot(yhat,data.test)
abline(0,1)
mean((yhat-data.test)^2)

# Now we use the cv.tree() function to see whether pruning the tree will improve performance.
cv.data=cv.tree(tree.data)
plot(cv.data$size,cv.data$dev,type='b')

# prune the tree
prune.data=prune.tree(tree.data,best=5)
plot(prune.data)
text(prune.data,pretty=0)

# mse value for pruned tree 
yhat=predict(prune.data,newdata=data[-train_data,])
data.test= data.matrix(data[-train_data,"Life.expectancy"])
plot(yhat,data.test)
abline(0,1)
mean((yhat-data.test)^2)

# In keeping with the cross-validation results, we use the unpruned tree

##################################################
# Random forest--category
##################################################
fit <- randomForest(factor(label)~., train, importance=TRUE)

# Accuracy for Random forest-category
exp.pred <- predict(fit,test, type = "class")
confMat = table(exp.pred, test$label)
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)

###############################################
# Random forest--continuous (perfect!)
###############################################
set.seed(1)
rf.data=randomForest(Life.expectancy~.,data=data,subset=train_data,importance=TRUE)

# MSE for Random forest-continuous
yhat.rf = predict(rf.data,newdata=data[-train_data,])
mean((yhat.rf-data.test)^2) #test set MSE 2.643488

#variable importance
importance(rf.data)
# measures can be produced using the varImpPlot() function
varImpPlot(rf.data)
