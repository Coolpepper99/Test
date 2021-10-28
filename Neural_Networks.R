# A useful website is https://datascienceplus.com/fitting-neural-network-in-r/

#### Table 11.2

rm(list=ls())
gc()

# michael

if (!require(neuralnet)) install.packages("neuralnet")
library(neuralnet)

df <- read.csv("tinyexample.csv")

df$Like <- df$Acceptance=="like"
df$Dislike <- df$Acceptance=="dislike"

str(df)
View(df)

set.seed(1)
nn <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 3)


# display predictions
prediction(nn2)

# plot network
plot(nn, rep="best")

# display weights
nn$weights


# NN with two hidden layers -- 2 nodes in 1st hidden layer and 3 nodes in 2nd hidden payer

nn2 <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = c(2,3))
plot(nn2,rep="best")


#### Table 11.3

if (!require(caret)) install.packages("caret")
library(caret)
if (!require(e1071)) install.packages('e1071', dependencies=TRUE)
library(e1071)
predict <- compute(nn, data.frame(df$Salt, df$Fat))

predicted.class=apply(predict$net.result,1,which.max)

confusionMatrix(factor(ifelse(predicted.class=="1", "like", "dislike")), df$Acceptance)



#------------------------------
  
library(MASS)
data <- Boston

str(data)

set.seed(1)
trainset <- data[1:303, ]
testset <- data[304:506, ]
nnb <- neuralnet(medv ~ ., data = trainset, linear.output = T, hidden = c(5,2),threshold = 0.01)

nn2 <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = c(2,3))

# display predictions
prediction(nnb)

# plot network
plot(nnb, rep="best")

# display weights
nnb$weights

#Test the resulting output
temp_test <- subset(testset, select = c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat"))
head(temp_test)
nn.results <- compute(nnb, temp_test)
results <- data.frame(actual = testset$medv, prediction = nn.results$net.result)
results


# mean absolute deviation basis (i.e. the average deviation between estimated and actual gasoline consumption stands at a mean of 10%).

predicted=results$prediction * abs(diff(range(medv))) + min(medv)
actual=results$actual * abs(diff(range(medv))) + min(medv)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

#.3258647


