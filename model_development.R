library(caret)

dat <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', na.strings=c("","NA"))

testing <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv')

dat <- dat[, colSums(is.na(dat)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

inTrain <- createDataPartition(y=dat$classe, p=0.7, list=FALSE)

training <- dat[inTrain,]
validation <- dat[-inTrain,]

fit <- train(classe ~ ., data = training, method = "glm")

training