library(caret)
#library(lattice)
library(rattle)

set.seed(1010)

dat <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', na.strings=c("","NA"))

testing_dat <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv')

# Clean the data, remove NA cols
dat <- dat[, colSums(is.na(dat)) == 0]
testing_dat <- testing_dat[, colSums(is.na(testing)) == 0]

# Remove metadata cols
dat_f <- dat[, -c(1:7)]
testing <- testing_dat[, -c(1:7)]

inTrain <- createDataPartition(y=dat$classe, p=0.7, list=FALSE)

# Finalize training and validation sets
training <- dat_f[inTrain,]
validation <- dat_f[-inTrain,]

# Set up control for CV
control <- trainControl(method = "cv", number=3, verboseIter=F)

# Fit decision tree
fit_trees <- train(classe ~ ., data=training, method="rpart", trControl=control, tuneLength=3)
fancyRpartPlot(fit_trees$finalModel)

# Make the decision tree prediction
pred_trees <- predict(fit_trees, validation)
cm_trees <- confusionMatrix(pred_trees, factor(validation$classe))
cm_trees

# Fit random forest
fit_rf <- train(classe ~ ., data=training, method="rf", trControl=control, tuneLength=3)

# Make the random forest prediction
pred_rf <- predict(fit_rf, validation)
cm_rf <- confusionMatrix(pred_rf, factor(validation$classe))
cm_rf

# Make the final predictions using the Random Forest
pred_final <- predict(fit_rf, testing)
pred_final
