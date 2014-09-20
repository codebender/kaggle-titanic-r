# dependency libs
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load train and test csv files
train <- read.csv("Data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("Data/test.csv", stringsAsFactors=FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method="class",
             control=rpart.control(minsplit=2, cp=0.005))
Prediction <- predict(fit, test, type = "class")

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "output/complex_decision_tree.csv", row.names = FALSE)
