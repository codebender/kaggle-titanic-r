# Load dependency libs
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load train and test csv files
train <- read.csv("Data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("Data/test.csv", stringsAsFactors=FALSE)

test$Survived <- NA

full <- rbind(train, test)
full$Name <- as.character(full$Name)

full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title)
full$Title[full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
full$Title <- factor(full$Title)

full$FamilySize <- full$SibSp + full$Parch + 1

full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

full$FamilyID <- paste(as.character(full$FamilySize), full$Surname, sep="")

full$FamilyID[full$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'
full$FamilyID <- factor(full$FamilyID)

clean_train <- full[1:891,]
clean_test <- full[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=clean_train, method="class")

Prediction <- predict(fit, clean_test, type = "class")
submit <- data.frame(PassengerId = clean_test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Output/engineered_features_tree.csv", row.names = FALSE)
