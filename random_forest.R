# Load dependency libs
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

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

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=full[!is.na(full$Age),], method="anova")
full$Age[is.na(full$Age)] <- predict(Agefit, full[is.na(full$Age),])

full$Embarked[c(62,830)] = "S"
full$Embarked <- factor(full$Embarked)

# which(is.na(full$Fare)) = 1044
full$Fare[1044] <- median(full$Fare, na.rm=TRUE)

full$FamilyID2 <- full$FamilyID
full$FamilyID2 <- as.character(full$FamilyID2)
full$FamilyID2[full$FamilySize <= 3] <- 'Small'
full$FamilyID2 <- factor(full$FamilyID2)

full$Sex <- factor(full$Sex)

clean_train <- full[1:891,]
clean_test <- full[892:1309,]

set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=clean_train, importance=TRUE, ntree=2000)

Prediction <- predict(fit, clean_test)
submit <- data.frame(PassengerId = clean_test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Output/random_forest.csv", row.names = FALSE)
