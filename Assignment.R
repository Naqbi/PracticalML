rm(list=ls())
library(caret)
library(randomForest)
library(rpart) 
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(tree)

set.seed(1234)
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]

myTraining <- myTraining[, -(1:7)]
myTesting <- myTesting[, -(1:7)]

#there are way to many NA's. I will create a function which will show
#which variables are containing them
#you can use: lapply(data_training, function(x) sum(is.na(x))) or
#show <- colSums(is.na(myTraining))
#show_testing <- colSums(is.na(myTesting))

#we can see that where we have NA's, those columns are almost totaly full of them, so we are safe if I remove those columns
#now I will keep just those variables with data
myTraining <- myTraining[, colSums(is.na(myTraining)) == 0]
myTesting <- myTesting[, colSums(is.na(myTesting)) == 0]

#find and remove corelated variables/centering and scaling the data
findCorrelation(cor(myTraining[,-53]), cutoff = .9, names = T)
findCorrelation(cor(myTesting[,-53]), cutoff = .9, names = T)

myTraining <- myTraining[, -c(10, 1, 9, 8, 31, 33, 18)]
myTesting <- myTesting[, -c(1, 10, 8, 9, 18)]

nzv <- nearZeroVar(myTraining, saveMetrics=TRUE)
if (any(nzv$nzv)) nzv else message("No variables with near zero variance")

nzv_test <- nearZeroVar(myTesting, saveMetrics = TRUE)
if (any(nzv_test$nzv_test)) nzv_test else message("No variables with near zero variance")

modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
pdf(file = "Dec_Tree.pdf", paper = "a4r")
rpart.plot(modFitA1, main="Decision Tree", extra=102, under=TRUE, faclen=0)
dev.off()

predictionsA1 <- predict(modFitA1, myTesting, type = "class")
confusionMatrix(predictionsA1, myTesting$classe)

modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)

# Generating Files to submit as answers for the Assignment:
predictionsB2 <- predict(modFitB1, testing, type = "class")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)
