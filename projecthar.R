# load required libraries
library(caret)
library(dplyr)
library(GGally)
library(ggplot2)

# load files for project

# load test set
fileurl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url=fileurl2, destfile="project/pml-testing.csv",method="curl")
pml.testing <- read.csv(file="project/pml-testing.csv", stringsAsFactors=FALSE)

# Load Training Set

fileurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url=fileurl, destfile="project/pml-training.csv",method="curl")
pml.training <- read.csv(file="project/pml-training.csv", stringsAsFactors=FALSE)

# split into traning an test datasets
set.seed(1000)
trainIndex <- createDataPartition(pml.training$classe, p = 0.60, list=FALSE)
trainset <- pml.training[trainIndex,]
testset <- pml.training[-trainIndex,]

# measured response data and summary statistics are captured together. Stats are captured when new_window == yes
# several variables capature statistic for the different sensors - gyros_belt, magnet_belt, gyros_dumbbel...)
# these columns contains many "NA" and blank values

# pre-process data
# convert "" to NA's and then check for NA 
trainset[trainset ==""] <- NA
x <- apply(trainset,2,is.na)
nasum <- apply(x, 2, sum)
colsna <- nasum[nasum > 0]

# test set
testset[testset ==""] <- NA
x <- apply(testset,2,is.na)
nasum <- apply(x, 2, sum)
colsna <- nasum[nasum > 0]

# remove stat variables from training. model will be based on response variables
z <- paste(names(colsna), sep=",") # create a column list of variables to remove
trainset <- select(trainset, -one_of(c(z)))

z <- paste(names(colsna), sep=",") # create a column list of variables to remove
testset <- select(testset, -one_of(c(z)))

# remove metadata variables - user_name, raw_timestamp_part_1, new_window, num_window
trainset <- trainset[,-(1:7)]
testset <- testset[, -(1:7)]
# check for near zero var. no issues
nzv <- nearZeroVar(trainset[,-53], saveMetrics= TRUE)

# investigate correlation variables
highcorr <- findCorrelation(cor(trainset[,-53]), cutoff=.80)
head(trainset[1:200,highcorr]) 
# remove highly correlated variables
trainset <- trainset[,-highcorr]
testset <- testset[, -highcorr]



# data visualization
plotSubset <- data.frame(scale(trainset[, c("gyros_arm_y", "accel_arm_y")]))
xyplot(nC ~ X4v,
       data = plotSubset,
       groups = mdrrClass,
       auto.key = list(columns = 2))

featurePlot(x = trainset[, c("magnet_dumbbell_x", "magnet_dumbbell_y")],
            y = as.factor(trainset$classe),
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 5),
            alpha=0.2)

 
GGally::ggpairs(trainset, c(1:10), color=trainset$classe)
qplot(roll_arm, pitch_arm,colour=classe,data=trainset,main="newdata Predictions",
      alpha=0.3)

# view stats
summarise_each(trainset[,2:39], funs(mean,sd))

#model analysis\qqq

fitControl <- trainControl(method = "cv",
                           number = 2,
                           repeats = 2,
                            )


modFit <- train(as.factor(classe) ~.,data=trainset, method="rf", 
                trControl=fitControl, prox=TRUE)
modFit$finalModel
pred <- predict(modFit,testset)
# save model with method repeatcv
saveRDS(modFit, "project/modfit.rds")
mod2 <- readRDS("project/modfit.rds")
 # save model with method = "cv"
saveRDS(modFit, "project/modfitcv.rds")
modrepeatcv <- readRDS("project/modfit.rds")
modcv <- readRDS("project/modfitcv.rds")
predict(mod2,pml.testing)
 
confusionMatrix(testset$classe, pred)

trellis.par.set(caretTheme())
plot(modFit)
trellis.par.set(caretTheme())
plot(modFit, metric = "Kappa")
ggplot(modFit)

# pre-process final test set
pml.testing[testset ==""] <- NA
x <- apply(pml.testing,2,is.na)
nasum <- apply(x, 2, sum)
colsna <- nasum[nasum > 0]
z <- paste(names(colsna), sep=",") # create a column list of variables to remove
pml.testing <- select(pml.testing, -one_of(c(z)))
# remove metadata variables - user_name, raw_timestamp_part_1, new_window, num_window
pml.testing <- pml.testing[, -(1:7)]
# remove highly correlated variables
 
pml.testing <- pml.testing[, -highcorr]
# predict with final test set
pred1 <- predict(modFit,pml.testing)
pred1$finalModel
confusionMatrix(testset$classe, pred)

answers <- as.character(pred1)


# answer function

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}