
library(data.table)
subjectTrain = read.table("subject_train.txt")
xTrain = read.table("X_train.txt")
yTrain = read.table("y_train.txt")

subjectTest = read.table("subject_test.txt")
xTest = read.table("X_test.txt")
yTest = read.table("y_test.txt")

xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)
subjectDataSet <- rbind(subjectTrain, subjectTest)
dim(xDataSet)
dim(yDataSet)
dim(subjectDataSet)
featuresDataset <- read.table("features.txt")
featuresDataset <- featuresDataset[,2]
colnames(xDataSet) <- featuresDataset
names(xDataSet)
head(xDataSet, n=1)
xDatasetmeanstd <- xDataSet[, grep("-(mean|std)\\(\\)", names(xDataSet))]
tail(xDatasetmeanstd)              
names(xDatasetmeanstd)
dim(xDatasetmeanstd)

names(xDatasetmeanstd)
names(yDataSet) <- "Activity"
head(yDataSet)
names(subjectDataSet) <- "Subject"

Dataset <- cbind(xDatasetmeanstd, yDataSet, subjectDataSet)
dim(Dataset)
names(Dataset)
names(Dataset) <- gsub("Acc", "Acceleration", names(Dataset))
names(Dataset) <- gsub("Gyro", "Gyroscope", names(Dataset))
names(Dataset) <- gsub("Mag", "Magnitude", names(Dataset))
names(Dataset) <- gsub("^t", "TimeDomain", names(Dataset))
names(Dataset) <- gsub("^f", "Frequency", names(Dataset))
grep('mean$', names(Dataset), value = TRUE)
table(grepl(".*mean()", names(Dataset)))
names(Dataset) <- gsub("mean", "Mean", names(Dataset))
names(Dataset) <- gsub("[()]", "", names(Dataset))
head(Dataset)
head(names(Dataset))
tail(names(Dataset))
library(reshape2)
library(dplyr)

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

Dataset2 <- mutate(Dataset, ActivityMean= mean(Activity))
head(mutate(Dataset, ActivityMean= mean(Activity)))

head(aggregate(formula= .~Activity+Subject, data = Dataset, FUN = mean))
tail(aggregate(formula= .~Activity+Subject, data = Dataset, FUN = mean))

meltDataset <- melt(Dataset, id.vars = names(Dataset)[1:66],
                    measure.vars = c("Activity","Subject"))
head(meltDataset)
tail(meltDataset)
dim(meltDataset)

Dataset2 <- data.table::dcast(meltDataset, Activity+Subject~variable, fun.aggregate = mean)
head(Dataset2)
tail(Dataset2)
data.table::dcast()

Dataset2 <- data.table::melt(Dataset, id=c("Activity","Subject"))
head(Dataset2)
tail(Dataset2)
Dataset2 <- data.table::dcast(Dataset2, Activity+Subject~variable, fun.aggregate = mean)
write.table(Dataset2, file = "tidydata.txt")





