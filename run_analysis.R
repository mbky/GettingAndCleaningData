library(dplyr)

createOutputFile <- function(pathToDataSet) {
   v1 <- c(1, rep(c(rep(c(16), each=6),-(16*34)), each = 5))
   v2 <- c(rep(c(rep(c(16), each=2),-(16*11)), each = 5))
   v3 <- c(rep(c(rep(c(16), each=6),-(16*34)), each = 3))
   v4 <- c(rep(c(rep(c(16), each=2),-(16*11)), each = 4))
   v5 <- c(c(-16*7))
   v <- c(v1, v2, v3, v4, v5)
   
   dataXTrain <- read.fwf(paste(pathToDataSet, "/UCI HAR Dataset/train/X_train.txt", sep = ""), v, header = FALSE)
   dataXTest <- read.fwf(paste(pathToDataSet, "/UCI HAR Dataset/test/X_test.txt", sep = ""), v, header = FALSE)
   dfXTrain <- select(tbl_df(dataXTrain), -V1)
   dfXTest <- select(tbl_df(dataXTest), -V1)
   dfX <- rbind(dfXTrain, dfXTest)
   
   features <- read.delim(paste(pathToDataSet, "/UCI HAR Dataset/features.txt", sep = ""), sep = " ",header = FALSE)
   features <- tbl_dt(features)
   features <- select(features, -V1)
   featuresMeanStd <- filter(features, grep(c("-std()|-mean()"), V2))
   featuresMeanStd <- filter(featuresMeanStd, !grepl("meanFreq", V2))
   colnames(dfX) <- as.character(featuresMeanStd[,V2])
   
   subTrain <- read.csv(paste(pathToDataSet, "/UCI HAR Dataset/train/subject_train.txt", sep = ""), header = FALSE, nrows = nrow(dataXTrain))
   subTest <- read.csv(paste(pathToDataSet, "/UCI HAR Dataset/test/subject_test.txt", sep = ""), header = FALSE, nrows = nrow(dataXTest))
   subjects <- rbind(subTrain, subTest)
   colnames(subjects) <- c("subject")
   
   activityDataTest <- read.csv(paste(pathToDataSet, "/UCI HAR Dataset/test/y_test.txt", sep = ""), header = FALSE, nrows = nrow(dataXTest))
   activityDataTrain <- read.csv(paste(pathToDataSet, "/UCI HAR Dataset/train/y_train.txt", sep = ""), header = FALSE, nrows = nrow(dataXTrain))
   activityData <- rbind(activityDataTrain, activityDataTest)
   
   activityNames <- read.delim(paste(pathToDataSet, "/UCI HAR Dataset/activity_labels.txt", sep = ""), header = FALSE, sep = " ", as.is = TRUE)
   
   fActivityData <- factor(c(activityData[,1]))
   dfActivityData <- tbl_df(data.frame(factor(fActivityData, labels=c(activityNames[,2]))))
   colnames(dfActivityData) <- c("activity")
   fActivityData <- factor(c(activityData[,1]))
   
   dfSubjects <- cbind(subjects, dfX)
   dfActivitySubject <- cbind(dfActivityData,dfSubjects)
   
   means <- tbl_df(summarise_each(group_by(dfActivitySubject, activity, subject), funs(mean)))
   write.table(means, paste(pathToDataSet, "/CourseProjectOutput.txt", sep=""), row.name = FALSE)
}
