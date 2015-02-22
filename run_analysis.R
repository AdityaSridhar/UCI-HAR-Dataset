# Load the dplyr package without warning/messages
suppressWarnings(suppressMessages(library(dplyr)))

# Load the test and training data sets.
xtest <- read.table("./test/X_test.txt",sep = "")
xtrain <- read.table("./train/X_train.txt", sep="")
ytest <- read.table("./test/Y_test.txt")
ytrain <- read.table("./train/Y_train.txt")

# Load the features info and remove the first column consisting of IDs.
features <- read.table("features.txt") 
features <- select(features, -V1)

# Rename the columns of the test and training data with the features.
names(xtest) <- features[,1] 
names(xtrain) <- features[,1] 

# Load the Activity labels.
activity_labels <- read.table("activity_labels.txt")
# Remove the first column consisting of IDs.
activity_labels <- select(activity_labels, -V1)
# Reset the levels of the Activity factor.
levels(activity_labels[,1]) <- c("Walking","Walking Up", "Walking Down", "Sitting", "Standing", "Laying")

# Coerce the test, training activity column as factor.
ytest[,1] <- as.factor(ytest[,1]) 
ytrain[,1] <- as.factor(ytrain[,1]) 
 
# Apply the activity label levels to the test/training data.
levels(ytest[,1]) <- levels(activity_labels[,1])
levels(ytrain[,1]) <- levels(activity_labels[,1])

# Name the activity column as "Activity"
colnames(ytest) <- c("Activity")
colnames(ytrain) <- c("Activity")

# bind the ytest and xtest data into a single data frame.
testData <- cbind.data.frame(ytest,xtest)

# Load the Subject data for Test. 
subject_test <- read.table("./test/subject_test.txt")

# Rename the column as "Subject"
colnames(subject_test) <- c("Subject")

# Bind the Subject Test data to Test data frame.
testData <- cbind(subject_test, testData)

# Bind the Y Training and X Training data sets.
trainingData <- cbind.data.frame(ytrain,xtrain)

# Load the Subject data for the Training set.
subject_train <- read.table("./train/subject_train.txt")
colnames(subject_train) <- c("Subject")

# Bind the subject data with the Training data set.
trainingData <- cbind(subject_train, trainingData)

# Bind the Training and Test data sets using a Row binding.
mergedData <- rbind(testData,trainingData)

# Remove all unwanted columns using Grep pattern search for "-mean()" & "-std()".
meanStdData <- mergedData[,grepl("-mean()|-std()",colnames(mergedData))]

# Add back the Subject and Activity columns
finalData <- cbind(mergedData[,1:2], meanStdData)

# Make the column names compatible to R's convention.
make.names(colnames(finalData))

# Create a tidy data set containing the Subject, Activity and Mean values of the measurements.
tidyDataWithMean <- finalData %>% group_by(Subject,Activity) %>% summarise_each(funs(mean))

# Melt the data into the long form with the Subject and Activity columns as the ID colummns and the others as measure var columns.
tidyDataMelted <- melt(data = tidyDataWithMean, id.vars = 1:2, measure.vars = 3:81, value.name = "Average")

write.table(x = tidyDataMelted, file = "TidyData.txt", row.name = F)

print(tidyDataMelted)  