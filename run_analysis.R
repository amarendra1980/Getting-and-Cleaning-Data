##Load packages
library(dplyr)
library(data.table)

## Set Path
path<- getwd()

##Get Data - Download the zip file in the folder
fileurl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
z<- "Dataset.zip"
if(!file.exists(path)) {dir.create(path)}
download.file(fileurl,file.path(path,z))

##Unzip the downloaded file
zipf<- file.path(path,z)
unzip(zipf,exdir=path)
pathIn <- file.path(path, "UCI HAR Dataset")

##Read data from files

subjectTrain<- read.table(file.path(pathIn, "train", "subject_train.txt"))
subjectTest<- read.table(file.path(pathIn, "test", "subject_test.txt"))
yTrain<- read.table(file.path(pathIn, "train", "Y_train.txt"))
yTest<- read.table(file.path(pathIn, "test" , "Y_test.txt" ))
xTrain<- read.table(file.path(pathIn, "train", "X_train.txt"))
xTest<- read.table(file.path(pathIn, "test" , "X_test.txt" ))
features<- read.table(file.path(pathIn, "features.txt"))
activityNames<- read.table(file.path(pathIn, "activity_labels.txt"))

##Merge the datasets
subject<- rbind(subjectTrain,subjectTest)
setnames(subject, "V1", "subject")
activity<- rbind(yTrain,yTest)
setnames(activity,"V1", "activityNum")
dataset<- rbind(xTrain,xTest)
dataset<- cbind(cbind(subject,activity),dataset)

##Extract only mean and standard deviation measurement data
featuresSubset<- features[grepl("mean\\(\\)|std\\(\\)", features$V2),]
featuresSubset$V1<- paste0("V", featuresSubset$V1)
setnames(featuresSubset,c("V1", "V2"), c("featureCode", "featureName"))
colNames<- as.character(featuresSubset$featureCode)
dataset<- dataset[,c("subject","activityNum", colNames)]

##Replace activity name with descriptive activity names
setnames(activityNames,c("V1","V2"), c("activityNum","activityName"))
dataset<-merge(activityNames,dataset,by="activityNum",all.x=TRUE)
dataset<- dataset[,-1]
dataset<- dataset[,c(2,1,3:68)]

##Label the dataset with descriptive variable names
varCode<- as.character(featuresSubset$featureCode)
varDesc<- as.character(featuresSubset$featureName)
setnames(dataset,varCode,varDesc)
names(dataset)<- gsub("\\(\\)","",names(dataset))
names(dataset)<- gsub("-","",names(dataset))

##Create tidy dataset with average of each variable for each activity and subject
tidy<- dataset%>%group_by(subject,activityName)%>%summarise_each(funs(mean))
tidy<-as.data.frame(tidy)
write.table(tidy,file="tidy_dataset.txt",row.name=FALSE)








