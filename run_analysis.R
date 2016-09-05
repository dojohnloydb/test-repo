#read data from train files
activity<-read.table("activity_labels.txt",header =F)
features<-read.table("features.txt",header = F)
subj_train<-read.table("train/subject_train.txt",header = F)
X_train<-read.table("train/X_train.txt",header=F)
y_train<-read.table("train/y_train.txt",header=F)

#name columns of train datasets with descriptive names
colnames(activity)<-c('activity_ID','activity')
colnames(features)<-c('feature_ID','feature')
colnames(X_train)<-features$feature
colnames(y_train)<-c('activity_ID')
colnames(subj_train)<-c('subject_ID')

#create complete train data from y_train,subj_train and X_train
train_data<-cbind(y_train,subj_train,X_train)

#read files of test data to create test dataset
y_test<-read.table("test/y_test.txt",header = F)
X_test<-read.table("test/X_test.txt",header = F)
subj_test<-read.table("test/subject_test.txt",header = F)

#name columns of test datasets with descriptive names
colnames(subj_test)<-c("subject_ID")
colnames(y_test)<-c("activity_ID")
colnames(X_test)<-features$feature

#create complete test data from y_test,subj_test and X_test
test_data<-cbind(y_test,subj_test,X_test)

#Merges the training and the test sets to create one data set
full_data<-rbind(train_data,test_data)

#Extracts only the measurements on the mean and standard deviation for each measurement.
column_names<-colnames(full_data)
mean_columns<-(grepl("-mean..",column_names) & !grepl("-mean..-",column_names)& !grepl("-meanFreq..",column_names))
std_columns<-(grepl("-std..",column_names)& !grepl("-std..-",column_names))
mean_std_columns<-column_names[grepl("activity..",column_names) | grepl("subject..",column_names) |mean_columns | std_columns]

#Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_data <- full_data[mean_std_columns]

#Uses descriptive activity names to name the activities in the data set
mean_std_data = merge(mean_std_data,activity,by='activity_ID',all.x=TRUE);

#Appropriately label the data set with descriptive activity names. 
column_names<-colnames(mean_std_data)
for (i in 1:length(column_names)) 
{
  column_names[i] = gsub("\\()","",column_names[i])
  column_names[i]= gsub("-std$","StdDev",column_names[i])
  column_names[i]= gsub("-mean","Mean",column_names[i])
  column_names[i]= gsub("^(t)","time",column_names[i])
  column_names[i]= gsub("^(f)","freq",column_names[i])
  column_names[i]= gsub("([Gg]ravity)","Gravity",column_names[i])
  column_names[i]= gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",column_names[i])
  column_names[i] = gsub("[Gg]yro","Gyro",column_names[i])
  column_names[i] = gsub("AccMag","AccMagnitude",column_names[i])
  column_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",column_names[i])
  column_names[i] = gsub("JerkMag","JerkMagnitude",column_names[i])
  column_names[i]= gsub("GyroMag","GyroMagnitude",column_names[i])
};
colnames(mean_std_data)<-column_names

#creates a second, independent tidy data set 
#with the average of each variable for each 
#activity and each subject.
final= mean_std_data[,names(mean_std_data) != 'activity'];
tidydata = aggregate(final[,names(final) != c('activity_ID','subject_ID')],by=list(activity_ID=final$activity_ID,subject_ID = final$subject_ID),mean);
tidydata = merge(tidydata,activity,by='activity_ID',all.x=TRUE);
write.table(tidydata, './tidydata2.txt',row.names=F,sep='\t')
