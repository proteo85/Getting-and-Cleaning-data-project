## I want to create a data set by joining train and test data sets using rbind command. Each row is an abservation for which the first and second column are the subject and the activity respectively, and the rest of columns corresponds to the different measurable stuff 


### Load the dplyr package
library(dplyr)

## Firstly, we read all the needed data files

## test data files
test<-read.table("R/project_cleaning_data/UCI HAR Dataset/test/X_test.txt")
test_sub<-read.table("R/project_cleaning_data/UCI HAR Dataset/test/subject_test.txt")
test_act<-read.table("R/project_cleaning_data/UCI HAR Dataset/test/y_test.txt")

## train data files

train<-read.table("R/project_cleaning_data/UCI HAR Dataset/train/X_train.txt")
train_sub<-read.table("R/project_cleaning_data/UCI HAR Dataset/train/subject_train.txt")
train_act<-read.table("R/project_cleaning_data/UCI HAR Dataset/train/y_train.txt")

## features
hnames<-read.table("R/project_cleaning_data/UCI HAR Dataset/features.txt",stringsAsFactors=FALSE)

## And finally activities
act<-read.table("R/project_cleaning_data/UCI HAR Dataset/activity_labels.txt",colClasses="character",stringsAsFactors=FALSE)


##Change the activity numbers by each proper name (step 3)

##Create a data frame with the activity names in both test and train data

test_act<-data.frame("activity"=act$V2[test_act[,1]])
train_act<-data.frame("activity"=act$V2[train_act[,1]])

##Variable name to the subjects in test and train

colnames(test_sub)<-c("subject")
colnames(train_sub)<-c("subject")

##Now will extract only the measurements on the mean and standard deviation for each feature (step 2)

## first the features name as a vector
hnames<-hnames[,2]


## use it to name the variable features in test and train data
colnames(test)<-hnames
colnames(train)<-hnames

## Create a vector of indexs corresponding to the variables which contain mean and standard desviation
sub_in<-union(grep("mean",hnames),grep("std",hnames))

## order it
sub_in<-sort(sub_in)

## Subset our test and train data to contain only the features required
test<-test[,sub_in]
train<-train[,sub_in]

## A bit of manipulation to make the features variable names readable
final_names<-tolower(names(test))
final_names<-gsub("-","",final_names)
final_names<-gsub("bodybody","body",final_names)
final_names<-gsub("\\()","",final_names)

## Assign these readable variable names to our test and train data
colnames(test)<-final_names
colnames(train)<-final_names

##Creo un data para test donde cada observación es para un subject haciendo un activity y las medias y desviaciones medidas. Hago lo mismo para train
data_test<-cbind(test_sub,test_act,test)
data_train<-cbind(train_sub,train_act,train)

## Finally, join them using rbind

dat_final<-rbind(data_test,data_train)

# The data to be write in an output file will correspond to a tidy data group by subject and activity. Each feature will then be averaged for each pair.

dat_5step<-summarise_each(group_by(dat_final,subject,activity),funs(mean))	

write.table(dat_5step,file="R/project_cleaning_data/data_project.txt",row.names=FALSE,quote=FALSE)
