# Getting-and-Cleaning-data-project

### Introduction

This project assignment requires us to work with a data collected from the accelerometers from the Samsung Galaxy S smartphone and write an R script which does the following:


 1-Merges the training and the test sets to create one data set.
 2-Extracts only the measurements on the mean and standard deviation for each measurement. 
 3-Uses descriptive activity names to name the activities in the data set
 4-Appropriately labels the data set with descriptive variable names. 
 5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### The script

The final goal of the script is to obtain a tidy data set by joining train and test data sets using rbind command, but after a bit of manipulation. Each row is an abservation for which the first and second column are the subject and the activity respectively, and the rest of columns corresponds to the different measurable stuff. 

In order to get to that final tidy data (step 5 above) all the previous 4 steps had to be fulfilled. In the script, these four steps are performed out of order as a matter of convenience. On general grounds, the script firstly loads all the data sets. Secondly, it manipulates activities names to fulfill step 3. Then, it extracts the measuraments on any mean and standard deviation to fulfill step 2 and changes the name of these measurement variables for more descriptive names (step 4). Everything is merged using cbind to create one test data set and one train data set. Finally, both data sets are merged using rbind (step 1). From this final data set, called dat_final in the script, is obtained the tidy data of step 5. This tidy data set contains 181 rows where the first row corresponds to the name of all variables and the rest 180 rows comes from the fact that we have 30 subjects*6activities=180 total observations of the averaged measurements on the mean and standard deviation.

Regarding the choice of measurements(features), step 2 only states extracting measurements on the mean and standard deviation, so in the script the command grep has been used to extract any measurement implying a mean or standard deviation measurement. 79 features are then those which imply a mean or standard deviation feature.

Finally, the code itself is commented before almost every single line to explain what that particular line does.

### The code book

The code book, both in pdf and md format, gives an explanation of the meaning of each variable in the tidy data set. 
