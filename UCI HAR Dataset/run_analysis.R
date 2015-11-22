# this is the code to tidy up the data for the programming assigment in the Coursera Getting and Cleaning data course.

#defining functions
make_new <- function(name,data){
  trak <- 0 
  for (i in indecies) {
    if (trak == 0) {
      name <- data[i]
      trak <- 1
    }
    else{
      name <- cbind(name,data[i])
    }
  }
  name <- tbl_df(name)
  return(name)
}


replace_terms <- function(col){
  size <- dim(col)[1]
  sprintf("size is %s ", size)
  count <- 0 
  problems <- 0
  for(i in 1:size){
    if(col[i,] == 1){
      col[i,] <- "WALKING"
    }
    else if(col[i,] == 2){
      col[i,] <- "WALKING_UPSTAIRS"
    }
    else if(col[i,] == 3){
      col[i,] <- "WALKING_DOWNSTAIRS"
    }
    else if(col[i,] == 4){
      col[i,] <- "SITTING"
    }
    else if(col[i,] == 5){
      col[i,] <- "STANDING"
    }
    else if(col[i,] == 6){
      col[i,] <- "LAYING"
    }
    else{
      problems <- problems + 1
    }
    count <- count +1
  }
  sprintf("count is %s", count)
  sprintf("how many problems %s", problems)
  return(col)
}


clean_up <- function(){
  y <- readline("would you like to delete all the redundant variables? y/n")
  if(y == "y"){
    rm(X_test,y_test,X_train,X_test_sorted,X_train_sorted,subject_test,subject_train,indecies,y_test,y_test_labeled,y_train,y_train_labeled,all_combine,all_labels,test_combined,train_combined,mean_std)
  }
  
}





# first thing we need to do is get the data

#insert downlaod file here
#setwd("~/Documents/Data Science/Getting and Cleaning Data")
#if(!file.exists("Project_2")){
#  dir.create("Project_2")
#}
#setwd("~/Documents/Data Science/Getting and Cleaning Data/Project_2")
#if(!file.exists("Project_2/Project_2_G&C/UCI HAR Dataset")){
#  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", method = "curl")
#}

#this set my working directory to the file that is downloaded from the website
#setwd("~/Documents/Data Science/Getting and Cleaning Data/Project_2/Project_2_G&C/UCI HAR Dataset")

#ls() #just to show the people what kind of directory they are working with
library(dplyr)
# now to download the features and labes

activity_labels <- tbl_df(read.table("./activity_labels.txt"))
features <- tbl_df(read.table("./features.txt"))

#now to dowoload all the files from the test file

X_test <- tbl_df(read.table("./test/X_test.txt"))
y_test <- tbl_df(read.table("./test/y_test.txt"))
subject_test <- tbl_df(read.table("./test/subject_test.txt"))
#now to download the files from the train foulder
X_train <- tbl_df(read.table("./train/X_train.txt"))
y_train <- tbl_df(read.table("./train/y_train.txt"))
subject_train <- tbl_df(read.table("./train/subject_train.txt"))
#and now it is time to find out what files we actually need from these files
mean_std <- filter(features, grepl("mean|std",V2)) #looking for just the parts with mean and standard deviation
indecies <- c(mean_std[,1])


#now to use the function described above to get the desired Columns from our data
X_test_sorted <- tbl_df(make_new(X_test_sorted,X_test))
X_train_sorted <- tbl_df(make_new(X_train_sorted,X_train))
# now that they are both sorted lets combine them

#lets actually put in the labels first

y_train_labeled <- replace_terms(y_train)
y_test_labeled <- replace_terms(y_test)
#first lets combine the test set
test_combined <- tbl_df(cbind(y_test_labeled,subject_test,X_test_sorted))

#and now train set
train_combined <- tbl_df(cbind(y_train_labeled,subject_train,X_train_sorted))

# and now lets combine the two data sets into one 

all_combine <- tbl_df(rbind(test_combined,train_combined))

#now to put labels on all the columns

labels <- as.character(mean_std$V2)
all_labels <- c("Activity","Subject",labels)

names(all_combine) <- all_labels
data <- all_combine
print("now take a look at the combined data")
View(data)




print("NOW WE WILL SEPERATE THE DATA INTO TWO SETS OF DATA THAT WILL BE TIDYIER AND WILL BE USEFULL FOR LOOKING AT THE DIFFERENT PARTS OF THE DATA")

grouped_data <- data %>% group_by(Subject,Activity) %>% summarise_each(funs(mean)) %>%arrange(Subject)
grouped_data
View(grouped_data)



#y <- readline("would you like to delete all the redundant variables? y/n")
#if(y == "y"){
#  rm(X_test,y_test,X_train,X_test_sorted,X_train_sorted,subject_test,subject_train,indecies,y_test,y_test_labeled,y_train,y_train_labeled,all_combine,all_labels,test_combined,train_combined,mean_std)
#}

  




