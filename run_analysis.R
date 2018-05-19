run_analysis<-function(){
  ##collecting data
  X_train<-read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
  X_test<-read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
  ##collecting names of the data
  names<-read.table(".\\UCI HAR Dataset\\features.txt")
  names<-names[,2]

  part1<- rbind(X_train,X_test)
  names(part1)<-names
  ##extracting only columns with mean/std
  particularNames<-grep("mean|std",names)
  particularNames<-names[particularNames]
  
  part2<-part1[,particularNames]
  ##extracting activity and merging
  activity1<-read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
  activity2<-read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
  totalActivity<-rbind(activity1,activity2)
  totalActivity<-totalActivity$V1
  activityDescription<-c()
  i<-1
  for(val in totalActivity){
    val<-as.integer(val)
    ##part3
    activityDescription[i]<-switch(val,"WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    i<-i+1
  }
  ##extracting subject and merging
  subject_train<-read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
  subject_test<-read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")
  subject<-rbind(subject_train,subject_test)$V1
  ##creating a data set with activityId, activityDescription and subject
  part4<-mutate(part2,activityId=totalActivity,activityDescription=activityDescription,subject=subject)
  ##grouping them with the column names and summarising them with their mean
  part5<-part4 %>% group_by(activityDescription,subject) %>% summarise_all(funs(mean))
  write.table(part5, file = "./tidyData.txt")
}