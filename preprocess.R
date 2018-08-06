library(data.table)

#read dataset
file<-read.csv("/yow_userstudy_raw.csv")

file
summary(file)
dim(file)
nrow(file)


user67 <- file[which(file$user_id == 67),]
nrow(user67)


#normalize the values
normalize <- function(x) {
  (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))
}

#call the normalize function for each column
norm_userlike<-as.data.frame(lapply(file["user_like"],normalize))
norm_timeonpage<-as.data.frame(lapply(file["TimeOnPage"],normalize))
norm_timeonmouse<-as.data.frame(lapply(file["TimeOnMouse"],normalize))
summary(norm_userlike)
summary(norm_timeonpage)
summary(norm_timeonmouse)
head(norm_userlike)
head(norm_timeonmouse)
head(norm_timeonpage)

#remove missing values (NA's)
rem_na <- function(x) {
  z <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

#only TimeOnPage has missing values, so those are removed
new_timeonpage<-as.data.frame(lapply(norm_timeonpage,rem_na))


#create a data frame for the new values
new_data<-data.frame(norm_userlike,norm_timeonmouse,new_timeonpage,file$DOC_ID,file$user_id)
head(new_data)
summary(new_data)
colnames(new_data)[1]<-"userlike"
colnames(new_data)[2]<-"timeonmouse"
colnames(new_data)[3]<-"timeonpage"
colnames(new_data)[4]<-"doc_id"
colnames(new_data)[5]<-"user_id"

#get unique users in the dataset
user_unique<-as.data.frame(unique(new_data$user_id))
user_unique
u_user <- nrow(user_unique)
 
#writing preprocessed data to csv file after removing duplicates
for(userid in user_unique[1:u_user,1])
{
   user <- data.table(new_data[which(new_data$user_id==userid),])
   u<-unique(user,by="doc_id")
   u<-data.frame(u)
   write.table(u,"/new_data_news_recommend.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
}
