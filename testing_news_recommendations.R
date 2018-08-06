library(sqldf)
#Testing the accuracy of recommendations produced

#read the file containing the recommended news articles for all users
rec_file <- read.csv("/recommendations.csv")


#FUnction to calculate the F measure
getscore <- function(user){
  
  print(paste("User: ",user))
  test_doc <- testing_set[which(testing_set$user_id==user),]
  rec_doc <- rec_file[which(rec_file$user_id==user),]
  
  doc<-sqldf('SELECT test_doc.doc_id FROM test_doc,rec_doc WHERE test_doc.doc_id==rec_doc.doc_id')
  #print(paste("Commom Documents: ",doc))
  d<-nrow(doc)  
  print(paste(nrow(test_doc)))
  tp<-d         #True positive
  fp<-nrow(rec_doc)-d      #false positive
  fn<-nrow(test_doc)-d     #false negative
  
  precision <- tp/(tp+fp)   
  recall <- tp/(tp+fn)
  fscore <- ((2*precision*recall)/(precision+recall))
  print(paste(fscore))
  if(is.na(fscore))
  {
    precision<-0
    recall<-0

  }
  score <- data.frame(userid=user,precision=precision,recall=recall,fmeasure=fscore)
  write.table(score,"/rec_score.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  
}

#unique users
user_train<-as.data.frame(unique(training_set$user_id))

#call the getscore() function
for(user in user_train[,])
{
  doc<-getscore(user)
}

#-----Plot the F measure for all users-----------------
library(ggplot2)
library(data.table)
score_file <- read.csv("rec_score.csv",header=T,dec=",")
t<-data.table(user_id=score_file[,1],fmeasure=score_file[,4])
qplot(user_id,fmeasure,data=t,geom="col",stat="identity",position = "dodge")
#------------------------------------------------------


