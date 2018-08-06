#reading the file with euclidean distance
e_distance_file <- read.csv("/e_distance_data3.csv")
nrow(e_distance_file)

#unique users
user1_train <- as.data.frame(unique(e_distance_file$user1))
nrow(user1_train)

#set active user and threshold value
active_user <- 92
threshold <- 20

user_train<-as.data.frame(unique(training_set$user_id))
user_train
row_train<-nrow(user_train)
row_train

user_train[,1]
#finding neighbors for active user
for(active_user in user_train[,1])
{
  for(user in user1_train[,1])
  {
    if(user==active_user)
    {
        neighbor <- data.frame(e_distance_file[which(e_distance_file$user1==active_user & e_distance_file$euclidean_distance>=threshold),2])
        print(paste(neighbor))
    }
  }
  if(nrow(neighbor)!=0)
  {
    #documents read by active user
    active_udoc <-data.frame(training_set[which(training_set$user_id==active_user),4])
    #nrow(active_udoc)
    
    #finding docs which the active user havent read
    #but neighbors have
    rec_docs<-c()
    for(nuser in neighbor[1:nrow(neighbor),1])
    {
      #ndoc <- training_set[which(training_set$user_id==nuser & training_set$userlike>0.8),4]
      ndoc <- training_set[which(training_set$user_id==nuser),4]
      s<-setdiff(ndoc,active_udoc)
      rec_docs <- append(rec_docs,s)
      #print(paste(s))
    }
    
    
    rec_docs<-as.data.frame(rec_docs)
    nrow(rec_docs)
    #rec_docs<-data.frame(rec_docs)
    rec_docs<-unique(rec_docs)
    
    rec_docs<-as.data.frame(rec_docs)
    nrow(rec_docs)
    
    #articles to be recommended
    #recommended <- sample(rec_docs,size=20)
    #recommended
    recommendations <- data.frame(userid=active_user,recommendation_doc=rec_docs)
    
    #writing recommendation to a file
    write.table(recommendations,"/recommendations.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  }
  else{
    neighbor<-0
    print(paste(neighbor))
    rec_docs<-0
    print(paste(rec_docs))
    recommendations <- data.frame(userid=active_user,recommendation_doc=rec_docs)
    write.table(recommendations,"/recommendations.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
    
  }
  
}
#-----------------plotting Neighbors for active user-----------
p_neighbor <- data.frame(e_distance_file[which(e_distance_file$user1==active_user),2:3])
plot(p_neighbor$user2,p_neighbor$euclidean_distance,xlab = "User ID",ylab="Euclidean Distance w.r.t. Active User",col=ifelse(p_neighbor$euclidean_distance>=threshold,"blue","red"))
abline(h=threshold)
#--------------------------------------------------------------
