library(sqldf)
library(data.table)

#load the dataset with pre processed values
new_file<-read.csv("/new_data_news_recommend.csv")
nrow(new_file)

#randomize the data
set.seed(123)
g<-runif(nrow(new_file))
tests<-new_file[order(g),]
tests

#Training data
train_ind <- 0.70*nrow(new_file)
train_ind
training_set<-tests[1:train_ind,]
nrow(training_set)

#testing data
testing_set <- tests[(train_ind+1):nrow(new_file),]

#unique user_id's for training set
user_train<-as.data.frame(unique(training_set$user_id))
user_train
row_train<-nrow(user_train)
row_train

#to find common documents rated by two users
getDoc <- function(userid1,userid2){
  
  print(paste("User 1: ",userid1))
  print(paste("User 2: ",userid2))
  
  user1 <- training_set[which(training_set$user_id==userid1),]
  user2 <- training_set[which(training_set$user_id==userid2),]
  
  doc<-sqldf('SELECT user1.doc_id FROM user1,user2 WHERE user1.doc_id==user2.doc_id')
  print(paste("Commom Documents: ",doc))
  z<-nrow(doc)  
  print(paste(z))
  return(doc)
  
}

#feature weights
weight=c(1100,1001,1011) #done
weight=c(1001,1100,1011)
weight=c(2022,2002,2200)



#for test run
doc<-getDoc(userid1=67,userid2=84)
doc

#calculate the feature differences and find the weighted
#sum for each doc which two users have in common
feature_difference <- function(userid1,userid2,docid){

  user1_like<-training_set[which(training_set$user_id==userid1 & training_set$doc_id==docid),1]
  user2_like<-training_set[which(training_set$user_id==userid2 & training_set$doc_id==docid),1]  
  user1_mouse<-training_set[which(training_set$user_id==userid1 & training_set$doc_id==docid),2]
  user2_mouse<-training_set[which(training_set$user_id==userid2 & training_set$doc_id==docid),2]
  user1_page<-training_set[which(training_set$user_id==userid1 & training_set$doc_id==docid),3]
  user2_page<-training_set[which(training_set$user_id==userid2 & training_set$doc_id==docid),3]  

  diff_user_like <- user1_like-user2_like
  diff_user_mouse <- user1_mouse-user2_mouse
  diff_user_page <- user1_page-user2_page
  
  feat_diff <- (weight[1]*(diff_user_like^2))+(weight[2]*(diff_user_mouse^2))+(weight[3]*(diff_user_page^2))
  return(feat_diff)
}

#------for test run--------------
d<-feature_difference(userid1=67,userid2=84,docid = 282200)
d
df<-data.frame(user1=userid1,user2=userid2,common_doc=docs,euclidean_dist=euclidean)
e_dist<-read.csv("/e_distance_data.csv")
a<-rbind(e_dist,df)
write.table(a,"/e_distance_data.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
row_train
#--------------------------------

#displays the docs 2 users have in common and the euclidean 
#distance between them

library(sqldf)
i<-1
for(userid1 in user_train[1:row_train,1])
{
  i<-1
  if(i<=row_train)
  {
    for(userid2 in user_train[i:row_train,1])  
    {
      euclidean<-0.000000
      if(userid2!=userid1)
      {
        docs<-getDoc(userid1,userid2)    
        docs
        if(!nrow(docs))
        {
          docs <- data.frame(doc_id=0)
          euclidean <- 0.000000
        }
        else
        {
            
            for(doc in docs[,1])
            {
              f <- feature_difference(userid1,userid2,doc)
              print(paste(f)) 
              euclidean <- euclidean + f[1]
            }
        }
        
        euclidean<-sqrt(euclidean)
        print(paste("Euclidean distance: ",euclidean))
        
        #create a dataframe to write to csv file
        df<-data.frame(user1=userid1,user2=userid2,euclidean_distance=euclidean)
        write.table(df,"/e_distance_data.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
        
      }
    }
  }
}


