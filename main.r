install.packages("rjson")
install.packages("tm")
install.packages("svd")
install.packages("fpc")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("cluster")

library(rjson)
library(tm)
library(svd)
library(fpc)
library(wordcloud)
library(cluster)
library(RColorBrewer)

# Rank a restaurant for a given topic
rankRestaurant <- function(restaurant_number,threshold,data,dtm,svd){
  
  result <- vector()
  rankings <- matrix(0,2000,116000) 
  rankings [1,1]<- data$stars[1][[1]]
  j<-1
  k<-1
  topics <- cbind(1,2,3,4,5)
  Uk <- scale(svd$u[,1:5])
  Vk <- scale(svd$v[,1:5])
  Dk <- scale(diag(svd$d[1:5]))
  Dk1 <- scale(solve(diag(svd$d[1:5])))
  
  
  for (t in topics){
    for(i in 2:length(data$business_id)){
      
      if (as.character(data$business_id[i]) == as.character(data$business_id[(i-1)])){
        
        review <-as.numeric(as.matrix(dtm[1,]))
        projection <- t(review)%*%Vk%*%Dk1
        
        if(projection[t]>threshold){
          rankings[k,j] <- data$stars[i][[1]]
        }
        k<-k+1
        
      }
      else{
        j<-j+1
        k<-1
        review <-as.numeric(as.matrix(dtm[i,]))
        projection <- t(review)%*%Vk%*%Dk1
        
        if(projection[t]>threshold){
          rankings[k,j] <- data$stars[i][[1]]
        }
        k<-k+1
      }
    } 
    k
    j
    final<- vector()
    
    for(i in 1:dim(rankings)[2]){
      sum<-0
      number<-0
      for(j in 1:dim(rankings)[1]){
        
        if(rankings[j,i] != 0){
          sum <- sum + rankings[j,i]
          number<- number+1
        }
        final[i]<-sum/number
      }
    }
    result <- cbind(result,final)
  }
  return(result)
}

# Convert raw JSON file into managable data frame
UnpackJSON <- function(filePath) {
  con <- file(filePath, "r")
  input <- readLines(con, -1L)
  jsonData <- sapply(input,fromJSON)
  close(con)
  df <- data.frame(jsonData)
  temp <- rownames(df)
  df <- as.data.frame(t(df))
  colnames(df) <- temp
  rownames(df) <- NULL
  return(df)
}

# Convert the nested lists into regular vectors
UnlistJSON <- function(df) {
  
  for(i in 1:ncol(df)) {
    temp <- unlist(df[,i])
    names(temp) <- NULL
    df[,i] <- temp
  }
  return(df)
}

# Convert the votes column from a list into 3 seperate columns for useful, funny, cool
UnwrapVotes <- function(df) {
  
  temp <- unlist(df$votes)
  names(temp) <- NULL
  index <- seq(from=1, to=length(temp)-2, by=3)
  df$useful <- temp[index]
  index <- seq(from=2, to=length(temp)-1, by=3)
  df$funny <- temp[index]
  index <- seq(from=3, to=length(temp), by=3)
  df$cool <- temp[index]
  return(df)
}

# Compute the document term matrix
computeDTM <- function(data){
  
  text <- UnlistJSON(t(data$text))
  text <- t(text)
  
   corpus <- VCorpus(VectorSource(text),readerControl=list(reader=readPlain))
   corpus <- tm_map(corpus, tolower)
   corpus <- tm_map(corpus, removeWords, stopwords("english"))
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, stemDocument) 
   corpus <- tm_map(corpus, removeNumbers)
   corpus <- tm_map(corpus, stripWhitespace)
   corpus <- tm_map(corpus, PlainTextDocument)


   dtm <- DocumentTermMatrix(corpus,control=list(weight=weightTfIdf))
   dtm <- removeSparseTerms(dtm, 0.8)

  return(dtm)
  
}

# Commpute the word distribution for the corresponding topic
getTopics <- function(dtm,Vk,topic_number,words_number){
  
  wordlist <- Vk[,]
  rownames(wordlist) <- colnames(dtm)
  result <- matrix()
  
  for (t in 1:topic_number){
    wordlist.sorted <-sort(abs(wordlist[,t]), decreasing = TRUE)[1:words_number]
    d <- data.frame(word = names(wordlist.sorted))
    if (t==1){
      result <- as.matrix(d)
      colnames(result) <- paste("Topic",as.character(t),sep=" ")
    }
    else{
      colnames(d) <- paste("Topic",as.character(t),sep=" ")
      result <- cbind(result,as.matrix(d))
      
    }
    result <- data.frame(result)
  }
  return(result)
}


### Data collection ###
#json_file <- "C:/Users/tofor_000/Desktop/Restaurant_Reviews/Restaurants_Reviews2.json"
json_file <- "C:/Users/tofor_000/Desktop/reviews2.json"
data <- UnpackJSON(json_file)
dtm <- computeDTM(data)

### SVD ####
svd <- propack.svd(as.matrix(dtm),neig = 200)
barplot(svd$d[1:50],beside=TRUE,legend=levels(svd$d[1:10]),main="Singular values Distribution", xlab="Singular value")
Uk <- scale(svd$u[,1:5])
Vk <- scale(svd$v[,1:5])
Dk <- scale(diag(svd$d[1:5]))
Mk <- Uk%*%Dk%*%t(Vk)

### K-Means  ###
cl <- kmeans(t(Mk),50)
colour <- cbind("red","green","blue","purple","yellow","brown","black","orange","blue2","red2")
plot(cl$centers,pch=19,col=colour)
plotcluster(Mk,cl$cluster)

### Words distribution  ###
wordlist <- svd$v[,]
topics <- getTopics(dtm,Vk,10,10)


###  Cloud of words  ### 
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=2400,height=1800)
wordcloud(d$word,d$freq, scale=c(1,20),min.freq=0.01,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

restaurant_rankings <- rankRestaurant(1,5,data,dtm,svd)

variances <- vector()
means <- vector()

vect <- cbind(3.645489,3.630118,3.614350,3.63440,3.622337)
vect2 <- cbind(4.5,3.6667,3.33333,4)
vect3 <- cbind(3.5,3.6667)
vect4 <- cbind(4,5)
vect5 <- cbind(4.375,5,4.375,4.8,4.8)
vect6 <- cbind(4,5,5,2)
vect7 <- cbind(4,2,5,1,4)
vect8 <- cbind(3,1.5,3,2,2.5)

variances[1] <-sd(vect)
means[1] <- mean(vect)
variances[2] <- sd(vect2)
means[2] <- mean(vect2)
variances[3] <- sd(vect3)
means[3] <- mean(vect3)
variances[4] <- sd(vect4)
means[4] <- mean(vect4)
variances[5] <- sd(vect5)
means[5] <- mean(vect5)
variances[6] <- sd(vect6)
means[6] <- mean(vect6)
variances[7] <- sd(vect7)
means[7] <- mean(vect7)
variances[8] <- sd(vect8)
means[8] <- mean(vect8)

grades <- cbind(4.5,4,3.5,4,4.5,4,3.5,3.5)
mat <- as.matrix(rbind(t(as.matrix(means)),grades))

barplot(variances,main="Variance for restaurant rankings",xlab="Restaurants",ylab="Standard deviation")
barplot(grades,main="Average of restaurant rankings",xlab="Restaurants",ylab="Mean")

barplot(mat,main="Restaurant rankings",xlab="Restaurants",ylab="ranking",col=c("darkblue","red"), beside=TRUE,)
