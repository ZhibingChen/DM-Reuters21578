setwd("/Users/apple/Documents/CS909/Exercise10/treetag")
reutersdf <- read.csv("~/Documents/CS909/Exercise10/treetag/task1.2.reutersdf.csv", header=T, sep=",")
c(nrow(reutersdf), ncol(reutersdf))
#[1] 9849   125
library(tm)
library(stringi)
library(proxy)
library(topicmodels)

# #LDA
#LDA process
vec.src <- VectorSource(reutersdf[, ncol(reutersdf)])
corpus <- Corpus(vec.src)
dtm <- DocumentTermMatrix(corpus)
lda <- LDA(dtm, control=list(alpha = 0.1), k=10, method = "VEM")
terms <- terms(lda, 10)
# topics(lda)
lda.terms <- c(terms[,1:10])
lda.terms.unique <- unique(lda.terms)

lda.topics <- topics(lda)
topics.length <- length(topics(lda))
col.names <- lda.terms.unique
lda.features <- as.data.frame(matrix(NA, topics.length, length(col.names)))
colnames(lda.features) <- col.names

for(i in 1:topics.length){
  feature.position <- match(terms[, lda.topics[i]], lda.terms.unique)
  lda.features[i,] <- 0
  lda.features[i, feature.position] <- 1
  print (i)
}

dlt <- c()
for(i in 1:ncol(lda.features)){
  if(sum(lda.features[,i]) == nrow(lda.features)){
    dlt <- c(i,dlt)
  }
}
lda.features <- lda.features[, -dlt]
write.csv(lda.features, "task2.lda.features.csv", row.names=F)

## TF*IDF
vec.src <- VectorSource(reutersdf[, ncol(reutersdf)])
corpus <- Corpus(vec.src)
dtm <- DocumentTermMatrix(corpus)
# dtm2 <- removeSparseTerms(dtm,sparse=0.98)#410
# dtm2 <- removeSparseTerms(dtm,sparse=0.97)#277
# dtm2 <- removeSparseTerms(dtm,sparse=0.96)#196
dtm2 <- removeSparseTerms(dtm,sparse=0.95)#149
# inspect(dtm2[10:15,100:110])
t2<-as.data.frame(inspect(dtm2))
c(nrow(t2),ncol(t2))
#[1] 9849  162

tf.idf <- matrix(0, nrow(t2), ncol(t2))
for(i in 1:ncol(t2)){
  for(j in 1:nrow(t2)){
    frqc.t2 <- t2[j ,i]
    max.frqc <- max(t2[,i])
    tf <- frqc.t2/max.frqc
    
    dfrqc <- length(which(t2[,i] != 0) )
    idf <- log2(nrow(t2)/dfrqc)
    
    tf.idf[j,i] <- tf * idf   
  }
  print(i)
}
tf.idf <- as.data.frame(tf.idf)
#count non-zero values in each row
end<-c()
for(i in 1:nrow(tf.idf)){
  print(i)
  end<-c(length(which(tf.idf[i,] != 0)), end)
}
# end <- ceiling(end*0.5)
#sort the TF*IDF values decreasingly and get the first quarter of the non-zero values
#then set the value in its row and col as 1, otherwise 0.
tf.idf.bi <- matrix(0, nrow(t2), ncol(t2))
for(i in 1:nrow(tf.idf)){
  print(i)
  t.i.order <- order(tf.idf[i,], decreasing = TRUE)
  tf.idf.bi[i,t.i.order[1:ceiling(end[i]*0.25)]] <- 1
}
#sum every row in tf.idf.bi as tf.idf.bi.sum
tf.idf.bi.sum <- c()
for(i in 1:ncol(tf.idf.bi)){
  tf.idf.bi.sum <- c(sum(tf.idf.bi[,i]) ,tf.idf.bi.sum)
}
tf.idf.bi.sum
summary(tf.idf.bi.sum)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 54.0   191.2   257.5   309.4   368.5  1548.0 
length(which(tf.idf.bi.sum <= 257.5))
# [1] 81
#delete 81 features whose sum value is less than 215.2(1st Qt value)
#now we have the data.frame with 81 features and 11340 rows
t2.rm <- t2[,-which(tf.idf.bi.sum <= 257.5)]
c(nrow(t2.rm), ncol(t2.rm))
write.csv(t2.rm , "task2.tfidf.features.csv", row.names=F)

#conbine TF*IDF selected features and LDA selected features
lda.features <- read.csv("~/Documents/CS909/Exercise10/treetag/task2.lda.features.csv", header=T, sep=",")
c(nrow(lda.features), ncol(lda.features))
#[1] 9849   45
tfidf.features <- read.csv("~/Documents/CS909/Exercise10/treetag/task2.tfidf.features.csv", header=T, sep=",")
c(nrow(tfidf.features), ncol(tfidf.features))
#[1] 9849   81
# ldat<-read.csv("~/Documents/CS909/Exercise10/treetag/task2.lda.features.csv", header=T, sep=",")

match <- match(colnames(tfidf.features), colnames(lda.features))
common.cols <- which(! is.na(match))
columns <- match[! is.na(match)]
combine.reutersdf <- cbind(lda.features, tfidf.features[, -common.cols])
c(nrow(combine.reutersdf), ncol(combine.reutersdf))
#[1] 9849  112

for(i in 1:length(columns)){
  for(j in 1:nrow(lda.features)){
    if(lda.features[j, columns[i]] || tfidf.features[j, common.cols[i]])
      combine.reutersdf[j, columns[i]] <- 1
  }
  print(i)
}
write.csv(combine.reutersdf, "task2.lda.combined.features.csv", row.names = F)



combined.features <- read.csv("~/Documents/CS909/Exercise10/treetag/task2.lda.combined.features.csv", header=T, sep=",")
c(nrow(combined.features),  ncol(combined.features))
#[1] 9849  112
lda.features <- read.csv("~/Documents/CS909/Exercise10/treetag/task2.lda.features2.csv", header=T, sep=",")
c(nrow(lda.features),  ncol(lda.features))
#[1] 9849  112
reutersdf <- read.csv("~/Documents/CS909/Exercise10/treetag/task1.reutersdf.csv", header=T, sep=",")
c(nrow(reutersdf),  ncol(reutersdf))
#[1] 9849  125

TOPIC.tags.names <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", 
                      "topic.crude", "topic.trade", "topic.interest", "topic.ship", 
                      "topic.wheat", "topic.corn")
TOPIC.tags.names <- sort(TOPIC.tags.names)
# [1] "topic.acq"      "topic.corn"     "topic.crude"    "topic.earn"     "topic.grain"   
# [6] "topic.interest" "topic.money.fx" "topic.ship"     "topic.trade"    "topic.wheat" 
col <- c()
for(i in 1:length(TOPIC.tags.names)){
  col <- c(col, which(names(reutersdf) == TOPIC.tags.names[i]))
}
col

#combine features with TOPIC tags
reuters.combined.features <- cbind(reutersdf, combined.features)
c(nrow(reuters.combined.features),  ncol(reuters.combined.features))
#[1] 9849  237
reuters.lda.features <- cbind(reutersdf, lda.features)
c(nrow(reuters.lda.features),  ncol(reuters.lda.features))
#[1] 9849  170

col1 <- ncol(reuters.combined.features)
col2 <- ncol(reuters.lda.features)
reuters.combined.features <- reuters.combined.features [, c(3,col,127:col1)]
c(nrow(reuters.combined.features),  ncol(reuters.combined.features))
#[1] 9849  122
reuters.lda.features <- reuters.lda.features [, c(3,col,127:col2)]
c(nrow(reuters.lda.features),  ncol(reuters.lda.features))
#[1] 9849   55

reuters.combined.features <- cbind(reuters.combined.features, class=NA)
c(nrow(reuters.combined.features),  ncol(reuters.combined.features))
#[1] 9849  123
reuters.lda.features <- cbind(reuters.lda.features, class=NA)
c(nrow(reuters.lda.features ),  ncol(reuters.lda.features ))
#[1] 9849   56

dlt <- which(apply(reuters.combined.features [1:nrow(reuters.combined.features ),2:11], 1, sum) == 0)
length(dlt)
#[1] 1558
reuters.combined.features  <- reuters.combined.features[-dlt,]
c(nrow(reuters.combined.features ),  ncol(reuters.combined.features))
#[1] 8291  123
dlt <- which(apply(reuters.lda.features [1:nrow(reuters.lda.features ),2:11], 1, sum) == 0)
length(dlt)
#[1] 1558
reuters.lda.features  <- reuters.lda.features[-dlt,]
c(nrow(reuters.lda.features ),  ncol(reuters.lda.features ))
#[1] 8291 56

row <- nrow(reuters.combined.features)
row
# [1] 8291
for(i in 1:row){#each row
  for(j in 2:11){#each topic in each row
    if(reuters.combined.features[i,j] == 1) {
      temp <- reuters.combined.features[i,]
      temp[,ncol(temp)] <- names(temp[,2:11])[j-1]
      temp[,2:11] <- 0
      temp[,j] <- 1
      reuters.combined.features <- rbind(reuters.combined.features, temp)
    }
  }
print(i)
}
c(nrow(reuters.combined.features ),  ncol(reuters.combined.features))
#[1] 17467   123
dlt <- c(1:row)
length(dlt)
#[1] 8291
reuters.combined.features <- reuters.combined.features[-dlt,]
c(nrow(reuters.combined.features ),  ncol(reuters.combined.features))
#[1] 9176  123
write.csv(reuters.combined.features, "task2.reuters.combined.features.csv", row.names = F)

row <- nrow(reuters.lda.features)
row
#[1] 8291
for(i in 1:row){#each row
  for(j in 2:11){#each topic in each row
    if(reuters.lda.features[i,j] == 1) {
      temp <- reuters.lda.features[i,]
      temp[,ncol(temp)] <- colnames(temp[,2:11])[j-1]
      temp[,2:11] <- 0
      temp[,j] <- 1
      reuters.lda.features <- rbind(reuters.lda.features, temp)
    }
  }
  print(i)
}
c(nrow(reuters.lda.features),  ncol(reuters.lda.features))
#[1] 17467    56
dlt <- c(1:row)
length(dlt)
#[1] 8291
reuters.lda.features <- reuters.lda.features[-dlt,]
c(nrow(reuters.lda.features),  ncol(reuters.lda.features))
#[1] 9176   56

write.csv(reuters.lda.features, "task2.reuters.lda.features.csv", row.names = F)

  
