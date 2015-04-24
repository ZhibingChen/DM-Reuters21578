#install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",repos=NULL, type = "source")
library("NLP")
library("openNLP")
library("openNLPmodels.en")
library("tm")
library("tau")
library("koRpus")
library("XML")
library("httr")
setwd("/Users/apple/Documents/CS909/Exercise10/treetag")
reutersCSV <- read.csv("~/Documents/CS909/Exercise10/reutersCSV.csv/reutersCSV.csv", header=T, sep=",")

#1.1 clean data
## remove rows with all topics are 0
row <- nrow(reutersCSV)
rmRow <- c()
for(i in 1:row){
  if(sum(reutersCSV[i,4:138]) == 0){
    rmRow <- c(rmRow, i)
  }
}
reutersCSV <- reutersCSV[-rmRow,]
## remove columns with all rows are 0
row <- nrow(reutersCSV)
col <- ncol(reutersCSV)
rmCol <- c()
for(j  in 4:138){
  if(sum(reutersCSV[1:row, j]) == 0){
    rmCol <- c(rmCol, j)
  }
}
reutersdf <- reutersCSV[, -rmCol]
reutersdf <- reutersdf[-which(reutersdf$purpose=="not-used"),]
## backup file
write.csv(reutersdf, "task1.1.reutersdf.csv", row.names = F)

#1.2 pro-pressing
reutersdf <- read.csv("~/Documents/CS909/Exercise10/treetag/task1.1.reutersdf.csv", header=T, sep=",")
reutersdf[, "processedTitle"] <- NA
reutersdf[, "processedText"] <- NA
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

person_entity_annotator <- Maxent_Entity_Annotator(kind = "person")
location_entity_annotator <- Maxent_Entity_Annotator(kind = "location")
organization_entity_annotator <- Maxent_Entity_Annotator(kind = "organization")
date_entity_annotator <- Maxent_Entity_Annotator(kind = "date")
money_entity_annotator <- Maxent_Entity_Annotator(kind = "money")
percentage_entity_annotator <- Maxent_Entity_Annotator(kind = "percentage")

NameEntities <- function(doc, kind){
  s <- doc$content
  a <- annotations(doc)[[1]]
  if( hasArg("kind")){
    k <- sapply(a$features, '[[', "kind")
    s[a[k == kind]]
  }
  else{
    s[a[a$type == "entity"]]
  }
}

##main loop
for(f in 1:nrow(reutersdf)){
  print(f)
    if(nchar(as.String(reutersdf[f,122])) != 0){
    ## 1.2.1 Tokenize: divide into words (unigrams)
    #     f=1
      text <- as.String(reutersdf[f, ncol(reutersdf)-3])
    text <- gsub("/", " ", text)
    #replace / as a space
    tokens <- scan_tokenizer(text)
    
    ## 1.2.3 Remove punctuation (note that sometimes punctuation can be a feature),
    ##      remove links or replace with link pllaceholder
    tokensRemove <- removePunctuation(tokens, preserve_intra_word_dashes = TRUE)
    
    ## 1.2.4 Run Part-Of-Speech tagger (find VB, NP and other POS)
    tagResult <- treetag(tokensRemove, treetagger="manual", format="obj",
                         TT.tknz=FALSE , lang="en",
                         TT.options=list(path="./TreeTagger", preset="en"))
    
    ## 1.2.5 Perform lemmatisation and/ or stemming
    tagLemma <- tagResult@TT.res$lemma
    unknown <- which(tagLemma == "<unknown>")
    # words cannot be recognized will be tagged as <unknown>
    card <- which(tagLemma == "@card@")
    # numbers will be tagged as @card@
    tagLemma[unknown] <- tokensRemove[unknown]
    tagLemma[card] <- tokensRemove[card]
    # copy the known words and numbers back to the tagLemma
    
    ## 1.2.6 Remove stop words (e.g. articles, personal pronouns, prepositions, auxiliary verbs, connectives) 
    tagLemmaS <- paste(tagLemma, collapse = " ")
    t1 <- annotate(tagLemmaS, list(sent_token_annotator, word_token_annotator))
    t2 <- annotate(tagLemmaS, pos_tag_annotator, t1)
    t2 <- subset(t2, type == "word")
    # recognize POS
    pos <- sapply(t2$features, '[[', "POS")
    rmWords <- c()
    rmWords <- c(rmWords, which(pos == "PRP"))
    rmWords <- c(rmWords, which(pos == "IN"))
    rmWords <- c(rmWords, which(pos == "TO"))
    rmWords <- c(rmWords, which(pos == "MD"))
    rmWords <- c(rmWords, which(pos == "DT"))
    rmWords <- c(rmWords, which(pos == "CC"))
    rmWords <- c(rmWords, which(pos == "WDT"))
    rmWords <- c(rmWords, which(pos == "VBP"))
    rmWords <- c(rmWords, which(pos == "PRP$"))
    
    if(length(rmWords) != 0){
      rmtagLemma <- tagLemma[-rmWords]
    }else{
      rmtagLemma <- tagLemma
    }
    
    ## 1.2.7 Run Named Entity recogniser
    rmtagLemmaS <- as.String(paste(rmtagLemma, collapse = " "))
    annotations <- annotate(rmtagLemmaS, list(sent_token_annotator, 
                                              word_token_annotator,
                                              person_entity_annotator,
                                              location_entity_annotator,
                                              organization_entity_annotator,
                                              date_entity_annotator,
                                              money_entity_annotator,
                                              percentage_entity_annotator))
    if( annotations$type[ nrow(as.data.frame(annotations))] == "entity"){
      #if entities been recognized (the last line in annotations is entity)
      textDoc <- AnnotatedPlainTextDocument(rmtagLemmaS, annotations)
      annoFeatures <- unique(annotations$features)
      annoFeaturesLength <- length(annoFeatures)
      kinds <- c()
      
      for(i in 1:annoFeaturesLength){
        if(length(as.data.frame(annoFeatures[[i]])) ==0 )
          start <- i
      }
      
      for( n in (start+1):annoFeaturesLength){
        kinds <- c(kinds, as.character(annoFeatures[[n]]))
      }
      replaceKinds <- toupper(kinds)
      
      for(i in 1:length(replaceKinds)){
        replace <- as.character(NameEntities(textDoc, kind = kinds[i]))
        for(j in 1:length(replace)){
          rmtagLemmaS <- gsub(replace[j], replaceKinds[i], rmtagLemmaS)
        }
      }
      rmtagLemmaS <- gsub("[[:digit:]]+", "NUMBER", rmtagLemmaS) 
    }
    #store the processed doc.text
    reutersdf[f,ncol(reutersdf)-1] <- rmtagLemmaS
  }
}
reutersdf <- reutersdf[-which(is.na(reutersdf[,ncol(reutersdf)])),]
write.csv(reutersdf, "task1.2.reutersdf.csv", row.names = F)

