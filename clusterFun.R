library(tm)
library(twitteR)
library(proxy)
library(wordcloud)
library(fpc)
library(topicmodels)
library(png)


#clusteringFun(30,0.99,0.9,FALSE,FALSE,0)
clusteringFun <- function(numCluster,sparsity,factor,flag,flag_topic,numTopic){
  # change this file location to suit your machine
  file_loc <- "trdata.csv"
  # change TRUE to FALSE if you have no column headings in the CSV
  trdata <- read.csv(file_loc, header = FALSE, sep="\n",strip.white=TRUE)
  trdata$id <- matrix(1:length(trdata[,1]),length(trdata[,1]),1) 
  colnames(trdata)[1]<-"Text"
  require(tm)
  
  trdataFinal <- data.frame(matrix(nrow=1591,ncol=0))
  
  
  #stem document
  #datasetCopy <- dataset
  #dataset <- tm_map(dataset, stemDocument)
  #dataset <-tm_map(dataset, stemCompletion, dictionary = datasetCopy)
  #######################
  for(i in 1:length(trdata$Text)){
    trdataFinal$version[i]<-""
    trdataFinal$numInfo[i]<-""
    trdataFinal$cvlt[i]<-""
    trdataFinal$organization[i]<-""
    trdataFinal$content[i]<-""
    textVec <- strsplit(toString(trdata$Text[[i]]), "-")    
    textContent <- ""
    for(j in 1:length(textVec[[1]])){
      textVec[[1]][j] <- trim(textVec[[1]][j])
      tmp <- ripNumPunc(textVec[[1]][j])
      if(tolower(tmp)=="v"){
        trdataFinal$version[i]<-textVec[[1]][j]
      }

      else if(tmp == ""){
        trdataFinal$numInfo[i]<-ripPunc(textVec[[1]][j])
      }
      else if(tolower(tmp) == "cvlt"){
        trdataFinal$cvlt[i]<-textVec[[1]][j]
      }
      else if(tolower(tmp)!="tr"){
        if(isAllUpper(tmp)){
          trdataFinal$organization[i]<-textVec[[1]][j]
        }      
        else{
          if(textContent=="")textContent<- tmp
          else textContent<- paste(textContent,tmp,sep="-") 
        }
      }
    }   
    if(textContent=="" & trdataFinal$organization[i]!=""){    
      trdataFinal$content[i] <- trdataFinal$organization[i]
      trdataFinal$organization[i]<- ""
    }
    else trdataFinal$content[i]<-textContent  
  }
  write.csv(trdataFinal, file=paste("cleaned_data.csv",sep=","))
  
  dataset <- Corpus(DataframeSource(as.data.frame(trdataFinal$Text)))  
  dataset <- tm_map(dataset, tolower)
  dataset <- tm_map(dataset, removeNumbers) 
  dataset <- tm_map(dataset, removeWords, stopwords("english"))  
  dataset <- tm_map(dataset, removePunctuation)  
  dataset <- tm_map(dataset, stripWhitespace)
  dataset <- tm_map(dataset,PlainTextDocument)
  
  dtm <- DocumentTermMatrix(dataset,control=list(wordLengths=c(2,Inf), bounds = list(global = c(2,floor(length(trdata[,1])*factor))),
                                                weighting = function(x)
                                                  weightTfIdf(x, normalize = TRUE)))
  tdmTF <- TermDocumentMatrix(dataset,control=list(wordLengths=c(2,Inf),bounds = list(global = c(2,floor(length(trdata[,1])*factor))),
                                                 weighting = function(x)
                                                   weightTf(x)))
  #Remove empty entry
  rowTotals <- apply(dtm , 1, sum) 
  trdata <- trdata[which(rowTotals!=0),]
  dataset <- dataset[which(rowTotals!=0)]
  dtm <- dtm[rowTotals> 0,]  
  colTotals <- apply(tdmTF , 2, sum) 
  tdmTF <- tdmTF[ ,colTotals> 0]

  #Remove duplicates
  trdata <- trdata[!duplicated(dtm,MARGIN=1),]
  dataset <- dataset[!duplicated(dtm,MARGIN=1)]
  dtm <- unique(dtm,MARGIN=1)
  tdmTF <- unique(tdmTF,MARGIN=2)
   
  findFreqTerms(tdmTF, 100)
  #WordCloud
  m <- as.matrix(tdmTF)
  word.freq <- sort(rowSums(m), decreasing =T)
  png("plots/WordCloudPlotAll.png")
  wordcloud(words = names(word.freq), freq = word.freq, min.freq = 15, random.order = F)
  dev.off()
  
  if(flag)
    dtm <- removeSparseTerms(dtm, sparse = sparsity)
  set.seed(100)
  m2 <- as.matrix(dtm)
  rownames(m2) <- 1:nrow(m2)   
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  m_norm <- norm_eucl(m2)
  cl <- kmeans(m_norm, numCluster,nstart=25)
  trdata$cluster <- matrix(cl$cluster,length(cl$cluster),1) 

  for(i in 1:numCluster){
    cat(paste("cluster",i,": ",sep = ""))
    s<-sort(cl$centers[i,], decreasing = T)
    cat(names(s)[1:5],"\n")
  }
  
  print(table(cl$cluster))
  write.csv(trdata, file=paste("Result_NumCluster#",numCluster,".csv",sep=""))

  plot(prcomp(m_norm)$x,col=(cl$cluster)+1, main = "K-Means Clustering Results", xlab = "", ylab = "", pch = 20, cex = 2)

  if(flag_topic){
    lda = LDA(as.DocumentTermMatrix(tdmTF),k=numTopic)
    term <- apply(terms(lda,5), MARGIN = 2, paste, collapse = ", ")
    term
    #for(i in 1:numTopic){
    print(topics(lda,1))
    #}
    
  }

  #Hierarchical clustering
  #d <- dist(m, method="cosine")
  #hc <- hclust(d, method="average")
  #plot(hclust)
  #myplclust(hclust, lab = rep())
  
  #cl <- cutree(hc, 50)
  #table(cl)
  #findFreqTerms(dtm[cl==1], 50)
  resultList<- list(m,trdata$cluster,numCluster)
  return(resultList)
}
#plotClusterWordCloud(resultList,TRUE,5,0)
plotClusterWordCloud <- function(resultList, batch, mfreq, clusterId){
  m <- resultList[1][[1]]
  trCluster<- resultList[2][[1]]
  numCluster<- resultList[3][[1]]
  if(batch==TRUE){
    for(i in 1:numCluster){
      if(length(which(trCluster==i))>0){
        m_clust <- m[,which(trCluster==i)]
        word.freq <- sort(rowSums(m_clust), decreasing =T)
        png(paste("plots/WordCloudPlot_Cluster#",i,".png",sep=""))
        wordcloud(words = names(word.freq), freq = word.freq, min.freq = mfreq, random.order = F)
        dev.off()
      }
    }
  }else{
    if(length(which(trCluster==clusterId))>0){
      m_clust <- m[,which(trCluster==clusterId)]
      word.freq <- sort(rowSums(m_clust), decreasing =T)
      png(paste("plots/WordCloudPlot_Cluster#",clusterId,".png",sep=""))
      wordcloud(words = names(word.freq), freq = word.freq, min.freq = mfreq, random.order = F)
      dev.off()
    }
  }
  
  
   
}

isNum <- function(x){
  return(!is.na(as.numeric(x)))
}

isUpper <- function(x) grepl("^[[:upper:]]+", x)

isAllUpper <- function(x){
  flag <- TRUE
  tmp <- strsplit(x, " ")
  for(i in 1:length(tmp[[1]])){
    if(!isUpper(tmp[[1]][i])) flag <- FALSE
  }
  return(flag)
}

ripNumPunc <- function(x){
  y <- gsub("\\d+","",x)
  y <- gsub("[[:punct:]]","",y)
  y<-trim(y)
  return(y)
}

ripPunc <- function(x){
  y <- gsub("[[:punct:]]","",y)
  y<-trim(y)
  return(y)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


myplclust <- function(hclust,lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1, ...){
  y<-rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang,...)
  text(x=x,y=y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order], col=lab.col[hclust$order], srt=90,adj=c(1,0.5), xpd = NA, ...)
}