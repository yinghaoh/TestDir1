library(tm)
library(proxy)

clusteringFun <- function(numCluster){
  # change this file location to suit your machine
  file_loc <- "C:\\Documents and Settings\\Administrator\\Desktop\\Book1.csv"
  # change TRUE to FALSE if you have no column headings in the CSV
  x <- read.csv(file_loc, header = TRUE)
  require(tm)
  dataset <- Corpus(DataframeSource(x))  
  dataset <- tm_map(dataset, as.PlainTextDocument)
  dataset <- tm_map(dataset, tolower)
  dataset <- tm_map(dataset, removeWords, stopwords("english"))
  dataset <- tm_map(dataset, removePunctuation)
  dataset <- tm_map(dataset, stripWhitespace)
  dtm <- DocumentTermMatrix(dataset)
  
  findFreqTerms(dtm, 100)
  #Kmeans
  dtm_tfxidf <- weightTfIdf(dtm)
  m <- as.matrix(dtm_tfxidf)
  rownames(m) <- 1:nrow(m)
  
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  m_norm <- norm_eucl(m)
  cl <- kmeans(m_norm, numCluster)
  
  table(cl$cluster)
  
  plot(prcomp(m_norm)$x, col=cl$cl)
  
  findFreqTerms(dtm[cl$cluster==1], 50)
  
  #Hierarchical clustering
  #d <- dist(m, method="cosine")
  #hc <- hclust(d, method="average")
  #plot(hclust)
  #myplclust(hclust, lab = rep())
  
  #cl <- cutree(hc, 50)
  #table(cl)
  #findFreqTerms(dtm[cl==1], 50)
  
}


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