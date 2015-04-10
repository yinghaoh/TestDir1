library("car")

citiBikeAccu <- function() {
  x <- read.csv(file="AccumulationData.csv", head=TRUE,sep=",")
  
  x = x[complete.cases(x),]

  x$weekDay <- weekdays(as.Date(x$Date))
  x$weekDay[x$weekDay=="Monday"] <- "1"
  x$weekDay[x$weekDay=="Tuesday"] <- "1" 
  x$weekDay[x$weekDay=="Wednesday"] <- "1" 
  x$weekDay[x$weekDay=="Thursday"] <- "1" 
  x$weekDay[x$weekDay=="Friday"] <- "1" 
  x$weekDay[x$weekDay=="Saturday"] <- "2" 
  x$weekDay[x$weekDay=="Sunday"] <- "2" 
  id<-grep("^weekDay$", colnames(x))
  x[,id] <- as.numeric(x[,id])
  
  groupResultsListByWeekDay <- printMeanAndSdByGroup(x[c(2,4,7,8,9)],x[10])
  groupResultsListByWeekDay <- cbind(groupResultsListByWeekDay[[1]],groupResultsListByWeekDay[[2]])
  write.table(groupResultsListByWeekDay, file="groupResultsListAccu.csv", sep=",",col.names=NA)
  corResult <- mosthighlycorrelated(x[c(2,4,7,8,9,10)],10)
  write.table(corResult, file="corResultAccu.csv", sep=",",col.names=NA)
  return(groupResultsListByWeekDay)
}

printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  
  return(list(meanMatrix=means,sdMatrix=sds))
}

statSummaryMonthly <- function(x_lean){
  
}


mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}


