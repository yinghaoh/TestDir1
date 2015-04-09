library("car")

citiBikeFunc <- function(month_start,year_start,month_end,year_end) {
  
  if (year_start>year_end | month_start<0 | month_start>12 | month_end<0 | month_end>12 | (year_start==year_end & month_start>month_end)){
    print("Error in signature! Please reinput start and end month and year.")
  }else{
    #numMonths <- (year_end-year_start-1)*12 + month_end + 12 - month_start + 1
    month <- month_start
    monthStr <- toString(month)
    if(month<10)monthStr = paste("0",monthStr,sep="")
    year<-year_start
    while( (month<=month_end & year==year_end) || year<year_end ){ 
      print(year)
      x <- read.csv(file=paste("C:/Users/yhuang/Desktop/CitiBike data/citibike-tripdata/citibike-tripdata/",toString(year),"-",monthStr," - Citi Bike trip data.csv",sep=""), head=TRUE,sep=",")
      #Remove null cases
      x = x[complete.cases(x),]
      x = x[x$birth.year!="\\N",]
      x = x[x$gender!=0,]
      x$birth.year = 97-as.numeric(x$birth.year)
      #Create dummy for user type
      x$userTypeNum[x$usertype=="Customer"] <- "1"
      x$userTypeNum[x$usertype=="Subscriber"] <- "2"
      id<-grep("^userTypeNum$", colnames(x))
      x[,id] <- as.numeric(x[,id])
      id = grep("^starttime$", colnames(x))
      x$triphour<-as.numeric(substr(x$starttime, 12, 13))
      x$weekDay <- weekdays(as.Date(x$starttime))
      x$weekDay[x$weekDay=="Monday"] <- "1"
      x$weekDay[x$weekDay=="Tuesday"] <- "1" 
      x$weekDay[x$weekDay=="Wednesday"] <- "1" 
      x$weekDay[x$weekDay=="Thursday"] <- "1" 
      x$weekDay[x$weekDay=="Friday"] <- "1" 
      x$weekDay[x$weekDay=="Saturday"] <- "2" 
      x$weekDay[x$weekDay=="Sunday"] <- "2" 
      id<-grep("^weekDay$", colnames(x))
      x[,id] <- as.numeric(x[,id])
      x$trail <- as.numeric(with(x,paste(x$start.station.id,x$end.station.id,sep="")))
      
      colName_Lean <-c("tripduration","triphour","start.station.id","end.station.id","userTypeNum","birth.year","gender","weekDay","trail")
      id <- colnames(x) %in% colName_Lean
      idT <- which(id==TRUE)
      idF <- which(id==FALSE)
      x_lean <- x[,idT]
      x <- x[,idF]

      #Group analysis
      groupResultsListByHour <- printMeanAndSdByGroup(x_lean[c(1,4,5,6,8)],x_lean[7])       
      groupResultsListByGender <- printMeanAndSdByGroup(x_lean[c(1,4,6,7,8)],x_lean[5])  
      groupResultsListByWeekday <- printMeanAndSdByGroup(x_lean[c(1,4,5,6,7)],x_lean[8]) 
      groupResultsListByHourFinal <-list()
      groupResultsListByGenderFinal <- list()
      groupResultsListByWeekdayFinal <- list()
      if(year==year_start & month == month_start){
        groupResultsListByHourFinal[[1]] <- cbind(groupResultsListByHour[[1]],groupResultsListByHour[[2]])
        groupResultsListByGenderFinal[[1]] <- cbind(groupResultsListByGender[[1]],groupResultsListByGender[[2]])
        groupResultsListByWeekdayFinal[[1]] <- cbind(groupResultsListByWeekday[[1]],groupResultsListByWeekday[[2]])
      }else{
        groupResultsListByHourFinal[[length(groupResultsListByHourFinal)+1]] <- cbind(groupResultsListByHour[[1]],groupResultsListByHour[[2]])
        groupResultsListByGenderFinal[[length(groupResultsListByGenderFinal)+1]] <- cbind(groupResultsListByGender[[1]],groupResultsListByGender[[2]])
        groupResultsListByWeekdayFinal[[length(groupResultsListByWeekdayFinal)+1]] <- cbind(groupResultsListByWeekday[[1]],groupResultsListByWeekday[[2]])
      }

      #Histogram of start and station and trail - By weekday & gender & hour
      trailList <- sort(table(x_lean[9]),decreasing = T)[1:10]
      startList <- sort(table(x_lean[2]),decreasing = T)[1:10]
      endList <- sort(table(x_lean[3]),decreasing = T)[1:10]
      nL <- as.numeric(names(trailList))
      trailList <- rbind(trailList,nL)
      rownames(trailList) <- NULL
      nL <- as.numeric(names(startList))
      startList <- rbind(startList,nL)
      rownames(startList) <- NULL
      nL <- as.numeric(names(endList))
      endList <- rbind(endList,nL)
      rownames(endList) <- NULL
      
      if(year==year_start & month == month_start){
        trailListFinal <- trailList
        startListFinal <- startList
        endListFinal <- endList
      }else{
        trailListFinal <- cbind(trailListFinal,trailList)
        startListFinal <- cbind(startListFinal,startList)
        endListFinal <- cbind(endListFinal,endList)
      }
      
      #Correlation analysis      
      if(month == 12){
        year = year + 1
        month = 1
      } 
      else month = month + 1     
    }
    
    groupResultsListByHourFinal <- Reduce("+", groupResultsListByHourFinal) / length(groupResultsListByHourFinal)
    groupResultsListByGenderFinal <- Reduce("+", groupResultsListByGenderFinal) / length(groupResultsListByGenderFinal)
    groupResultsListByWeekdayFinal <- Reduce("+", groupResultsListByWeekdayFinal) / length(groupResultsListByWeekdayFinal)
    
    trailListFinalHist <- sapply(split(1:ncol(trailListFinal),trailListFinal[2,]), function(i) sum(trailListFinal[1,i]))
    startListFinalHist <- sapply(split(1:ncol(startListFinal),startListFinal[2,]), function(i) sum(startListFinal[1,i]))
    endListFinalHist <- sapply(split(1:ncol(endListFinal),endListFinal[2,]), function(i) sum(endListFinal[1,i]))
  
    trailListFinalHist <- sort(trailListFinalHist, decreasing=TRUE)[1:10]
    startListFinalHist <- sort(startListFinalHist, decreasing=TRUE)[1:10]
    endListFinalHist <- sort(endListFinalHist, decreasing=TRUE)[1:10]
    
    write.table(groupResultsListByHourFinal, file="groupResultsList.csv", sep=",", col.names=NA)
    write.table(groupResultsListByGenderFinal, file="groupResultsList.csv",sep=",", append = TRUE,col.names=NA)
    write.table(groupResultsListByWeekdayFinal, file="groupResultsList.csv", sep=",",append = TRUE,col.names=NA)
        
    trailListFinalHist <- data.frame(trailListFinalHist)
    startListFinalHist <- data.frame(startListFinalHist)
    endListFinalHist <- data.frame(endListFinalHist)
    
    names(trailListFinalHist) <- "Trail Count Average"
    names(startListFinalHist) <- "Start Station Count Average"
    names(endListFinalHist) <- "End Station Count Average"
     
    write.table(trailListFinalHist, file="StationListFinalHist.csv", sep=",", col.names=NA)
    write.table(startListFinalHist, file="StationListFinalHist.csv",sep=",", append = TRUE,col.names=NA)
    write.table(endListFinalHist, file="StationListFinalHist.csv",sep=",", append = TRUE,col.names=NA)
    
    print("Success!")
  } 
  
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