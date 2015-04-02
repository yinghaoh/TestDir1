myfunction <- function(month_start,year_start,month_end,year_end) {
  
  if (year_start>year_end | month_start<0 | month_start>12 | month_end<0 | month_end>12 | (year_start==year_end & month_start>month_end)){
    print("Error in signature! Please reinput start and end month and year.")
  }else{
    #numMonths <- (year_end-year_start-1)*12 + month_end + 12 - month_start + 1
    month <- month_start
    monthStr <- toString(month)
    if(month<10)monthStr = paste("0",monthStr,sep="")
    year<-year_start
    while(year!=year_end & month != month_end){        
      x <- read.csv(file=paste("C:/Users/yhuang/Desktop/CitiBike data/citibike-tripdata/citibike-tripdata/",toString(year),"-",monthStr," - Citi Bike trip data.csv",sep=""), head=TRUE,sep=",")
      #Remove null cases
      x = x[complete.cases(x),]
      x = x[x$birth.year!="\\N",]
      x$birth.year = 97-as.numeric(x$birth.year)
      #Create dummy for user type
      x$userTypeNum[x$usertype=="Customer"] <- "1"
      x$userTypeNum[x$usertype=="Subscriber"] <- "2"
      id<-grep("^userTypeNum$", colnames(x))
      x[,id] <- as.numeric(x[,id])
      #Convert start time to hour
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
      x$trail <- as.numeric(with(x,paste(start.station.id,end.station.id,sep="")))
      
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
      #Histogram of station - By weekday & gender
      
      
      if(month == 12){
        year = year + 1
        month = 1
      } 
      else month = month + 1     
    }
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
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
  
  return(list(meanMatrix=means,sdMatrix=sds))
}

statSummaryMonthly <- function(x_lean){
  
}