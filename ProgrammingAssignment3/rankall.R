rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv" , header =TRUE, stringsAsFactors=F, na.strings="Not Available") 

  ## Check that state and outcome are valid
  
  outcomeV <- c("heart attack","heart failure","pneumonia")
  if(!(outcome %in% outcomeV)) stop("invalid outcome")   
  if(class(num)=="character")
    if(!(num %in% c("best","worst"))) stop ("invalid num")
  
  ## For each state, find the hospital of the given rank
  X <- data # store data as X for manipulation
  names(X)[c(11,17,23)]<- outcomeV # set names for column to follow outcome
  X <- X[complete.cases(X[,outcome]),c("Hospital.Name","State",outcome)]# set subset for mortality outcome
  X1 <- split(X,X$State)
  ans <- lapply(X1,function(y,num)
  {
    y=y[order(as.numeric(y[,outcome]),y$"Hospital.Name"),]
    ## Check Ranking
    if(class(num)=="character")
    {
      if(num=="best") ranking <- 1
      else if(num=="worst") ranking <- nrow(y)
    }
    else
    {
      if(num %in% 1:nrow(y)) ranking <- as.numeric(num)
      else if(as.numeric(num)>nrow(y)) return("NA") # return NA 
      else return("NA") # for negative value
    }  
  return(y$"Hospital.Name"[ranking])
  },num)
  
  
  ## tried to create ranking from data but seems to be having incorrect output
  #st <- transform(X,rank=ave(1:nrow(X),X$State,FUN=function(x)order(as.numeric(X[,outcome])[x])))
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  return(data.frame(hospital=unlist(ans),state=names(ans)))
  
#  return(st[st[,"rank"]==ranking,])
}

