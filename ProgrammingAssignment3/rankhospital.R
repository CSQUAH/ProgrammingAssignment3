rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv" , header =TRUE, stringsAsFactors=F, na.strings="Not Available") 
  #data <- read.csv("outcome-of-care-measures.csv" ,colClasses = "character")
  ## Check that state and outcome are valid
  outcomeV <- c("heart attack","heart failure","pneumonia")
  
  if(!(state %in% unique(data$State))) stop("invalid state")
  if(!(outcome %in% outcomeV)) stop("invalid outcome")   
  
  ## Return hospital name in that state with the given rank
  ## get subset for state

  X <- data[data$State==state,]    # set subset for state
  names(X)[c(11,17,23)]<- outcomeV # set names for column to follow outcome
  st <- X[complete.cases(X[,outcome]),c("Hospital.Name","State",outcome)]# set subset for mortality outcome
  st$rank[order(as.numeric(st[,outcome]),st[,"State"],st[,"Hospital.Name"])]<-1:nrow(st) # add ranking based on outcome
  
  ## check on num inserted
  
  if(class(num)=="character")
  {
    if(num=="best") ranking <- 1
    else if(num=="worst") ranking <- nrow(st)
    else stop("invalid num")
  }
  else
  {
    if(num %in% 1:nrow(st)) ranking <- as.numeric(num)
    else if(as.numeric(num)>nrow(st)) return("NA") # return NA 
  }
     
     
  ## 30-day death rate
  return(st[st[,"rank"]==ranking,1])
}