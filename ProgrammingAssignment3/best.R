best <- function(state, outcome) {
  # state = the 2-character abbreviated name 
  # outcome = outcome name
  ## Read outcome data from outcome-of-care-measures.csv
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=F,colClasses = "character",na.strings="N/A")

##  data[,11] <- as.numeric(data[,11]) #Heart Attack
##  data[,17] <- as.numeric(data[,17]) #Heart Failure
##  data[,23] <- as.numeric(data[,23]) #Pneumonia  
  
  output <- NULL #initiate output to null
  
  ## Check that state and outcome are valid
  if (!(state %in% unique(data[,7])))
  {
    stop("invalid state")  
  }
  
  if (!(outcome %in% c('heart attack','heart failure','pneumonia')))
  {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate

     hosp <- split(data,data$State)[[state]]
     {
       if(outcome=="heart attack")
       {
        # output <- sort(hosp[complete.cases(hosp[,11]),],"Hospital.Name")

         output <- sort(hosp[hosp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(as.numeric(hosp[,11]),na.rm=T),"Hospital.Name"])[1]
       }
       else if(outcome=="heart failure")
       {
         output <- sort(hosp[hosp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(as.numeric(hosp[,17]),na.rm=T),"Hospital.Name"])[1]
       }
       else if(outcome=="pneumonia")
       {
         output <- sort(hosp[hosp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(as.numeric(hosp[,23]),na.rm=T),"Hospital.Name"])[1]
       }
       else
       {
         stop("invalid outcome") #incase not using above check
       }
     }
return(output) 
  
}

best2 <- function(state, outcome) {

  x <- read.csv("outcome-of-care-measures.csv" , header =TRUE, stringsAsFactors=F,na.strings="Not Available")          
  outcomevector <- c("heart attack" , "heart failure" , "pneumonia")         
  if(!(state %in% unique(x$State))) stop("Invalid State")
  if(!(outcome %in% outcomevector)) stop("Invalid Outcome")
  
  X <- x[x$State== state,]
  names(X)[c(11, 17, 23)] <- outcomevector
  answer <- X[as.numeric(X[,outcome]) == min(as.numeric(X[,outcome])),][2]   
  FA <- answer[with(answer, order(Hospital.Name)), ]
  FA[1] 
  return(answer)
}

