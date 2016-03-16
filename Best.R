#Write a function called best that take two arguments: the 2-character abbreviated 
#name of a state and an outcome name. The function reads the 
#outcome-of-care-measures.csv file and returns a character vector 
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality 
#for the specified outcome in that state. The hospital name is the name provided 
#in the Hospital.Name variable. The outcomes can be one of "heart attack", 
#"heart failure", or "pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
#Handling ties. If there is a tie for the best hospital for a given outcome, 
#then the hospital names shouldbe sorted in alphabetical order and the 
#first hospital in that set should be chosen (i.e. if hospitals "b", "c",
#and "f" are tied for best, then hospital "b" should be returned)


# Output Instance
#> source("Best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"

#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : Invalid State

#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : Invalid Outcome




best <- function(state,outcome)
{
  health <- read.csv("outcome-of-care-measures.csv", na.strings= "Not Available",
                      stringsAsFactors=FALSE)
  
  if(!(state %in% health$State))
  {
     stop('Invalid State')
  }
  
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  
  if (outcome %in% names(outcomes))
   {
     outcomeDf = health[,c(2,7,outcomes[outcome])] # this works
     outcomeDf <- na.omit(outcomeDf)
     names(outcomeDf) <- c("hospital", "state", "outcome")
     
    outcomeDf<-outcomeDf[order(outcomeDf$state, outcomeDf$outcome,outcomeDf$hospital),]
    newsplit<- split(outcomeDf,outcomeDf$state)
   }
  else
  {
    stop('Invalid Outcome')
  }

  newsplit[[state]][1,1]
  
}