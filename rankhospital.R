#  Ranking hospitals by outcome in a state

#> source("rankhospital.R") 
#  > rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"

#> rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"




rankhospital <- function(state,outcome,num)
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
    if (num=="best")
    num <-1
    if (num=="worst")
      num <- nrow(newsplit[[state]])
      
#unlist(lapply(newsplit[[state]][1], function(data) data(num)))
    
     
  }
  else
  {
    stop('Invalid Outcome')
  }
  
newsplit[[state]][num,1]
  
}