#Ranking hospitals in all states

#> head(rankall("heart attack", 20), 10)

                              #hospital state
#AK                                <NA>    AK
#AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
#AR   ARKANSAS METHODIST MEDICAL CENTER    AR
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
#CA               SHERMAN OAKS HOSPITAL    CA
#CO            SKY RIDGE MEDICAL CENTER    CO
#CT             MIDSTATE MEDICAL CENTER    CT
#DC                                <NA>    DC
#DE                                <NA>    DE
#FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL


#> tail(rankall("pneumonia", "worst"), 3)
#hospital state
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
#WV                     PLATEAU MEDICAL CENTER    WV
#WY           NORTH BIG HORN HOSPITAL DISTRICT    WY



rankall <- function(outcome,num = "best")
{
  
  health <- read.csv("outcome-of-care-measures.csv", na.strings= "Not Available",
                     stringsAsFactors=FALSE)
  
 
  
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  if (outcome %in% names(outcomes))
  {
    outcomeDf = health[,c(2,7,outcomes[outcome])] # this works
    outcomeDf <- na.omit(outcomeDf)
    names(outcomeDf) <- c("hospital", "state", "outcome")
    if (num=="worst")
    {
    outcomeDf<-outcomeDf[order(outcomeDf$state, -outcomeDf$outcome,outcomeDf$hospital),]
    }
    else
    {
    outcomeDf<-outcomeDf[order(outcomeDf$state, outcomeDf$outcome,outcomeDf$hospital),]
    }
    newsplit<- split(outcomeDf,outcomeDf$state)
    if (num=="best" || num=="worst")
      num <-1
    # num <- nrow(newsplit[[state]])
    newDF <- sapply(newsplit, function(data) data[num,1])
    newDF <- data.frame(hospital=newDF,state=names(newDF),row.names=names(newDF))
    
    #if (num=="best")
    #  num <-1
    #if (num=="worst")
    #  num <- nrow(newsplit[[state]])
    
    #unlist(lapply(newsplit[[state]][1], function(data) data(num)))
    
    
  }
  else
  {
    stop('Invalid Outcome')
  }
  
  newDF
  
}