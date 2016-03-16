# function named 'pollutantmean' that 
#calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. The function 'pollutantmean' 
#takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' 
#reads that monitors' particulate matter data from the 
#directory specified in the 'directory' argument and returns 
#the mean of the pollutant across all of the monitors, ignoring 
#any missing values coded as NA.

#  Output for below code
#> pollutionmean("specdata","nitrate",1:25)
#[1] 1.403551




pollutionmean <- function (directory,pollutant,ID=1:332)
   {
  
  transform <- function (nos)
  {
    if (nos %in% (1:9))
      nos <- paste("00",nos,sep="")
    else if (nos %in% (10:99))
    {
      nos <- paste("0",nos,sep="")
    }
    nos 
  } 
  
     path <-paste("C:/Users/Avadhoot/Documents/Course_era/",directory,sep="")
     setwd(path)
         for (i in ID )
         {
           Tid <-transform(i)
            if (!exists("DF1"))
            DF1 <- read.csv(paste(get("Tid"),".csv",sep=""),header=TRUE)
            else
                {
                 temp_DF1 <-read.csv(paste(get("Tid"),".csv",sep=""))
                 DF1<-rbind(DF1, temp_DF1)
                 rm(temp_DF1)
                 }
         }
     
    # DF1 <- na.omit(DF1)
     
     if (pollutant == "nitrate")
     {
        Means<- mean(DF1[,3],na.rm = TRUE)
     }
     else if (pollutant=="sulfate")
     {
       Means<- mean(DF1[,2],na.rm = TRUE)
     }
         
              
        Means
  }
 