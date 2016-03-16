#  function that reads a directory full of files and reports 
# the number of completely observed cases in each data file. 
# The function  returns a data frame where the first column 
# is the name of the file and the second column is the number of 
# complete cases. 


# A Output For the Program


#> Complete("specdata",12:15)
#id nobs
#1 12   96
#2 13   46
#3 14   96
#4 15   83
#> 


Complete <- function (directory,ID)
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
  
  transform <- function (nos)
  {
    if (nos %in% (1:9))
      nos <- paste("00",nos,sep="")
    else if (nos %in% (10:99))
    {
      nos <- paste("0",nos,sep="")
    }
    
  }    
  
  
  for (i in ID )
  {
    Tid <-transform(i)
    DF1 <- read.csv(paste(get("Tid"),".csv",sep=""),header=TRUE)
    comp <-complete.cases(DF1)
    obs <- nrow(DF1[comp,])
     
    if (!exists("newDF"))
{
        newDF <- data.frame(id= get("i"),nobs= get("obs"))
    }

     else
     {
       newDF <- rbind (newDF,c(get("i"),get("obs")))
     }
   

  } 
  
  newDF
}
