#The function that takes a directory of data files and a threshold 
#for complete cases and calculates the correlation between sulfate 
#and nitrate for monitor locations where the number of completely
#observed cases (on all variables) is greater than the threshold. 
#The function returns  a vector of correlations for the monitors 
#that meet the threshold requirement. If no monitors meet the 
#threshold requirement, then the function  returns a 
#numeric vector of length 0. 


#> cr <- Corr("specdata", 150)
#> head(cr)
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#> summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310 


Corr <- function(directory,threshold=0)
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
   flag <- 0
   newdf <- vector(mode="integer", length=0)
  for (i in 1:332)
  {
    Tid <-transform(i)
    DF1 <- read.csv(paste(get("Tid"),".csv",sep=""),header=TRUE)
    comp <-complete.cases(DF1)
    if (nrow(DF1[comp,]) > threshold)
{    

    flag <- 1   
   newdf <- append(newdf ,cor(DF1[comp,][,2],DF1[comp,] [,3]))
    
}
  
}   
 
  newdf
} 