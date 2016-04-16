
library(dplyr)


setwd("C:/Users/John/Desktop/R-Code/Projects/MCL Reports Chromalloy")

#DATA Vars
        labNames <- c("IMR", "NSL")
# Read Text File into single varible table each line is a charecter string
        myData <- read.csv("BMS1008_IMR_Chem_Main_201600351.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM",colClasses = "character")
#Find Lab Name
        myLab <- myData$V1[1]
        str_match(myLab,labNames)
        labNames %in%  myLab
        
#Determine Lab
        
        substring1="CHEMISTRY|^1"
        Chem_start_stop <-  grep(substring1,myData$V1)
        my_chem <- myData[c(Chem_start_stop[1]+2):c(Chem_start_stop[2]-1),]

 
