
library(dplyr)


setwd("C:/Users/John/Desktop/R-Code/Projects/MCL Reports Chromalloy")

#DATA Vars
        labNames <- c("IMR", "NSL")
# Read Text File into single varible table each line is a charecter string
        myData <- read.csv("BMS1008_IMR_Chem_Main_201600351.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM",colClasses = "character")
#Find Lab Name
        myLab <- word(myData$V1[1],1)
        labNames %in%  myLab
        is.element(labNames,myLab)
        
                rm("labNames")
        
#Find Lab Report Information from IMR
        
#Sub strings to search for         
        sub_1 = "PO Number"
        sub_2 = "Alloy or Product"
        sub_3 = "Heat orBatch No."
        sub_4 = "CHEMISTRY|^1"
        
# Get Job data    
        myPO <- grep(sub_1,myData$V1)
        my_PO <- myData[c(myPO+1),]
        myAlloy <- grep(sub_2,myData$V1)
        my_Alloy <- myData[c(myAlloy+1),]
        myHeat <- grep(sub_3,myData$V1)
        my_Heat <- myData[c(myHeat+1),]
        
                rm("sub_1","sub_2","sub_3","myPO","myAlloy","myHeat")
#get Chemistry      
        Chem_start_stop <-  grep(sub_4,myData$V1)
        my_chem <- myData[c(Chem_start_stop[1]+2):c(Chem_start_stop[2]-1),]
        my_Chem <- strsplit(my_chem, split=" +")
        a <- as.data.frame(my_chem)

                rm("sub_4","Chem_start_stop")
 
