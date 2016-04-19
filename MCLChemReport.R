library(stringr)
library(dplyr)
library(qdap)

setwd("C:/Users/John/Desktop/R-Code/Projects/MCL Reports Chromalloy/MCLproject")


# Read Text File into single varible table each line is a charecter string
       myData <- read.csv("BMS1008_IMR_Chem_Main_201600351.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM",colClasses = "character")
        #myData <- read.csv("X00284_6119_BMS1008_Ingot.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM",colClasses = "character")
         
#Find Lab Name
        myLab <- ""
        labNames <- c("IMR", "NSL")    
                
        which_lab <- function( ){
                for( i in 1:length(labNames)){
                                myLab <-  grep(paste("^",labNames, sep="")[i],myData$V1, value = TRUE)
                                myLab <- (word(myLab[1],1))
                                if(is.element(myLab,labNames)){break}
                }
                return(word(myLab[1]))
        }
                
        myLab <- which_lab()
        
        
                
                        
        
             #   rm("labNames","theName")
        
#Find Lab Report Information from IMR
        
#Sub strings to search for         
        sub_1 = "PO Number"
        sub_2 = "Alloy or Product"
        sub_3 = "Heat orBatch No."
        sub_4 = "CHEMISTRY|^1"
        sub_5 = "\\(Cb\\)"
        
# Get Job data    
        myPO <- grep(sub_1,myData$V1)
        my_PO <- myData[c(myPO+1),]
        myAlloy <- grep(sub_2,myData$V1)
        my_Alloy <- myData[c(myAlloy+1),]
        myHeat <- grep(sub_3,myData$V1)
        my_Heat <- myData[c(myHeat+1),]
        
                rm("sub_1","sub_2","sub_3","myPO","myAlloy","myHeat")
#get Chemistry 
        trim_1 <-  grep(sub_5,myData$V1)        # find location of two chem names for Nb and Cb
        trim_2 <-  grep(" –",myData$V1)         # find location of " -" in values
        myData$V1<- str_trim(clean(myData$V1))  # trim white space from main data set

# Trim data strings        
        myData$V1[trim_1] <- str_replace_all(myData$V1[trim_1], sub_5, "") 
        myData$V1 <- str_replace_all(myData$V1, " -", "")

#find chemistry data in main data set and remove white space
        Chem_start_stop <-  grep(sub_4,myData$V1)
        my_chem <- myData[c(Chem_start_stop[1]+2):c(Chem_start_stop[2]-1),]
        my_chem<- str_trim(clean(my_chem))
      
        pad_1  <-  grep("Balance",my_chem)      # Find row with "Balance"
       # trim_4 <- grep("Maximum", chem_max)     # Find rows with "Maximum"
# add NA as place holder         
        my_chem[pad_1]<-paste(my_chem[pad_1],"NA",sep= " ")

# pull measured values chemistry from main data set       
        chem_list <- word(my_chem,1)                    #Get var names
        chem_val <- word(my_chem,2)                     #Get measured value
        
        
        chem_min <-word(my_chem,3)                      # get minimum spec
        chem_max <-word(my_chem,4)                      # Get maximum spec
               
 
        trim_3 <- trim_2-(Chem_start_stop[1] + 1)       # Find rows that had "-"
        chem_max[trim_3] <-word(my_chem[trim_3],5)      # Update chem_max rows that were missing values
                
# Transfer values from chem_min to chem_max for Maximum only spec                
        trim_4 <- grep("Maximum", chem_max)             # Find rows where maximum only was specified
        chem_max[trim_4] <- chem_min[trim_4]            # Transfer value that was in chem_min to chem_max
        chem_min[trim_4] <- "NA"                        # Replace entries in chem_min with NA
   
        BDL <-  grep("<",chem_val)                      #Find below measurable limits
        chem_val <- str_replace_all(chem_val, "<", "")  #Remove "<" from data values
        chem_val <- as.numeric(chem_val)                # make numeric
        chem_min<- as.numeric(chem_min)                 # make numeric
        chem_max<- as.numeric(chem_max)                 # make numeric
             
        rm("trim_1","trim_3","trim_2","trim_4", "my_chem")
        rm("pad_1","sub_4","Chem_start_stop","sub_5")
# Make table         
        my_chem_table <- cbind.data.frame(chem_list, chem_val, chem_min,chem_max)
        
# Make List Entry
        Elements <- c("Ag", "Al", "As","Au","B", "Bi","C", "Cb (Nb)","Cd","Co","Cr", "Cu","Fe","Ga","Ge","Hf","Hg","In","K","Mg","Mn","Mo","N","Na","Ni", "Nv3B", "O","P","Pb","Re","S","Sb","Se","Si","Sn", "Ta","Te","Th", "Ti","Tl", "U", "W", "Zn", "Zr")
        col_names <- c("Alloy", "Heat", "PO", "Lab", "Test Date", "Report", Elements, "Values","Min", "Max"  )
#Logic Testing        
        my_chem_table$chem_val <= my_chem_table$chem_max
        my_chem_table$chem_val >= my_chem_table$chem_min
        