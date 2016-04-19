library(stringr)
library(dplyr)
library(qdap)

setwd("C:/Users/John/Desktop/R-Code/Projects/MCL Reports Chromalloy/MCLproject")


# Read Text File into single varible table each line is a charecter string
       myData <- read.csv("BMS1008_IMR_Chem_Main_201600351.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM",colClasses = "character")
        #myData <- read.csv("X00284_6119_BMS1008_Ingot.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM",colClasses = "character")
        myData$V1<- str_trim(clean(myData$V1))  # trim white space from main data set
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
                               rm("labNames","which_lab")
        
#Find Lab Report Information from IMR
        
#Sub strings to search for         
        sub_1 = c("PO Number","Alloy or Product","Heat orBatch No.","CHEMISTRY|^1","\\(Cb\\)")
        sub_2 = c("PO Number:","Client Description:", "Sample No.:", "Units Method" )
        sub_3 = ""
        #sub_4 = ""
        #sub_5 = ""
        
        if(is.element(myLab,"IMR")){ 
                my_sub<- sub_1  
        }else if(is.element(myLab,"NSL")){ 
                my_sub<- sub_2  
                }
               
# Get Job data 
which_job <- function(){
        
        myPO <- grep(my_sub[1],myData$V1)
        myAlloy <- grep(my_sub[2],myData$V1)
        myHeat <- grep(my_sub[3],myData$V1)
        
        
        if(is.element(myLab,"IMR")){ 
                my_PO  <- myData[c(myPO+1),] 
                my_Alloy <- myData[c(myAlloy+1),]
                my_Heat <- myData[c(myHeat+1),]
                
        }else if(is.element(myLab,"NSL")){ 
                my_PO <- word(myData$V1[myPO[1]],3) 
                my_Alloy <- word(myData$V1[myAlloy[1]],6)
                my_Heat <- word(myData$V1[myHeat[1]],12)
        }
        my_job_data <- c(my_PO, my_Alloy,my_Heat )
         return(my_job_data)
}        

job_data <- which_job()

my_chem <- ""

which_chem <- function(){

        if(is.element(myLab,"IMR")){
#get Chemistry 
                trim_1 <-  grep(my_sub[5],myData$V1)     # find location of two chem names for Nb and Cb
                
                myData$V1<- str_trim(clean(myData$V1))  # trim white space from main data set

# Trim data strings        
                myData$V1[trim_1] <- str_replace_all(myData$V1[trim_1], sub_1[5], "") 
                myData$V1 <- str_replace_all(myData$V1, " -", "")

#find chemistry data in main data set and remove white space
                Chem_start_stop <-  grep(my_sub[4],myData$V1)
                my_chem <- myData[c(Chem_start_stop[1]+2):c(Chem_start_stop[2]-1),]
                my_chem<- str_trim(clean(my_chem))
        return(my_chem)
        
        }
         else if(is.element(myLab,"NSL")){
                  print("something")
                
        }
}     

my_chem <- which_chem()


clean_Data <- function(){
if(is.element(myLab,"IMR")){
        Chem_start_stop <-  grep(my_sub[4],myData$V1)
        pad_1  <-  grep("Balance",my_chem)              # Find row with "Balance"
        trim_2 <-  grep("-",myData$V1 )        # find location of " -" in values
# add NA as place holder         
        my_chem[pad_1]<-paste(my_chem[pad_1],"NA",sep= " ")

# pull measured values chemistry from main data set       
        chem_list <- word(my_chem,1)                    #Get var names
        chem_val <- word(my_chem,2)                     #Get measured value
        
        
        chem_min <-word(my_chem,3)                      # get minimum spec
        chem_max <-word(my_chem,4)                      # Get maximum spec
               
        #trim_test<- (trim_2 >= (Chem_start_stop[1] + 2))
        
        #trim_3 <- (trim_2[trim_test])-(Chem_start_stop[1] + 1)       # 
        #chem_max[trim_3] <-word(my_chem[trim_3],5)      # Update chem_max rows that were missing values
                
# Transfer values from chem_min to chem_max for Maximum only spec                
        trim_4 <- grep("Maximum", chem_max)             # Find rows where maximum only was specified
        chem_max[trim_4] <- chem_min[trim_4]            # Transfer value that was in chem_min to chem_max
        chem_min[trim_4] <- "NA"                        # Replace entries in chem_min with NA
   
        
        BDL <-  grep("<",chem_val)                      #Find below measurable limits
        chem_val <- str_replace_all(chem_val, "<", "")  #Remove "<" from data values
        chem_val <- as.numeric(chem_val)                # make numeric
        chem_min<- as.numeric(chem_min)                 # make numeric
        chem_max<- as.numeric(chem_max)                 # make numeric
        ND <- character(length(chem_val))
        ND[BDL]<- "BDL"
}
        # Make table         
        my_chem_table <- cbind.data.frame(chem_list, chem_val, chem_min,chem_max, ND)
        return(my_chem_table)
}        
        
my_data_table <- clean_Data()

        rm("my_sub", "my_chem")
        rm("sub_1","sub_2","sub_3")
        
# Make List Entry
        Elements <- c("Ag", "Al", "As","Au","B", "Bi","C", "Cb (Nb)","Cd","Co","Cr", "Cu","Fe","Ga","Ge","Hf","Hg","In","K","Mg","Mn","Mo","N","Na","Ni", "Nv3B", "O","P","Pb","Re","S","Sb","Se","Si","Sn", "Ta","Te","Th", "Ti","Tl", "U", "W", "Zn", "Zr")
        col_names <- c("Alloy", "Heat", "PO", "Lab", "Test Date", "Report", Elements, "Values","Min", "Max"  )
#Logic Testing  
        
 check_chem <- function(){
         
      ans<-""
        if(FALSE %in% (my_data_table$chem_val <= my_data_table$chem_max)){
                ans<- "Fails Maximum Chemistry"
        }else{
                ans<-"Passes Maximum Chemistry"
        }
        
        if(FALSE %in% (my_data_table$chem_val >= my_data_table$chem_min)){
                ans<-c(ans,"Fails Minimum Chemistry")
        }else{
                ans<-c(ans,"Passes Minimum Chemistry")
        }
return(ans)
 }
   my_check <- check_chem()    
        my_data_table$chem_val <= my_data_table$chem_max
        my_data_table$chem_val >= my_data_table$chem_min
        