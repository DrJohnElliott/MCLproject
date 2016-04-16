
library(dplyr)
setwd("C:/Users/John/Desktop/R-Code/Projects/MCL Reports Chromalloy")

reportdata <- read.table("BMS1008_IMR_Chem_Main_201600351.txt",  header = FALSE)

A <- readLines(con <- file("BMS1008_IMR_Chem_Main_201600351.txt", encoding = "UTF-8"),skipNul = TRUE)
close(con)

myData <- read.csv("BMS1008_IMR_Chem_Main_201600351.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM")
#unique(Encoding(A)) # will most likely be UTF-8
myChem <- read.table("BMS1008_IMR_Chem_Main_201600351.csv", sep = ",", header = TRUE)
myMech<- read.table("BMS1008_Mech_IMR_201603099Chormally.csv" ,sep = ",", header = TRUE)
#Make List of Column Names with Activity, Subject, Mean and Standard Deveation Data 
#FindList <- names(my_data)
substring="CHEMISTRY|^1"
Chemistry_filter <-  grepl(substring,A)
chem_rows <- which(Chemistry_filter)
myNum <- as.integer(chem_rows[2]-chem_rows[1])
 
my_chem <- readLines(A, skip = chem_rows[1] ,  )
final_names1 <- my_names[Chemistry_filter]


final_names <- (c( "Activity", "Subject", final_names1) )
