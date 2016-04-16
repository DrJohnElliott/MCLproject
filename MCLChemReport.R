
library(dplyr)
setwd("C:/Users/John/Desktop/R-Code/Projects/MCL Reports Chromalloy")



myData <- read.csv("BMS1008_IMR_Chem_Main_201600351.txt", sep = "/", header = FALSE, fileEncoding="UTF-8-BOM")
#unique(Encoding(A)) # will most likely be UTF-8

substring="CHEMISTRY|^1"
Chemistry_filter <-  grepl(substring,myData)
chem_rows <- which(Chemistry_filter)
myNum <- as.integer(chem_rows[2]-chem_rows[1])
 
my_chem <- readLines(A, skip = chem_rows[1] ,  )
final_names1 <- my_names[Chemistry_filter]


final_names <- (c( "Activity", "Subject", final_names1) )
