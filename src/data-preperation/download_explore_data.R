# Some initial Bookkeeping: Clean the workspace
rm(list=ls(all=TRUE)) 

# (Install and) load required packages
library(utils)

# Directory where this code is saved in working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#setwd("../../")


# --- Download data --- # 


# Read data
data82total<-read.csv("data/data_scenario_82.csv")

# Start sending analysis output to text-file with "name.txt" and to screen (split = TRUE)
sink("gen/temp/temp_output.txt") 

# Start sending plotting output to pdf file with "name.pdf"
pdf("gen/temp/temp_output.pdf")



write.csv(data82total,"data/rawdata.csv")




