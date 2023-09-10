### collecte information from Wenjuan 
setwd("~/Documents/wenjuan")
library(readxl)
library(stringr)

### load function
source("scripts/AQ_adult.R")
source("scripts/SRS.R")
source("scripts/Conner_ASQ.R")
source("scripts/Basic_info.R")


### 1)AQ 

wenjuan_AQ_Adults <- readxl::read_xlsx("data/AQ_Adult.xlsx")
wenjuan_AQ_Adults <- as.data.frame(wenjuan_AQ_Adults)
#View(wenjuan_AQ_Adults)  

subjid <- 74:77
tmp<- AQ_adult(wenjuan_AQ_Adults,subjid)
tmp
#View(AQ_Adult_final)
write.csv(tmp,"AQ_Adult_results.csv")



### 2)SRS

wenjuan_SRS <- readxl::read_xlsx("data/SRS.xlsx")
wenjuan_SRS <- as.data.frame(wenjuan_SRS)

subjid <- 42:45
tmp <- SRS(wenjuan_SRS,subjid)
tmp
#View(tmp)
write.csv(tmp,"SRS_results.csv")



### 3)Conner_ASQ
wenjuan_ASQ <- readxl::read_xlsx("data/Conner_ASQ.xlsx")
wenjuan_ASQ <- as.data.frame(wenjuan_ASQ)
#View(wenjuan_ASQ)

subjid <- 26:28
tmp <- Conner_ASQ(wenjuan_ASQ,subjid)
tmp
#View(tmp)
write.csv(tmp,"Conner_ASQ_results.csv")



### 4)Basic information
basic_info <- readxl::read_xlsx("data/basic_info.xlsx")
basic_info <- as.data.frame(basic_info)
View(basic_info)

subjid <- 51:54
tmp <- Basic_info_final(basic_info,subjid)
tmp
#View(tmp)
write.csv(tmp,"Basic_info_results.csv")



