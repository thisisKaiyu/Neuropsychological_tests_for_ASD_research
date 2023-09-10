## run function
library(readxl)
setwd("~/Documents/wenjuan")
library(stringr)
library(ggplot2)
library(grDevices)
# AQ adult
wenjuan_raw <- readxl::read_xlsx("data/AQ_Adult.xlsx")
wenjuan_raw <- as.data.frame(wenjuan_raw)
View(wenjuan_raw)
subj <- 27

# 20, 23
wenjuan_raw$Q1_请填写以下个人信息[c(17:26)]  

assessment <- "AQ_Adult"
AQ_adult(wenjuan_raw,subj,assessment)

# AQ child

wenjuan_child <- readxl::read_xlsx("data/AQ_Child.xlsx")
wenjuan_child <- as.data.frame(wenjuan_child)
source("scripts/AQ_Child.R")
View(wenjuan_child)
assessment <- "AQ_Child"

# subj <- 2
# AQ_child(wenjuan_child,subj,assessment)

AQ_Child_scores <- data.frame(matrix(0,4,7))
for (subj in 1:4) {
  AQ_Child_scores[subj,] <- AQ_Child(wenjuan_child,subj,assessment)
}

colnames(AQ_Child_scores) <- c("subj","total","Socialness","Social_communicative","Imagination","Patterns","Attention_switching")
View(AQ_Child_scores)


# SRS
wenjuan_SRS <- readxl::read_xlsx("data/SRS.xlsx")
wenjuan_SRS <- as.data.frame(wenjuan_SRS)

source("scripts/SRS.R")
View(wenjuan_SRS)
assessment <- "SRS"

subj <- 1

SRS_scores <- data.frame(matrix(0,7,8))
# c(1,3,4)
for (subj in 8:9) {
  SRS_scores[subj,] <- SRS(wenjuan_SRS,subj,assessment)
}

colnames(SRS_scores) <- c("subj","total_SRS","SCI_SRS","Awareness","Cognition",
                         "Communication","Motivation","RRB")
View(SRS_scores)

stringi::stri_split_fixed(wenjuan_SRS$IP城市[subj],"|")

wenjuan_SRS$IP城市

# Conner_ASQ 
wenjuan_ASQ <- readxl::read_xlsx("data/Conner_ASQ.xlsx")
wenjuan_ASQ <- as.data.frame(wenjuan_ASQ)

source("scripts/Conner_ASQ.R")
View(wenjuan_ASQ)
assessment <- "Conner_ASQ"

# subj <- 1

Conner_ASQ_scores <- data.frame(matrix(0,2,7))
# c(1,3,4)
for (subj in 11:12) {
  Conner_ASQ_scores[subj,] <- Conner_ASQ(wenjuan_ASQ,subj,assessment)
}
colnames(Conner_ASQ_scores) <- c("subj","total_ASQ")
View(Conner_ASQ_scores)

stringi::stri_split_fixed(wenjuan_ASQ$IP城市[subj],"|")

wenjuan_ASQ$IP城市