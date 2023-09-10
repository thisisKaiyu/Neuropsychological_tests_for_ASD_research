# library(readxl)
# setwd("~/Documents/wenjuan")
# library(stringr)


# subjid <- 42:45
# wenjuan_SRS <- readxl::read_xlsx("data/SRS.xlsx")
# wenjuan_SRS <- as.data.frame(wenjuan_SRS)
# 
# tmp <- SRS(wenjuan_SRS,subjid)
# View(tmp)
# dim(tmp)


##function
SRS <- function(wenjuan_SRS,subjid) {

reverse_keyed <- c(7,32,45,52,15,17,40,48,12,21,22,26,38,55,3,11,43)
keyed <- setdiff(1:65,reverse_keyed)
length(keyed)

## scores
score_all <- data.frame(matrix(0,1,9))
colnames(score_all) <- c("subj","EvalDate","total_SRS","SCI_SRS","Awareness","Cognition",
                         "Communication","Motivation","RRB")
#score_all

  SRS_final <- as.data.frame(matrix(0,length(subjid),9))

  aa <- 0
  for (subj in subjid) {
    
    aa <- aa + 1
  
  ## subscales
  Awareness <- c(2,25,54,56,7,32,45,52)
  Cognition <- c(5,10,30,42,44,58,59,62,15,17,40,48)
  Communication <- c(13,16,18,19,33,35,36,37,41,46,47,
                     51,53,57,60,61,12,21,22,26,38,55)
  Motivation <- c(1,6,9,23,27,34,64,65,3,11,43)
  RRB <- c(4,8,14,20,24,28,29,31,39,49,50,63)
  
  length(Awareness)
  length(Cognition)
  length(Communication)
  length(Motivation)
  length(RRB)
  
  # Awareness
  k <- intersect(keyed, Awareness)
  m <- intersect(reverse_keyed, Awareness)
  
  Awareness_1 <- 0
  for (i in k) { 
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Awareness_1 <- Awareness_1 + 0
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Awareness_1 <- Awareness_1 + 1
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Awareness_1 <- Awareness_1 + 2
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Awareness_1 <- Awareness_1 + 3
    }
  }
  Awareness_1
  
  Awareness_2 <- 0
  for (i in m) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Awareness_2 <- Awareness_2 + 3
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Awareness_2 <- Awareness_2 + 2
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Awareness_2 <- Awareness_2 + 1
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Awareness_2 <- Awareness_2 + 0
    }
  }
  Awareness_2
  
  score_all$Awareness <- Awareness_1 + Awareness_2
  score_all$Awareness
  
  # Cognition
  k <- intersect(keyed, Cognition)
  m <- intersect(keyed, Cognition)
  
  Cognition_1 <- 0
  
  for (i in k) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Cognition_1 <- Cognition_1 + 0
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Cognition_1 <- Cognition_1 + 1
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Cognition_1 <- Cognition_1 + 2
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Cognition_1 <- Cognition_1 + 3
    }
  }
  Cognition_1
  
  Cognition_2 <- 0
  for (i in m) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Cognition_2 <- Cognition_2 + 3
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Cognition_2 <- Cognition_2 + 2
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Cognition_2 <- Cognition_2 + 1
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Cognition_2 <- Cognition_2 + 0
    }
  }
  Cognition_2
  
  score_all$Cognition <- Cognition_1 + Cognition_2
  
  # Communication
  k <- intersect(keyed, Communication)
  m <- intersect(reverse_keyed, Communication)
  
  Communication_1 <- 0
  for (i in k) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Communication_1 <- Communication_1 + 0
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Communication_1 <- Communication_1 + 1
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Communication_1 <- Communication_1 + 2
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Communication_1 <- Communication_1 + 3
    }
  }
  Communication_1
  
  Communication_2 <- 0
  for (i in m) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Communication_2 <- Communication_2 + 3
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Communication_2 <- Communication_2 + 2
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Communication_2 <- Communication_2 + 1
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Communication_2 <- Communication_2 + 0
    }
  }
  Communication_2
  
  score_all$Communication <- Communication_1 + Communication_2
  
  # Motivation
  k <- intersect(keyed, Motivation)
  m <- intersect(reverse_keyed, Motivation)
  
  Motivation_1 <- 0
  for (i in k) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Motivation_1 <- Motivation_1 + 0
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Motivation_1 <- Motivation_1 + 1
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Motivation_1 <- Motivation_1 + 2
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Motivation_1 <- Motivation_1 + 3
    }
  }
  Motivation_1
  
  Motivation_2 <- 0
  for (i in m) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      Motivation_2 <- Motivation_2 + 3
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      Motivation_2 <- Motivation_2 + 2
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      Motivation_2 <- Motivation_2 + 1
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      Motivation_2 <- Motivation_2 + 0
    }
  }
  Motivation_2
  
  score_all$Motivation <- Motivation_1 + Motivation_2
  
  # Restrict repeated behaviors
  k <- intersect(keyed, RRB)
  
  RRB_1 <- 0
  for (i in k) {
    q <- colnames(wenjuan_SRS)[i+3]
    if (wenjuan_SRS[subj,q] == "没有" ) {
      RRB_1 <- RRB_1 + 0
    }
    else if (wenjuan_SRS[subj,q] == "有时" ) {
      RRB_1 <- RRB_1 + 1
    }
    else if (wenjuan_SRS[subj,q] == "经常" ) {
      RRB_1 <- RRB_1 + 2
    }
    else if (wenjuan_SRS[subj,q] == "总是" ) {
      RRB_1 <- RRB_1 + 3
    }
  }
  RRB_1
  score_all$RRB <- RRB_1
  
  # total scores
  score_all$total_SRS <- score_all$Awareness + score_all$Cognition + score_all$Communication + score_all$Motivation + score_all$RRB
  score_all$SCI_SRS <- score_all$Awareness + score_all$Cognition + score_all$Communication + score_all$Motivation
  
  # add subject name
  ss <- stringi::stri_split_fixed(wenjuan_SRS[subj,3],"|")[[1]][9]
  score_all$subj <- stringi::stri_split_fixed(ss,":")[[1]][2]
  
  # add evaluation date
  mm <- stringi::stri_split_fixed(wenjuan_SRS[subj,3],"|")[[1]][c(4:6)]
  score_all$EvalDate <- str_c(str_split_fixed(mm,":",2)[,2],collapse = "/")
  score_all$EvalDate
  #View(score_all)
  
  tmp <- as.data.frame(score_all)
  
  SRS_final[aa,] <- tmp
  colnames(SRS_final) <- c("subj","EvalDate","total_SRS","SCI_SRS","Awareness","Cognition",
                           "Communication","Motivation","RRB")
  }
return(SRS_final)
}





