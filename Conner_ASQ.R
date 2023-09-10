# ### collecte information from Wenjuan 
# setwd("~/Documents/wenjuan")
# library(readxl)
# library(stringr)
# 
# subjid <- 26:28
# wenjuan_ASQ <- readxl::read_xlsx("data/Conner_ASQ.xlsx")
# wenjuan_ASQ <- as.data.frame(wenjuan_ASQ)
# #View(wenjuan_ASQ)
# tmp <- Conner_ASQ(wenjuan_ASQ,subjid)




## function
Conner_ASQ <- function(wenjuan_ASQ,subjid) {
  

# scores
score_all <- data.frame(matrix(0,1,7))
colnames(score_all) <- c("subj","EvalDate","total_ASQ","date of birth","gender","age","name_caregiver")
score_all


Conner_ASQ_final <- as.data.frame(matrix(0,length(subjid),7))

aa <- 0
for (subj in subjid) {
  
  aa <- aa + 1


k <- c(1:10)
total_ASQ <- 0

for (i in k) {
  q <- colnames(wenjuan_ASQ)[i+3]
  if (wenjuan_ASQ[subj,q] == "无此表现" ) {
    total_ASQ <- total_ASQ + 0
  }
  else if (wenjuan_ASQ[subj,q] == "偶尔有点" ) {
    total_ASQ <- total_ASQ + 1
  }
  else if (wenjuan_ASQ[subj,q] == "常有" ) {
    total_ASQ <- total_ASQ + 2
  }
  else if (wenjuan_ASQ[subj,q] == "十分常见" ) {
    total_ASQ <- total_ASQ + 3
  }
}
total_ASQ

score_all$total_ASQ <- total_ASQ

# add subject name
pp <- stringi::stri_split_fixed(wenjuan_ASQ[subj,3],"|")[[1]][9]
score_all$subj <- stringi::stri_split_fixed(pp,":")[[1]][2]
score_all$subj

# add birthday
qq <- stringi::stri_split_fixed(wenjuan_ASQ[subj,3],"|")[[1]][c(1:3)]
#score_all$birthday <- stringi::stri_split_fixed(qq,":")[[1]][2]
score_all$"date of birth"  <- str_c(str_split_fixed(qq,":",2)[,2],collapse = "/")
score_all$"date of birth"

# add gender
gender <- stringi::stri_split_fixed(wenjuan_ASQ[subj,3],"|")[[1]][10]
score_all$gender <- str_c(str_split_fixed(gender,":",2)[,2])
score_all$gender


# add evaluation date
mm <- stringi::stri_split_fixed(wenjuan_ASQ[subj,3],"|")[[1]][c(4:6)]
score_all$EvalDate <- str_c(str_split_fixed(mm,":",2)[,2],collapse = "/")
score_all$EvalDate

# add age year/month
age <- stringi::stri_split_fixed(wenjuan_ASQ[subj,3],"|")[[1]][c(7:8)]
score_all$age <- str_c(str_split_fixed(age,":",2)[,2],collapse = "/")
score_all$age

# add name of caregiver
namecg <- stringi::stri_split_fixed(wenjuan_ASQ[subj,3],"|")[[1]][12]
score_all$name_caregiver <- str_c(str_split_fixed(namecg,":",2)[,2])
score_all$name_caregiver

#View(score_all)

tmp <- as.data.frame(score_all)

Conner_ASQ_final[aa,] <- tmp
colnames(Conner_ASQ_final) <- c("subj","EvalDate","total_ASQ","date of birth",
                                "gender","age","name_caregiver")

}
return(Conner_ASQ_final)
}




