library(readxl)
setwd("~/Documents/wenjuan")
library(stringr)

wenjuan_CAST <- readxl::read_xlsx("data/儿童自闭症类群测验（CAST）_数据详情表_原始数据_202112221642.xlsx")
wenjuan_CAST <- as.data.frame(wenjuan_CAST)
View(wenjuan_CAST)


# CAST scores
score_CAST <- data.frame(matrix(0,1,10))
colnames(score_CAST) <- c("subj","total_CAST","Q38","Q39","Q40","date of birth","gender","EvalDate","age","name_caregiver")
score_CAST


k <- c(1,2,5:11,13:21,23:25,27:32,34:37)# The 6 control items (not scored) are items 3, 4, 12, 22, 26, and 33. Q38-40 were special need.
total_CAST <- 0
subj <- 2

for (i in k) {
  q <- colnames(wenjuan_CAST)[i+3]
  if (wenjuan_CAST[subj,q] == "否" ) {
    total_CAST <- total_CAST + 0
  }
  else if (wenjuan_CAST[subj,q] == "是" ) {
    total_CAST <- total_CAST + 1
  }
}
total_CAST


score_CAST$total_CAST <- total_CAST

# add subject name
pp <- stringi::stri_split_fixed(wenjuan_CAST[subj,3],"|")[[1]][9]
score_CAST$subj <- stringi::stri_split_fixed(pp,":")[[1]][2]
score_CAST$subj

# add birthday
qq <- stringi::stri_split_fixed(wenjuan_CAST[subj,3],"|")[[1]][c(1:3)]
#score_CAST$birthday <- stringi::stri_split_fixed(qq,":")[[1]][2]
score_CAST$"date of birth"  <- str_c(str_split_fixed(qq,":",2)[,2],collapse = "/")
score_CAST$"date of birth"

# add gender
gender <- stringi::stri_split_fixed(wenjuan_CAST[subj,3],"|")[[1]][10]
score_CAST$gender <- str_c(str_split_fixed(gender,":",2)[,2])
score_CAST$gender


# add evaluation date
mm <- stringi::stri_split_fixed(wenjuan_CAST[subj,3],"|")[[1]][c(4:6)]
score_CAST$EvalDate <- str_c(str_split_fixed(mm,":",2)[,2],collapse = "/")
score_CAST$EvalDate

# add age year/month
age <- stringi::stri_split_fixed(wenjuan_CAST[subj,3],"|")[[1]][c(7:8)]
score_CAST$age <- str_c(str_split_fixed(age,":",2)[,2],collapse = "/")
score_CAST$age

# add name of caregiver
namecg <- stringi::stri_split_fixed(wenjuan_CAST[subj,3],"|")[[1]][12]
score_CAST$name_caregiver <- str_c(str_split_fixed(namecg,":",2)[,2])
score_CAST$name_caregiver

# Q38-Q40
score_CAST$Q38 <- wenjuan_CAST[subj,41]
score_CAST$Q39 <- wenjuan_CAST[subj,42]
score_CAST$Q40 <- wenjuan_CAST[subj,43]

View(score_CAST)



