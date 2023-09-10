## function 
Basic_info_final <- function(basic_info,subjid) {

#View(basic_info)

  #all info in Q1
  Q1 <- data.frame(matrix(0,1,9))
  colnames(Q1) <- c("subj","出生日期","填写日期","儿童年龄(y/m)","儿童性别","孕龄(w)","填写人姓名",
                    "填写人与孩子关系","联系电话")
  #View(Q1)
  
  info_final <- as.data.frame(matrix(0,length(subjid),57))
  
  i <- 0
  for (subj in subjid) {
    
    i <- i + 1
  # add subject name
  pp <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][9]
  #Q1$subj <- stringi::stri_split_fixed(pp,":")[[1]][2]
  Q1$subj <- str_c(str_split_fixed(pp,":",2)[,2])
  Q1$subj
  
  # add birthday
  qq <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][c(1:3)]
  #Q1$birthday <- stringi::stri_split_fixed(qq,":")[[1]][2]
  Q1$出生日期  <- str_c(str_split_fixed(qq,":",2)[,2],collapse = "/")
  Q1$出生日期
  
  # add evaluation date
  mm <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][c(4:6)]
  Q1$填写日期 <- str_c(str_split_fixed(mm,":",2)[,2],collapse = "/")
  Q1$填写日期
  
  # add age year/month
  age <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][c(7:8)]
  Q1$`儿童年龄(y/m)` <- str_c(str_split_fixed(age,":",2)[,2],collapse = "/")
  Q1$`儿童年龄(y/m)`
  
  # add gender
  gender <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][10]
  Q1$儿童性别 <- str_c(str_split_fixed(gender,":",2)[,2])
  Q1$儿童性别
  
  # add gestational age
  GA <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][11]
  Q1$`孕龄(w)` <- str_c(str_split_fixed(GA,":",2)[,2])
  Q1$`孕龄(w)`
  
  # add name of caregiver
  namecg <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][12]
  Q1$填写人姓名 <- str_c(str_split_fixed(namecg,":",2)[,2])
  Q1$填写人姓名
  
  #add relationship
  rr <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][13]
  Q1$填写人与孩子关系 <- str_c(str_split_fixed(rr,":",2)[,2])
  Q1$填写人与孩子关系
  
  #add phone number
  pn <- stringi::stri_split_fixed(basic_info[subj,3],"|")[[1]][14]
  Q1$联系电话 <- str_c(str_split_fixed(pn,":",2)[,2])
  Q1$联系电话
  
  #View(Q1)
  
  
  ###information except Q1
  
  basic_info$Q2_出生时的体重 <- stringi::stri_split_fixed(basic_info[subj,4],":")[[1]][2]
  basic_info$Q8_产妇年龄 <- stringi::stri_split_fixed(basic_info[subj,10],":")[[1]][2]
  basic_info$Q9_出生时父亲的年龄 <- stringi::stri_split_fixed(basic_info[subj,11],":")[[1]][2]
  basic_info$`Q16_孩子出生后，母亲全职在家照顾孩子的时间` <- stringi::stri_split_fixed(basic_info[subj,18],":")[[1]][2]
  basic_info$Q17_母乳喂养时间 <- stringi::stri_split_fixed(basic_info[subj,19],":")[[1]][2]
  basic_info$`Q18_孩子6–12个月大时，父母与孩子平均每天互动的时间` <- stringi::stri_split_fixed(basic_info[subj,20],":")[[1]][2]
  basic_info$`Q20_孩子1岁后，父母与孩子平均每天互动的时间` <- stringi::stri_split_fixed(basic_info[subj,22],":")[[1]][2]
  basic_info$`Q23_兄弟姐妹情况及其年龄（例如：有一个姐姐，10岁）` <- stringi::stri_split_fixed(basic_info[subj,25],":")[[1]][2]
  basic_info$Q36_最早出现自闭症或发育迟缓信号的年龄 <- stringi::stri_split_fixed(basic_info[subj,38],":")[[1]][2]
  basic_info$Q39_开始干预的年龄 <- stringi::stri_split_fixed(basic_info[subj,41],":")[[1]][2]
  basic_info$Q40_平均每周干预时间 <- stringi::stri_split_fixed(basic_info[subj,42],":")[[1]][2]
  
  cz <- stringi::stri_split_fixed(basic_info[subj,40],"|")[[1]][c(1:2)]
  basic_info$Q38_初次就诊和正式诊断的年龄分别为  <- str_c(str_split_fixed(cz,":",2)[,2],collapse = "/")
  basic_info$Q38_初次就诊和正式诊断的年龄分别为
  
  #View(basic_info)
  
  
  subscale2 <- basic_info[subj,4:51]
  #View(subscale2)
  
  tmp <- as.data.frame(cbind(Q1,subscale2))
  
  info_final[i,] <- tmp
  colnames(info_final) <- c("subj","出生日期","填写日期","儿童年龄(y/m)","儿童性别","孕龄(w)","填写人姓名",
                    "填写人与孩子关系","联系电话")
  }
  #View(Basic_info_final)
  return(info_final)
  
}

#writexl::write_xlsx(Basic_info_final,"Basic_info_final.xlsx")
