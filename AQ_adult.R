# setwd("~/Documents/wenjuan")
# library(readxl)
# library(stringr)

# subjid <- 47:50
# wenjuan_AQ_Adults <- readxl::read_xlsx("data/AQ_Adult.xlsx")
# wenjuan_AQ_Adults <- as.data.frame(wenjuan_AQ_Adults)
# #View(wenjuan_AQ_Adults)  
# tmp <- AQ_adult(wenjuan_AQ_Adults,subjid)

##function
AQ_adult <- function(wenjuan_AQ_Adults,subjid) {

list_agree <- c(1, 2, 4, 5, 6, 7,9, 12, 13, 16, 18, 19, 20, 21, 22, 23, 26, 33, 
		35, 39, 41,42, 43, 45, 46)
list_disagree <- c(3, 8, 10, 11, 14, 15, 17, 24, 25, 27, 28, 29, 30, 31, 32, 34, 
		   36, 37, 38, 40, 44, 47, 48, 49, 50)
length(list_disagree)
## scores
score_all <- data.frame(matrix(0,1,8))
colnames(score_all) <- c("subj_name","EvalDate","total","social","attention_switching","attention_to_detail",
 			 "communication","imagination")
#View(score_all)

AQ_Adult_final <- as.data.frame(matrix(0,length(subjid),8))

aa <- 0
for (subj in subjid) {
    
    aa <- aa + 1
  
    # add subject name
    ss <- stringi::stri_split_fixed(wenjuan_AQ_Adults[subj,3],"|")[[1]][5]
    score_all$subj_name <- stringi::stri_split_fixed(ss,":")[[1]][2]
    score_all$subj_name
    
    
    # total scores
    score_agree <- 0
    score_disagree <- 0
    
    for (i in list_agree+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微同意" || wenjuan_AQ_Adults[subj,q] == "完全同意") {
    		score_agree <- score_agree + 1
    	}
    }
    
    score_agree
    
    for (i in list_disagree+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微不同意" || wenjuan_AQ_Adults[subj,q] == "完全不同意") {
    		score_disagree <- score_disagree + 1
    	}
    }
    
    score_disagree
    score_all$total <- score_agree + score_disagree
    
    # subscales
    social <- c(1,11,13,15,22,36,44,45,47,48)
    attention_switching <- c(2, 4, 10, 16, 25,32,34,37,43,46)
    attention_to_detail <- c(5,6,9,12,19,23,28,29,30, 49)
    communication <- c(7,17,18,26,27,31,33,35,38,39)
    imagination <- c(3,8,14,20,21,24,40,41,42,50)
    
    length(attention_switching)
    length(attention_to_detail)
    length(communication)
    length(imagination)
    length(social)
    
    # social
    k <- intersect(list_agree, social)
    m <- intersect(list_disagree, social)
    
    social_1 <- 0
    for (i in k+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微同意" || wenjuan_AQ_Adults[subj,q] == "完全同意") {
    		social_1 <- social_1 + 1
    	}
    }
    
    social_1
    
    social_2 <- 0
    for (i in m+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微不同意" || wenjuan_AQ_Adults[subj,q] == "完全不同意") {
    		social_2 <- social_2 + 1
    	}
    }
    social_2
    
    score_all$social <- social_1 + social_2
    score_all$social
    
    # attention_switching
    k <- intersect(list_agree, attention_switching)
    m <- intersect(list_disagree, attention_switching)
    
    attention_switching_1 <- 0
    for (i in k+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微同意" || wenjuan_AQ_Adults[subj,q] == "完全同意") {
    		attention_switching_1 <- attention_switching_1 + 1
    	}
    }
    
    attention_switching_1
    
    attention_switching_2 <- 0
    for (i in m+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微不同意" || wenjuan_AQ_Adults[subj,q] == "完全不同意") {
    		attention_switching_2 <- attention_switching_2 + 1
    	}
    }
    attention_switching_2
    
    score_all$attention_switching <- attention_switching_1 + attention_switching_2
    score_all$attention_switching
    
    # attention_to_detail
    k <- intersect(list_agree, attention_to_detail)
    m <- intersect(list_disagree, attention_to_detail)
    
    attention_to_detail_1 <- 0
    for (i in k+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微同意" || wenjuan_AQ_Adults[subj,q] == "完全同意") {
    		attention_to_detail_1 <- attention_to_detail_1 + 1
    	}
    }
    
    attention_to_detail_1
    
    attention_to_detail_2 <- 0
    for (i in m+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微不同意" || wenjuan_AQ_Adults[subj,q] == "完全不同意") {
    		attention_to_detail_2 <- attention_to_detail_2 + 1
    	}
    }
    attention_to_detail_2
    
    score_all$attention_to_detail <- attention_to_detail_1 + attention_to_detail_2
    score_all$attention_to_detail
    
    # communication
    k <- intersect(list_agree, communication)
    m <- intersect(list_disagree, communication)
    
    communication_1 <- 0
    for (i in k+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微同意" || wenjuan_AQ_Adults[subj,q] == "完全同意") {
    		communication_1 <- communication_1 + 1
    	}
    }
    
    communication_1
    
    communication_2 <- 0
    for (i in m+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微不同意" || wenjuan_AQ_Adults[subj,q] == "完全不同意") {
    		communication_2 <- communication_2 + 1
    	}
    }
    communication_2
    
    score_all$communication <- communication_1 + communication_2
    score_all$communication
    
    # imagination
    k <- intersect(list_agree, imagination)
    m <- intersect(list_disagree, imagination)
    
    imagination_1 <- 0
    for (i in k+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微同意" || wenjuan_AQ_Adults[subj,q] == "完全同意") {
    		imagination_1 <- imagination_1 + 1
    	}
    }
    
    imagination_1
    
    imagination_2 <- 0
    for (i in m+1) {
    	q <- colnames(wenjuan_AQ_Adults)[i+2]
    	if (wenjuan_AQ_Adults[subj,q] == "稍微不同意" || wenjuan_AQ_Adults[subj,q] == "完全不同意") {
    		imagination_2 <- imagination_2 + 1
    	}
    }
    imagination_2
    
    score_all$imagination <- imagination_1 + imagination_2
    score_all$imagination
    
    ##add EvalDate
    qq <- stringi::stri_split_fixed(wenjuan_AQ_Adults[subj,3],"|")[[1]][c(1:3)]
    score_all$EvalDate  <- str_c(str_split_fixed(qq,":",2)[,2],collapse = "/")
    score_all$EvalDate
    #View(score_all)
    
    
  tmp <- as.data.frame(score_all)
  AQ_Adult_final[aa,] <- tmp
  colnames(AQ_Adult_final) <- c("subj_name","EvalDate","total","social","attention_switching",
                                "attention_to_detail","communication","imagination")
  }
return(AQ_Adult_final)
}


