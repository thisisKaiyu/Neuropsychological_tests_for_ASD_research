# AQ-Child scores

AQ_Child <- function(wenjuan_child,subj,assessment) {

reverse_keyed <- c(1,2,3,4,5,6,8,9,11,13,14, 15, 16, 17, 18, 20, 21, 24, 26, 28)
keyed <- setdiff(1:30,reverse_keyed)
length(keyed)

## scores
score_all <- data.frame(matrix(0,4,7))
colnames(score_all) <- c("subj","total","Socialness","Social_communicative","Imagination","Patterns","Attention_switching")

# total scores
stringi::stri_split_fixed(wenjuan_child[subj,3],"|")
ans_str <- stringi::stri_split_fixed(wenjuan_child[subj,4],"|")
ans_str
stringi::stri_split_fixed(wenjuan_child[subj,3],"|")

score_reverse_keyed <- 0
score_keyed <- 0

for (i in reverse_keyed) {
	
	if (ans_str[[1]][i] == "非常同意") {
		score_reverse_keyed <- score_reverse_keyed + 3
	} else if (ans_str[[1]][i]  == "稍微同意") {
		score_reverse_keyed <- score_reverse_keyed + 2
	} else if (ans_str[[1]][i]  == "稍微不同意") {
		score_reverse_keyed <- score_reverse_keyed + 1
	}
}

for (i in keyed) {
	if (ans_str[[1]][i]  == "稍微同意") {
		score_keyed <- score_keyed + 1
	} else if (ans_str[[1]][i]  == "稍微不同意") {
		score_keyed <- score_keyed + 2
	} else if (ans_str[[1]][i]  == "完全不同意") {
		score_keyed <- score_keyed + 3
	}
}

score_all$total <- score_reverse_keyed + score_keyed

## subscales
Socialness <- c(7, 12, 23, 27, 29)
Social_communicative <- c(5, 14, 15, 16, 18, 20, 21, 24, 28)
Imagination <- c(10, 19, 22, 25,30)
Patterns <- c(4, 6, 8, 9, 13, 17, 26)
Attention_switching <- c(1, 2, 3, 11)

kkk <- c(Socialness,Social_communicative,Imagination,Patterns,Attention_switching)

# socialness
keyed_s <- intersect(keyed, Socialness)
reverse_keyed_s <- intersect(reverse_keyed, Socialness)

ans_str[keyed_s]

keyed_s
reverse_keyed_s

socialness_1 <- 0

for (i in keyed_s) {
	if (ans_str[[1]][i] == "稍微同意") {
		socialness_1 <- socialness_1 + 1
	} else if (ans_str[[1]][i] == "稍微不同意") {
		socialness_1 <- socialness_1 + 2
	} else if (ans_str[[1]][i] == "完全不同意") {
		socialness_1 <- socialness_1 + 3
	}
}

socialness_1

socialness_2 <- 0
for (i in reverse_keyed_s) {
	if (ans_str[[1]][i] == "非常同意") {
		socialness_2 <- socialness_2 + 3
	} else if (ans_str[[1]][i] == "稍微同意") {
		socialness_2 <- socialness_2 + 2
	} else if (ans_str[[1]][i] == "稍微不同意") {
		socialness_2 <- socialness_2 + 1
	}
}

socialness_2

score_all$Socialness <- socialness_1 + socialness_2

# Social_communicative
keyed_s <- intersect(keyed, Social_communicative)
reverse_keyed_s <- intersect(reverse_keyed, Social_communicative)

keyed_s
reverse_keyed_s

Social_communicative_1 <- 0

for (i in keyed_s) {
	if (ans_str[[1]][i] == "稍微同意") {
		Social_communicative_1 <- Social_communicative_1 + 1
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Social_communicative_1 <- Social_communicative_1 + 2
	} else if (ans_str[[1]][i] == "完全不同意") {
		Social_communicative_1 <- Social_communicative_1 + 3
	}
}

Social_communicative_1

Social_communicative_2 <- 0
for (i in reverse_keyed_s) {
	if (ans_str[[1]][i]  == "非常同意") {
		Social_communicative_2 <- Social_communicative_2 + 3
	} else if (ans_str[[1]][i] == "稍微同意") {
		Social_communicative_2 <- Social_communicative_2 + 2
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Social_communicative_2 <- Social_communicative_2 + 1
	}
}

Social_communicative_2

score_all$Social_communicative <- Social_communicative_1 + Social_communicative_2


# Imagination
keyed_s <- intersect(keyed, Imagination)
reverse_keyed_s <- intersect(reverse_keyed, Imagination)

keyed_s
reverse_keyed_s

Imagination_1 <- 0

for (i in keyed_s) {
	if (ans_str[[1]][i] == "稍微同意") {
		Imagination_1 <- Imagination_1 + 1
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Imagination_1 <- Imagination_1 + 2
	} else if (ans_str[[1]][i] == "完全不同意") {
		Imagination_1 <- Imagination_1 + 3
	}
}

Imagination_1

Imagination_2 <- 0
for (i in reverse_keyed_s) {
	if (ans_str[[1]][i] == "非常同意") {
		Imagination_2 <- Imagination_2 + 3
	} else if (ans_str[[1]][i] == "稍微同意") {
		Imagination_2 <- Imagination_2 + 2
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Imagination_2 <- Imagination_2 + 1
	}
}

Imagination_2

score_all$Imagination <- Imagination_1 + Imagination_2


# Patterns
keyed_s <- intersect(keyed, Patterns)
reverse_keyed_s <- intersect(reverse_keyed, Patterns)

keyed_s
reverse_keyed_s

Patterns_1 <- 0

for (i in keyed_s) {
	if (ans_str[i] == "稍微同意") {
		Patterns_1 <- Patterns_1 + 1
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Patterns_1 <- Patterns_1 + 2
	} else if (ans_str[[1]][i] == "完全不同意") {
		Patterns_1 <- Patterns_1 + 3
	}
}

Patterns_1

Patterns_2 <- 0
for (i in reverse_keyed_s) {
	if (ans_str[[1]][i] == "非常同意") {
		Patterns_2 <- Patterns_2 + 3
	} else if (ans_str[[1]][i] == "稍微同意") {
		Patterns_2 <- Patterns_2 + 2
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Patterns_2 <- Patterns_2 + 1
	}
}

Patterns_2

score_all$Patterns <- Patterns_1 + Patterns_2

# Attention_switching
keyed_s <- intersect(keyed, Attention_switching)
reverse_keyed_s <- intersect(reverse_keyed, Attention_switching)

keyed_s
reverse_keyed_s

Attention_switching_1 <- 0

for (i in keyed_s) {
	if (ans_str[[1]][i] == "稍微同意") {
		Attention_switching_1 <- Attention_switching_1 + 1
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Attention_switching_1 <- Attention_switching_1 + 2
	} else if (ans_str[[1]][i] == "完全不同意") {
		Attention_switching_1 <- Attention_switching_1 + 3
	}
}

Attention_switching_1

Attention_switching_2 <- 0
for (i in reverse_keyed_s) {
	if (ans_str[[1]][i] == "非常同意") {
		Attention_switching_2 <- Attention_switching_2 + 3
	} else if (ans_str[[1]][i] == "稍微同意") {
		Attention_switching_2 <- Attention_switching_2 + 2
	} else if (ans_str[[1]][i] == "稍微不同意") {
		Attention_switching_2 <- Attention_switching_2 + 1
	}
}

Attention_switching_2

score_all$Attention_switching <- Attention_switching_1 + Attention_switching_2

score_all

# ## plot
# mainstream <- as.data.frame(matrix(0,6,4))
# colnames(mainstream) <- c("factors","mean","sd","subj")
# mainstream$factors <- colnames(score_all)
# mainstream$factors <- factor(mainstream$factors, 
# 			     levels = c("total","Socialness","Social_communicative",
# 			     	   "Imagination","Patterns","Attention_switching"))
# mainstream$mean <- c(33.85, 4.31,7.37,5.13,9.93,7.11)
# mainstream$sd <- c(8.83,3.05,4.54,2.69,3.97,2.48)
# mainstream$subj <- as.numeric(score_all[1,])
# 
# p <- ggplot(mainstream[1,], aes(x = factors,y = mean)) +
# 	geom_bar(stat = "identity", color = "black", 
# 		 position = position_dodge()) +
# 	geom_errorbar(aes(ymin=mean-sd, ymax = mean+sd), width = .2,
# 		      position = position_dodge(0.9)) +
# 	theme(axis.text = element_text(size = 10, face = "bold"),
# 	      axis.title = element_text(size = 10, face = "bold"),
# 	      axis.text.x = element_blank()) +
# 	labs(x = "Total", y = "Mean scores") +
# 	theme(panel.background = element_blank(),
# 	      panel.grid = element_blank(),
# 	      panel.border = element_blank(),
# 	      axis.line = element_line(colour = "black")) + # remove background
# 	coord_cartesian(ylim=c(0,60)) + 
# 	scale_y_continuous(breaks = seq(0.00,60, 10))
# 
# p + geom_point(data=mainstream[1,], aes(x=factors, y=subj), 
# 	      colour = "red",size = 4)
# 
# ggsave(paste0("wenjuan/results/plots/",assessment, "/s", subj,"_total.jpg"),
#        width = 6,height = 4)
# 
# p <- ggplot(mainstream[2:6,], aes(x = factors,y = mean)) +
# 	geom_bar(stat = "identity", color = "black", 
# 		 position = position_dodge()) +
# 	geom_errorbar(aes(ymin=mean-sd, ymax = mean+sd), width = .2,
# 		      position = position_dodge(0.9)) +
# 	theme(axis.text = element_text(size = 10, face = "bold"),
# 	      axis.title = element_text(size = 10, face = "bold")) +
# 	labs(x = " ", y = "Mean scores") +
# 	theme(panel.background = element_blank(),
# 	      panel.grid = element_blank(),
# 	      panel.border = element_blank(),
# 	      axis.line = element_line(colour = "black")) + # remove background
# 	coord_cartesian(ylim=c(0,20)) + 
# 	scale_y_continuous(breaks = seq(0,20, 5))
# 	
# p + geom_point(data=mainstream[2:6,], aes(x=factors, y=subj), colour = "red",size = 4)
# 
# ggsave(paste0("wenjuan/results/plots/",assessment, "/s", subj,"_subscales.png"),
#        width = 8,height =5)

return(score_all)
}
