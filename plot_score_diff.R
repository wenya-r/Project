my_data <- read.csv("test_data.csv",nrow=434150)
score_ave1 <- numeric()
score_ave2 <- numeric()
mix1 <- my_data[which(my_data$NRC_DESC =="Average Needs" & my_data$SUBGROUP_NAME=="All Students"),]
mix2 <- my_data[which(my_data$NRC_DESC =="" & my_data$SUBGROUP_NAME=="All Students"),]
mix_grade4_1 <- mix1[which(mix1$ITEM_DESC == "Grade 4 ELA"),]
mix_grade4_2 <- mix2[which(mix2$ITEM_DESC == "Grade 4 ELA"),]
counties <- unique(my_data$COUNTY_DESC) # list of counties
for (cty in counties[2:length(counties)]){

	mix_cty_1 <- mix_grade4_1[which(mix_grade4_1$COUNTY_DESC==cty),]
	mix_cty_2 <- mix_grade4_2[which(mix_grade4_2$COUNTY_DESC==cty),]

	num1 <-	as.numeric(as.character(mix_cty_1$TOTAL_TESTED))
	num2 <- as.numeric(as.character(mix_cty_2$TOTAL_TESTED))
	mean1 <- as.numeric(as.character(mix_cty_1$MEAN_SCALE_SCORE))
	mean2 <- as.numeric(as.character(mix_cty_2$MEAN_SCALE_SCORE))
	score_ave1 <- c(score_ave1,sum(num1*mean1)/sum(num1))	
	score_ave2 <- c(score_ave2,sum(num2*mean2)/sum(num2))	
}
diff <- score_ave1-score_ave2

pdf('plot_score_diff1.pdf')
plot(diff~score_ave1,xlab="Average Score for All Students",ylab="Score_avg-Score_[Disability]",main="English test scores of 4th grade students", sub="Each point represents the average result for each county in NY state.")
dev.off()
