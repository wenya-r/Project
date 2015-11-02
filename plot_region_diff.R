#my_data <- read.csv("test_data.csv",nrow=434150)
score_ave <- numeric()
score_max <- numeric()
score_min <- numeric()
mix <- my_data[which( my_data$SUBGROUP_NAME=="All Students"),]

mix_grade4 <- mix[which(mix$ITEM_DESC == "Grade 4 ELA"),]

counties <- unique(my_data$COUNTY_DESC) # list of counties
for (cty in counties[2:length(counties)]){
	#print(cty)
	
	mix_cty <- mix_grade4[which(mix_grade4_1$COUNTY_DESC==cty),]
	num <-	as.numeric(as.character(mix_cty$TOTAL_TESTED))
	mean_s <- as.numeric(as.character(mix_cty$MEAN_SCALE_SCORE))
	#print(mix_cty)
	score_ave <- c(score_ave,sum(num*mean_s)/sum(num))	
	score_max <- c(score_max,max(mean_s))	
	score_min <- c(score_min,min(mean_s))	
}
diff <- score_max-score_min

pdf('plot_region.pdf')
plot(diff~score_ave,xlab="Average Score for All Students",ylab="The maximum difference in a county",main="English test scores of 4th grade students", sub="Each point represents the result for each county in NY state.")
dev.off()
