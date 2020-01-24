library(ggplot2)
library(plyr)
require(gridExtra)

#Extract dataset in liste
liste <- read.csv("StudentsPerformance.csv", sep=",")

# We set the pass mark at 50
passmark = 50 

#Add 2 columns in liste : the average of 3 marks and the result (pass or fail)
liste$moyenne_generale=((liste$math.score+liste$reading.score+liste$writing.score)/3)
liste$result=ifelse(liste$moyenne_generale >= passmark, "PASS", "FAIL")

df = data.frame(liste);

##########################################################################################################################################################################################################################
################################### Success considering gender ##############################################################################################################################################################

#Create a tab with number of men and women
nbMenWomen = count(df,c("gender","result")) #Count the number of men & women

#Plot success rate per gender
plot1 <- ggplot(nbMenWomen, aes(x=gender, fill=result, y=freq, label=freq)) + geom_col() + geom_text(size = 3, position=position_stack(vjust = 0.5)) + ylab("Number of student") + xlab("Gender of student") + ggtitle("Success rate per gender") +
  theme(plot.title = element_text(hjust = 0.5))
plot1 + scale_fill_manual(values=c("grey","#5F87CC"))


##########################################################################################################################################################################################################################
################################### Level considering parental education ##############################################################################################################################################################

successPerParents = count(df,c("parental.level.of.education","result"))

#Getting percentage values of success considering parental level of education
parents = count(df,c("parental.level.of.education"))
failed = (subset((subset(successPerParents,result=="FAIL")),parental.level.of.education==parents$parental.level.of.education))
success = (subset((subset(successPerParents,result=="PASS")),parental.level.of.education==parents$parental.level.of.education))
parents$FAIL=failed$freq
parents$PASS=success$freq
parents$FAILrate=floor((parents$FAIL/parents$freq)*100)
parents$PASSrate=floor((parents$PASS/parents$freq)*100)

#Reorder parents' tab by descending value for the display
parents=arrange(parents,-PASSrate)
parents$parental.level.of.education=as.character(parents$parental.level.of.education)
parents$parental.level.of.education=factor(parents$parental.level.of.education,level=unique(parents$parental.level.of.education))

#Plot success rate per parent's studies
plot2<-ggplot(parents, aes(x=parental.level.of.education, y=PASSrate, label=PASSrate)) + ylim(0,100) + geom_text(size=3,position=position_stack(vjust=1.02)) + geom_col(aes(fill=PASSrate)) + ylab("Student ratio") + xlab("Parental level of education") + ggtitle("Success rate per parental level of education") +
theme(plot.title = element_text(hjust = 0.5))
plot2+scale_fill_gradient(high="red4",low="red")


###########################################################################################################################################################################################################################################################

##################################### Food habits study ########################################################################################################

#Prepare two pie charts : one with special diet and another with normal diet
foodStudy = count(df,c("lunch","result"))
free_lunch = subset(foodStudy,lunch=="free/reduced")
normal_lunch = subset(foodStudy,lunch=="standard")

plot3 <- ggplot(free_lunch, aes(x="", y=freq, label=freq, fill=result)) + geom_bar(stat="identity", width=1, color="white") + xlab("") + ylab("") + geom_text(size = 3, position=position_stack(vjust = 0.5)) + ggtitle("Success rate for reduced diet") + coord_polar("y",start = 0) +
 theme(plot.title = element_text(hjust = 0.5)) 

plot4 <- ggplot(normal_lunch, aes(x="", y=freq, label=freq, fill=result)) + geom_bar(stat="identity", width=1, color="white") + xlab("") + ylab("") + geom_text(size = 3, position=position_stack(vjust = 0.5)) + ggtitle("Success rate for normal diet") + coord_polar("y",start = 0) +
  theme(plot.title = element_text(hjust = 0.5))

plot4

###########################################################################################################################################################################################################################################################

##################################### Gender influence on several skills ########################################################################################################

#Plot math and writing score considering gender
plot5 = ggplot(df,aes(x=writing.score,y=math.score)) + geom_point(aes(col=gender)) + ylab("Math score") + xlab("Writing score") + ggtitle("Score per assignment per gender") + theme(plot.title = element_text(hjust = 0.5))
plot5 + scale_color_manual(values=c("#999999", "#E69F00"))


###########################################################################################################################################################################################################################################################

##################################### Impact of preparation on performance ########################################################################################################

#Plot scores violin considering previous preparation 
plot6 = ggplot(df,aes(x=test.preparation.course, y=moyenne_generale)) + geom_violin(aes(col=test.preparation.course, fill=test.preparation.course), draw_quantiles = c(0.25,0.5,0.75)) + ylab("General score") + xlab("Preparation course") + ggtitle("Score considering preparation course") + theme(plot.title = element_text(hjust = 0.5))
plot6 + scale_color_manual(values=c("purple4", "orange4")) + scale_fill_manual(values=c("#E6C1EB","#FFD988"))
