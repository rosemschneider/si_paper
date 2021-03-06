rm(list=ls())
library(lme4)
library(plotrix)
library(ggplot2)
library(diptest)
library(plyr)
d1 <- read.csv("../data/implicatures_adhocScalar_data.csv")


##########################TEST TRIALS#########################
library(reshape)

d2=melt.data.frame(d1,c("Subj_ID", "age","agegroup", "list"),c("carrots", "hats", "cookies", "trains", "cats", "purses",  "keys", "shirts", "breads", "horses", "bears", "frogs", "plates", "books", "elephants", "lamps", "bananas", "butterflies", 
"carrots_condition", "hats_condition", "cookies_condition", "trains_condition", "cats_condition", "purses_condition",  "keys_condition", "shirts_condition", "breads_condition", "horses_condition", "bears_condition", "frogs_condition", "plates_condition", "books_condition", "elephants_condition", "lamps_condition", "bananas_condition", "butterflies_condition", 
"carrots_type", "hats_type", "cookies_type", "trains_type", "cats_type", "purses_type",  "keys_type", "shirts_type", "breads_type", "horses_type", "bears_type", "frogs_type", "plates_type", "books_type", "elephants_type", "lamps_type", "bananas_type", "butterflies_type"))



data <- d2[1:864,] 
data$condition <- d2$value[865:1728]
data$type <- d2$value[1729:2592]
names(data)[5] <- "item"
names(data)[6] <- "correct"

data$older <- (data$age>4.5)
data$correct <- data$correct==1

data %<>%
  mutate(correct = as.numeric(correct))

attach(data)
data$trial_type[type == "implicature"] <- "implicature"
data$trial_type[type != "implicature"] <- "control"
detach(data)


#age data for paper
library(dplyr)
d_age <- d2 %>%
  mutate(younger = agegroup == "4.0--4.5", older = agegroup == "4.5--5.0")%>%
  group_by(agegroup)%>%
  summarise(m = mean(age), std = sd(age), median = median(age))


######## data by condition and trial type ##########
agg.data <- aggregate(data$correct, list(data$condition, data$type, data$trial_type, data$agegroup), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$condition, data$type, data$trial_type, data$agegroup), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("implicature_type", "type", "trial_type", "Age", "correct")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$correct / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)
#agg.data$age <- ifelse(agg.data$older, "4.5-5.0", "4.0-4.5")

########  plot by age
plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(.2,.2),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 


qplot(data = agg.data,
	x = Age,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=implicature_type,	
	#main="Preschooler results", 
	ylab="Proportion Correct",
	xlab="Trial Type",
	position=dodge,
	ylim=c(0,1)) +  facet_wrap("type") + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0.25) + theme_bw() + plot.style
	# + scale_fill_manual(values=c("orange", "red"))
	
### alternate figure start
agg.data$type <- factor(agg.data$type, 
					levels = c("implicature","control_comparison", 
					"control_distractor",
					"control_none","control_unambiguous", "control_all"), 
					labels = c("Implicature","Comparison", 
					"Distractor",
					"None","Unambig. Some", "All"))

agg.data$implicature_type <- factor(agg.data$implicature_type, 
					levels = c("adhoc","scalar"), 
					labels = c("Ad-hoc","Scalar"))
					
									
qplot(x = type, 
	y = prop.corr,
	geom="bar", 
	stat="identity", 
	position="dodge",
	fill = Age, 
	data=agg.data) + 
	facet_grid(.~implicature_type,scale="free_x") + 
	geom_errorbar(limits, position=dodge, width=.25) +
	geom_abline(intercept=.5,slope=0,lty=2) + 
	# theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5)) + 
	ylab("Proportion Correct") + xlab("Trial Type") + 
	theme_bw()

### alternate figure end	
	



### individual differences scatterplot
ms <- ddply(data, .(type, condition, Subj_ID), summarise, 
	mean = mean(correct),
	agegroup = agegroup[1])

ms %<>%
  filter(condition == "scalar")

names(ms) <- c("type", "condition", "Subj_ID", "mean", "Age")


cs <- cast(ms, Subj_ID + Age ~ type + condition, value="mean")


qplot(control_none_scalar, implicature_scalar, col= Age, position=position_jitter(.05), data=cs) + geom_smooth(method="lm")

### to match Experiment 2 pretty plot
library(dplyr)
qplot(control_none_scalar, implicature_scalar, col=Age, position=position_jitter(.05), data=cs) + geom_smooth(method="lm")

qplot(control_none_scalar, implicature_scalar, col=Age, 
	ylab="Proportion of 'some' trials correct",
	xlab="Proportion of 'none' trials correct",
		position=position_jitter(.02), data=cs) + 
	geom_smooth(method="lm", col="black", lty=1) + 
		theme_bw()+
	geom_smooth(aes(col= Age, group= Age), 
				se=FALSE, method="lm",lty=3) 
				

library(dplyr)
cor.test(cs$control_none_scalar, cs$implicature_scalar)
cs %>% group_by(agegroup) %>% 
	summarise(r = cor.test(control_none_scalar, implicature_scalar)$estimate,
			  p = cor.test(control_none_scalar, implicature_scalar)$p.value)

cor.test(cs$control_all_scalar, cs$implicature_scalar)
cs %>% group_by(agegroup) %>% 
	summarise(r = cor.test(control_all_scalar, implicature_scalar)$estimate,
			  p = cor.test(control_all_scalar, implicature_scalar)$p.value)					  	  
cor.test(cs$control_all_scalar, cs$control_none_scalar)
cs %>% group_by(agegroup) %>% 
	summarise(r = cor.test(control_all_scalar, control_none_scalar)$estimate,
			  p = cor.test(control_all_scalar, control_none_scalar)$p.value)		
			  





dip.test(cs$implicature_scalar)
dip.test(cs$control_none_scalar)
dip.test(cs$control_all_scalar)




##############################

gl <- glmer(correct ~ trial_type * condition * age   + (trial_type | Subj_ID), data=data, family=binomial)
summary(gl)

###



gl <- glmer(correct ~ half * trial_type * condition  * age   + (trial_type | Subj_ID), data=data, family=binomial)
summary(gl)


gl <- glmer(correct ~ half   * age   + (trial_type | Subj_ID), data=data, family=binomial)
summary(gl)


