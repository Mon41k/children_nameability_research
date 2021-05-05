##### Analysis Overview ####

#load libraries
library(datasets) #version 3.3.1
library(graphics) #version 3.3.1
library(grDevices) #version 3.3.1
library(methods) #version 3.3.1
library(stats) #version 3.3.1
library(utils) #version 3.3.1
library(ggplot2)

library(Matrix) #version 1.2-10
library(cowplot) #version 0.8.0
library(lme4) #version 1.1-14
library(plyr) #version 1.8.4
source('summarizeData.R') #helper functions for summarizing data

#read in data
d <- read.csv("cnames_data.csv", header = TRUE, sep = ";")

d$conditionC[d$condition=="high"] <- 0.5
d$conditionC[d$condition=="low"] <- -0.5

d$conditionC[d$condition=="verb"] <- 0.5
d$conditionC[d$condition=="silence"] <- -0.5

d$totalTrialNumC[d$totalTrialNum==23] <- 11.5
d$totalTrialNumC[d$totalTrialNum==22] <- 10.5
d$totalTrialNumC[d$totalTrialNum==21] <- 9.5
d$totalTrialNumC[d$totalTrialNum==20] <- 8.5
d$totalTrialNumC[d$totalTrialNum==19] <- 7.5
d$totalTrialNumC[d$totalTrialNum==18] <- 6.5
d$totalTrialNumC[d$totalTrialNum==17] <- 5.5
d$totalTrialNumC[d$totalTrialNum==16] <- 4.5
d$totalTrialNumC[d$totalTrialNum==15] <- 3.5
d$totalTrialNumC[d$totalTrialNum==14] <- 2.5
d$totalTrialNumC[d$totalTrialNum==13] <- 1.5
d$totalTrialNumC[d$totalTrialNum==12] <- 0.5
d$totalTrialNumC[d$totalTrialNum==11] <- -0.5
d$totalTrialNumC[d$totalTrialNum==10] <- -1.5
d$totalTrialNumC[d$totalTrialNum==9] <- -2.5
d$totalTrialNumC[d$totalTrialNum==8] <- -3.5
d$totalTrialNumC[d$totalTrialNum==7] <- -4.5
d$totalTrialNumC[d$totalTrialNum==6] <- -5.5
d$totalTrialNumC[d$totalTrialNum==5] <- -6.5
d$totalTrialNumC[d$totalTrialNum==4] <- -7.5
d$totalTrialNumC[d$totalTrialNum==3] <- -8.5
d$totalTrialNumC[d$totalTrialNum==2] <- -9.5
d$totalTrialNumC[d$totalTrialNum==1] <- -10.5
d$totalTrialNumC[d$totalTrialNum==0] <- -11.5


write.table(d, "cnames_data.txt", sep="\t")
d <- read.table("cnames_data.txt", header = TRUE)



####Experiment 1A####

#summarizing subject performance
subj_1A=ddply(subset(d,experiment=="1A"),.(subject,condition),summarize,
              accuracy=mean(isRight),
  )

#overall accuracy
#high condition
mean(subj_1A$accuracy[subj_1A$condition=="high"])
confint(lm(accuracy~1,data=subset(subj_1A,condition=="high")))
#low condition
mean(subj_1A$accuracy[subj_1A$condition=="low"])
confint(lm(accuracy~1,data=subset(subj_1A,condition=="low")))


#overall model fit
mLearn_1A=glmer(isRight~conditionC+ (1|subject),data=subset(d,experiment=="1A"),family=binomial)
summary(mLearn_1A)
confint(mLearn_1A,method="Wald")[2:3,]

#interaction with trial number
mTrialInteraction_1A=glmer(isRight~conditionC*totalTrialNumC+ (1+totalTrialNumC|subject),data=subset(d,experiment=="1A"),family=binomial)
summary(mTrialInteraction_1A)
confint(mTrialInteraction_1A,method="Wald")[4:7,]


####Experiment 3####

#summarizing subject performance
subj_3=ddply(subset(d,experiment=="3"),.(subject,condition),summarize,
              accuracy=mean(isRight))

#overall accuracy
#high condition
mean(subj_3$accuracy[subj_3$condition=="high"])
confint(lm(accuracy~1,data=subset(subj_3,condition=="high")))
#low condition
mean(subj_3$accuracy[subj_3$condition=="low"])
confint(lm(accuracy~1,data=subset(subj_3,condition=="low")))


#overall model fit
mLearn_3=glmer(isRight~conditionC+ (1|subject),data=subset(d,experiment=="3"),family=binomial)
summary(mLearn_3)
confint(mLearn_3,method="Wald")[2:3,]

#interaction with trial number
mTrialInteraction_3=glmer(isRight~conditionC*totalTrialNumC+ (1+totalTrialNumC|subject),data=subset(d,experiment=="3"),family=binomial)
summary(mTrialInteraction_3)
confint(mTrialInteraction_3,method="Wald")[4:7,]

####Experiment 4 - children with train####

#summarizing subject performance
subj_4=ddply(subset(d,experiment=="4"),.(subject,condition),summarize,
             accuracy=mean(isRight))

#overall accuracy
#verb condition
mean(subj_4$accuracy[subj_4$condition=="verb"])
confint(lm(accuracy~1,data=subset(subj_4,condition=="verb")))
#silence condition
mean(subj_4$accuracy[subj_4$condition=="silence"])
confint(lm(accuracy~1,data=subset(subj_4,condition=="silence")))

#overall model fit
mLearn_4=glmer(isRight~conditionC+ (1|subject),data=subset(d,experiment=="4"),family=binomial)
summary(mLearn_4)
confint(mLearn_4,method="Wald")[2:3,]

#interaction with trial number
mTrialInteraction_4=glmer(isRight~conditionC*totalTrialNumC+ (1+totalTrialNumC|subject),data=subset(d,experiment=="4"),family=binomial)
summary(mTrialInteraction_4)
confint(mTrialInteraction_4,method="Wald")[4:7,]


#### GRAPHING ####

#Experiment
subj_blocks=ddply(d,.(experiment,subject,condition,blockNum),summarize,
                           accuracy=mean(isRight))


#### Experiment 1 ####
subjOverall_byBlock_exp1A = summarySEwithin(subset(subj_blocks,experiment=="1A"),"accuracy",betweenvars=c("condition"),withinvars=c("blockNum"),idvar="subject")
subjOverall_byBlock_exp3= summarySEwithin(subset(subj_blocks,experiment=="3"),"accuracy",betweenvars=c("condition"),withinvars=c("blockNum"),idvar="subject")
subjOverall_byBlock_exp4= summarySEwithin(subset(subj_blocks,experiment=="4"),"accuracy",betweenvars=c("condition"),withinvars=c("blockNum"),idvar="subject")

library(plyr)
subjOverall_byBlock_exp3$condition=revalue(subjOverall_byBlock_exp3$condition, c("high"="легко называемые", "low"="трудно называемые"))
subjOverall_byBlock_exp4$condition=revalue(subjOverall_byBlock_exp4$condition, c("silence"="без вербализации", "verb"="с вербализацией"))


## 1A
#plot by block
p1A = ggplot(subjOverall_byBlock_exp1A, aes(blockNum,accuracy,color=condition,group=condition))+
  geom_line(size=2,position=position_dodge(.05))+
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),width=0,size=0.75,position=position_dodge(.05))+
  geom_point(aes(fill=condition),size=4,position=position_dodge(.05))+
  theme_classic(base_size=24)+
  scale_color_brewer(palette="Set1",name="cond")+
  scale_fill_brewer(palette="Set1",name="cond")+
  xlab("block\n\nExp1")+
  ylab("accuracy")+
  ylim(c(0.48,1))+
  geom_hline(yintercept=0.5,linetype="dotted",size=1.1)+
  theme(legend.position=c(.7, .2),legend.text=element_text(size=16),legend.title=element_text(size=16,face="bold"))


p3 = ggplot(subjOverall_byBlock_exp3, aes(blockNum,accuracy,color=condition,group=condition))+
  geom_line(size=2,position=position_dodge(.05))+
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),width=0,size=0.75,position=position_dodge(.05))+
  geom_point(aes(fill=condition),size=4,position=position_dodge(.05))+
  theme_classic(base_size=24)+
  scale_color_brewer(palette="Set1",name="называемость")+
  scale_fill_brewer(palette="Set1",name="называемость")+
  xlab("Блок")+
  ylab("Успешность научения")+
  ylim(c(0.48,1))+
  geom_hline(yintercept=0.5,linetype="dotted",size=1.1)+
  theme(legend.position=c(.7, .2),legend.text=element_text(size=16),legend.title=element_text(size=16,face="bold"))

p4 = ggplot(subjOverall_byBlock_exp4, aes(blockNum,accuracy,color=condition,group=condition))+
  geom_line(size=2,position=position_dodge(.05))+
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),width=0,size=0.75,position=position_dodge(.05))+
  geom_point(aes(fill=condition),size=4,position=position_dodge(.05))+
  theme_classic(base_size=24)+
  scale_color_brewer(palette="Set1",name="вербализация")+
  scale_fill_brewer(palette="Set1",name="вербализация")+
  xlab("Блок")+
  ylab("Успешность научения")+
  ylim(c(0.48,1))+
  geom_hline(yintercept=0.5,linetype="dotted",size=1.1)+
  theme(legend.position=c(.7, .2),legend.text=element_text(size=16),legend.title=element_text(size=16,face="bold"))


#plot together
p_exp1=plot_grid(p1A,p3,ncol=2,labels=c("A","B"))
p_exp1

p_exp2=plot_grid(p3,p4,ncol=2,labels=c("A","B"))
p_exp2



write.csv(subj_1A, 'exp1A.csv')
write.csv(subj_1B, 'exp1B.csv')
write.csv(subj_2, 'exp2.csv')
sub2=subset(d,experiment=="2")
write.csv(sub2, 'exp2_chi.csv')
