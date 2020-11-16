setwd("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA")

library(languageR)
library(car)
library(ggplot2)
library(lme4)
library(tidyverse)
library(dplyr)
library(stats)
library(pastecs)
library(rptR)
library(multcomp)
library(psych)
####################################
####################################T####################

DATASTROOP<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/STROOP.csv", header=TRUE)


DATASTROOP<-subset(DATASTROOP,react_time>200 & react_time<35000)# select perf between thresholds
DATASTROOP = droplevels(DATASTROOP)
summary(DATASTROOP)

#transform data for normality of residuals
max<-max(DATASTROOP$control_Stroop, na.rm = TRUE)
DATASTROOP$stroop<-sqrt((max+1)-DATASTROOP$control_Stroop)

hist(DATASTROOP$stroop)

##########
#REPEATABILITY
rpt(stroop~ (1 | subj_id), grname = "subj_id", data = DATASTROOP, datatype = "Gaussian", nboot =1000, npermut =  1000)

#ADJUSTED R
rpt(stroop~ trial + session + ordre_block +sex:stim+  (1 | subj_id), grname = "subj_id", data = DATASTROOP, datatype = "Gaussian", nboot =1000, npermut =  1000)

###############################
##############################
##FULL MODEL FOR males & females
#
Modelstroop<-lmer(stroop~ stim*sex+ age+ rank + trial+ ordre_block +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
summary(Modelstroop)

Baseline<-lmer(stroop~ 1+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
anova(Baseline,Modelstroop)
#############################

# VISUAL Check for approximation of normality of residuals

par(mfrow=c(1,2))
qqnorm(residuals(Modelstroop))
hist(residuals(Modelstroop))

###########################
##EXPORT TABLE

t<-coef(summary(Modelstroop))

write.table(t, file = "tablestroop.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
####BCKWARD VARIABLE SELECTION
Model_rank<-lmer(stroop~ stim*sex+ age+ rank + trial+ ordre_block +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
summary(Model_rank)

Model_age<-lmer(stroop~ stim*sex+ age+ trial+ ordre_block +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
summary(Model_age)

Model_optimum<-lmer(stroop~ stim*sex+   trial+ ordre_block +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
summary(Model_optimum)
#####################
t2<-anova(Model_rank, Model_age, Model_optimum)
##################################

write.table(t2, file = "tablestroop.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
#################################################
Model_trial<-lmer(stroop~ stim*sex+   ordre_block +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
Model_ordreblock<-lmer(stroop~ stim*sex+   trial +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
Model_stimsex<-lmer(stroop~  ordre_block+   trial +session+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
Model_session<-lmer(stroop~  stim*sex+ ordre_block+   trial + (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)
Baseline<-lmer(stroop~ 1+ (1|subj_id/type_picture),data=DATASTROOP, REML=FALSE)

anova(Model_optimum,  Model_ordreblock)
anova(Model_optimum, Model_session)
anova(Model_optimum,   Model_stimsex)
anova(Model_optimum,   Model_trial)
anova(Model_optimum,   Baseline)


##CALCUL MEAN AND SD
DATASTROOP<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/STROOP.csv", header=TRUE)
DATASTROOP<-subset(DATASTROOP,react_time>200 & react_time<35000)# select perf between thresholds
DATASTROOPFF<-subset(DATASTROOP, stim=='pic')
psych::describeBy(DATASTROOPFF$control_Stroop, group = DATASTROOPFF$sex)







#############################################################################################
#########################
#####FOR MALES
###################################
DATASTROOP<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/STROOP.csv", header=TRUE)
DATASTROOP<-subset(DATASTROOP,react_time>200 & react_time<35000)# select perf between thresholds
DATASTROOP = droplevels(DATASTROOP)
#transform data for normality of residuals
max<-max(DATASTROOP$control_Stroop, na.rm = TRUE)
DATASTROOP$stroop<-sqrt((max+1)-DATASTROOP$control_Stroop)

###########
###########
DATASTROOPM<-subset(DATASTROOP, sex=='         M')
DATASTROOPM<-subset(DATASTROOP, session==1)
DATASTROOPM<-subset(DATASTROOP, ordre_block==1)
#############################
.none_vs_neutral<-c(1,-1,0,0)
.none_vs_object<-c(0, -1, 1, 0)
.none_vs_threat<-c(0,-1,0,1)
m<-cbind(.none_vs_neutral,.none_vs_object,.none_vs_threat)
contrasts(DATASTROOPM$type_picture)<-m
###############
ModelstroopM<-lmer(stroop~  type_picture+ age + rank +trial+ (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)

summary(ModelstroopM)

##################
# VISUAL Check normality of residuals

par(mfrow=c(1,2))
qqnorm(residuals(ModelstroopM))
hist(residuals(ModelstroopM))

###EXPORT TABLE males

t<-coef(summary(ModelstroopM))

write.table(t, file = "tablestroop2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

############
#################
Model_rank<-lmer(stroop~  type_picture+ age + rank +trial+ (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)
summary(Model_rank)
#
Model_age<-lmer(stroop~  type_picture+ age + trial+ (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)
summary(Model_age)
#
Model_optimum<-lmer(stroop~  type_picture+ trial+ (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)
summary(Model_optimum)

anova(Model_rank, Model_age, Model_optimum)
#############
Model_trial<-lmer(stroop~  type_picture+  (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)
summary(Model_trial)
#
Model_typepic<-lmer(stroop~  trial+ (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)
summary(Model_typepic)
#
Baseline<-lmer(stroop~  1+ (1|subj_id/type_picture),data=DATASTROOPM, REML=FALSE)

#
anova(Baseline, Model_optimum)
anova(Model_trial, Model_optimum)
anova(Model_typepic, Model_optimum)

##POST HOC TEST
postHocs<-glht(ModelstroopM, linfct=mcp(type_picture="Tukey"))


summary(postHocs)


#######################################################################################################
##########################
###########FOR females

DATASTROOP<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/STROOP.csv", header=TRUE)
DATASTROOP<-subset(DATASTROOP,react_time>200 & react_time<35000)# select perf between thresholds
DATASTROOP = droplevels(DATASTROOP)
#transform data for normality of residuals
max<-max(DATASTROOP$control_Stroop, na.rm = TRUE)
DATASTROOP$stroop<-sqrt((max+1)-DATASTROOP$control_Stroop)

###########

DATASTROOPF<-subset(DATASTROOP, sex=='         F')
DATASTROOPM<-subset(DATASTROOP, session==1)
DATASTROOPM<-subset(DATASTROOP, ordre_block==1)
####
none_vs_neutral<-c(1,-1,0,0)
.none_vs_object<-c(0, -1, 1, 0)
.none_vs_threat<-c(0,-1,0,1)
m<-cbind(.none_vs_neutral,.none_vs_object,.none_vs_threat)
contrasts(DATASTROOPF$type_picture)<-m
######

ModelstroopF<-lmer(stroop~  type_picture  + age + rank + trial+ (1|subj_id/type_picture),data=DATASTROOPF, REML=FALSE)

summary(ModelstroopF)
##########################
##POST HOC TEST
postHocs<-glht(ModelstroopF, linfct=mcp(type_picture="Tukey"))
summary(postHocs)

#
# VISUAL Check normality of residuals

par(mfrow=c(1,2))
qqnorm(residuals(ModelstroopF))
hist(residuals(ModelstroopF))

###EXPORT TABLE Females

t<-coef(summary(ModelstroopF))

write.table(t, file = "tablestroop2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

####BACKWARD SELECTION

Model_type_picture<-lmer(stroop~  type_picture+ age + rank +trial+ (1|subj_id/type_picture),data=DATASTROOPF, REML=FALSE)
summary(Model_type_picture)
#
Model_trial<-lmer(stroop~  age + rank + trial+ (1|subj_id/type_picture),data=DATASTROOPF, REML=FALSE)
summary(Model_trial)
#
Model_age<-lmer(stroop~  age + rank + (1|subj_id/type_picture),data=DATASTROOPF, REML=FALSE)
summary(Model_age)
#
Model_rank<-lmer(stroop~   rank + (1|subj_id/type_picture),data=DATASTROOPF, REML=FALSE)
summary(Model_rank)
##
Baseline<-lmer(stroop~   1+ (1|subj_id/type_picture),data=DATASTROOPF, REML=FALSE)



#
t<-anova(Model_type_picture,Model_trial, Model_age, Model_rank, Baseline)
#############


write.table(t, file = "tablestroop2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


##################################################################################################
####GRAPHS
DATASTROOPS<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/STROOP.csv", header=TRUE)
DATASTROOPS<-subset(DATASTROOPS,react_time>200 & react_time<35000)

DATASTROOPS$type_picture <- factor(DATASTROOPS$type_picture, levels = c("None", "Object", "Neutral", "Threat"))

DATASTROOPS<-subset(DATASTROOPS, stim=='pic')

###
p <- ggplot(DATASTROOPS, aes(x=sex, y=control_Stroop, fill=sex)) + 
  geom_boxplot()+  
  
  geom_hline(yintercept=20, linetype="dashed", color = "black")+
 geom_jitter(color="black", size=0.4, alpha=0.3) +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=6,show_guide = FALSE)+
 
  labs(x = "\nSex of the subjects", y = "Distraction control score\n") +
  theme_classic(base_size = 17) 
p
###



DATASTROOPS<-subset(DATASTROOPS, session==1)
DATASTROOPS<-subset(DATASTROOPS, ordre_block==1)

p <- ggplot(DATASTROOPS, aes(x=type_picture, y=control_Stroop, fill=sex)) + 
    geom_boxplot()+  geom_hline(yintercept=20, linetype="dashed", color = "black")+
  geom_jitter(color="black", size=0.4, alpha=0.3) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed", color="red", fill="red")+
    labs(x = "\nType of picture", y = "Stroop control score\n") +
    theme_classic(base_size = 15) 
p

############ABDN

##############COUNT NUMBER OF FACIAL EXPRESSIONS

DATAEXP<-subset(DATASTROOP, Bared.teeth==1)
summary(DATASTROOP)


###############

table(DATASTROOP$Bared.teeth==1 & DATASTROOP$type_picture=="Threat") #6
table(DATASTROOP$Bared.teeth==1 & DATASTROOP$type_picture=="Neutral")#4
table(DATASTROOP$Bared.teeth==1 & DATASTROOP$type_picture=="None")#0
table(DATASTROOP$Bared.teeth==1& DATASTROOP$type_picture=="Object")#0
###################
##CALCULATE MEAN AND SD
DATASTROOPMM<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/STROOP.csv", header=TRUE)
DATASTROOPMM<-subset(DATASTROOP,react_time>200 & react_time<35000)
DATASTROOPMM<-subset(DATASTROOPMM, sex=='         M')

psych::describeBy(DATASTROOPMM$control_Stroop, group = DATASTROOPMM$type_picture)
DATASTROOPMM$type_picture <- factor(DATASTROOPMM$type_picture, levels = c("None", "Threat", "Object", "Neutral"))



############PLOT FOR MALES
p <- ggplot(DATASTROOPMM, aes(x=type_picture, y=control_Stroop)) + 
  geom_boxplot()+  geom_hline(yintercept=20, linetype="dashed", color = "black")+
  
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=6,show_guide = FALSE)+

  labs(x = "\nType of picture", y = "Distraction control score\n") +
  theme_classic(base_size = 17) 
p


##############

