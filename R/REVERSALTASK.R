setwd("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA")

library(languageR)
library(nlme)
library(car)
library(reshape)
library(ggplot2)
library(lme4)
library (pastecs)
library(nlme)
library(reshape)
library(tidyverse)
library(dplyr)
library(lmerTest)
library(stats)
library(multcomp)
library(tidyr)
#########################################################
#########################################################

##ACCURACY

DATAREVERS<-read.csv("C:/Users/Marine/Desktop/PORTSMOUTH/ARTICLE INDIV DIFF/DATA/REVERSAL.csv", header=TRUE)

DATAREVERS$rule<-as.factor(DATAREVERS$rule)

DATAREVERS$success<-as.factor(DATAREVERS$success)

DATAREVERS = droplevels(DATAREVERS)

summary(DATAREVERS)

##########
#REPEATABILITY FOR SUCCESS PN A TRIAL

rpt(success~ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Binary", nboot =1000, npermut =  1000)

#ADJUSTED REPEATABILITY
rpt(success~ session +(1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Binary", nboot =100, npermut =  100)


#################

#FULL MODEL FOR ACCURACY

Modelrule<- glmer(success~ rule +  sex + age + session + trial+ (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Modelrule)

t<-coef(summary(Modelrule))

write.table(t, file = "tablerule.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


####

#LOOK FOR NORMALITY OF RESIDUALS

library(DHARMa)
res = simulateResiduals(Modelrule)
plot(res)


##EXPORT TABLE

t<-coef(summary(Modelrule))

write.table(t, file = "tabler2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


#######ANOVA BACKWARD
##################################

Model_trial<- glmer(success~  rule +sex+ age+ trial + session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_trial)
#
Model_sex<- glmer(success~  rule +sex+ age + session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_sex)
#
Model_rule<- glmer(success~  rule+ age+   session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rule)
#
Model_optimum<- glmer(success~   age+ session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_optimum)
#
Model_age<- glmer(success~   session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_age)
#
Model_session<- glmer(success~   age + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


Baseline<- glmer(success~   (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
###
anova(Model_age, Model_optimum)
anova(Model_session, Model_optimum)
###
table<-anova( Model_trial, Model_sex,Model_rule,Model_optimum)
table
write.table(table, file = "tabler2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)



###############################
####ONLY FEMALES
##################
DATAREVERSF<-subset(DATAREVERS, sex=='F')
##
ModelruleF<- glmer(success~ rule + rank+ age + session + trial+ (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(ModelruleF)

t<-coef(summary(ModelruleF))

write.table(t, file = "tableruleF.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

####

#######ANOVA BACKWARD
##################################

Model_age<- glmer(success~ rule + rank+ age + session + trial+ (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_age)
##
Model_trial<- glmer(success~ rule + rank+ session + trial+ (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_trial)
#
Model_rule<- glmer(success~ rule + rank+ session + (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rule)
#
Model_rank<- glmer(success~  rank+ session + (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rank)

#
Model_session<- glmer(success~  session + (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_session)



Baseline<- glmer(success~   (1|subj_id), data = DATAREVERSF,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
###
anova(Model_age, Model_optimum)
anova(Model_session, Model_optimum)
###
table<-anova( Model_age, Model_trial,Model_rule,Model_rank,Model_session, Baseline)
table
write.table(table, file = "tabler2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


##################################################
###############################################
###NUMBER OF TAPS
################

DATAtaps<-subset(DATAREVERS, success==0)#number of taps on the wrong stimulus

###
#Full model

Modeltaps<-lmer(log10(nb_taps)~rule+ sex+ age +  trial+  session + (1|subj_id),data= DATAtaps)
summary(Modeltaps)


Model0<-lmer(log10(nb_taps)~ 1+ (1|subj_id),data= DATAtaps)
anova(Modeltaps, Model0)

#################
#EXPRT DATA
t<-coef(summary(Modeltaps))

write.table(t, file = "tabletaps.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
##########
# VISUAL Check normality of residuals

qqnorm(residuals(Modeltaps)) 

hist(residuals(Modeltaps))

#####

#ANOVA BACKWARD

Model_sex<-lmer(log10(nb_taps)~ sex+ age+  rule+ session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_sex)
#
Model_age<-lmer(log10(nb_taps)~ age+  rule+ session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_age)
#
Model_optimum<-lmer(log10(nb_taps)~ rule+  session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_optimum)
###
####
Model_rule<-lmer(log10(nb_taps)~   session + trial+ (1|subj_id),data= DATAtaps)

Model_trial<-lmer(log10(nb_taps)~    rule +session +  (1|subj_id),data= DATAtaps)

Model_session<-lmer(log10(nb_taps)~   rule+  trial +  (1|subj_id),data= DATAtaps)
##
anova(Model_optimum,Model_rule)
anova(Model_optimum,Model_trial)
anova(Model_optimum,Model_session)
#
Baseline<-lmer(log10(nb_taps)~   1 + (1|subj_id),data= DATAtaps)

table<-anova( Model_sex,Model_age, Model_optimum)

#table<-anova (Model_optimum, Model_session)
anova(Model_optimum, Baseline)

write.table(table, file = "T5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

########################
###########ONLY FEMALES
########################
DATAtapsF<-subset(DATAtaps, sex=='F')



###
#Full model

ModeltapsF<-lmer(log10(nb_taps)~rule+ rank+age +  trial+  session + (1|subj_id),data= DATAtapsF)
summary(ModeltapsF)


Model0<-lmer(log10(nb_taps)~ 1+ (1|subj_id),data= DATAtapsF)
anova(ModeltapsF, Model0)

#################
#EXPRT DATA
t<-coef(summary(ModeltapsF))

write.table(t, file = "tabletapsF.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
##########
# VISUAL Check normality of residuals

qqnorm(residuals(ModeltapsF)) 

hist(residuals(ModeltapsF))

#####

#ANOVA BACKWARD

Model_rule<-lmer(log10(nb_taps)~ rule+ rank+age +  trial+  session +(1|subj_id),data= DATAtapsF)
summary(Model_rule)
#
Model_rank<-lmer(log10(nb_taps)~ rank+ age+   session + trial+ (1|subj_id),data= DATAtapsF)
summary(Model_rank)
#
Model_age<-lmer(log10(nb_taps)~ age+  session + trial+ (1|subj_id),data= DATAtapsF)
summary(Model_age)
###
Model_optimum<-lmer(log10(nb_taps)~  session + trial+ (1|subj_id),data= DATAtapsF)
summary(Model_optimum)


####
Model_trial<-lmer(log10(nb_taps)~session +  (1|subj_id),data= DATAtapsF)

Model_session<-lmer(log10(nb_taps)~   trial +  (1|subj_id),data= DATAtapsF)
##

anova(Model_optimum,Model_trial)
anova(Model_optimum,Model_session)
#
Baseline<-lmer(log10(nb_taps)~   1 + (1|subj_id),data= DATAtapsF)

table<-anova( Model_rule, Model_rank, Model_age, Model_optimum)
table

anova(Model_optimum, Baseline)

write.table(table, file = "taps.txt", sep = "\t",
            row.names = TRUE, col.names = NA)




############################
############RT when success
############################


DATARL<-subset(DATAREVERS, success==1)## Look at response latency when success
######

#####BACKWARD SELECTION
#full model
ModelRT<-lmer(log(react_time)~ rule +  sex+ age+   trial +session +(1|subj_id),data= DATARL)
summary(ModelRT)

# VISUAL Check normality of residuals

qqnorm(residuals(ModelRT))
hist(residuals(ModelRT))
#
#export the table
t<-coef(summary(ModelRT))

write.table(t, file = "tableRT.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#############

Model_age<-lmer(log(react_time)~  rule +sex+ age+ trial +session +  (1|subj_id),data= DATARL)
summary(Model_age)
#
Model_trial<-lmer(log(react_time)~  rule +sex+ trial +session +  (1|subj_id),data= DATARL)
summary(Model_trial)
#
Model_sex<-lmer(log(react_time)~  rule +sex+ session + (1|subj_id),data= DATARL)
summary(Model_sex)
#
Model_optimum<-lmer(log(react_time)~  rule + session +(1|subj_id),data= DATARL)
summary(Model_optimum)
######################################################
#
Model_rule<-lmer(log(react_time)~ sex+ (1|subj_id),data= DATARL)

Model_session<-lmer(log(react_time)~ rule +(1|subj_id),data= DATARL)

anova(Model_sex ,Model_optimum)
#
Baseline<-lmer(log(react_time)~ 1+ (1|subj_id),data= DATARL)

anova(Baseline, Model_optimum)
anova(Model_optimum, Model_rule)
anova(Model_optimum,Model_session)

table<-anova(Model_age,Model_trial,Model_sex, Model_optimum)
table

write.table(table, file = "TRL.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
####
########################
####ONLY FEMALES
########################
DATARLF<-subset(DATARL, sex=='F')

ModelRTF<-lmer(log(react_time)~ rule +   age+  rank + trial +session +(1|subj_id),data= DATARLF)
summary(ModelRFT)

# VISUAL Check normality of residuals

qqnorm(residuals(ModelRTF))
hist(residuals(ModelRTF))
#
#export the table
t<-coef(summary(ModelRTF))

write.table(t, file = "tableRTF.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#############

Model_session<-lmer(log(react_time)~  rule + age+ rank+trial +session +  (1|subj_id),data= DATARLF)
summary(Model_session)
#
Model_trial<-lmer(log(react_time)~ age+ rule+ rank+trial+  (1|subj_id),data= DATARLF)
summary(Model_trial)
#
Model_age<-lmer(log(react_time)~  age +rule + rank +(1|subj_id),data= DATARLF)
summary(Model_age)
#
Model_rank<-lmer(log(react_time)~  rule +rank + (1|subj_id),data= DATARLF)
summary(Model_rank)
#
Model_rule<-lmer(log(react_time)~  rule + (1|subj_id),data= DATARLF)
summary(Model_optimum)
######################################################
#

#
Baseline<-lmer(log(react_time)~ 1+ (1|subj_id),data= DATARLF)

anova(Baseline, Model_rule)


table<-anova(Model_session,Model_trial,Model_age, Model_rank,Model_rule, Baseline)
table 

write.table(table, file = "TRLF.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
####
#########
####################################
################NB TRIALS TO SUCCESS
####################################

DATAREVERS<-read.csv("C:/Users/Marine/Desktop/PORTSMOUTH/ARTICLE INDIV DIFF/DATA/NBTRIAL.csv", header=TRUE)

summary(DATAREVERS)
########

#REPEATABILITY
rpt(perf~ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Poisson", nboot =100, npermut =  100)
#ADJ
rpt(perf~ age+ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Poisson", nboot =100, npermut =  100)

###########


#FULL MODEL

Modelnbtrials<- glmer(perf~ rule +sex + age +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(Modelnbtrials)
#LOOK FOR NORMALITY OF RESIDUALS

library(DHARMa)
res = simulateResiduals(Modelnbtrials)
plot(res)
##EXPORT THE TABLE

t<-coef(summary(Modelnbtrials))

write.table(t, file = "tableNbT.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
############################################################################
##SELECTION OF VARIABLES BACKWARD

##############################

#
Model_sex<- glmer(perf~ rule +sex + age +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(Model_sex)
#
Model_optimum<- glmer(perf~ rule + age +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(Model_optimum)
#

Model_rule<- glmer(perf~  age +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Model_age<- glmer(perf~  rule +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

Baseline<- glmer(perf~ 1 +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

#interaction age et rule
Model_P<- glmer(perf~  rule + age+ (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Model_I<- glmer(perf~  rule *age + (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

anova(Model_P, Model_I)
##

anova(Model_rule, Model_optimum)
anova(Model_age, Model_optimum)
anova(Model_optimum, Baseline)

table<-anova( Model_sex,  Model_optimum)


write.table(table, file = "table5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#############
##############ONLY FEMALES
#########################


DATAREVERSF<-read.csv("C:/Users/Marine/Desktop/PORTSMOUTH/ARTICLE INDIV DIFF/DATA/NBTRIAL.csv", header=TRUE)
DATAREVERSF<-subset(DATAREVERSF, sex=='         F')
summary(DATAREVERSF)
########
##########


#FULL MODEL

ModelnbtrialsF<- glmer(perf~ rank+ age + rule+  (1|subj_id), data = DATAREVERSF,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(ModelnbtrialsF)
#LOOK FOR NORMALITY OF RESIDUALS

library(DHARMa)
res = simulateResiduals(Modelnbtrials)
plot(res)
##EXPORT THE TABLE

t<-coef(summary(ModelnbtrialsF))

write.table(t, file = "tableNbTF.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
############################################################################
##SELECTION OF VARIABLES BACKWARD

##############################
###########################

############################
#
Model_rank<- glmer(perf~ rank+ age+ rule +  (1|subj_id), data = DATAREVERSF,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(Model_rank)
#
Model_age<- glmer(perf~ rule + age +  (1|subj_id), data = DATAREVERSF,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(Model_age)
#
Model_rule<- glmer(perf~ rule +  (1|subj_id), data = DATAREVERSF,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

Baselinee<- glmer(perf~ 1 +  (1|subj_id), data = DATAREVERSF,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


anova(Baseline, Model_rule)
anova(Model_rule, Model_optimum)
anova(Model_age, Model_optimum)


table<-anova( Model_rank,  Model_age ,Model_rule, Baseline)
table

write.table(table, file = "table5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)











###############################

##LOOKING AT RULE BY RULE
DATAREVERS<-read.csv("C:/Users/Marine/Desktop/PORTSMOUTH/ARTICLE INDIV DIFF/DATA/NBTRIAL.csv", header=TRUE)

DATAREVERS<-subset(DATAREVERS, rule==1)

Model_sex<- glmer(perf~ sex + age +(1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rank)
##
Model_age<- glmer(perf~ sex +  (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_sex)


Baseline<- glmer(perf~  1 + (1|subj_id), data = DATAREVERS,family=poisson,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(Model_sex,Model_age)






#####GRAPH
DATAREVERS$rule<-as.factor(DATAREVERS$rule)
ggplot(DATAREVERS, aes(x=age, y=perf, color=rule, shape=rule)) + 

  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_classic(base_size = 22)+
  labs( x = "Age", y = "Number of trials to learn the rules")

