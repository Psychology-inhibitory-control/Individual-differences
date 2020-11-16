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
library(MuMIn)
library(rptR)
####################################
####################################T####################

DATAGONO<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/GONOGO.csv", header=TRUE)


#DATAGONO$goodresp<-as.factor(DATAGONO$goodresp)

DATAGONO<-subset(DATAGONO,type=="NoGo")#Look at Nogo trials

DATAGONO = droplevels(DATAGONO)
summary(DATAGONO)

###############################
##FOR ACCURACY ON NOGO TRIALS

##############################
#REPEATABILITY

rpt(goodresp~ (1 | subj_id), grname = "subj_id", data = DATAGONO, datatype = "Binary", nboot =1000, npermut =  1000)

#####################
#MODEL ANALYSIS

#full model

Modelgono<- glmer(goodresp~  sex +  age + rank+  trial +session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(Modelgono)

#LOOK FOR NORMALITY OF RESIDUALS

library(DHARMa)
res = simulateResiduals(Modelstroop)
plot(res)

##export the table

t<-coef(summary(Modelgono))

write.table(t, file = "tablegono.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

###########

###BACKWARD ACCURACY ANALYSIS

Model_sex<- glmer(goodresp~  sex +  age + rank+  trial +session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_sex)
#
Model_trial<- glmer(goodresp~ age + rank+  trial +session  +  (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_trial)
#
Model_age<- glmer(goodresp~  age + rank+ session  + (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_age)
#
Model_rank<- glmer(goodresp~  rank +session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rank)
#
Model_session<- glmer(goodresp~    session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_session)


Baseline<- glmer(goodresp~     (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


anova( Model_sex,Model_trial ,Model_age,  Model_rank,Model_session, Baseline )


write.table(table, file = "T4.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


###############################
##FOR RESPONSE LATENCY ON NOGO TRIALS

##############################

#REPEATABILITY

rpt(log(react_time)~ (1 | subj_id), grname = "subj_id", data = DATAGONO, datatype = "Gaussian", nboot =1000, npermut =  1000)

##########

##################

#full model 
ModelRT<-lmer(log(react_time)~  sex+ age+  rank +  trial +session +(1|subj_id),data= DATAGONO)
summary(ModelRT)

# VISUAL Check normality of residuals

qqnorm(residuals(ModelRT))
hist(residuals(ModelRT))
###
#Compare Model null model full
ModelRT<-lmer(log(react_time)~  sex+ age+  rank +  trial +session +(1|subj_id),data= DATAGONO)
Model0<-lmer(log(react_time)~  1 +(1|subj_id),data= DATAGONO)
anova(Model0, ModelRT)

###

#export the table
t<-coef(summary(ModelRT))

write.table(t, file = "tableRT.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

##########################################


Model_session<- lmer(log(react_time)~  sex+ age+  rank   +trial +session +(1|subj_id),data= DATAGONO, REML = FALSE)
summary(Model_session)
#
Model_age<- lmer(log(react_time)~  sex+ age+  rank  +trial +(1|subj_id),data= DATAGONO, REML = FALSE)
summary(Model_age)
#
Model_trial<- lmer(log(react_time)~  sex+  rank +trial +(1|subj_id),data= DATAGONO, REML = FALSE)
summary(Model_trial)
#
Model_sex<- lmer(log(react_time)~  sex+ rank + (1|subj_id),data= DATAGONO, REML = FALSE)
summary(Model_sex)
#
Model_rank<- lmer(log(react_time)~  rank+ (1|subj_id),data= DATAGONO, REML = FALSE)
summary(Model_rank)
#
Baseline<- lmer(log(react_time)~  1 +(1|subj_id),data= DATAGONO, REML = FALSE)
#

table<-anova(Model_age,Model_sex, Model_trial,Model_rank,  Model_session, Baseline )


write.table(table, file = "T1.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
############


