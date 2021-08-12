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
########################################################
#########################################################

DATAGONO<-read.csv("C:/Users/Marine/Desktop/PORTSMOUTH/ARTICLE INDIV DIFF/DATA/GONOGO.csv", header=TRUE)


DATAGONO<-subset(DATAGONO,type=="NoGo")#Look at Nogo trials

DATAGONO = droplevels(DATAGONO)
summary(DATAGONO)

###############################
##FOR ACCURACY ON NOGO TRIALS

##############################
#REPEATABILITY
DATAGONOrpt<-read.csv("C:/Users/Marine/Desktop/PORTSMOUTH/ARTICLE INDIV DIFF/DATA/GONOGOrpt.csv", header=TRUE)

rpt(goodresp~ (1 | subj_id), grname = "subj_id", data = DATAGONOrpt, datatype = "Binary", nboot =1000, npermut =  1000)

#####################
#MODEL ANALYSIS

Modelgono<- glmer(goodresp~   sex + age + trial +session + sex*session+  (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Modelgono)

#LOOK FOR NORMALITY OF RESIDUALS

library(DHARMa)
res = simulateResiduals(Modelgono)
plot(res)

##export the table

t<-coef(summary(Modelgono))

write.table(t, file = "tablegono.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
#####
#BACKWARD REGRESSION

Modelage<- glmer(goodresp~   sex + age + trial +session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Modelsex<- glmer(goodresp~   sex +  trial +session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Modelsession<- glmer(goodresp~   trial +session +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Modeltrial<- glmer(goodresp~   trial +  (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Baseline<- glmer(goodresp~   1 +   (1|subj_id), data = DATAGONO,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

table<-anova(Modelage, Modelsex, Modelsession ,Modeltrial,Baseline)


write.table(table, file = "Table.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#################################################
#full model Females
DATAGONOF<-subset(DATAGONO, sex=='F')

ModelgonoF<- glmer(goodresp~    age + rank+  trial +session +   (1|subj_id), data = DATAGONOF,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(ModelgonoF)

#full model for Males

DATAGONOM<-subset(DATAGONO, sex=='         M')

ModelgonoM<- glmer(goodresp~    age +   trial +session +   (1|subj_id), data = DATAGONOM,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(ModelgonoM)

#LOOK FOR NORMALITY OF RESIDUALS

library(DHARMa)
res = simulateResiduals(ModelgonoF)
plot(res)

##export the table

t<-coef(summary(ModelgonoM))

write.table(t, file = "tablegonM.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

###########

###BACKWARD ACCURACY ANALYSIS for F

Model_rank<- glmer(goodresp~    age + rank+  trial +session +   (1|subj_id), data = DATAGONOF,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rank)
#
Model_age<- glmer(goodresp~ age + trial +session  +  (1|subj_id), data = DATAGONOF,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_age)
#
Model_trial<- glmer(goodresp~  trial + session  + (1|subj_id), data = DATAGONOF,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_trial)
#
Model_session<- glmer(goodresp~  session +   (1|subj_id), data = DATAGONOF,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_session)
#


anova(Model_session, Baseline)

Baseline<- glmer(goodresp~     (1|subj_id), data = DATAGONOF,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


anova( Model_rank,Model_age ,Model_trial, Model_session)


write.table(table, file = "T4.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
###BACKWARD SELECTION FOR M
Modelage<- glmer(goodresp~    age +   trial +session +   (1|subj_id), data = DATAGONOM,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Modeltrial<- glmer(goodresp~     trial +session +   (1|subj_id), data = DATAGONOM,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Modelsession<- glmer(goodresp~   session +   (1|subj_id), data = DATAGONOM,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Baseline<- glmer(goodresp~  1 +   (1|subj_id), data = DATAGONOM,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

TM<-anova(Modelage,Modeltrial,Modelsession,Baseline)
write.table(TM, file = "TM.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

###############################
##FOR RESPONSE LATENCY ON NOGO TRIALS

##############################

#REPEATABILITY

rpt(log(react_time)~ (1 | subj_id), grname = "subj_id", data = DATAGONOrpt, datatype = "Gaussian", nboot =1000, npermut =  1000)

##########

##################

#full model 
ModelRT<-lmer(log(react_time)~  sex+ age+  trial +session +(1|subj_id),data= DATAGONO)

summary(ModelRT)
##
ModelRTF<-lmer(log(react_time)~  rank+age+  trial +session +(1|subj_id),data= DATAGONOF)
summary(ModelRTF)
# VISUAL Check normality of residuals

qqnorm(residuals(ModelRT))
hist(residuals(ModelRT))
###
#Compare Model null model full
ModelRT<-lmer(log(react_time)~  sex+ age+   trial +session +(1|subj_id),data= DATAGONO)
Model0<-lmer(log(react_time)~  1 +(1|subj_id),data= DATAGONO)
anova(Model0, ModelRT)

###

#export the table
t<-coef(summary(ModelRTF))

write.table(t, file = "tableRTF.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

##########################################
Modelsex<-lmer(log(react_time)~  sex + age+  trial +session +(1|subj_id),data= DATAGONO)
Modeltrial<-lmer(log(react_time)~  age+  trial +session +(1|subj_id),data= DATAGONO)
Modelsession<-lmer(log(react_time)~  age+  session +(1|subj_id),data= DATAGONO)
Modelage<-lmer(log(react_time)~ age+ (1|subj_id),data= DATAGONO)
Baseline<-lmer(log(react_time)~ 1+ (1|subj_id),data= DATAGONO)
summary(Modelage)

table<-anova(Modelsex, Modeltrial, Modelsession ,Modelage ,Baseline)
table

write.table(table, file = "Table.txt", sep = "\t",
            row.names = TRUE, col.names = NA)




##########################

Model_rank<- lmer(log(react_time)~ age+  rank   +trial +session +(1|subj_id),data= DATAGONOF, REML = FALSE)
summary(Model_rank)
#
Model_age<- lmer(log(react_time)~   age +trial + session +(1|subj_id),data= DATAGONOF, REML = FALSE)
summary(Model_age)
#
Model_session<- lmer(log(react_time)~ trial + session+ (1|subj_id),data= DATAGONOF, REML = FALSE)
summary(Model_session)
#
Model_trial<- lmer(log(react_time)~ trial+ (1|subj_id),data= DATAGONOF, REML = FALSE)
summary(Model_trial)

#
Baseline<- lmer(log(react_time)~  1 +(1|subj_id),data= DATAGONOF, REML = FALSE)
#
anova(Model_rank, Model_age)

anova(Baseline, Model_trial)
table<-anova(Model_rank, Model_age, Model_session, Model_trial)
table

write.table(table, file = "T1.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
############


