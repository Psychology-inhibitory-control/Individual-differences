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
####################################
####################################T####################

##ACCURACY

DATAREVERS<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/REVERSAL.csv", header=TRUE)

DATAREVERS$rule<-as.factor(DATAREVERS$rule)


#DATAREVERS$success<-as.factor(DATAREVERS$success)

DATAREVERS <- na.omit(DATAREVERS)


summary(DATAREVERS)

##########
#REPEATABILITY

rpt(success~ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Binary", nboot =1000, npermut =  1000)

rpt(success~ session +(1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Binary", nboot =100, npermut =  100)



#################

Modelrule<- glmer(success~ rule +  sex + age + rank  + session + trial+ (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Modelrule)

t<-coef(summary(Modelrule))

write.table(t, file = "tabler2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#######ANOVA FORWARD
##################################
Baseline<-glmer(success~   (1|subj_id),  data = DATAREVERS,family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Model_sex=update(Baseline, . ~ .+ sex)
Model_age=update(Model_sex, . ~ .+ age)
Model_rank=update(Model_age, . ~ .+ rank)
Model_groupsize=update(Model_rank,. ~ .+ group_size)
Model_trial=update(Model_groupsize,. ~ .+ trial)
Model_session=update(Model_trial,. ~ .+ session)

table<-anova(Baseline, Model_sex, Model_age, Model_rank,  Model_groupsize, Model_trial, Model_session)


write.table(table, file = "T7.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#######ANOVA BACKWARD
##################################

Model_rank<- glmer(success~  rule +sex+ age+ rank  + trial + session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rank)
#
Model_trial<- glmer(success~  rule +sex+ age + trial + session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_trial)
#
Model_sex<- glmer(success~  rule +sex+ age+   session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_sex)
#
Model_rule<- glmer(success~  rule + age+ session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_rule)
#
Model_age<- glmer(success~  age+  session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(Model_age)
#
Model_session<- glmer(success~   session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


Baseline<- glmer(success~   (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
###
anova(Model_rule, Model_optimum)

table<-anova( Model_rank, Model_trial,Model_sex,Model_rule, Model_age,Model_session ,Baseline)

write.table(table, file = "tabler2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)



Model_age<- glmer(success~   session + (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
Model_session<- glmer(success~  age+ (1|subj_id), data = DATAREVERS,family=binomial,  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))


##################################################
###############################################
###NB TAPS
################

DATAtaps<-subset(DATAREVERS, success==0)
#DATAtaps<-subset(DATAREVERS, session==1)
Modeltaps<-lmer(nb_taps~rule+ sex+ age + rank+  trial+  session + (1|subj_id),data= DATAtaps)
summary(Modeltaps)
Model0<-lmer(nb_taps~ 1+ (1|subj_id),data= DATAtaps)
anova(Modeltaps, Model0)

#REPEATABILITY
rpt(nb_taps~ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Poisson", nboot =1000, npermut =  1000)

#ADJ

rpt(nb_taps~ trial + session +(1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Poisson", nboot =100, npermut =  100)

#################

t<-coef(summary(Modeltaps))

write.table(t, file = "tabletaps.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

##ANOVA FORWARD
Baseline<-lmer(nb_taps~ 1+ (1|subj_id),data= DATAtaps)
Model_sex= update(Baseline, . ~ .+ sex)
Model_age=update(Model_sex, . ~ .+ age)
Model_rank=update(Model_age, . ~ .+ rank)
Model_groupsize=update(Model_rank,. ~ .+ group_size)
Model_rule=update(Model_groupsize,. ~ .+ rule)
Model_trial=update(Model_rule,. ~ .+ trial)
Model_session=update(Model_trial,. ~ .+ session)

table<-anova(Baseline, Model_sex, Model_age, Model_rank, Model_groupsize, Model_rule, Model_trial, Model_session)


write.table(table, file = "T5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

##########

##########

#ANOVA BACKWARD
Model_rank<-lmer(nb_taps~ sex+ age+ rank+  rule+ session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_rank)
#
Model_sex<-lmer(nb_taps~ sex+ age+  rule+ session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_sex)
#
Model_rule<-lmer(nb_taps~ age+  rule+ session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_rule)
#
Model_age<-lmer(nb_taps~  age+   session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_age)
#
Model_optimum<-lmer(nb_taps~   session + trial+ (1|subj_id),data= DATAtaps)
summary(Model_)
###
################################################
Model_trial<-lmer(nb_taps~    session +  (1|subj_id),data= DATAtaps)
summary(Model_)
#
Model_session<-lmer(nb_taps~    trial +  (1|subj_id),data= DATAtaps)
summary(Model_)


#
Baseline<-lmer(nb_taps~   1 + (1|subj_id),data= DATAtaps)

table<-anova(Model_rank,  Model_sex, Model_rule, Model_age, Model_optimum)

table<-anova (Model_optimum, Model_session)
anova(Model_optimum, Baseline)

write.table(table, file = "T5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)




#########
############RT when success



DATARL<-subset(DATAREVERS, success==1)


############


##########
#REPEATABILITY
rpt(react_time~ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Gaussian", nboot =1000, npermut =  1000)

#ADJUSTED R
rpt(react_time~ rule + (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Gaussian", nboot =1000, npermut =  1000)
######



#DATAtaps<-subset(DATAREVERS, session==1)
Model_session<-lmer(react_time~  rule +sex+ age+ rank+trial +session +  (1|subj_id),data= DATARL)
summary(Model_session)
#
Model_age<-lmer(react_time~  rule +sex+ age+ rank+trial +  (1|subj_id),data= DATARL)
summary(Model_age)
#
Model_rank<-lmer(react_time~  rule +sex+ rank+trial + (1|subj_id),data= DATARL)
summary(Model_rank)
#
Model_trial<-lmer(react_time~  rule +sex+trial +  (1|subj_id),data= DATARL)
summary(Model_trial)
#
Model_optimum<-lmer(react_time~  rule +sex+  (1|subj_id),data= DATARL)
summary(Model_)
######################################################
#
Model_rule<-lmer(react_time~ sex+ (1|subj_id),data= DATARL)

Model_sex<-lmer(react_time~ rule +(1|subj_id),data= DATARL)


#
Baseline<-lmer(react_time~ 1+ (1|subj_id),data= DATARL)

anova(Baseline, Model_optimum)


table<-anova(Model_session,Model_age,Model_rank,  Model_trial,Model_optimum)
table<-anova(Model_optimum, Model_sex)

write.table(table, file = "T5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
####
t<-coef(summary(Model_rank))

write.table(table, file = "tabler2.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

##ANOVA FORWARD
Baseline<-lmer(react_time~ 1+ (1|subj_id),data= DATARL)
Model_sex= update(Baseline, . ~ .+ sex)
Model_age=update(Model_sex, . ~ .+ age)
Model_rank=update(Model_age, . ~ .+ rank)
Model_groupsize=update(Model_rank,. ~ .+ group_size)
Model_rule=update(Model_groupsize,. ~ .+ rule)
Model_trial=update(Model_rule,. ~ .+ trial)
Model_session=update(Model_trial,. ~ .+ session)

table<-anova(Baseline, Model_sex, Model_age, Model_rank, Model_groupsize, Model_rule, Model_trial, Model_session)

anova(Model_rank, Model_sex)
write.table(table, file = "T5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)



#########
####################################
################NB TRIALS TO SUCCESS
####################################

DATAREVERS<-read.csv("C:/Users/Marine/Desktop/ARTICLE INDIV DIFF/DATA/NBTRIAL.csv", header=TRUE)

ggplot(DATAREVERS, aes(x=age, y=perf), color=rule) + 
  geom_point()+
  geom_smooth(method=lm, se=TRUE)



summary(DATAREVERS)
hist(DATAREVERS$perf)
Model1<-lmer(perf~  sex+age+ rank+ group_size+ group+ rule+ (1|subj_id),data= DATAREVERS)
Model0<-lmer(perf~ 1+ (1|subj_id),data= DATAREVERS)
anova(Model0, Model1)
summary(Model1)

t<-coef(summary(Model1))

write.table(t, file = "table5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

########
#REPEATABILITY
rpt(perf~ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Poisson", nboot =100, npermut =  100)
#ADJ
rpt(perf~ age+ (1 | subj_id), grname = "subj_id", data = DATAREVERS, datatype = "Poisson", nboot =100, npermut =  100)


#############FORWARD

Baseline<-lmer(perf~ 1+ (1|subj_id),data= DATAREVERS)
Model_sex= update(Baseline, . ~ .+ sex)
Model_age=update(Model_sex, . ~ .+ age)
Model_rank=update(Model_age, . ~ .+ rank)
Model_groupsize=update(Model_rank,. ~ .+ group_size)
Model_rule=update(Model_groupsize,. ~ .+ rule)

summary(Model_rule)
table<-anova(Baseline, Model_sex, Model_age, Model_rank, Model_groupsize, Model_rule)


write.table(table, file = "T3.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

############################################################################
##BACKWARD
##############################
Model_rank<-lmer(perf~ rule + age+ sex+ rank+   (1|subj_id),data= DATAREVERS)
summary(Model_rank)
#
Model_sex<-lmer(perf~ rule + age+ sex+  (1|subj_id),data= DATAREVERS)
summary(Model_sex)
#
Model_rule<-lmer(perf~ rule + age+  (1|subj_id),data= DATAREVERS)
summary(Model_rule)
#
Model_age<-lmer(perf~  age+  (1|subj_id),data= DATAREVERS)
summary(Model_age)



Baseline<-lmer(perf~ 1+    (1|subj_id),data= DATAREVERS)

table<-anova( Model_rank,Model_sex,  Model_rule, Model_age, Baseline)
anova(Baseline, Model_age)

table<-coef(summary(Model_rank))


write.table(table, file = "table5.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#############FORWARD
##

DATAplot<-read.csv("C:/Users/Marine/Desktop/RMRC/DATA/DATATASK4_RAW.csv", header=TRUE)
# only monkeys below threshold

DATAplot<-subset(DATAplot, time_point==1)

#MODEL NULL
####

DATAplot = aggregate(DATAplot$success, by=list(DATAplot$subj_id,  DATAplot$age,  DATAplot$rule,DATAplot$sex, DATAplot$rank, DATAplot$group_size), mean,na.rm=TRUE)
colnames(DATAplot) = c('subj_id',  'age','rule', 'sex','rank','group_size', 'perf')


ggplot(DATAplot, aes(x=age, y=perf), color=rule) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

##########

kruskal.test(success ~ group, data = DATAREVERS)
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=DATAREVERS$success, g=DATAREVERS$group, method="Chisq")

library(dplyr)
group_by(DATAREVERS, group) %>%
  summarise(
    count = n(),
    mean = mean(success, na.rm = TRUE),
    sd = sd(success, na.rm = TRUE)
  )

res.aov <- aov(success ~ group, data = DATAREVERS)

# Summary of the analysis
summary(res.aov)
##

plot(res.aov, 1)
plot(res.aov, 2)

aov_residuals <- residuals(object = res.aov )
DATAREVERS= aggregate((DATAREVERS$success), by=list(DATAREVERS$subj_id, DATAREVERS$group, DATAREVERS$session), mean, na.rm=TRUE)
colnames(DATAREVERS) = c('subj_id',  'group','session', 'success' )

res.aov <- aov(success ~ group, data = DATAREVERS)
# Summary of the analysis
summary(res.aov)
