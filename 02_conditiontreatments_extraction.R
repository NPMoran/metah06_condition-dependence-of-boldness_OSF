##########_____Meta-analysis project (NC3 DO5)_____#########
##TITLE: Poor nutritional condition promotes high-risk behaviours: A systematic review and meta-analysis

#Principles used in data extraction:
#1-Use the finest scale data presented/obtained (e.g. from each repeat, from each treatment group)
#2-Factorial designs (diet X predation pressure treatments) are marginalised across non-focal treatment
#3-Where multiple treatment groups included when compared to a control/intermediate group
#4-Compound variables across multiple assays not extracted

###########################_################################

###DATA EXTRACTION FROM FIGURES ----
#
#library("devtools")
#devtools::install_github("daniel1noble/metaDigitise")
library(metaDigitise)
setwd("~/metah06_condition-dependence-of-boldness")

#requires folder to be created in the project folder called 'data_extraction_figs'
extracted.data <- metaDigitise(dir = "~/metah06_condition-dependence-of-boldness/data_extraction_figures")
write.csv(extracted.data, 
          file = "extracted_data.csv")


###########################_################################

###REPROCCESSING OF RAW DATA ----
setwd("~/metah06_condition-dependence-of-boldness/data_extraction_files")
library(data.table); library(dplyr)

#CD003 ---- 
dat <- read.csv("CD003_journal.pone.0187657.s004.csv", strip.white = TRUE) 
dat <- subset(dat, risk_taking_raw != "NA")
dat.1 <- subset(dat, repeat. == 1)
dat.1.long<-subset(dat.1, basking=="long")
dat.1.short<-subset(dat.1, basking=="short")
setDT(dat.1.long)[ , list(mean = mean(risk_taking_raw), sd = sd(risk_taking_raw)), 
              by = .(food)]
setDT(dat.1.short)[ , list(mean = mean(risk_taking_raw), sd = sd(risk_taking_raw)), 
                   by = .(food)]

dat.2 <- subset(dat, repeat. == 2)
dat.2.long<-subset(dat.2, basking=="long")
dat.2.short<-subset(dat.2, basking=="short")
setDT(dat.2.long)[ , list(mean = mean(risk_taking_raw), sd = sd(risk_taking_raw)), 
                   by = .(food)]
setDT(dat.2.short)[ , list(mean = mean(risk_taking_raw), sd = sd(risk_taking_raw)), 
                    by = .(food)]

dat.3 <- subset(dat, repeat. == 3)
dat.3.long<-subset(dat.3, basking=="long")
dat.3.short<-subset(dat.3, basking=="short")
setDT(dat.3.long)[ , list(mean = mean(risk_taking_raw), sd = sd(risk_taking_raw)), 
                   by = .(food)]
setDT(dat.3.short)[ , list(mean = mean(risk_taking_raw), sd = sd(risk_taking_raw)), 
                    by = .(food)]

#CD011 ----
dat1 <- read.csv("CD011_Experiment1.csv", strip.white = TRUE) 
dat2 <- read.csv("CD011_Experiment2.csv", strip.white = TRUE) 
#need to exlcude fish ID 8 in experiment 2, as was cannibalistic and so not resrticted
dat1 <- subset(dat1, ACT2 != "NA")
dat2 <- subset(dat2, ACT2 != "NA")
dat2 <- subset(dat2, ID != 8)
nrow(subset(dat1, TR == "LR"))
nrow(subset(dat1, TR == "HR"))
nrow(subset(dat2, TR == "LH"))
nrow(subset(dat2, TR == "HH"))
setDT(dat1)[ , list(mean = mean(ACT2), sd = sd(ACT2)), 
                                       by = .(TR)]
setDT(dat2)[ , list(mean = mean(ACT2), sd = sd(ACT2)), 
                                       by = .(TR)]

#CD033 ----
dat <- read.csv("CD033_Andrews et al Supplementary material (data) risk task.csv", strip.white = TRUE) 
dat<-subset(dat, TrialType=="choice")
summary(dat$BirdID)
dat$Sex<-as.numeric(dat$Sex)-1
dat$Amount<-as.numeric(dat$Amount)-1
dat$Effort<-as.numeric(dat$Effort)-1
dat$ChoiceMade<-(as.numeric(dat$ChoiceMade)-1)/2
#calculating the proportion of risky choices, average latency to choose, and average latency to initiate trial per individual
dat.byID<-setDT(dat)[ , list(age = mean(EntryAge), 
                                       sex = mean(Sex), 
                                       amount = mean(Amount), 
                                       effort = mean(Effort), 
                                       choice = mean(ChoiceMade),
                                       mean_latinit = mean(LatInitiate), 
                                       mean_latchoose = mean(LatChoose)), 
                                by = .(BirdID)]
dat.F<-subset(dat.byID, sex == 0)
nrow(subset(dat.F,amount==0))
nrow(subset(dat.F,amount==1))
dat.M<-subset(dat.byID, sex == 1)
nrow(subset(dat.M,amount==0))
nrow(subset(dat.M,amount==1))
setDT(dat.M)[ , list(meanrisk = mean(choice), 
                     sdrisk = sd(choice), 
                     meanlatinit = mean(mean_latinit), 
                     sdlatinit = sd(mean_latinit),
                     meanlatchoose = mean(mean_latchoose), 
                     sdlatchoose = sd(mean_latchoose)), 
              by = .(amount)]
setDT(dat.F)[ , list(meanrisk = mean(choice), 
                     sdrisk = sd(choice), 
                     meanlatinit = mean(mean_latinit), 
                     sdlatinit = sd(mean_latinit),
                     meanlatchoose = mean(mean_latchoose), 
                     sdlatchoose = sd(mean_latchoose)), 
              by = .(amount)]
        
#CD159 ----
dat <- read.csv("CD159_development_assays.csv", strip.white = TRUE) 
summary(dat)
dat.trial1A<-subset(dat, web == 1)
dat.trial2A<-subset(dat, web == 2)
dat.trial3A<-subset(dat, web == 3)
CD159_attackrate1<-setDT(dat.trial1A)[ , list(mean_attack1 = mean(attacked)), 
                                      by = .(ID)]
CD159_attackrate2<-setDT(dat.trial2A)[ , list(mean_attack2 = mean(attacked)), 
                                     by = .(ID)]
CD159_attackrate3<-setDT(dat.trial3A)[ , list(mean_attack3 = mean(attacked)), 
                                     by = .(ID)]
merge1<-merge(CD159_attackrate1, CD159_attackrate2, by = "ID", all.x = TRUE)
merge2<-merge(merge1, CD159_attackrate3, by = "ID", all.x = TRUE)

dates<-subset(dat, behav.assay == "1")
dates<-subset(dates, location.order == "1")
dates1<-subset(dates, web == "1")
dates1$date1<-dates1$date
dates1<-dates1[, c("ID", "date1")]
dates2<-subset(dates, web == "2")
dates2$date2<-dates2$date
dates2<-dates2[, c("ID", "date2")]
dates3<-subset(dates, web == "3")
dates3$date3<-dates3$date
dates3<-dates3[, c("ID", "date3")]
merge3<-merge(merge2, dates1, by = "ID", all.x = TRUE)
merge4<-merge(merge3, dates2, by = "ID", all.x = TRUE)
merge5<-merge(merge4, dates3, by = "ID", all.x = TRUE)


dat.trial1B<-subset(dat.trial1A, retreated != "NA")
dat.trial2B<-subset(dat.trial2A, retreated != "NA")
dat.trial3B<-subset(dat.trial3A, retreated != "NA")

CD159_retreatrate1<-setDT(dat.trial1B)[ , list(mean_retreat1 = mean(retreated)), 
                                       by = .(ID)]
CD159_retreatrate2<-setDT(dat.trial2B)[ , list(mean_retreat2 = mean(retreated)), 
                                       by = .(ID)]
CD159_retreatrate3<-setDT(dat.trial3B)[ , list(mean_retreat3 = mean(retreated)), 
                                       by = .(ID)]
merge6<-merge(merge5, CD159_retreatrate1, by = "ID", all.x = TRUE)
merge7<-merge(merge6, CD159_retreatrate2, by = "ID", all.x = TRUE)
merge8<-merge(merge7, CD159_retreatrate3, by = "ID", all.x = TRUE)


dat <- read.csv("CD159_development_morph.csv", strip.white = TRUE) 
dat <-dat[, c("ID", "maturation")]
merge9<-merge(merge8, dat, by = "ID", all.x = TRUE)

dat <- read.csv("CD159_development_treatments.csv", strip.white = TRUE) 
dat <- subset(dat, web == 1)
dat <-dat[, c("ID", "developmental.treatment", "web.treatment")]
merge10<-merge(merge9, dat, by = "ID", all.x = TRUE)

merge10$date1a<-(as.Date(paste0(substr(merge10$date1,1,4),"/",substr(merge10$date1,5,6),"/",substr(merge10$date1,7,8))))
merge10$date2a<-(as.Date(paste0(substr(merge10$date2,1,4),"/",substr(merge10$date2,5,6),"/",substr(merge10$date2,7,8))))     
merge10$date3a<-(as.Date(paste0(substr(merge10$date3,1,4),"/",substr(merge10$date3,5,6),"/",substr(merge10$date3,7,8))))             
merge10$maturationa<-(as.Date(paste0(substr(merge10$maturation,1,4),"/",substr(merge10$maturation,5,6),"/",substr(merge10$maturation,7,8))))             

write.csv(merge10, file = "~/CD159_processed.csv") #means and sds, and time between dates1,2,3 and maturation date calculated externally

#CD174 ----
#note: split database and added 300s as a maximum latency for individuals that did not reach the relevant point in each task
dat <- read.csv("CD174_Duffield et al_single data (spatial task).csv", strip.white = TRUE) 
dat_small<-subset(dat, SIZE=="small")
dat_large<-subset(dat, SIZE=="large")

dat_small<-setDT(dat_small)[ , list(mean = mean(SPAT_DIVIDER), sd = sd(SPAT_DIVIDER)), 
                                       by = .(HUNGER)]
dat_large<-setDT(dat_large)[ , list(mean = mean(SPAT_DIVIDER), sd = sd(SPAT_DIVIDER)), 
                             by = .(HUNGER)]
dat_small
dat_large

dat <- read.csv("CD174_Duffield et al_single data (manip task).csv", strip.white = TRUE) 
dat_small<-subset(dat, SIZE=="small")
dat_large<-subset(dat, SIZE=="large")

setDT(dat_small)[ , list(mean = mean(MANIP_TO_BOX), sd = sd(MANIP_TO_BOX)), 
                             by = .(HUNGER)]
setDT(dat_large)[ , list(mean = mean(MANIP_TO_BOX), sd = sd(MANIP_TO_BOX)), 
                             by = .(HUNGER)]


#CD238 ----
dat <- read.csv("CD238_labwithnewt_warm.csv", strip.white = TRUE) 
nrow(subset(dat, Treatment == "Satiated"))
nrow(subset(dat, Treatment == "Hungry"))
CD238a<-setDT(dat)[ , list(mean = mean(Dragonfly_warm_patch), sd = sd(Dragonfly_warm_patch)), 
                                        by = .(Treatment)]
dat <- read.csv("CD238_labwithoutnewt_warm.csv", strip.white = TRUE) 
nrow(subset(dat, Treatment == "Satiated"))
nrow(subset(dat, Treatment == "Hungry"))
CD238b<-setDT(dat)[ , list(mean = mean(Warm_patch), sd = sd(Warm_patch)), 
                    by = .(Treatment)]
dat <- read.csv("CD238_seminatural_preyeaten.csv", strip.white = TRUE) 
nrow(subset(dat, Treatment == "Satiated"))
nrow(subset(dat, Treatment == "Hungry"))
CD238c<-setDT(dat)[ , list(mean = mean(Killed), sd = sd(Killed)), 
                    by = .(Treatment)]
dat <- read.csv("CD238_seminatural_warm2011.csv", strip.white = TRUE) 
dat$ID <- as.factor(dat$ID)
dat.treat<-subset(dat, Hour == "10:00")
nrow(subset(dat.treat, Treatment == "Satiated"))
nrow(subset(dat.treat, Treatment == "Hungry"))
CD238d<-setDT(dat)[ , list(mean = mean(Warm_parch)), 
                    by = .(ID)]
CD238d.merge<-merge(CD238d, dat.treat, by = "ID", all.x = TRUE)
CD238d<-setDT(CD238d.merge)[ , list(mean = mean(mean), sd = sd(mean)), 
                    by = .(Treatment)]
dat <- read.csv("CD238_seminatural_warm2012.csv", strip.white = TRUE) 
nrow(subset(dat, Treatment == "Satiated"))
nrow(subset(dat, Treatment == "Hungry"))
CD238e<-setDT(dat)[ , list(mean = mean(Warm_patch), sd = sd(Warm_patch)), 
                    by = .(Treatment)]

#CD248 ----
dat <- read.table("CD248_DATAFILE_HAN_DINGEMANSE_2017_PRSB.txt", header = TRUE, strip.white = TRUE) 
datf <- dat_small<-subset(dat, SEX=="f")
datm <- dat_small<-subset(dat, SEX=="m")
datf.trial1 <- dat_small<-subset(datf, TESTINGORDER == -1.5)
datf.trial2 <- dat_small<-subset(datf, TESTINGORDER == -0.5)
datf.trial3 <- dat_small<-subset(datf, TESTINGORDER == 0.5)
datf.trial4 <- dat_small<-subset(datf, TESTINGORDER == 1.5)
datm.trial1 <- dat_small<-subset(datm, TESTINGORDER == -1.5)
datm.trial2 <- dat_small<-subset(datm, TESTINGORDER == -0.5)
datm.trial3 <- dat_small<-subset(datm, TESTINGORDER == 0.5)
datm.trial4 <- dat_small<-subset(datm, TESTINGORDER == 1.5)
#imb is LC imbalanced diet, b is the balanced diet
nrow(subset(datf.trial1, DIET == "imb")); nrow(subset(datf.trial1, DIET == "b"))
nrow(subset(datf.trial2, DIET == "imb")); nrow(subset(datf.trial2, DIET == "b"))
nrow(subset(datf.trial3, DIET == "imb")); nrow(subset(datf.trial3, DIET == "b"))
nrow(subset(datf.trial4, DIET == "imb")); nrow(subset(datf.trial4, DIET == "b"))
nrow(subset(datm.trial1, DIET == "imb")); nrow(subset(datm.trial1, DIET == "b"))
nrow(subset(datm.trial2, DIET == "imb")); nrow(subset(datm.trial2, DIET == "b"))
nrow(subset(datm.trial3, DIET == "imb")); nrow(subset(datm.trial3, DIET == "b"))
nrow(subset(datm.trial4, DIET == "imb")); nrow(subset(datm.trial4, DIET == "b"))
setDT(datf.trial1)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datf.trial2)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datf.trial3)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datf.trial4)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datm.trial1)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datm.trial2)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datm.trial3)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]
setDT(datm.trial4)[ , list(mean = mean(EXPLORATION), sd = sd(EXPLORATION)), by = .(DIET)]

#CD321 ----
dat <- read.table("CD321_Krause Krüger Schielzeth_Behav.txt", header = TRUE, strip.white = TRUE) 
dat <- subset(dat, Age  != "NA") #removing rows without behavioural measures
datF <- subset(dat, Sex  == "F"); datM <- subset(dat, Sex  == "M")
dat1F <- subset(datF, Series == "D")
dat1M <- subset(datM, Series == "D")
dat2F <- subset(datF, Series == "E")
dat2M <- subset(datM, Series == "E")
dat3F <- subset(datF, Series == "F")
dat3M <- subset(datM, Series == "F")

dat1F$treat <- with(dat1F, interaction(TreatEarly,  TreatLate))
dat1M$treat <- with(dat1M, interaction(TreatEarly,  TreatLate))
dat2F$treat <- with(dat2F, interaction(TreatEarly,  TreatLate))
dat2M$treat <- with(dat2M, interaction(TreatEarly,  TreatLate))
dat3F$treat <- with(dat3F, interaction(TreatEarly,  TreatLate))
dat3M$treat <- with(dat3M, interaction(TreatEarly,  TreatLate))

nrow(subset(dat1F, treat == "L.L"))
nrow(subset(dat1F, treat == "L.H"))
nrow(subset(dat1F, treat == "H.L"))
nrow(subset(dat1F, treat == "H.H"))
nrow(subset(dat1M, treat == "L.L"))
nrow(subset(dat1M, treat == "L.H"))
nrow(subset(dat1M, treat == "H.L"))
nrow(subset(dat1M, treat == "H.H"))

nrow(subset(dat2F, treat == "L.L"))
nrow(subset(dat2F, treat == "L.H"))
nrow(subset(dat2F, treat == "H.L"))
nrow(subset(dat2F, treat == "H.H"))
nrow(subset(dat2M, treat == "L.L"))
nrow(subset(dat2M, treat == "L.H"))
nrow(subset(dat2M, treat == "H.L"))
nrow(subset(dat2M, treat == "H.H"))

nrow(subset(dat3F, treat == "L.L"))
nrow(subset(dat3F, treat == "L.H"))
nrow(subset(dat3F, treat == "H.L"))
nrow(subset(dat3F, treat == "H.H"))
nrow(subset(dat3M, treat == "L.L"))
nrow(subset(dat3M, treat == "L.H"))
nrow(subset(dat3M, treat == "H.L"))
nrow(subset(dat3M, treat == "H.H"))

setDT(dat1F)[ , list(mean.envlatency = mean(SE_FirstFeeding), sd.envlatency = sd(SE_FirstFeeding),
                     mean.envhops = mean(SE_Hops), sd.envhops = sd(SE_Hops),
                     mean.objlatency = mean(SO_FirstFeeding), sd.objlatency = sd(SO_FirstFeeding),
                     mean.objhops = mean(SO_Hops), sd.objhops = sd(SO_Hops)),
                             by = .(treat)]
setDT(dat1M)[ , list(mean.envlatency = mean(SE_FirstFeeding), sd.envlatency = sd(SE_FirstFeeding),
                     mean.envhops = mean(SE_Hops), sd.envhops = sd(SE_Hops),
                     mean.objlatency = mean(SO_FirstFeeding), sd.objlatency = sd(SO_FirstFeeding),
                     mean.objhops = mean(SO_Hops), sd.objhops = sd(SO_Hops)),
              by = .(treat)]
setDT(dat2F)[ , list(mean.envlatency = mean(SE_FirstFeeding), sd.envlatency = sd(SE_FirstFeeding),
                     mean.envhops = mean(SE_Hops), sd.envhops = sd(SE_Hops),
                     mean.objlatency = mean(SO_FirstFeeding), sd.objlatency = sd(SO_FirstFeeding),
                     mean.objhops = mean(SO_Hops), sd.objhops = sd(SO_Hops)),
              by = .(treat)]
setDT(dat2M)[ , list(mean.envlatency = mean(SE_FirstFeeding), sd.envlatency = sd(SE_FirstFeeding),
                     mean.envhops = mean(SE_Hops), sd.envhops = sd(SE_Hops),
                     mean.objlatency = mean(SO_FirstFeeding), sd.objlatency = sd(SO_FirstFeeding),
                     mean.objhops = mean(SO_Hops), sd.objhops = sd(SO_Hops)),
              by = .(treat)]
setDT(dat3F)[ , list(mean.envlatency = mean(SE_FirstFeeding), sd.envlatency = sd(SE_FirstFeeding),
                     mean.envhops = mean(SE_Hops), sd.envhops = sd(SE_Hops),
                     mean.objlatency = mean(SO_FirstFeeding), sd.objlatency = sd(SO_FirstFeeding),
                     mean.objhops = mean(SO_Hops), sd.objhops = sd(SO_Hops)),
              by = .(treat)]
setDT(dat3M)[ , list(mean.envlatency = mean(SE_FirstFeeding), sd.envlatency = sd(SE_FirstFeeding),
                     mean.envhops = mean(SE_Hops), sd.envhops = sd(SE_Hops),
                     mean.objlatency = mean(SO_FirstFeeding), sd.objlatency = sd(SO_FirstFeeding),
                     mean.objhops = mean(SO_Hops), sd.objhops = sd(SO_Hops)),
              by = .(treat)]


#CD466 ----
dat <- read.csv("CD466_Dataset for publication.csv", strip.white = TRUE) 
datF <- subset(dat, sex  == "F"); datM <- subset(dat, sex  == "M")
datF <- subset(datF, present  != "no") #removing rows without behavioural measures
datM <- subset(datM, present  != "no") #removing rows without behavioural measures
nrow(subset(datF, manipulation == "food supplemented"))
nrow(subset(datF, manipulation == "control"))
nrow(subset(datM, manipulation == "food supplemented"))
nrow(subset(datM, manipulation == "control"))
setDT(datF)[ , list(mean.M2 = mean(M2), sd.M2 = sd(M2),
                    mean.P10 = mean(P10), sd.P10 = sd(P10),
                    mean.hovering = mean(hovering), sd.hovering = sd(hovering),
                    mean.swooping = mean(swooping), sd.swooping = sd(swooping),
                    mean.attacks = mean(attacks), sd.attacks = sd(attacks),
                    mean.alarm = mean(alarm), sd.alarm = sd(alarm),
                    mean.activity = mean(activity), sd.activity = sd(activity)),
              by = .(manipulation)]
setDT(datM)[ , list(mean.M2 = mean(M2), sd.M2 = sd(M2),
                    mean.P10 = mean(P10), sd.P10 = sd(P10),
                    mean.hovering = mean(hovering), sd.hovering = sd(hovering),
                    mean.swooping = mean(swooping), sd.swooping = sd(swooping),
                    mean.attacks = mean(attacks), sd.attacks = sd(attacks),
                    mean.alarm = mean(alarm), sd.alarm = sd(alarm),
                    mean.activity = mean(activity), sd.activity = sd(activity)),
             by = .(manipulation)]

#CD481 ----
dat <- read.csv("CD481_BEHECO-2016-0276_main_data.csv", strip.white = TRUE) 
#calculating separate means and sds for each repeated activity measure
dat1 <- subset(dat, Rep  == 1)
dat2 <- subset(dat, Rep  == 2)
dat3 <- subset(dat, Rep  == 3)
dat4 <- subset(dat, Rep  == 4)
dat5 <- subset(dat, Rep  == 5)
dat6 <- subset(dat, Rep  == 6)
dat1OC <- subset(dat1, Activity_OC  != "NA")
dat2OC <- subset(dat2, Activity_OC  != "NA")
dat3OC <- subset(dat3, Activity_OC  != "NA")
dat4OC <- subset(dat4, Activity_OC  != "NA")
dat5OC <- subset(dat5, Activity_OC  != "NA")
dat6OC <- subset(dat6, Activity_OC  != "NA")
dat1AP <- subset(dat1, Activity_AP  != "NA")
dat2AP <- subset(dat2, Activity_AP  != "NA")
dat3AP <- subset(dat3, Activity_AP  != "NA")
dat4AP <- subset(dat4, Activity_AP  != "NA")
dat5AP <- subset(dat5, Activity_AP  != "NA")
dat6AP <- subset(dat6, Activity_AP  != "NA")
nrow(subset(dat1OC, Treatment == "LowQual")); nrow(subset(dat2OC, Treatment == "LowQual")); nrow(subset(dat3OC, Treatment == "LowQual")); nrow(subset(dat4OC, Treatment == "LowQual")); nrow(subset(dat5OC, Treatment == "LowQual")); nrow(subset(dat6OC, Treatment == "LowQual"))
nrow(subset(dat1AP, Treatment == "LowQual")); nrow(subset(dat2AP, Treatment == "LowQual")); nrow(subset(dat3AP, Treatment == "LowQual")); nrow(subset(dat4AP, Treatment == "LowQual")); nrow(subset(dat5AP, Treatment == "LowQual")); nrow(subset(dat6AP, Treatment == "LowQual"))
nrow(subset(dat1OC, Treatment == "HighQual")); nrow(subset(dat2OC, Treatment == "HighQual")); nrow(subset(dat3OC, Treatment == "HighQual")); nrow(subset(dat4OC, Treatment == "HighQual")); nrow(subset(dat5OC, Treatment == "HighQual")); nrow(subset(dat6OC, Treatment == "HighQual"))
nrow(subset(dat1AP, Treatment == "HighQual")); nrow(subset(dat2AP, Treatment == "HighQual")); nrow(subset(dat3AP, Treatment == "HighQual")); nrow(subset(dat4AP, Treatment == "HighQual")); nrow(subset(dat5AP, Treatment == "HighQual")); nrow(subset(dat6AP, Treatment == "HighQual"))
mean(dat1OC$Day); mean(dat2OC$Day); mean(dat3OC$Day); mean(dat4OC$Day); mean(dat5OC$Day); mean(dat6OC$Day)
mean(dat1AP$Day); mean(dat2AP$Day); mean(dat3AP$Day); mean(dat4AP$Day); mean(dat5AP$Day); mean(dat6AP$Day)
setDT(dat1OC)[ , list(activity = mean(Activity_OC), sd = sd(Activity_OC)), by = .(Treatment)]
setDT(dat2OC)[ , list(activity = mean(Activity_OC), sd = sd(Activity_OC)), by = .(Treatment)]
setDT(dat3OC)[ , list(activity = mean(Activity_OC), sd = sd(Activity_OC)), by = .(Treatment)]
setDT(dat4OC)[ , list(activity = mean(Activity_OC), sd = sd(Activity_OC)), by = .(Treatment)]
setDT(dat5OC)[ , list(activity = mean(Activity_OC), sd = sd(Activity_OC)), by = .(Treatment)]
setDT(dat6OC)[ , list(activity = mean(Activity_OC), sd = sd(Activity_OC)), by = .(Treatment)]
setDT(dat1AP)[ , list(activity = mean(Activity_AP), sd = sd(Activity_AP)), by = .(Treatment)]
setDT(dat2AP)[ , list(activity = mean(Activity_AP), sd = sd(Activity_AP)), by = .(Treatment)]
setDT(dat3AP)[ , list(activity = mean(Activity_AP), sd = sd(Activity_AP)), by = .(Treatment)]
setDT(dat4AP)[ , list(activity = mean(Activity_AP), sd = sd(Activity_AP)), by = .(Treatment)]
setDT(dat5AP)[ , list(activity = mean(Activity_AP), sd = sd(Activity_AP)), by = .(Treatment)]
setDT(dat6AP)[ , list(activity = mean(Activity_AP), sd = sd(Activity_AP)), by = .(Treatment)]
#rate of escape from predator assay
summary(dat)
datpred<-subset(dat, Rep  != 0)
datpred$Escaped<-as.numeric(datpred$Escaped)-1
mean(datpred$Day)
datpredLC<-subset(datpred, Treatment == "LowQual")
datpredHC<-subset(datpred, Treatment == "HighQual")
LC<-setDT(datpredLC)[ , list(escaperate = mean(Escaped)), by = .(Id)]
HC<-setDT(datpredHC)[ , list(escaperate = mean(Escaped)), by = .(Id)]
mean(LC$escaperate); sd(LC$escaperate)
mean(HC$escaperate); sd(HC$escaperate)

#CD579 ----
#data from experiment 2 separated from file, and added 3600s as a maximum latency for individuals that did not reach the relevant point in each task
dat <- read.csv("CD579_Copy of Webster (2016) Leader-follower data for DRYAD.csv", strip.white = TRUE) 
dat_train1 <- subset(dat, Trained == 1)
dat_train2 <- subset(dat, Trained== 2)
nrow(subset(dat_train1, Hunger.level == 1))
nrow(subset(dat_train1, Hunger.level == 2))
nrow(subset(dat_train1, Hunger.level == 3))
nrow(subset(dat_train1, Hunger.level == 4))
nrow(subset(dat_train1, Hunger.level == 5))
nrow(subset(dat_train2, Hunger.level == 1))
nrow(subset(dat_train2, Hunger.level == 2))
nrow(subset(dat_train2, Hunger.level == 3))
nrow(subset(dat_train2, Hunger.level == 4))
nrow(subset(dat_train2, Hunger.level == 5))
setDT(dat_train1)[ , list(mean1 = mean(X1st), sd1 = sd(X1st),
                          mean2 = mean(X2nd), sd2 = sd(X2nd),
                          mean3 = mean(X3rd), sd3 = sd(X3rd),
                          mean4 = mean(X4th), sd4 = sd(X4th)), 
                   by = .(Hunger.level)]
setDT(dat_train2)[ , list(mean1 = mean(X1st), sd1 = sd(X1st),
                          mean2 = mean(X2nd), sd2 = sd(X2nd),
                          mean3 = mean(X3rd), sd3 = sd(X3rd),
                          mean4 = mean(X4th), sd4 = sd(X4th)), 
                   by = .(Hunger.level)]
#as a measure of how closely the individual followed each other, we calculated the difference between the latencies of the 1st and 2nd naïve fish to arrive relative to the leader
#a maximum latency of 3600 was not used for this as this only applies to fish that did arrive
dat_train1<-subset(dat_train1, Leader != "NA")
dat_train2<-subset(dat_train2, Leader != "NA")
dat_train1$firstdiff<-(dat_train1$X1st - dat_train1$Leader)
dat_train2$firstdiff<-(dat_train2$X1st - dat_train2$Leader)
nrow(subset(dat_train1, Hunger.level == 1)); nrow(subset(dat_train1, Hunger.level == 2)); nrow(subset(dat_train1, Hunger.level == 3)); nrow(subset(dat_train1, Hunger.level == 4)); nrow(subset(dat_train1, Hunger.level == 5))
nrow(subset(dat_train2, Hunger.level == 1)); nrow(subset(dat_train2, Hunger.level == 2)); nrow(subset(dat_train2, Hunger.level == 3)); nrow(subset(dat_train2, Hunger.level == 4)); nrow(subset(dat_train2, Hunger.level == 5))
setDT(dat_train1)[ , list(mean1 = mean(firstdiff), sd1 = sd(firstdiff)), 
                   by = .(Hunger.level)]
setDT(dat_train2)[ , list(mean1 = mean(firstdiff), sd1 = sd(firstdiff)), 
                   by = .(Hunger.level)]
dat_train1<-subset(dat_train1, X2nd != 3600)
dat_train2<-subset(dat_train2, X2nd != 3600)
dat_train1$seconddiff<-(dat_train1$X2nd - dat_train1$Leader)
dat_train2$seconddiff<-(dat_train2$X2nd - dat_train2$Leader)
nrow(subset(dat_train1, Hunger.level == 1)); nrow(subset(dat_train1, Hunger.level == 2)); nrow(subset(dat_train1, Hunger.level == 3)); nrow(subset(dat_train1, Hunger.level == 4)); nrow(subset(dat_train1, Hunger.level == 5))
nrow(subset(dat_train2, Hunger.level == 1)); nrow(subset(dat_train2, Hunger.level == 2)); nrow(subset(dat_train2, Hunger.level == 3)); nrow(subset(dat_train2, Hunger.level == 4)); nrow(subset(dat_train2, Hunger.level == 5))
setDT(dat_train1)[ , list(mean2 = mean(seconddiff), sd2 = sd(seconddiff)), 
                   by = .(Hunger.level)]
setDT(dat_train2)[ , list(mean2 = mean(seconddiff), sd2 = sd(seconddiff)), 
                   by = .(Hunger.level)]

#CD632 ----
#data from sheet 3 of dryad data minimally reformatted before analysis
dat <- read.csv("CD632_Copy of For Dryad.csv", strip.white = TRUE) 
datf <- subset(dat, Sex1 == "Female")
datm <- subset(dat, Sex1 == "Male")
nrow(subset(datf, Dispersal.status1 == "Non-disperser"))
nrow(subset(datf, Dispersal.status1 == "Disperser"))
nrow(subset(datm, Dispersal.status1 == "Non-disperser"))
nrow(subset(datm, Dispersal.status1 == "Disperser"))
setDT(datf)[ , list(meanLC = mean(Exploratory.Trips1), sdLC = sd(Exploratory.Trips1),
                    meanHC = mean(Exploratory.Trips2), sdHC = sd(Exploratory.Trips2)), 
                   by = .(Dispersal.status1)]
setDT(datm)[ , list(meanLC = mean(Exploratory.Trips1), sdLC = sd(Exploratory.Trips1),
                    meanHC = mean(Exploratory.Trips2), sdHC = sd(Exploratory.Trips2)), 
             by = .(Dispersal.status1)]


###########################_################################

###PROCCESSING OF AUTHOR PROVIDED DATA ----
setwd("~/metah06_condition-dependence-of-boldness/data_extraction_files")

#CD006 ----
dat <- read.table("CD006_hopping.txt", header = TRUE, strip.white = TRUE) 
dat1 <- subset(dat, day == 1)
dat2 <- subset(dat, day == 2)
dat3 <- subset(dat, day == 3)
dat1LC <- subset(dat1, food == "R")
dat1HC <- subset(dat1, food == "C")
dat2LC <- subset(dat2, food == "R")
dat2HC <- subset(dat2, food == "C")
dat3LC <- subset(dat3, food == "R")
dat3HC <- subset(dat3, food == "C")
#taking individual means for max hopping distance and number of hops in 3 trials on the day
dat1LCmeans <- setDT(dat1LC)[ , list(meanbighop = mean(bighop), meannhops = mean(nhops)), by = .(ID)]
dat1HCmeans <- setDT(dat1HC)[ , list(meanbighop = mean(bighop), meannhops = mean(nhops)), by = .(ID)]
dat2LCmeans <- setDT(dat2LC)[ , list(meanbighop = mean(bighop), meannhops = mean(nhops)), by = .(ID)]
dat2HCmeans <- setDT(dat2HC)[ , list(meanbighop = mean(bighop), meannhops = mean(nhops)), by = .(ID)]
dat3LCmeans <- setDT(dat3LC)[ , list(meanbighop = mean(bighop), meannhops = mean(nhops)), by = .(ID)]
dat3HCmeans <- setDT(dat3HC)[ , list(meanbighop = mean(bighop), meannhops = mean(nhops)), by = .(ID)]
mean(dat1LCmeans$meanbighop); sd(dat1LCmeans$meanbighop); mean(dat1LCmeans$meannhops); sd(dat1LCmeans$meannhops)
mean(dat1HCmeans$meanbighop); sd(dat1HCmeans$meanbighop); mean(dat1HCmeans$meannhops); sd(dat1HCmeans$meannhops)
mean(dat2LCmeans$meanbighop); sd(dat2LCmeans$meanbighop); mean(dat2LCmeans$meannhops); sd(dat2LCmeans$meannhops)
mean(dat2HCmeans$meanbighop); sd(dat2HCmeans$meanbighop); mean(dat2HCmeans$meannhops); sd(dat2HCmeans$meannhops)
mean(dat3LCmeans$meanbighop); sd(dat3LCmeans$meanbighop); mean(dat3LCmeans$meannhops); sd(dat3LCmeans$meannhops)
mean(dat3HCmeans$meanbighop); sd(dat3HCmeans$meanbighop); mean(dat3HCmeans$meannhops); sd(dat3HCmeans$meannhops)

#CD030 ----
#raw data copied into csv for analysis
dat <- read.csv("CD030_trials.csv", strip.white = TRUE)
mean(dat$Hactiv); sd(dat$Hactiv)
mean(dat$Lactiv); sd(dat$Lactiv)
datpred <- subset(dat, Hactiv30 != "NA") 
summary(datpred)
mean(datpred$Hactiv30); sd(datpred$Hactiv30)
mean(datpred$Lactiv30); sd(datpred$Lactiv30)

#CD042 ----
dat <- read.csv("CD042_data feeding under stress.csv", strip.white = TRUE)
datF <- subset(dat, SEX == "f") 
datM <- subset(dat, SEX == "m") 
setDT(datF)[ , list(mean = mean(Food.acceptance.rate), sd = sd(Food.acceptance.rate)), by = .(TREATMENT)]
setDT(datM)[ , list(mean = mean(Food.acceptance.rate), sd = sd(Food.acceptance.rate)), by = .(TREATMENT)]

#CD257 ----
#raw datapoints copied into csv for analysis
dat <- read.csv("CD257_Chapt3figs.csv", strip.white = TRUE) 
mean(dat$sstotfed); sd(dat$sstotfed)
mean(dat$sstotf.d); sd(dat$sstotf.d) 
mean(dat$ss1st5fed); sd(dat$ss1st5fed) 
mean(dat$ss1st5f.d); sd(dat$ss1st5f.d) 
mean(dat$ss2nd5fed); sd(dat$ss2nd5fed) 
mean(dat$ss2nd5f.d); sd(dat$ss2nd5f.d) 
mean(dat$tot1fed); sd(dat$tot1fed) 
mean(dat$tot1f.d); sd(dat$tot1f.d) 
mean(dat$pre.sfed); sd(dat$pre.sfed) 
mean(dat$pre.sf.d); sd(dat$pre.sf.d) s
mean(dat$post.sfed); sd(dat$post.sfed) 
mean(dat$post.sf.d); sd(dat$post.sf.d) 

#CD258 ----
#HABI	1 marine 2 pond
#PREDATOR	0 no 1 present
#FEEDING	0 low 1 high
#SEX	1 male 2 female
#time to feed binary: responder=1; nonresponder=0	
dat <- read.csv("CD258_data_for_moran.csv", strip.white = TRUE) 
summary(dat$POP)
datABB <- subset(dat, POP == "ABB")
datABB.F <- subset(datABB, Sex == "2")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datABB.F.LC <- subset(datABB.F, FEEDING == 0)
nrow(datABB.F.LC); mean(datABB.F.LC$binary.time.to.feed..attack.)#nLC meanLC
datABB.F.HC <- subset(datABB.F, FEEDING == 1)
nrow(datABB.F.HC); mean(datABB.F.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datABB.F.PRED <- subset(datABB.F, PREDATOR == "1")
datABB.F.NOPR <- subset(datABB.F, PREDATOR == "0")
nrow(subset(datABB.F.PRED, FEEDING == 0)); nrow(subset(datABB.F.PRED, FEEDING == 1))
nrow(subset(datABB.F.NOPR, FEEDING == 0)); nrow(subset(datABB.F.NOPR, FEEDING == 1))
setDT(datABB.F.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datABB.F.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datABB.M <- subset(datABB, Sex == "1")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datABB.M.LC <- subset(datABB.M, FEEDING == 0)
nrow(datABB.M.LC); mean(datABB.M.LC$binary.time.to.feed..attack.)#nLC meanLC
datABB.M.HC <- subset(datABB.M, FEEDING == 1)
nrow(datABB.M.HC); mean(datABB.M.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datABB.M.PRED <- subset(datABB.M, PREDATOR == "1")
datABB.M.NOPR <- subset(datABB.M, PREDATOR == "0")
nrow(subset(datABB.M.PRED, FEEDING == 0)); nrow(subset(datABB.M.PRED, FEEDING == 1))
nrow(subset(datABB.M.NOPR, FEEDING == 0)); nrow(subset(datABB.M.NOPR, FEEDING == 1))
setDT(datABB.M.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datABB.M.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datBÖL <- subset(dat, POP == "BÖL")
datBÖL.F <- subset(datBÖL, Sex == "2")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datBÖL.F.LC <- subset(datBÖL.F, FEEDING == 0)
nrow(datBÖL.F.LC); mean(datBÖL.F.LC$binary.time.to.feed..attack.)#nLC meanLC
datBÖL.F.HC <- subset(datBÖL.F, FEEDING == 1)
nrow(datBÖL.F.HC); mean(datBÖL.F.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datBÖL.F.PRED <- subset(datBÖL.F, PREDATOR == "1")
datBÖL.F.NOPR <- subset(datBÖL.F, PREDATOR == "0")
nrow(subset(datBÖL.F.PRED, FEEDING == 0)); nrow(subset(datBÖL.F.PRED, FEEDING == 1))
nrow(subset(datBÖL.F.NOPR, FEEDING == 0)); nrow(subset(datBÖL.F.NOPR, FEEDING == 1))
setDT(datBÖL.F.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datBÖL.F.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datBÖL.M <- subset(datBÖL, Sex == "1")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datBÖL.M.LC <- subset(datBÖL.M, FEEDING == 0)
nrow(datBÖL.M.LC); mean(datBÖL.M.LC$binary.time.to.feed..attack.)#nLC meanLC
datBÖL.M.HC <- subset(datBÖL.M, FEEDING == 1)
nrow(datBÖL.M.HC); mean(datBÖL.M.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datBÖL.M.PRED <- subset(datBÖL.M, PREDATOR == "1")
datBÖL.M.NOPR <- subset(datBÖL.M, PREDATOR == "0")
nrow(subset(datBÖL.M.PRED, FEEDING == 0)); nrow(subset(datBÖL.M.PRED, FEEDING == 1))
nrow(subset(datBÖL.M.NOPR, FEEDING == 0)); nrow(subset(datBÖL.M.NOPR, FEEDING == 1))
setDT(datBÖL.M.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datBÖL.M.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datHEL <- subset(dat, POP == "HEL")
datHEL.F <- subset(datHEL, Sex == "2")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datHEL.F.LC <- subset(datHEL.F, FEEDING == 0)
nrow(datHEL.F.LC); mean(datHEL.F.LC$binary.time.to.feed..attack.)#nLC meanLC
datHEL.F.HC <- subset(datHEL.F, FEEDING == 1)
nrow(datHEL.F.HC); mean(datHEL.F.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datHEL.F.PRED <- subset(datHEL.F, PREDATOR == "1")
datHEL.F.NOPR <- subset(datHEL.F, PREDATOR == "0")
nrow(subset(datHEL.F.PRED, FEEDING == 0)); nrow(subset(datHEL.F.PRED, FEEDING == 1))
nrow(subset(datHEL.F.NOPR, FEEDING == 0)); nrow(subset(datHEL.F.NOPR, FEEDING == 1))
setDT(datHEL.F.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datHEL.F.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datHEL.M <- subset(datHEL, Sex == "1")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datHEL.M.LC <- subset(datHEL.M, FEEDING == 0)
nrow(datHEL.M.LC); mean(datHEL.M.LC$binary.time.to.feed..attack.)#nLC meanLC
datHEL.M.HC <- subset(datHEL.M, FEEDING == 1)
nrow(datHEL.M.HC); mean(datHEL.M.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datHEL.M.PRED <- subset(datHEL.M, PREDATOR == "1")
datHEL.M.NOPR <- subset(datHEL.M, PREDATOR == "0")
nrow(subset(datHEL.M.PRED, FEEDING == 0)); nrow(subset(datHEL.M.PRED, FEEDING == 1))
nrow(subset(datHEL.M.NOPR, FEEDING == 0)); nrow(subset(datHEL.M.NOPR, FEEDING == 1))
setDT(datHEL.M.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datHEL.M.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datKRI <- subset(dat, POP == "KRI")
datKRI.F <- subset(datKRI, Sex == "2")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datKRI.F.LC <- subset(datKRI.F, FEEDING == 0)
nrow(datKRI.F.LC); mean(datKRI.F.LC$binary.time.to.feed..attack.)#nLC meanLC
datKRI.F.HC <- subset(datKRI.F, FEEDING == 1)
nrow(datKRI.F.HC); mean(datKRI.F.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datKRI.F.PRED <- subset(datKRI.F, PREDATOR == "1")
datKRI.F.NOPR <- subset(datKRI.F, PREDATOR == "0")
nrow(subset(datKRI.F.PRED, FEEDING == 0)); nrow(subset(datKRI.F.PRED, FEEDING == 1))
nrow(subset(datKRI.F.NOPR, FEEDING == 0)); nrow(subset(datKRI.F.NOPR, FEEDING == 1))
setDT(datKRI.F.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datKRI.F.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datKRI.M <- subset(datKRI, Sex == "1")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datKRI.M.LC <- subset(datKRI.M, FEEDING == 0)
nrow(datKRI.M.LC); mean(datKRI.M.LC$binary.time.to.feed..attack.)#nLC meanLC
datKRI.M.HC <- subset(datKRI.M, FEEDING == 1)
nrow(datKRI.M.HC); mean(datKRI.M.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datKRI.M.PRED <- subset(datKRI.M, PREDATOR == "1")
datKRI.M.NOPR <- subset(datKRI.M, PREDATOR == "0")
nrow(subset(datKRI.M.PRED, FEEDING == 0)); nrow(subset(datKRI.M.PRED, FEEDING == 1))
nrow(subset(datKRI.M.NOPR, FEEDING == 0)); nrow(subset(datKRI.M.NOPR, FEEDING == 1))
setDT(datKRI.M.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datKRI.M.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datNYK <- subset(dat, POP == "NYK")
datNYK.F <- subset(datNYK, Sex == "2")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datNYK.F.LC <- subset(datNYK.F, FEEDING == 0)
nrow(datNYK.F.LC); mean(datNYK.F.LC$binary.time.to.feed..attack.)#nLC meanLC
datNYK.F.HC <- subset(datNYK.F, FEEDING == 1)
nrow(datNYK.F.HC); mean(datNYK.F.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datNYK.F.PRED <- subset(datNYK.F, PREDATOR == "1")
datNYK.F.NOPR <- subset(datNYK.F, PREDATOR == "0")
nrow(subset(datNYK.F.PRED, FEEDING == 0)); nrow(subset(datNYK.F.PRED, FEEDING == 1))
nrow(subset(datNYK.F.NOPR, FEEDING == 0)); nrow(subset(datNYK.F.NOPR, FEEDING == 1))
setDT(datNYK.F.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datNYK.F.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datNYK.M <- subset(datNYK, Sex == "1")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datNYK.M.LC <- subset(datNYK.M, FEEDING == 0)
nrow(datNYK.M.LC); mean(datNYK.M.LC$binary.time.to.feed..attack.)#nLC meanLC
datNYK.M.HC <- subset(datNYK.M, FEEDING == 1)
nrow(datNYK.M.HC); mean(datNYK.M.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datNYK.M.PRED <- subset(datNYK.M, PREDATOR == "1")
datNYK.M.NOPR <- subset(datNYK.M, PREDATOR == "0")
nrow(subset(datNYK.M.PRED, FEEDING == 0)); nrow(subset(datNYK.M.PRED, FEEDING == 1))
nrow(subset(datNYK.M.NOPR, FEEDING == 0)); nrow(subset(datNYK.M.NOPR, FEEDING == 1))
setDT(datNYK.M.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datNYK.M.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datPYÖ <- subset(dat, POP == "PYÖ")
datPYÖ.F <- subset(datPYÖ, Sex == "2")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datPYÖ.F.LC <- subset(datPYÖ.F, FEEDING == 0)
nrow(datPYÖ.F.LC); mean(datPYÖ.F.LC$binary.time.to.feed..attack.)#nLC meanLC
datPYÖ.F.HC <- subset(datPYÖ.F, FEEDING == 1)
nrow(datPYÖ.F.HC); mean(datPYÖ.F.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datPYÖ.F.PRED <- subset(datPYÖ.F, PREDATOR == "1")
datPYÖ.F.NOPR <- subset(datPYÖ.F, PREDATOR == "0")
nrow(subset(datPYÖ.F.PRED, FEEDING == 0)); nrow(subset(datPYÖ.F.PRED, FEEDING == 1))
nrow(subset(datPYÖ.F.NOPR, FEEDING == 0)); nrow(subset(datPYÖ.F.NOPR, FEEDING == 1))
setDT(datPYÖ.F.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datPYÖ.F.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]

datPYÖ.M <- subset(datPYÖ, Sex == "1")
#data for binary variable not marginalised, so calculated before splitting into pred and no pred
datPYÖ.M.LC <- subset(datPYÖ.M, FEEDING == 0)
nrow(datPYÖ.M.LC); mean(datPYÖ.M.LC$binary.time.to.feed..attack.)#nLC meanLC
datPYÖ.M.HC <- subset(datPYÖ.M, FEEDING == 1)
nrow(datPYÖ.M.HC); mean(datPYÖ.M.HC$binary.time.to.feed..attack.)#nHC meanHC
#to marginalise across predator pre-treatments
datPYÖ.M.PRED <- subset(datPYÖ.M, PREDATOR == "1")
datPYÖ.M.NOPR <- subset(datPYÖ.M, PREDATOR == "0")
nrow(subset(datPYÖ.M.PRED, FEEDING == 0)); nrow(subset(datPYÖ.M.PRED, FEEDING == 1))
nrow(subset(datPYÖ.M.NOPR, FEEDING == 0)); nrow(subset(datPYÖ.M.NOPR, FEEDING == 1))
setDT(datPYÖ.M.PRED)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]
setDT(datPYÖ.M.NOPR)[ , list(timetofeed = mean(time.to.feed..attack.), sdtimetofeed = sd(time.to.feed..attack.),
                             emerge = mean(time.to.body.out), sdemerge = sd(time.to.body.out),
                             cross1 = mean(time.to.cross.1), sdcross1 = sd(time.to.cross.1),
                             cross2 = mean(time.to.cross.2), sdcross2 = sd(time.to.cross.2)), 
                      by = .(FEEDING)]


#CD273 ----
#first sheet converted to csv before analysis
dat <- read.csv("CD273_behavioural data Nicholas Moran.csv", strip.white = TRUE) 
datHC<-subset(dat, food == "High"); datLC<-subset(dat, food == "Low")
summary(dat)
datHC1<-subset(datHC, repeat. == 1)
datHC2<-subset(datHC, repeat. == 2)
datHC3<-subset(datHC, repeat. == 3)
datHC4<-subset(datHC, repeat. == 4)
datLC1<-subset(datLC, repeat. == 1)
datLC2<-subset(datLC, repeat. == 2)
datLC3<-subset(datLC, repeat. == 3)
datLC4<-subset(datLC, repeat. == 4)
setDT(datHC1)[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.),
                      meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
                   by = .(vitamin.D)]
setDT(datHC2)[ , list(meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]
setDT(subset(datHC2, shelter.use..sec. != "NA"))[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.)), 
               by = .(vitamin.D)]#1 NoVit shelter is an NA
setDT(datHC3)[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.),
                      meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]
setDT(datHC4)[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.)), 
               by = .(vitamin.D)]
setDT(subset(datHC4, risk.taking_latency..sec. != "NA"))[ , list(meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]#1 Vit risk is an NA
setDT(datLC1)[ , list(meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]
setDT(subset(datLC1, shelter.use..sec. != "NA"))[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.)), 
               by = .(vitamin.D)]#1 NoVit shelter is a NA
setDT(datLC2)[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.),
                      meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]
setDT(datLC3)[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.),
                      meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]
setDT(datLC4)[ , list(meanshelter = mean(shelter.use..sec.), sdshelter = sd(shelter.use..sec.),
                      meanrisk = mean(risk.taking_latency..sec.), sdrisk = sd(risk.taking_latency..sec.)), 
               by = .(vitamin.D)]

#CD420 ----
dat <- read.csv("CD420_Data_NäslundEFF.csv", strip.white = TRUE) 
dat <- subset(dat, EtoF != "NA")
nrow(subset(dat, Tr == "HH")) 
nrow(subset(dat, Tr == "HL"))
nrow(subset(dat, Tr == "LH")) 
nrow(subset(dat, Tr == "LL"))
setDT(dat)[ , list(mean = mean(EtoF), sd = sd(EtoF)), by = .(Tr)]

#CD457 ----
dat <- read.csv("CD457_Rawdata Christine behavior food intake H&B food dep_reformatted.csv", strip.white = TRUE) 
datF <- subset(dat, Sex == "F")
datM <- subset(dat, Sex == "M")
setDT(datF)[ , list(latencycontact = mean(latencycontact), sdlatencycontact = sd(latencycontact),
                    exploring = mean(exploring), sdexploring = sd(exploring),
                    contactfood = mean(contactfood), sdcontactfood = sd(contactfood),
                    inactive = mean(inactive), sdinactive = sd(inactive),
                    locomotion = mean(locomotion), sdlocomotion = sd(locomotion),
                    latencymove = mean(latencymove), sdlatencymove = sd(latencymove),
                    sweep = mean(sweep), sdsweep = sd(sweep),
                    wipe = mean(wipe), sdwipe = sd(wipe),
                    hindlimbkick = mean(hindlimbkick), sdhindlimbkick = sd(hindlimbkick)), 
               by = .(Treatment.A)]
setDT(datM)[ , list(latencycontact = mean(latencycontact), sdlatencycontact = sd(latencycontact),
                    exploring = mean(exploring), sdexploring = sd(exploring),
                    contactfood = mean(contactfood), sdcontactfood = sd(contactfood),
                    inactive = mean(inactive), sdinactive = sd(inactive),
                    locomotion = mean(locomotion), sdlocomotion = sd(locomotion),
                    latencymove = mean(latencymove), sdlatencymove = sd(latencymove),
                    sweep = mean(sweep), sdsweep = sd(sweep),
                    wipe = mean(wipe), sdwipe = sd(wipe),
                    hindlimbkick = mean(hindlimbkick), sdhindlimbkick = sd(hindlimbkick)), 
             by = .(Treatment.A)]

#CD524 ----
#diet 1 = barnacle fed, diet 2 = mussel fed, diet 3 = starved
#CRAB = 1, with crab cue, CONSP = 1, with conspecific cue, both 1 is the mixed cue
dat_juv <- read.csv("CD524_JUVVLAR.csv", strip.white = TRUE) 
dat_juv$FORAGPROP <- (dat_juv$FORAG/30)
dat_juv.nocue <- subset(subset(dat_juv, CONSP == 0), CRAB == 0)
dat_juv.crabcue <- subset(subset(dat_juv, CONSP == 0), CRAB == 1)
dat_juv.conspcue <- subset(subset(dat_juv, CONSP == 1), CRAB == 0)
dat_juv.mixedcue <- subset(subset(dat_juv, CONSP == 1), CRAB == 1)
setDT(dat_juv.nocue)[ , list(mean = mean(FORAGPROP), sd = sd(FORAGPROP)), by = .(IDIET)]
setDT(dat_juv.crabcue)[ , list(mean = mean(FORAGPROP), sd = sd(FORAGPROP)), by = .(IDIET)]
setDT(dat_juv.conspcue)[ , list(mean = mean(FORAGPROP), sd = sd(FORAGPROP)), by = .(IDIET)]
setDT(dat_juv.mixedcue)[ , list(mean = mean(FORAGPROP), sd = sd(FORAGPROP)), by = .(IDIET)]
#for adults diet 0 = starved, 1 = barn, 2 = mussel
dat_ad <- read.csv("CD524_ADULVLA.csv", strip.white = TRUE) 
dat_ad$SEARCHPROP <- (dat_ad$SEARCH/30)
dat_ad.nocue <- subset(subset(dat_ad, CONSP == 0), CRAB == 0)
dat_ad.crabcue <- subset(subset(dat_ad, CONSP == 0), CRAB == 1)
dat_ad.conspcue <- subset(subset(dat_ad, CONSP == 1), CRAB == 0)
dat_ad.mixedcue <- subset(subset(dat_ad, CONSP == 1), CRAB == 1)
setDT(dat_ad.nocue)[ , list(mean = mean(SEARCHPROP), sd = sd(SEARCHPROP)), by = .(DIET)]
setDT(dat_ad.crabcue)[ , list(mean = mean(SEARCHPROP), sd = sd(SEARCHPROP)), by = .(DIET)]
setDT(dat_ad.conspcue)[ , list(mean = mean(SEARCHPROP), sd = sd(SEARCHPROP)), by = .(DIET)]
setDT(dat_ad.mixedcue)[ , list(mean = mean(SEARCHPROP), sd = sd(SEARCHPROP)), by = .(DIET)]

#CD580 ----
#calculated total visits, duration of visiting and latency to visit the central pots in a novel environment, 
#if a pot was not visited, pot.latency was manually set to 900s.
dat <- read.csv("CD580_File for Moran - raw data of nutritional study.csv", strip.white = TRUE) 
datvar <- subset(dat, Pot != "O-NO") #removing preference data
datvar <- select(datvar, -c(First.choice, IDSORT, Block, Temperature, Position)) #removing unrequired variables
datvarA <- subset(datvar, Pot == "A") #formatting to calculate total pot visits and pot duration
datvarB <- subset(datvar, Pot == "B") 
datvarB <- select(datvarB, -c(Treatment, Sex, Nutrition, Pot)) 
datvar <- merge(datvarA, datvarB, by = "Replicate", all.x = TRUE)
datvar$pot.visits.total <- (datvar$Pot.visits.x + datvar$Pot.visits.y) #calculating total visits and durations
datvar$pot.duration.total <- (datvar$Pot.duration.x + datvar$Pot.duration.y)
datvar <- select(datvar, -c(Pot.visits.x, Pot.duration.x, Pot.visits.y, Pot.duration.y)) 
datvar$pot.latency <- ifelse(datvar$Pot.latency.x > datvar$Pot.latency.y, datvar$pot.latency <- datvar$Pot.latency.y, datvar$pot.latency <- datvar$Pot.latency.x)
datvar <- select(datvar, -c(Pot.latency.x, Pot.latency.y))
#splitting by sex
datf <- subset(datvar, Sex == "F")
datm <- subset(datvar, Sex == "M")
#splitting data by experimental environment treatment, (F = Bed bug-exposed paper, C = control)
datfC <- subset(datf, Treatment == "C")
datfF <- subset(datf, Treatment == "F")
datmC <- subset(datm, Treatment == "C")
datmF <- subset(datm, Treatment == "F")
setDT(datfC)[ , list(visits = mean(pot.visits.total), visitssd = sd(pot.visits.total),
                     duration = mean(pot.duration.total), durationsd = sd(pot.duration.total),
                     latency = mean(pot.latency), latencysd = sd(pot.latency)), 
                  by = .(Nutrition)]
setDT(datfF)[ , list(visits = mean(pot.visits.total), visitssd = sd(pot.visits.total),
                     duration = mean(pot.duration.total), durationsd = sd(pot.duration.total),
                     latency = mean(pot.latency), latencysd = sd(pot.latency)), 
              by = .(Nutrition)]
setDT(datmC)[ , list(visits = mean(pot.visits.total), visitssd = sd(pot.visits.total),
                     duration = mean(pot.duration.total), durationsd = sd(pot.duration.total),
                     latency = mean(pot.latency), latencysd = sd(pot.latency)), 
              by = .(Nutrition)]
setDT(datmF)[ , list(visits = mean(pot.visits.total), visitssd = sd(pot.visits.total),
                     duration = mean(pot.duration.total), durationsd = sd(pot.duration.total),
                     latency = mean(pot.latency), latencysd = sd(pot.latency)), 
              by = .(Nutrition)]

#CD594 ----
#male and female, proportion of behavioural observations data (Sheet 2 used)
dat <- read.csv("CD594_Copy of TrisBehaviorFull.csv", strip.white = TRUE) 
nrow(dat)
dat.nov.f <-subset(subset(dat, Treatment == "Control"), Sex == "Female")
dat.pred.f <-subset(subset(dat, Treatment == "Pred"), Sex == "Female")
dat.nov.m <-subset(subset(dat, Treatment == "Control"), Sex == "Male")
dat.pred.m <-subset(subset(dat, Treatment == "Pred"), Sex == "Male")
nrow(subset(dat.nov.f, Food == "Low")); nrow(subset(dat.nov.f, Food == "Medium")); nrow(subset(dat.nov.f, Food == "High"))
nrow(subset(dat.pred.f, Food == "Low")); nrow(subset(dat.pred.f, Food == "Medium")); nrow(subset(dat.pred.f, Food == "High"))
nrow(subset(dat.nov.m, Food == "Low")); nrow(subset(dat.nov.m, Food == "Medium")); nrow(subset(dat.nov.m, Food == "High"))
nrow(subset(dat.pred.m, Food == "Low")); nrow(subset(dat.pred.m, Food == "Medium")); nrow(subset(dat.pred.m, Food == "High"))
setDT(dat.nov.f)[ , list(bottom = mean(pBottom), sdbottom = sd(pBottom),
                         surface = mean(pSurface), sdsurface = sd(pSurface),
                         middle = mean(pMiddle), sdmiddle = sd(pMiddle),
                         wall = mean(pWall), sdwall= sd(pWall),
                         trash = mean(pThrash), sdtrash = sd(pThrash),
                         rest = mean(pRest), sdrest = sd(pRest),
                         filter = mean(pFilter), sdfilter = sd(pFilter),
                         browse = mean(pBrowse), sdbrowse = sd(pBrowse)), 
               by = .(Food)]
setDT(dat.pred.f)[ , list(bottom = mean(pBottom), sdbottom = sd(pBottom),
                         surface = mean(pSurface), sdsurface = sd(pSurface),
                         middle = mean(pMiddle), sdmiddle = sd(pMiddle),
                         wall = mean(pWall), sdwall= sd(pWall),
                         trash = mean(pThrash), sdtrash = sd(pThrash),
                         rest = mean(pRest), sdrest = sd(pRest),
                         filter = mean(pFilter), sdfilter = sd(pFilter),
                         browse = mean(pBrowse), sdbrowse = sd(pBrowse)), 
                  by = .(Food)]
setDT(dat.nov.m)[ , list(bottom = mean(pBottom), sdbottom = sd(pBottom),
                         surface = mean(pSurface), sdsurface = sd(pSurface),
                         middle = mean(pMiddle), sdmiddle = sd(pMiddle),
                         wall = mean(pWall), sdwall= sd(pWall),
                         trash = mean(pThrash), sdtrash = sd(pThrash),
                         rest = mean(pRest), sdrest = sd(pRest),
                         filter = mean(pFilter), sdfilter = sd(pFilter),
                         browse = mean(pBrowse), sdbrowse = sd(pBrowse)), 
                  by = .(Food)]
setDT(dat.pred.m)[ , list(bottom = mean(pBottom), sdbottom = sd(pBottom),
                          surface = mean(pSurface), sdsurface = sd(pSurface),
                          middle = mean(pMiddle), sdmiddle = sd(pMiddle),
                          wall = mean(pWall), sdwall= sd(pWall),
                          trash = mean(pThrash), sdtrash = sd(pThrash),
                          rest = mean(pRest), sdrest = sd(pRest),
                          filter = mean(pFilter), sdfilter = sd(pFilter),
                          browse = mean(pBrowse), sdbrowse = sd(pBrowse)), 
                   by = .(Food)]

#CD609 ----
#No feeding trials in run 1, so only analysed run 2-3
dat <- read.csv("CD609_Personality--dead omitted.csv", strip.white = TRUE) 
dat234 <- subset(dat, run != 1)
dat234M <- subset(dat234, gender == "M"); dat234F <- subset(dat234, gender == "F")
nrow(subset(dat234F, trt == "NE")); nrow(subset(dat234F, trt == "NP")) 
nrow(subset(dat234F, trt == "FE")); nrow(subset(dat234F, trt == "FP"))
nrow(subset(dat234M, trt == "NE")); nrow(subset(dat234M, trt == "NP")) 
nrow(subset(dat234M, trt == "FE")); nrow(subset(dat234M, trt == "FP"))
setDT(dat234F)[ , list(time = mean(time), sdtime = sd(time)), 
              by = .(trt)]
setDT(dat234M)[ , list(time = mean(time), sdtime = sd(time)), 
                by = .(trt)]


#CD634 ----
dat <- read.csv("CD634_fly-perf2019.csv", strip.white = TRUE) 
mean(subset(dat, dat$age_breeding!="NA")$age_breeding)
datF <- subset(dat, sex == "F")
nrow(subset(datF, diet_group == "HH")); nrow(subset(datF, diet_group == "LL")); nrow(subset(datF, diet_group == "HL")); nrow(subset(datF, diet_group == "LH"))
setDT(datF)[ , list(to30 = mean(ini.max0_30), sdto30 = sd(ini.max0_30),
                       to50 = mean(ini.max0_50), sdto50 = sd(ini.max0_50),
                       post30 = mean(post.fly0_30_ms), sdpost30 = sd(post.fly0_30_ms),
                       post50 = mean(post.fly_0_50_ms), sdpost50 = sd(post.fly_0_50_ms)), 
                by = .(diet_group)]
datM <- subset(dat, sex == "M")
nrow(subset(datM, diet_group == "HH")); nrow(subset(datM, diet_group == "LL")); nrow(subset(datM, diet_group == "HL")); nrow(subset(datM, diet_group == "LH"))
datM2 <- subset(datM, post.fly_0_50_ms != "NA")
nrow(subset(datM2, diet_group == "HH")); nrow(subset(datM2, diet_group == "LL")); nrow(subset(datM2, diet_group == "HL")); nrow(subset(datM2, diet_group == "LH"))
setDT(datM)[ , list(to30 = mean(ini.max0_30), sdto30 = sd(ini.max0_30),
                    to50 = mean(ini.max0_50), sdto50 = sd(ini.max0_50),
                    post30 = mean(post.fly0_30_ms), sdpost30 = sd(post.fly0_30_ms)), 
             by = .(diet_group)]
setDT(datM2)[ , list(post50 = mean(post.fly_0_50_ms), sdpost50 = sd(post.fly_0_50_ms)), 
             by = .(diet_group)]
summary(datM)

#CD642 ----
dat <- read.csv("CD642_Research Data Jul 2013 copied data.csv", strip.white = TRUE) 
datLC <- subset(dat, DietG1P2 == 2)
datHC <- subset(dat, DietG1P2 == 1)
nrow(subset(datLC, V1nV2 == 1)); nrow(subset(datLC, V1nV2 == 2))
nrow(subset(datHC, V1nV2 == 2)); nrow(subset(datHC, V1nV2 == 2))
setDT(datLC)[ , list(change = mean(change), sdchange = sd(change),
                          Post = mean(Post), sdPost = sd(Post)), 
                   by = .(V1nV2)]
setDT(datHC)[ , list(change = mean(change), sdchange = sd(change),
                     Post = mean(Post), sdPost = sd(Post)), 
              by = .(V1nV2)]

###########################_################################

###PREPARATION OF DATABASES FOR AST RE-EXTRACTION DOUBLE CHECK ----
#Only data extracted from papers re-extracted (i.e. data from calculated from raw data and from author are excluded here) 
#Extractions from papers where NM could not extract usable data can be included in  re-extraction
#25% of studies re-checked drawn from 145 qualitatively included studies (i.e. 37 Studies)
library(dplyr)
setwd("~/metah06_condition-dependence-of-boldness/data_extraction_files")

extractdat <- read.csv("CD_extractdat.csv", strip.white = TRUE) 
extractdat_cut <- subset(extractdat, DataPresentation != "raw") #to exclude studies that exclusively extracted from raw data
extractdat_cut <- subset(extractdat_cut, DataPresentation != "author correspondence") #to exclude studies where data was exclusively provided directly from authors 
extractdat_cut <- subset(extractdat_cut, ExclusionReason != "finer scale data extracted") #not required as data from author correspondence superceded this data
nrow(extractdat_cut); summary(extractdat_cut$StudyID)

nrow(extractdat_cut)
write.csv(extractdat_cut, file = "CD_reextraction_processing.csv")
extractdat_cut <- read.csv("CD_reextraction_processing.csv", strip.white = TRUE)
reextract_studiesunique <- unique(extractdat_cut$StudyID) #to produce a list of studies that can be included in re-extraction

set.seed(182)
reextract_studiessample <- sample(reextract_studiesunique, 37, replace = FALSE, prob = NULL) #randomly sampling 37 studies from the dataset
reextract_studiessample <- as.data.frame(reextract_studiessample)
reextract_studiessample <- reextract_studiessample %>% 
  rename(StudyID = reextract_studiessample)

studydat <- read.csv("CD_studydat.csv", strip.white = TRUE) 
studydat <- select(studydat, -c(FirstContact, FirstDate, SecondaryContact, SecondaryDate, ResponseReceived,
                                RepliedtoResponse, FollowupContact, FollowupDate, ResponseReceivedFollowup, RepliedtoResponseFollowup)) #removing some unnessessary information from database
reextraction_studies <- merge(reextract_studiessample, studydat, by = "StudyID", all.x = FALSE)
nrow(reextraction_studies) #extracted 37 studies 

reextraction_dat <- merge(reextract_studiessample, extractdat, by = "StudyID", all.x = FALSE)
nrow(reextraction_dat) #extracted 300 rows 

summary(reextraction_dat$DataPresentation) #effect sizes from raw (0) and authors correspondence (10) 
summary(reextraction_dat$Exclude)
summary(reextraction_dat$ExclusionReason) # 150 exclude_all, most for having insufficient data (114)

write.csv(reextraction_studies, "CD_reextraction_studies.csv")
write.csv(reextraction_dat, "CD_reextraction_dat.csv")


#Re-extraction findings (conducted by student research assistant Jonathan Grone)
#Manual checking of data points found 1420 (N, Mean, SD/SEs only) checked. Potential issues identified with 26 values.
reextracted_grone <- read.csv("CD_reextraction_dat_WORKING_JonathanGrone.csv" , strip.white = TRUE)
summary(reextracted_grone$error) #11 of 302 lines impacted
summary(reextracted_grone$error_description) 
#1x "Figure shows different se" _____no correction required, reported means/SE are marginalised so do not directly match image
#1x "Figure shows different se, species-column shows journal name"_____no correction required, reported means/SE are marginalised so do not directly match image
#1x "for me it looked like the HC group has the bigger se, so I guess the LC and HC might be switched"_____no correction required, reported means/SE are marginalised so do not directly match image
#3x "I can't confirm these values from the image"_____no correction required, reported means/SE are marginalised so do not directly match image
#3x "I cannot figure out, where the data comes from"_____data from published supplementary material, checked and no correction required
#1x "marked values are dive duration, not swimming duration"_____2 values corrected, HC mean and SD correction
#1x "To me it looks like you took data of the wrong treatment for your low condition data; in the figure there are lines for high ration and low ration, but you chose the lines for high ration and "predictable food delivery". I am not sure if that is right"_____2 values corrected, LC means
#2x "value correct, but saved as text not as value"_____formatting changed, not correction required
#1x "wrong values"_____2 values corrected, HC mean and SD correction

#Total 6 vlues (N, Mean, SD/SEs only) required correction out of 1420.

###########################_################################
