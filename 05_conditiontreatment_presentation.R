##########_____Meta-analysis project (NC3 DO5)_____#########
##TITLE: The Condition Dependence of Risk-Taking Behaviours: Systematic review and meta-analysis

Sys.setenv(LANGUAGE = 'en')
library(metafor)


load("./models/rr.SpeciesStudyExp.Rdata")
load("./models/rr.Full.Rdata")
load("./models/cvr.SpeciesStudyExp.Rdata")
load("./models/cvr.Full.Rdata")
load("./models/smd.SpeciesStudyExp.Rdata")
load("./models/smd.Full.Rdata")
load("./models/vr.SpeciesStudyExp.Rdata")
load("./models/vr.Full.Rdata")
load("./models/rr.Full.h2.Rdata")
load("./models/cvr.Full.h2.Rdata")
load("./models/rr.Full.h3.Rdata")
load("./models/cvr.Full.h3.Rdata")
load("./models/rr.Full.h4.Rdata")
load("./models/cvr.Full.h4.Rdata")
load("./models/rr.Full.h5.i.Rdata")
load("./models/cvr.Full.h5.i.Rdata") 
load("./models/rr.Full.h5.ii.Rdata")
load("./models/cvr.Full.h5.ii.Rdata")
load("./models/rr.Full.pub1.Rdata")
load("./models/rr.Full.pub2.Rdata")
load("./models/rr.Full.pub3.Rdata")
load("./models/rr.Full.exp.a.Rdata")
load("./models/cvr.Full.exp.a.Rdata")
load("./models/rr.Full.exp.b.Rdata")
load("./models/cvr.Full.exp.b.Rdata")
load("./models/rr.Full.exp.c.Rdata")
load("./models/cvr.Full.exp.c.Rdata")
load("./models/rr.Full.exp.d.Rdata")
load("./models/cvr.Full.exp.d.Rdata")
load("./models/rr.sen1.RData")
load("./models/cvr.sen1.RData")


#loading package metaAidR, https://github.com/daniel1noble/metaAidR,
library(metaAidR); library(forestmangr)
#fix(I2) #Editing the function to (1) changing the name of the residual variance from obs to EffectID, (2) extract a residual heterogeneity estimate.


#loading packages for plotting
library(rlang); library(tidyverse); library(ggplot2); library(labeling)


###########################_################################

#INTERCEPT MODELS (FIG 1, FIG S5, TABLE 1): ----
#lnRR
rr.SpeciesStudyExp 
rr.Full 
predict(rr.SpeciesStudyExp)
predict(rr.Full)
I2(rr.SpeciesStudyExp, dat_rr$vi)
I2(rr.Full, dat_rr$vi, phylo = "SpeciesSci")


#lnCVR
cvr.SpeciesStudyExp
cvr.Full
predict(cvr.SpeciesStudyExp)
predict(cvr.Full)
I2(cvr.SpeciesStudyExp, dat_cvr$vi)
I2(cvr.Full, dat_cvr$vi, phylo = "SpeciesSci")


#Fig 1
fig1<-NULL
fig1$estimate <- c(cvr.SpeciesStudyExp$b, cvr.Full$b, rr.SpeciesStudyExp$b, rr.Full$b)
fig1$lower <- c(cvr.SpeciesStudyExp$ci.lb, cvr.Full$ci.lb, rr.SpeciesStudyExp$ci.lb, rr.Full$ci.lb)
fig1$upper <- c(cvr.SpeciesStudyExp$ci.ub, cvr.Full$ci.ub, rr.SpeciesStudyExp$ci.ub, rr.Full$ci.ub)
fig1$pos <- c(.75,1.5,3.5,4.25)
fig1$labels <- c("lnCVR (non-phylo)","lnCVR (phylo)","lnRR (non-phylo)","lnRR (phylo)")
fig1$k <- c("k = 1235","k = 1235","k = 1297","k = 1297")
fig1<-as.data.frame(fig1)

Fig.1 <- ggplot(fig1, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig1$lower, y = fig1$pos, xend = fig1$upper, yend = fig1$pos, size = 1) + 
  geom_point(size = 4, shape = 21, fill = "white") +
  geom_text(aes(label=fig1$labels), hjust = "left", x =-0.52, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig1$k), hjust = "left", x =-0.52, vjust=0.7, size = 2.5) +
  labs(x = "Effect Size, lnRR/lnCVR",
       y = "") +
  xlim(-.5, .5) +
  ylim(0, 5) 

Fig.1 

#ggsave("figures/Fig.1.pdf", width = 8, height = 12, units = "cm", Fig.1, dpi = 600)
ggsave("figures/Fig.1.jpg", width = 8, height = 12, units = "cm", Fig.1, dpi = 600)


#SMDH
smd.SpeciesStudyExp
smd.Full
predict(smd.SpeciesStudyExp)
predict(smd.Full)
I2(smd.SpeciesStudyExp, dat_smd$vi)
I2(smd.Full, dat_smd$vi, phylo = "SpeciesSci")


#lnVR
vr.SpeciesStudyExp
vr.Full
predict(vr.SpeciesStudyExp)
predict(vr.Full)
I2(vr.SpeciesStudyExp, dat_vr$vi)
I2(vr.Full, dat_vr$vi, phylo = "SpeciesSci")


#Fig S5
figS5<-NULL
figS5$estimate <- c(vr.SpeciesStudyExp$b, vr.Full$b, smd.SpeciesStudyExp$b, smd.Full$b)
figS5$lower <- c(vr.SpeciesStudyExp$ci.lb, vr.Full$ci.lb, smd.SpeciesStudyExp$ci.lb, smd.Full$ci.lb)
figS5$upper <- c(vr.SpeciesStudyExp$ci.ub, vr.Full$ci.ub, smd.SpeciesStudyExp$ci.ub, smd.Full$ci.ub)
figS5$pos <- c(.75,1.5,3.5,4.25)
figS5$labels <- c("lnVR (non-phylo)","lnVR (phylo)","SMDH (non-phylo)","SMDH (phylo)")
figS5$k <- c("k = 1248","k = 1248","k = 1334","k = 1334")
figS5<-as.data.frame(figS5)


Fig.S5 <- ggplot(figS5, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = figS5$lower, y = figS5$pos, xend = figS5$upper, yend = figS5$pos, size = 1, colour = "black") + 
  geom_point(size = 4, shape = 21, fill = "white") +
  geom_text(aes(label=figS5$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=figS5$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) +
  labs(x = "Effect Size, SMDH/lnVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0, 5)

Fig.S5

#("figures/Fig.S4.pdf", width = 8, height = 12, units = "cm", Fig.S4, dpi = 600)
ggsave("figures/Fig.S5.jpg", width = 8, height = 12, units = "cm", Fig.S5, dpi = 600)



###########################_################################

#MODERATOR MODELS (TABLES 2-5) ----
#Code used to calculate n per category estimate
n_distinct(subset(dat_cvr, RiskContext == "novelenvironment_activity")$StudyID)

#Hypothesis Testing
summary(dat_rr$RiskContext)
rr.Full.h2
summary(dat_cvr$RiskContext)
cvr.Full.h2

summary(dat_rr$Sex)
rr.Full.h3
summary(dat_cvr$Sex)
cvr.Full.h3

summary(dat_rr$ManipLifeStage)
rr.Full.h4
summary(dat_cvr$ManipLifeStage)
cvr.Full.h4

rr.Full.h5.i
cvr.Full.h5.i
rr.Full.h5.ii
cvr.Full.h5.ii

#Publication Bias
rr.Full.pub1
rr.Full.pub2

summary(dat_rr$EffectSizesFromPublication)
rr.Full.pub3

#Exploratory Models
summary(dat_rr$ManipType)
summary(dat_cvr$ManipType)
rr.Full.exp.d
cvr.Full.exp.d

summary(dat_rr$ManipDirection)
summary(dat_cvr$ManipDirection)
rr.Full.exp.b
cvr.Full.exp.b

rr.Full.exp.c
cvr.Full.exp.c

summary(dat_rr$WildLabRear)
summary(dat_cvr$WildLabRear)
rr.Full.exp.d
cvr.Full.exp.d



#Output for Table 2
output<-NULL
output$model<-c("rr.Full.h2","rr.Full.h3","rr.Full.h4","rr.Full.h5.i","rr.Full.h5.ii","rr.Full.pub1","rr.Full.pub2","rr.Full.pub3","rr.Full.exp.a","rr.Full.exp.b","rr.Full.exp.c","rr.Full.exp.d")
output$k<-c(rr.Full.h2$k,rr.Full.h3$k,rr.Full.h4$k,rr.Full.h5.i$k,rr.Full.h5.ii$k,rr.Full.pub1$k,rr.Full.pub2$k,rr.Full.pub3$k,rr.Full.exp.a$k,rr.Full.exp.b$k,rr.Full.exp.c$k,rr.Full.exp.d$k)
output$QE<-c(rr.Full.h2$QE,rr.Full.h3$QE,rr.Full.h4$QE,rr.Full.h5.i$QE,rr.Full.h5.ii$QE,rr.Full.pub1$QE,rr.Full.pub2$QE,rr.Full.pub3$QE,rr.Full.exp.a$QE,rr.Full.exp.b$QE,rr.Full.exp.c$QE,rr.Full.exp.d$QE)
output$QEp<-c(rr.Full.h2$QEp,rr.Full.h3$QEp,rr.Full.h4$QEp,rr.Full.h5.i$QEp,rr.Full.h5.ii$QEp,rr.Full.pub1$QEp,rr.Full.pub2$QEp,rr.Full.pub3$QEp,rr.Full.exp.a$QEp,rr.Full.exp.b$QEp,rr.Full.exp.c$QEp,rr.Full.exp.d$QEp)
output$QM<-c(rr.Full.h2$QM,rr.Full.h3$QM,rr.Full.h4$QM,rr.Full.h5.i$QM,rr.Full.h5.ii$QM,rr.Full.pub1$QM,rr.Full.pub2$QM,rr.Full.pub3$QM,rr.Full.exp.a$QM,rr.Full.exp.b$QM,rr.Full.exp.c$QM,rr.Full.exp.d$QM)
output$QMp<-c(rr.Full.h2$QMp,rr.Full.h3$QMp,rr.Full.h4$QMp,rr.Full.h5.i$QMp,rr.Full.h5.ii$QMp,rr.Full.pub1$QMp,rr.Full.pub2$QMp,rr.Full.pub3$QMp,rr.Full.exp.a$QMp,rr.Full.exp.b$QMp,rr.Full.exp.c$QMp,rr.Full.exp.d$QMp)
output$R2a<-c(sum(rr.Full.h2$sigma2),sum(rr.Full.h3$sigma2),sum(rr.Full.h4$sigma2),sum(rr.Full.h5.i$sigma2),sum(rr.Full.h5.ii$sigma2),sum(rr.Full.pub1$sigma2),sum(rr.Full.pub2$sigma2),sum(rr.Full.pub3$sigma2),sum(rr.Full.exp.a$sigma2),sum(rr.Full.exp.b$sigma2),sum(rr.Full.exp.c$sigma2),sum(rr.Full.exp.d$sigma2))
output$R2b<-sum(rr.Full$sigma2)
output$R2<-((output$R2b-output$R2a)/output$R2b) #using a psuedo r2 estimate as the sum of variance components in the moderator model and a percentage of summed variance components in the non-moderator model

output<-as.data.frame(output)
output$QE<-round(output$QE, digits = 2)
output$QEp <- "< 0.0001" #as all p values are below 0.0001
output$QM <- round(output$QM, digits = 2)
output$QMp <- round(output$QMp, digits = 4)
output$R2 <- round(output$R2, digits = 4)
output$R2 <- 100*output$R2

output <- transform(output, Residual = paste(QE, QEp, sep = " p "))
output <- transform(output, Moderator = paste(QM, QMp, sep = " p = "))

write.csv(output, "figures/Table2.csv",)


#Output for Table 3
output<-NULL
output$model<-c("cvr.Full.h2","cvr.Full.h3","cvr.Full.h4","cvr.Full.h5.i","cvr.Full.h5.ii","cvr.Full.exp.a","cvr.Full.exp.b","cvr.Full.exp.c","cvr.Full.exp.d")
output$k<-c(cvr.Full.h2$k,cvr.Full.h3$k,cvr.Full.h4$k,cvr.Full.h5.i$k,cvr.Full.h5.ii$k,cvr.Full.exp.a$k,cvr.Full.exp.b$k,cvr.Full.exp.c$k,cvr.Full.exp.d$k)
output$QE<-c(cvr.Full.h2$QE,cvr.Full.h3$QE,cvr.Full.h4$QE,cvr.Full.h5.i$QE,cvr.Full.h5.ii$QE,cvr.Full.exp.a$QE,cvr.Full.exp.b$QE,cvr.Full.exp.c$QE,cvr.Full.exp.d$QE)
output$QEp<-c(cvr.Full.h2$QEp,cvr.Full.h3$QEp,cvr.Full.h4$QEp,cvr.Full.h5.i$QEp,cvr.Full.h5.ii$QEp,cvr.Full.exp.a$QEp,cvr.Full.exp.b$QEp,cvr.Full.exp.c$QEp,cvr.Full.exp.d$QEp)
output$QM<-c(cvr.Full.h2$QM,cvr.Full.h3$QM,cvr.Full.h4$QM,cvr.Full.h5.i$QM,cvr.Full.h5.ii$QM,cvr.Full.exp.a$QM,cvr.Full.exp.b$QM,cvr.Full.exp.c$QM,cvr.Full.exp.d$QM)
output$QMp<-c(cvr.Full.h2$QMp,cvr.Full.h3$QMp,cvr.Full.h4$QMp,cvr.Full.h5.i$QMp,cvr.Full.h5.ii$QMp,cvr.Full.exp.a$QMp,cvr.Full.exp.b$QMp,cvr.Full.exp.c$QMp,cvr.Full.exp.d$QMp)
output$R2a<-c(sum(cvr.Full.h2$sigma2),sum(cvr.Full.h3$sigma2),sum(cvr.Full.h4$sigma2),sum(cvr.Full.h5.i$sigma2),sum(cvr.Full.h5.ii$sigma2),sum(cvr.Full.exp.a$sigma2),sum(cvr.Full.exp.b$sigma2),sum(cvr.Full.exp.c$sigma2),sum(cvr.Full.exp.d$sigma2))
output$R2b<-sum(cvr.Full$sigma2)
output$R2<-((output$R2b-output$R2a)/output$R2b) #using a psuedo r2 estimate as the sum of variance components in the moderator model and a percentage of summed variance components in the non-moderator model

output<-as.data.frame(output)
output$QE<-round(output$QE, digits = 2)
output$QEp <- "< 0.0001" #as all p values are below 0.0001
output$QM <- round(output$QM, digits = 2)
output$QMp <- round(output$QMp, digits = 4)
output$R2 <- round(output$R2, digits = 4)
output$R2 <- 100*output$R2

output <- transform(output, Residual = paste(QE, QEp, sep = " p "))
output <- transform(output, Moderator = paste(QM, QMp, sep = " p = "))

write.csv(output, "figures/Table3.csv",)


#Output for Table 4
output<-NULL
output$estimate<-c(rr.Full.h2$beta,rr.Full.h3$beta,rr.Full.h4$beta,rr.Full.h5.i$beta,rr.Full.h5.ii$beta,rr.Full.pub1$beta,rr.Full.pub2$beta,rr.Full.pub3$beta,rr.Full.exp.a$beta,rr.Full.exp.b$beta,rr.Full.exp.c$beta,rr.Full.exp.d$beta)
output$lb<-c(rr.Full.h2$ci.lb,rr.Full.h3$ci.lb,rr.Full.h4$ci.lb,rr.Full.h5.i$ci.lb,rr.Full.h5.ii$ci.lb,rr.Full.pub1$ci.lb,rr.Full.pub2$ci.lb,rr.Full.pub3$ci.lb,rr.Full.exp.a$ci.lb,rr.Full.exp.b$ci.lb,rr.Full.exp.c$ci.lb,rr.Full.exp.d$ci.lb)
output$ub<-c(rr.Full.h2$ci.ub,rr.Full.h3$ci.ub,rr.Full.h4$ci.ub,rr.Full.h5.i$ci.ub,rr.Full.h5.ii$ci.ub,rr.Full.pub1$ci.ub,rr.Full.pub2$ci.ub,rr.Full.pub3$ci.ub,rr.Full.exp.a$ci.ub,rr.Full.exp.b$ci.ub,rr.Full.exp.c$ci.ub,rr.Full.exp.d$ci.ub)
output<-as.data.frame(output)
output$estimate <- round(output$estimate, digits = 2)
output$lb <- round(output$lb, digits = 2)
output$ub <- round(output$ub, digits = 2)
output <- transform(output, Estimate = paste(estimate, lb, sep = " ["))
output <- transform(output, Estimate = paste(Estimate, ub, sep = ", "))
output$x <- ""
output <- transform(output, Estimate = paste(Estimate, x, sep = "]"))
write.csv(output, "figures/Table4.csv",)


#Output for Table 5
output<-NULL
output$estimate<-c(cvr.Full.h2$beta,cvr.Full.h3$beta,cvr.Full.h4$beta,cvr.Full.h5.i$beta,cvr.Full.h5.ii$beta,cvr.Full.exp.a$beta,cvr.Full.exp.b$beta,cvr.Full.exp.c$beta,cvr.Full.exp.d$beta)
output$lb<-c(cvr.Full.h2$ci.lb,cvr.Full.h3$ci.lb,cvr.Full.h4$ci.lb,cvr.Full.h5.i$ci.lb,cvr.Full.h5.ii$ci.lb,cvr.Full.exp.a$ci.lb,cvr.Full.exp.b$ci.lb,cvr.Full.exp.c$ci.lb,cvr.Full.exp.d$ci.lb)
output$ub<-c(cvr.Full.h2$ci.ub,cvr.Full.h3$ci.ub,cvr.Full.h4$ci.ub,cvr.Full.h5.i$ci.ub,cvr.Full.h5.ii$ci.ub,cvr.Full.exp.a$ci.ub,cvr.Full.exp.b$ci.ub,cvr.Full.exp.c$ci.ub,cvr.Full.exp.d$ci.ub)
output<-as.data.frame(output)
output$estimate <- round(output$estimate, digits = 2)
output$lb <- round(output$lb, digits = 2)
output$ub <- round(output$ub, digits = 2)
output <- transform(output, Estimate = paste(estimate, lb, sep = " ["))
output <- transform(output, Estimate = paste(Estimate, ub, sep = ", "))
output$x <- ""
output <- transform(output, Estimate = paste(Estimate, x, sep = "]"))
write.csv(output, "figures/Table5.csv",)



###########################_################################

#HYPOTHESIS TESTING MODELS (FIG 2) ----
#Fig 2a, 2b
fig2a<-NULL; fig2b<-NULL
fig2a$estimate <- rr.Full.h2$b
fig2a$lower <- rr.Full.h2$ci.lb
fig2a$upper <- rr.Full.h2$ci.ub
fig2a$pos <- c(2, 13:6, 1, 5:3)
fig2a$labels <- c("Dispersal/migration", "Novel env (activity)",
                  "Novel env (exploration)","Novel env (feeding)",
                  "Novel env (light-dark test)","Novel env (refuge emerg.)",
                  "Novel env (refuge use)","Novel env (shoaling)",
                  "Novel object response","Other","Predation (feeding)",
                  "Predation (response)","Predation (shoaling)")
fig2a$k <- c("k = 15","k = 248","k = 153","k = 331","k = 26","k = 39","k = 75","k = 29","k = 92","k = 16","k = 81","k = 172","k = 20")
fig2a$cex.k <- c(15,248,153,331,26,39,75,29,92,16,81,172,20)
fig2a$cex.k.area <- sqrt(fig2a$cex.k/pi)
fig2b$estimate <- cvr.Full.h2$b
fig2b$lower <- cvr.Full.h2$ci.lb
fig2b$upper <- cvr.Full.h2$ci.ub
fig2b$pos <- fig2a$pos
fig2b$labels <- fig2a$labels
fig2b$k <- c("k = 13","k = 248","k = 153","k = 312","k = 24","k = 39","k = 75","k = 29","k = 88","k = 6","k = 61","k = 167","k = 20")
fig2b$cex.k <- c(13,248,153,312,24,39,75,29,88,6,61,167,20)
fig2b$cex.k.area <- sqrt(fig2b$cex.k/pi)
fig2a<-as.data.frame(fig2a); fig2b<-as.data.frame(fig2b)


Fig.2a <- ggplot(fig2a, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "lightblue", cex = fig2b$cex.k.area) +
  geom_point(size = 0.9, shape = 19, fill = 1) +  
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig2a$lower, y = fig2a$pos, xend = fig2a$upper, yend = fig2a$pos, size = 0.60) + 
  geom_text(aes(label=fig2a$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig2a$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnRR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 13.75)

Fig.2a

Fig.2b <- ggplot(fig2b, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "lightblue", cex = fig2b$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +  
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig2b$lower, y = fig2b$pos, xend = fig2b$upper, yend = fig2b$pos, size = 0.60) + 
  geom_text(aes(label=fig2b$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnCVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 13.75)

Fig.2b


ggsave("figures/Fig.2a.jpg", width = 8, height = 16, units = "cm", Fig.2a, dpi = 600)
ggsave("figures/Fig.2b.jpg", width = 8, height = 16, units = "cm", Fig.2b, dpi = 600)


#Fig 2c, 2d
summary(dat_rr$Sex)
fig2c<-NULL; fig2d<-NULL
fig2c$estimate <- rr.Full.h3$b
fig2c$lower <- rr.Full.h3$ci.lb
fig2c$upper <- rr.Full.h3$ci.ub
fig2c$pos <- c(4:1)
fig2c$labels <- c("Female", "Male",
                  "Mixed","Unknown")
fig2c$k <- c("k = 421", "k = 291",
             "k = 120","k = 465")
fig2c$cex.k <- c(421,291,120,465)
fig2c$cex.k.area <- sqrt(fig2c$cex.k/pi)
fig2d$estimate <- cvr.Full.h3$b
fig2d$lower <- cvr.Full.h3$ci.lb
fig2d$upper <- cvr.Full.h3$ci.ub
fig2d$pos <- fig2c$pos
fig2d$labels <- fig2c$labels
fig2d$k <- c("k = 401", "k = 276",
             "k = 117","k = 441")
fig2d$cex.k <- c(401,276,117,441)
fig2d$cex.k.area <- sqrt(fig2d$cex.k/pi)
fig2c<-as.data.frame(fig2c); fig2d<-as.data.frame(fig2d)


Fig.2c <- ggplot(fig2c, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "lightblue", cex = fig2c$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +  
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig2c$lower, y = fig2c$pos, xend = fig2c$upper, yend = fig2c$pos, size = 0.60) + 
  geom_text(aes(label=fig2c$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig2c$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnRR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 5)

Fig.2c

Fig.2d <- ggplot(fig2d, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "lightblue", cex = fig2d$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig2d$lower, y = fig2d$pos, xend = fig2d$upper, yend = fig2d$pos, size = 0.60) + 
  geom_text(aes(label=fig2d$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnCVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 5)

Fig.2d

ggsave("figures/Fig.2c.jpg", width = 8, height = 6.5, units = "cm", Fig.2c, dpi = 600)
ggsave("figures/Fig.2d.jpg", width = 8, height = 6.5, units = "cm", Fig.2d, dpi = 600)



#Fig 2e, 2f
fig2e<-NULL; fig2f<-NULL
fig2e$estimate <- rr.Full.h4$b
fig2e$lower <- rr.Full.h4$ci.lb
fig2e$upper <- rr.Full.h4$ci.ub
fig2e$pos <- c(3, 2, 4, 1, NA)
fig2e$labels <- c("Adult", "Both",
                  "Juvenile","Unknown/Mixed", NA)
fig2e$k <- c("k = 423", "k = 166",
             "k = 601","k = 94", NA)
fig2e$cex.k <- c(423,166,601,94,NA)
fig2e$cex.k.area <- sqrt(fig2e$cex.k/pi)
fig2f$estimate <- cvr.Full.h4$b
fig2f$lower <- cvr.Full.h4$ci.lb
fig2f$upper <- cvr.Full.h4$ci.ub
fig2f$pos <- fig2e$pos
fig2f$labels <- fig2e$labels
fig2f$k <- c("k = 402", "k = 166",
             "k = 578","k = 89", NA)
fig2f$cex.k <- c(402,166,578,89,NA)
fig2f$cex.k.area <- sqrt(fig2f$cex.k/pi)
fig2e<-as.data.frame(fig2e); fig2f<-as.data.frame(fig2f)
fig2e <- subset(fig2e, pos != "NA"); fig2f <- subset(fig2f, pos != "NA") #excluding continuous moderator from figure

Fig.2e <- ggplot(fig2e, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "lightblue", cex = fig2e$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig2e$lower, y = fig2e$pos, xend = fig2e$upper, yend = fig2e$pos, size = 0.60) + 
  geom_text(aes(label=fig2e$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig2e$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnRR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 5)

Fig.2e

Fig.2f <- ggplot(fig2f, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "lightblue", cex = fig2f$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig2f$lower, y = fig2f$pos, xend = fig2f$upper, yend = fig2f$pos, size = 0.60) + 
  geom_text(aes(label=fig2f$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnCVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 5)

Fig.2f

ggsave("figures/Fig.2e.jpg", width = 8, height = 6.5, units = "cm", Fig.2e, dpi = 600)
ggsave("figures/Fig.2f.jpg", width = 8, height = 6.5, units = "cm", Fig.2f, dpi = 600)



###########################_################################

##PARTIAL PUBLICATION MODEL (FIG 3) ----

rr.Full.pub3
summary(dat_rr$EffectSizesFromPublication)

fig3<-NULL
fig3$estimate <- rr.Full.pub3$b
fig3$lower <- rr.Full.pub3$ci.lb
fig3$upper <- rr.Full.pub3$ci.ub
fig3$pos <- c(1.20,2,2.80)
fig3$labels <- c("Not reported","Partially reported","Fully reported")
fig3$k <- c("k = 130","k = 360","k = 807")
fig3$cex.k <- c(130,360,807)
fig3$cex.k.area <- sqrt(fig3$cex.k/pi)
fig3<-as.data.frame(fig3)


Fig.3 <- ggplot(fig3, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "seagreen2", cex = fig3$cex.k.area) +
  geom_point(size = 0.9, shape = 19, fill = 1) +  
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig3$lower, y = fig3$pos, xend = fig3$upper, yend = fig3$pos, size = 0.6) + 
  geom_text(aes(label=fig3$labels), hjust = "left", x =-0.52, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig3$k), hjust = "left", x =-0.52, vjust=0.7, size = 2.5) +
  labs(x = "Effect Size, lnRR",
       y = "") +
  xlim(-0.5, 0.5) +
  ylim(0.5, 3.5)
 
Fig.3

ggsave("figures/Fig.3.jpg", width = 8, height = 7, units = "cm", Fig.3, dpi = 600)


###########################_################################

##EXPLORATORY MODELS (FIG 4) ----
#Fig4a,b
summary(dat_rr$ManipType)
summary(dat_cvr$ManipType)
rr.Full.exp.a
cvr.Full.exp.a

fig4a<-NULL; fig4b<-NULL
fig4a$estimate <- rr.Full.exp.a$b
fig4a$lower <- rr.Full.exp.a$ci.lb
fig4a$upper <- rr.Full.exp.a$ci.ub
fig4a$pos <- c(5,3.667,2.334,1)
fig4a$labels <- c("Combined", "Quality",
                  "Quantity","Starvation")
fig4a$k <- c("k = 24", "k = 248",
             "k = 390","k = 635")
fig4a$cex.k <- c(24,248,390,635)
fig4a$cex.k.area <- sqrt(fig4a$cex.k/pi)
fig4b$estimate <- cvr.Full.exp.a$b
fig4b$lower <- cvr.Full.exp.a$ci.lb
fig4b$upper <- cvr.Full.exp.a$ci.ub
fig4b$pos <- fig4a$pos
fig4b$labels <- fig4a$labels
fig4b$k <- c("k = 24", "k = 246",
             "k = 363","k = 602")
fig4b$cex.k <- c(24,246,469,602)
fig4b$cex.k.area <- sqrt(fig4b$cex.k/pi)
fig4a<-as.data.frame(fig4a); fig4b<-as.data.frame(fig4b)


Fig.4a <- ggplot(fig4a, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "orange1", cex = fig4a$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig4a$lower, y = fig4a$pos, xend = fig4a$upper, yend = fig4a$pos, size = 0.60) + 
  geom_text(aes(label=fig4a$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig4a$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnRR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 6)

Fig.4a

Fig.4b <- ggplot(fig4b, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "orange1", cex = fig4b$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig4b$lower, y = fig4b$pos, xend = fig4b$upper, yend = fig4b$pos, size = 0.60) + 
  geom_text(aes(label=fig4b$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnCVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 6)

Fig.4b

ggsave("figures/Fig.4a.jpg", width = 8, height = 6.5, units = "cm", Fig.4a, dpi = 600)
ggsave("figures/Fig.4b.jpg", width = 8, height = 6.5, units = "cm", Fig.4b, dpi = 600)


#Fig4c,d
summary(dat_rr$ManipDirection)
summary(dat_cvr$ManipDirection)
rr.Full.exp.b
cvr.Full.exp.b

fig4c<-NULL; fig4d<-NULL
fig4c$estimate <- rr.Full.exp.b$b
fig4c$lower <- rr.Full.exp.b$ci.lb
fig4c$upper <- rr.Full.exp.b$ci.ub
fig4c$pos <- c(4,2.5,1)
fig4c$labels <- c("Dual", "Restriction",
                  "Supplementation")
fig4c$k <- c("k = 60", "k = 1170",
             "k = 67")
fig4c$cex.k <- c(60,1170,67)
fig4c$cex.k.area <- sqrt(fig4c$cex.k/pi)
fig4d$estimate <- cvr.Full.exp.b$b
fig4d$lower <- cvr.Full.exp.b$ci.lb
fig4d$upper <- cvr.Full.exp.b$ci.ub
fig4d$pos <- fig4c$pos
fig4d$labels <- fig4c$labels
fig4d$k <- c("k = 60", "k = 1116",
             "k = 59")
fig4d$cex.k <- c(60,1116,59)
fig4d$cex.k.area <- sqrt(fig4d$cex.k/pi)
fig4c<-as.data.frame(fig4c); fig4d<-as.data.frame(fig4d)


Fig.4c <- ggplot(fig4c, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "orange1", cex = fig4d$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig4c$lower, y = fig4c$pos, xend = fig4c$upper, yend = fig4c$pos, size = 0.60) + 
  geom_text(aes(label=fig4c$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig4c$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnRR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 5)


Fig.4c

Fig.4d <- ggplot(fig4d, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "orange1", cex = fig4d$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig4d$lower, y = fig4d$pos, xend = fig4d$upper, yend = fig4d$pos, size = 0.60) + 
  geom_text(aes(label=fig4d$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnCVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 5)

Fig.4d

ggsave("figures/Fig.4c.jpg", width = 8, height = 5, units = "cm", Fig.4c, dpi = 600)
ggsave("figures/Fig.4d.jpg", width = 8, height = 5, units = "cm", Fig.4d, dpi = 600)


#Fig 4e,f
summary(dat_rr$WildLabRear)
summary(dat_cvr$WildLabRear)
rr.Full.exp.d
cvr.Full.exp.d

fig4e<-NULL; fig4f<-NULL
fig4e$estimate <- rr.Full.exp.d$b
fig4e$lower <- rr.Full.exp.d$ci.lb
fig4e$upper <- rr.Full.exp.d$ci.ub
fig4e$pos <- c(5,3.667,2.334,1)
fig4e$labels <- c("Commercial", "Laboratory",
                  "Mixed","Wild")
fig4e$k <- c("k = 139", "k = 711",
             "k = 15","k = 432")
fig4e$cex.k <- c(139,711,15,432)
fig4e$cex.k.area <- sqrt(fig4e$cex.k/pi)
fig4f$estimate <- cvr.Full.exp.d$b
fig4f$lower <- cvr.Full.exp.d$ci.lb
fig4f$upper <- cvr.Full.exp.d$ci.ub
fig4f$pos <- fig4e$pos
fig4f$labels <- fig4e$labels
fig4f$k <- c("k = 127", "k = 679",
             "k = 15","k = 414")
fig4f$cex.k <- c(127,679,15,414)
fig4f$cex.k.area <- sqrt(fig4f$cex.k/pi)
fig4e<-as.data.frame(fig4e); fig4f<-as.data.frame(fig4f)


Fig.4e <- ggplot(fig4e, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "orange1", cex = fig4e$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig4e$lower, y = fig4e$pos, xend = fig4e$upper, yend = fig4e$pos, size = 0.60) + 
  geom_text(aes(label=fig4e$labels), hjust = "left", x =-1.04, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=fig4e$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnRR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 6)


Fig.4e

Fig.4f <- ggplot(fig4f, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "orange1", cex = fig4f$cex.k.area)  +
  geom_point(size = 0.9, shape = 19, fill = 1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = fig4f$lower, y = fig4f$pos, xend = fig4f$upper, yend = fig4f$pos, size = 0.60) + 
  geom_text(aes(label=fig4f$k), hjust = "left", x =-1.04, vjust=0.7, size = 2.5) + 
  labs(x = "Effect size, lnCVR",
       y = "") +
  xlim(-1, 1) +
  ylim(0.25, 6)

Fig.4f

ggsave("figures/Fig.4e.jpg", width = 8, height = 6.5, units = "cm", Fig.4e, dpi = 600)
ggsave("figures/Fig.4f.jpg", width = 8, height = 6.5, units = "cm", Fig.4f, dpi = 600)




###########################_################################

##SENSITIVITY ANALYSIS: DATA SOURCE (FIG S4) ----
rr.sen1
cvr.sen1
predict(rr.sen1)
predict(cvr.sen1)
I2(rr.sen1, dat_rr_sen1$vi, phylo = "SpeciesSci")
I2(cvr.sen1, dat_cvr_sen1$vi, phylo = "SpeciesSci")


#Fig S4
figS4<-NULL
figS4$estimate <- c(cvr.Full$b, cvr.sen1$b, rr.Full$b, rr.sen1$b)
figS4$lower <- c(cvr.Full$ci.lb, cvr.sen1$ci.lb, rr.Full$ci.lb, rr.sen1$ci.lb)
figS4$upper <- c(cvr.Full$ci.ub, cvr.sen1$ci.ub, rr.Full$ci.ub, rr.sen1$ci.ub)
figS4$pos <- c(.75,1.5,3.5,4.25)
figS4$labels <- c("lnCVR (full)","lnCVR (reduced)","lnRR (full)","lnRR (reduced)")
figS4$k <- c("k = 1235","k = 1211","k = 1297","k = 1272")
figS4<-as.data.frame(figS4)


Fig.S4 <- ggplot(figS4, aes(x = estimate, y = pos)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(size = 4, shape = 21, fill = "black") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.75) +
  geom_segment(x = figS4$lower, y = figS4$pos, xend = figS4$upper, yend = figS4$pos, size = 1) + 
  geom_text(aes(label=figS4$labels), hjust = "left", x =-0.52, vjust=-0.9, size = 2.5) +
  geom_text(aes(label=figS4$k), hjust = "left", x =-0.52, vjust=0.7, size = 2.5) +
  labs(x = "Effect Size, lnRR/lnCVR",
       y = "") +
  xlim(-.5, .5) +
  ylim(0, 5)

Fig.S4

#ggsave("figures/Fig.S4.pdf", width = 8, height = 12, units = "cm",Fig.S4, dpi = 600)
ggsave("figures/Fig.S4.jpg", width = 8, height = 12, units = "cm", Fig.S4, dpi = 600)




###########################_################################

##TIME-LAG BIAS PLOT ----
labels(rr.Full.pub2)
rr.Full.pub2$formula.mods
labels(dat_rr_pub)
summary(dat_rr_pub$Year)
summary(dat_rr_pub$yi)

figtimelag <- NULL
figtimelag$year <- dat_rr_pub$Year
figtimelag$lnrr <- dat_rr_pub$yi
figtimelag$cex.precision <- dat_rr_pub$Precision
figtimelag$cex.precision.smol <- (figtimelag$cex.precision) / 10
figtimelag <- as.data.frame(figtimelag)

#using input dataset and intercept and effect estimates to show covariate trend line
subset(dat_rr_pub, Year == 2019) #finding year.c values
subset(dat_rr_pub, Year == 1977)
#using covariate effect of -0.0531, and intercept to predict lnrr at year.c = -3.177435 (1977), and year.c = 1.205691 (2019)
0.2553+(-0.0475*-3.168145) #0.4057869
0.2553+(-0.0475*1.211652) #0.1977465

Fig.TimeLag <- ggplot(figtimelag, aes(x = year, y = lnrr)) +
  theme(axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_point(shape = 21, fill = "mediumpurple", cex = figtimelag$cex.precision.smol)  +
  geom_segment(x = 1977, y = 0.4057869, xend = 2019, yend = 0.1977465, size = 0.60) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black", size = 0.6) +
  labs(x = "Year",
       y = "lnRR") +
  xlim(1975, 2019) +
  ylim(-6, 6)

Fig.TimeLag

ggsave("figures/Fig.TimeLag.jpg", width = 20, height = 8, units = "cm", Fig.TimeLag, dpi = 600)
