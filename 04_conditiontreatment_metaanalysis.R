##########_____Meta-analysis project (NC3 DO5)_____#########
##TITLE: Poor nutritional condition promotes high-risk behaviours: A systematic review and meta-analysis


###########################_################################

###CALCULATING EFFECT SIZES ----
library(metafor); library(dplyr)
#Re-importing processed dataset
dat <- read.csv("CD_processed.csv", strip.white = TRUE) 


#Calculating mean effect sizes: Hedges G (with correction for heteroscedastic variance, "SMDH"), Log ratio of means ("lnRR")
#note: coded so that a positive effect size is associated with incresed risk taking in the low condition treatment group)
##SMDH (adjusted to using the heterscedastic)
dat_smd <- escalc(measure="SMDH", n1i=nLC, n2i=nHC, m1i=meanLC, m2i=meanHC, sd1i=sdLC, sd2i=sdHC, data=dat)
dat_smd <- as.data.frame(dat_smd) 
nrow(dat_smd); n_distinct(dat_smd$StudyID) #1334 effect sizes from 128 studies


##lnRR
summary((subset(dat, Exclude == "exclude_lnrr")$ExclusionReason)) 
#Summary of exclusion reasons 
#        data cannot be used to calculate a sampling variance: 2 effect sizes
#        data produces extemely high sampling variance which reduced model stability: 1 effect size
#        data cannot be used to calculate log ratio: 20 effect sizes (e.g. meanHC or mean LC is zero)
#        non-ratio scale data: 14 effect sizes
dat_rr <- subset(dat, Exclude != "exclude_lnrr")
dat_rr <- escalc(measure="ROM", n1i=nLC, n2i=nHC, m1i=meanLC, m2i=meanHC, sd1i=sdLC, sd2i=sdHC, data=dat_rr)
dat_rr <- as.data.frame(dat_rr)
nrow(dat_rr); n_distinct(dat_rr$StudyID) #1297 effect sizes from 126 studies


#Calculating variance effect sizes: Log coefficient of variation ratio (lnCVR), Log variability ratio (lnVR)
##lnCVR
summary((subset(dat, Exclude == "exclude_lncvr")$ExclusionReason)) 
#Summary of exclusion reasons 
#        group level proportional data: 62 effect sizes
#        data cannot be used to calculate log ratio: 4 effect sizes (e.g. sdHC or mean sdLC is zero)
dat_cvr <- subset(subset(dat, Exclude != "exclude_lnrr"), Exclude != "exclude_lncvr")
dat_cvr <- escalc(measure="CVR", n1i=nLC, n2i=nHC, m1i=meanLC, m2i=meanHC, sd1i=sdLC, sd2i=sdHC, data=dat_cvr)
dat_cvr <- as.data.frame(dat_cvr)
nrow(dat_cvr); n_distinct(dat_cvr$StudyID) #1235 effect sizes from 119 studies


##lnVR
dat_vr <- subset(subset(dat, Exclude != "exclude_lnrr"), Exclude != "exclude_lncvr") #effect sizes excluded for lncvr also excluded for lnvr
dat_vr2 <- subset(subset(dat, ExclusionReason == "non-ratio scale data")) #non-ratio scale data is fine for lnvr, so adding them back in
dat_vr <- rbind(dat_vr, dat_vr2)
dat_vr <- escalc(measure="VR", n1i=nLC, n2i=nHC, m1i=meanLC, m2i=meanHC, sd1i=sdLC, sd2i=sdHC, data=dat_vr)
dat_vr <- as.data.frame(dat_vr)
nrow(dat_vr); n_distinct(dat_vr$StudyID) #1249 effect sizes from 120 studies
summary(dat_vr)


###########################_################################

###SIGN INVERSIONS ----
#note: as a positive effect size for lnCVR and lnVR already reflects an increase in the variation in the treatment v control group,
#sign corr only apply to the mean effects
dat_rr$Direction <- ifelse(dat_rr$RiskEffectDirection == "negative", dat_rr$Direction <- -1, dat_rr$Direction <- 1)
dat_rr$yi <- (dat_rr$yi*dat_rr$Direction)

dat_smd$Direction <- ifelse(dat_smd$RiskEffectDirection == "negative", dat_smd$Direction <- -1, dat_smd$Direction <- 1)
dat_smd$yi <- (dat_smd$yi*dat_smd$Direction)


###########################_################################

###CENTERING, Z-TRANSFORMING MODERATORS ----

##Centering, z-tansforming continuous moderators for each dataset to aid interpretation
dat_rr$RelativeTimeFromTreatment.C <- scale(dat_rr$RelativeTimeFromTreatment)
dat_rr$MaxLongevity.C <- scale(dat_rr$MaxLongevity)
dat_rr$lnMaxLongevity.C <- scale(dat_rr$lnMaxLongevity)
dat_rr$RelativeManipDuration.C <- scale(dat_rr$RelativeManipDuration)
dat_cvr$RelativeTimeFromTreatment.C <- scale(dat_cvr$RelativeTimeFromTreatment)
dat_cvr$MaxLongevity.C <- scale(dat_cvr$MaxLongevity)
dat_cvr$lnMaxLongevity.C <- scale(dat_cvr$lnMaxLongevity)
dat_cvr$RelativeManipDuration.C <- scale(dat_cvr$RelativeManipDuration)


###########################_################################

###BUILDING VAR-COVAR MATRICES ----
##adapted from function "make_VCV_matrix" in "daniel1noble/metaAidR" package
#VCV matrix for dat_rr
#calculates shared covariance between effect sizes as (Sharedsd)^2/((SharedN)*(Sharedmean^2)), per Lajeunesse 2011
VCV_rr <- matrix(0, nrow = dim(dat_rr)[1], ncol = dim(dat_rr)[1])
rownames(VCV_rr) <- dat_rr[, "EffectID"]
colnames(VCV_rr) <- dat_rr[, "EffectID"]

shared_coord <- which(dat_rr[, "SharedGroup"] %in% dat_rr[duplicated(dat_rr[, "SharedGroup"]), "SharedGroup"] == TRUE)
combinations <- do.call("rbind", tapply(shared_coord, dat_rr[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2)))) 
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- (dat_rr[p1, "Sharedsd"])^2 / 
      ((dat_rr[p1, "SharedN"]) * ((dat_rr[p1, "Sharedmean"])^2))
   VCV_rr[p1, p2] <- p1_p2_cov
   VCV_rr[p2, p1] <- p1_p2_cov
} 
diag(VCV_rr) <- dat_rr[, "vi"] 


#VCV matrix for dat_smd
#calculates shared covariance between effect sizes assuming a 0.5 correlation (0.5 * sqrt(vi.1) * sqrt(vi.2))
VCV_smd <- matrix(0, nrow = dim(dat_smd)[1], ncol = dim(dat_smd)[1])
rownames(VCV_smd) <- dat_smd[, "EffectID"]
colnames(VCV_smd) <- dat_smd[, "EffectID"]

shared_coord <- which(dat_smd[, "SharedGroup"] %in% dat_smd[duplicated(dat_smd[, "SharedGroup"]), "SharedGroup"] == TRUE) #finds effect sizes that share a control group
combinations <- do.call("rbind", tapply(shared_coord, dat_smd[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2)))) #finds each combination of effect sizes with a shared control group
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- 0.5 * sqrt(dat_smd[p1, "vi"]) * sqrt(dat_smd[p2, "vi"])
   VCV_smd[p1, p2] <- p1_p2_cov
   VCV_smd[p2, p1] <- p1_p2_cov
} #calculates the covariance between effect sizes and enters them in each combination of coordinates
diag(VCV_smd) <- dat_smd[, "vi"] #enters recalculated effect size sampling variances into diagonals 


#VCV matrix for dat_cvr
#the sampling variance for lnCVR was recreated to check how it is calculated in escalc, which appears to be based on Nakagawa 2015 but assumes a zero corr_lnxC_lnsC and corr_lnxT_lnsT
#dat_cvr$vi2 <-     ((dat_cvr$sdHC^2/(dat_cvr$nHC*dat_cvr$meanHC^2)) +
#                   (1/(2*(dat_cvr$nHC - 1)))
#                   +
#                   (dat_cvr$sdLC^2/(dat_cvr$nLC*dat_cvr$meanLC^2)) +
#                   (1/(2*(dat_cvr$nLC - 1))))
#based on this, the shared covariance between effect sizes as ((sC^2)/(nC*(xC^2)))+(1/(2*(nC-1)))), 
VCV_cvr <- matrix(0, nrow = dim(dat_cvr)[1], ncol = dim(dat_cvr)[1])
rownames(VCV_cvr) <- dat_cvr[, "EffectID"]
colnames(VCV_cvr) <- dat_cvr[, "EffectID"]

shared_coord <- which(dat_cvr[, "SharedGroup"] %in% dat_cvr[duplicated(dat_cvr[, "SharedGroup"]), "SharedGroup"] == TRUE)
combinations <- do.call("rbind", tapply(shared_coord, dat_cvr[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2))))
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- (((dat_cvr[p1, "Sharedsd"])^2 )/((dat_cvr[p1, "SharedN"])*((dat_cvr[p1, "Sharedmean"]^2)))) 
   + (1 / (2*((dat_cvr[p1, "SharedN"])-1)))
   VCV_cvr[p1, p2] <- p1_p2_cov
   VCV_cvr[p2, p1] <- p1_p2_cov
}
diag(VCV_cvr) <- dat_cvr[, "vi"] 


#VCV matrix for dat_vr
#calculates shared covariance between effect sizes as 1/(2(n_control - 1)), per Nakagawa 2015
VCV_vr <- matrix(0, nrow = dim(dat_vr)[1], ncol = dim(dat_vr)[1])
rownames(VCV_vr) <- dat_vr[, "EffectID"]
colnames(VCV_vr) <- dat_vr[, "EffectID"]

shared_coord <- which(dat_vr[, "SharedGroup"] %in% dat_vr[duplicated(dat_vr[, "SharedGroup"]), "SharedGroup"] == TRUE)
combinations <- do.call("rbind", tapply(shared_coord, dat_vr[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2))))
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- 1 / 
      (2 * (dat_vr[p1, "SharedN"] - 1))
   VCV_vr[p1, p2] <- p1_p2_cov
   VCV_vr[p2, p1] <- p1_p2_cov
}
diag(VCV_vr) <- dat_vr[, "vi"] 



###########################_################################


#INTERCEPT MODELS, MEAN EFFECTS ----
#lnRR
#unstructured base model 
rr.Base <- rma(yi, vi, method="DL", data=dat_rr)
#non-phylogenetic model
rr.SpeciesStudyExp <- rma.mv(yi, VCV_rr, 
                             random = list(~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                             method = "REML",
                             data = dat_rr)
#phylogenetic model
rr.Full <- rma.mv(yi, VCV_rr, 
                  random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                  R = list(SpeciesSci = tree_branchmat),
                  method = "REML", 
                  data = dat_rr)
summary(rr.Base) 
summary(rr.SpeciesStudyExp) 
summary(rr.Full) 
save(rr.Base, file = "./models/rr.Base.Rdata")
save(rr.SpeciesStudyExp, file = "./models/rr.SpeciesStudyExp.Rdata")
save(rr.Full, file = "./models/rr.Full.Rdata")


#SMDH
#unstructured base model
smd.Base <- rma(yi, vi, method="DL", data=dat_smd)
#non-phylogenetic model
smd.SpeciesStudyExp <- rma.mv(yi, VCV_smd, 
                              random = list(~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                              method = "REML", 
                              data = dat_smd) 
#phylogenetic model
smd.Full <- rma.mv(yi, VCV_smd, 
                   random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                   R = list(SpeciesSci = tree_branchmat),
                   method = "REML", 
                   data = dat_smd) 
summary(smd.Base) 
summary(smd.SpeciesStudyExp) 
summary(smd.Full) 
save(smd.Base, file = "./models/smd.Base.RData")
save(smd.SpeciesStudyExp, file = "./models/smd.SpeciesStudyExp.Rdata")
save(smd.Full, file = "./models/smd.Full.Rdata")



###########################_################################

###INTERCEPT MODELS, VARIANCE EFFECTS ----
#lnCVR
#unstructured base model 
cvr.Base <- rma(yi, vi, method="DL", data=dat_cvr)
#non-phylogenetic model
cvr.SpeciesStudyExp <- rma.mv(yi, VCV_cvr, 
                              random = list(~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                              method = "REML", 
                              data = dat_cvr)
#phylogenetic model
cvr.Full <- rma.mv(yi, VCV_cvr, 
                   random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                   R = list(SpeciesSci = tree_branchmat),
                   method = "REML", 
                   data = dat_cvr)
summary(cvr.Base)
summary(cvr.SpeciesStudyExp)
summary(cvr.Full)
save(cvr.Base, file = "./models/cvr.Base.Rdata")
save(cvr.SpeciesStudyExp, file = "./models/cvr.SpeciesStudyExp.Rdata")
save(cvr.Full, file = "./models/cvr.Full.Rdata")


#lnVR
#unstructured base model 
vr.Base <- rma(yi, vi, method="DL", data=dat_vr)
#non-phylogenetic model
vr.SpeciesStudyExp <- rma.mv(yi, VCV_vr, 
                             random = list(~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                             method = "REML", 
                             data = dat_vr) 
#phylogenetic model
vr.Full <- rma.mv(yi, VCV_vr, 
                  random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                  R = list(SpeciesSci = tree_branchmat),
                  method = "REML", 
                  data = dat_vr) 
summary(vr.Base)
summary(vr.SpeciesStudyExp)
summary(vr.Full)
save(vr.Base, file = "./models/vr.Base.Rdata")
save(vr.SpeciesStudyExp, file = "./models/vr.SpeciesStudyExp.Rdata")
save(vr.Full, file = "./models/vr.Full.Rdata")



###########################_################################

###MODERATOR MODELS: HYPOTHESIS TESTNG ----
#for non-factor variables, convert to factors
##2. Risk measurement category (lnRR, lnCVR)
#Moderator(s): RiskContext
rr.Full.h2 <- rma.mv(yi, VCV_rr, mods = ~-1 + RiskContext,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


cvr.Full.h2 <- rma.mv(yi, VCV_cvr, mods = ~-1 + RiskContext,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_cvr)


summary(rr.Full.h2)
summary(cvr.Full.h2)
save(rr.Full.h2, file = "./models/rr.Full.h2.Rdata")
save(cvr.Full.h2, file = "./models/cvr.Full.h2.Rdata")


##3. Sex (lnRR, lnCVR)
#Moderator(s): Sex
summary(dat_rr$Sex)
rr.Full.h3 <- rma.mv(yi, VCV_rr, mods = ~-1 + Sex,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


cvr.Full.h3 <- rma.mv(yi, VCV_cvr, mods = ~-1 + Sex,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_cvr)


summary(rr.Full.h3)
summary(cvr.Full.h3)
save(rr.Full.h3, file = "./models/rr.Full.h3.Rdata")
save(cvr.Full.h3, file = "./models/cvr.Full.h3.Rdata")


##4. Life-stage (lnRR, lnCVR)
#Moderator(s): ManipLifeStage + RelativeTimeFromTreatment.C
rr.Full.h4 <- rma.mv(yi, VCV_rr, mods = ~-1 + ManipLifeStage + RelativeTimeFromTreatment.C,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


cvr.Full.h4 <- rma.mv(yi, VCV_cvr, mods = ~-1 + ManipLifeStage + RelativeTimeFromTreatment.C,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_cvr)


summary(rr.Full.h4)
summary(cvr.Full.h4)
save(rr.Full.h4, file = "./models/rr.Full.h4.Rdata")
save(cvr.Full.h4, file = "./models/cvr.Full.h4.Rdata")


##5. Life-history
#Moderator(s): MaxLongevity, lnMaxLongevity
rr.Full.h5.i <- rma.mv(yi, VCV_rr, mods = ~ MaxLongevity.C,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


rr.Full.h5.ii <- rma.mv(yi, VCV_rr, mods = ~ lnMaxLongevity.C,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_rr)


cvr.Full.h5.i <- rma.mv(yi, VCV_cvr, mods = ~ MaxLongevity.C,
                         random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                         R = list(SpeciesSci = tree_branchmat),
                         method = "REML", 
                         data = dat_cvr,                               
                         control=list(optimizer="optim", optmethod="Nelder-Mead")) #alternate optimizer to aid convergence

cvr.Full.h5.ii <- rma.mv(yi, VCV_cvr, mods = ~ lnMaxLongevity.C,
                         random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                         R = list(SpeciesSci = tree_branchmat),
                         method = "REML", 
                         data = dat_cvr)


summary(rr.Full.h5.i)
summary(rr.Full.h5.ii)
summary(cvr.Full.h5.i)
summary(cvr.Full.h5.ii)
save(rr.Full.h5.i, file = "./models/rr.Full.h5.i.RData")
save(rr.Full.h5.ii, file = "./models/rr.Full.h5.ii.RData")
save(cvr.Full.h5.i, file = "./models/cvr.Full.h5.i.Rdata")
save(cvr.Full.h5.ii, file = "./models/cvr.Full.h5.ii.Rdata")



###########################_################################

###MODERATOR MODELS: PUBLICATION BIAS ---- 
#Note: using lnRR only, as we expect to only find publicaiton bias in relation to mean effects, as very few studies included were focusing on variance effect
#Moderator(s): Precision, Year.z, EffectSizesFromPublication
dat_rr$Precision <- sqrt(1/dat_rr$vi) #calculating Precision from sampling variance (Nakagawa 2012)
dat_rr$Year.C <- scale(dat_rr$Year) #using centered z-transformed Year to test for time lag bias (Schielzeth 2010)

#creating data and VCV matrix excluding effect sizes from derived from author correspondence for use in rr.Full.pub1, rr.Full.pub2
dat_rr_pub <- subset(dat_rr, DataPresentation != "author correspondence")
VCV_rr_pub <- matrix(0, nrow = dim(dat_rr_pub)[1], ncol = dim(dat_rr_pub)[1])
rownames(VCV_rr_pub) <- dat_rr_pub[, "EffectID"]
colnames(VCV_rr_pub) <- dat_rr_pub[, "EffectID"]

shared_coord <- which(dat_rr_pub[, "SharedGroup"] %in% dat_rr_pub[duplicated(dat_rr_pub[, "SharedGroup"]), "SharedGroup"] == TRUE)
combinations <- do.call("rbind", tapply(shared_coord, dat_rr_pub[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2)))) 
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- (dat_rr_pub[p1, "Sharedsd"])^2 / 
      ((dat_rr_pub[p1, "SharedN"]) * ((dat_rr_pub[p1, "Sharedmean"])^2))
   VCV_rr_pub[p1, p2] <- p1_p2_cov
   VCV_rr_pub[p2, p1] <- p1_p2_cov
} 
diag(VCV_rr_pub) <- dat_rr_pub[, "vi"] 

#Funnel plots for initial visual assessment
funnel(dat_rr_pub$yi, dat_rr_pub$vi, yaxis="seinv", xlim = c(-6,6), ylim = c(0.00000001, 250), at = c(-6,-4,-2,0,2,4,6), 
       xlab = "lnRR", ylab = "Precision", back="white", shade="lightgrey", hlines="white")
dat_rr_funnel <- subset(dat_rr_pub, Precision <= 50)
funnel(dat_rr_funnel$yi, dat_rr_funnel$vi, yaxis="seinv", xlim = c(-6,6), ylim = c(0.00000001, 50), at = c(-6,-4,-2,0,2,4,6),
       xlab = "lnRR", ylab = "Precision", back="white", shade="lightgrey", hlines="white")
       
#failsafe N calculation
fsn(dat_rr_pub$yi, dat_rr_pub$vi)

rr.Full.pub1 <- rma.mv(yi, VCV_rr_pub, mods = ~ Precision,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_rr_pub)
rr.Full.pub2 <- rma.mv(yi, VCV_rr_pub, mods = ~ Year.C,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_rr_pub)
rr.Full.pub3 <- rma.mv(yi, VCV_rr, mods = ~-1 + EffectSizesFromPublication,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_rr)

summary(rr.Full.pub1)
summary(rr.Full.pub2)
summary(rr.Full.pub3)
save(rr.Full.pub1, file = "./models/rr.Full.pub1.RData")
save(rr.Full.pub2, file = "./models/rr.Full.pub2.RData")
save(rr.Full.pub3, file = "./models/rr.Full.pub3.Rdata")



###########################_################################

##MODERATOR MODELS: EXPLORATORY ----
#Moderator(s): ManipType, ManipDirection, RelativeManipDuration.C, WildLabRear
rr.Full.exp.a <- rma.mv(yi, VCV_rr, mods = ~-1 + ManipType,
                     random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                     R = list(SpeciesSci = tree_branchmat),
                     method = "REML", 
                     data = dat_rr)


cvr.Full.exp.a <- rma.mv(yi, VCV_cvr, mods = ~-1 + ManipType,
                      random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                      R = list(SpeciesSci = tree_branchmat),
                      method = "REML", 
                      data = dat_cvr)


summary(rr.Full.exp.a)
summary(cvr.Full.exp.a)
save(rr.Full.exp.a, file = "./models/rr.Full.exp.a.Rdata")
save(cvr.Full.exp.a, file = "./models/cvr.Full.exp.a.Rdata")


rr.Full.exp.b <- rma.mv(yi, VCV_rr, mods = ~-1 + ManipDirection,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


cvr.Full.exp.b <- rma.mv(yi, VCV_cvr, mods = ~-1 + ManipDirection,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_cvr)


summary(rr.Full.exp.b)
summary(cvr.Full.exp.b)
save(rr.Full.exp.b, file = "./models/rr.Full.exp.b.Rdata")
save(cvr.Full.exp.b, file = "./models/cvr.Full.exp.b.Rdata")


rr.Full.exp.c <- rma.mv(yi, VCV_rr, mods = ~ RelativeManipDuration.C,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


cvr.Full.exp.c <- rma.mv(yi, VCV_cvr, mods = ~ RelativeManipDuration.C,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_cvr)


summary(rr.Full.exp.c)
summary(cvr.Full.exp.c)
save(rr.Full.exp.c, file = "./models/rr.Full.exp.c.Rdata")
save(cvr.Full.exp.c, file = "./models/cvr.Full.exp.c.Rdata")



rr.Full.exp.d <- rma.mv(yi, VCV_rr, mods = ~-1 + WildLabRear,
                       random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                       R = list(SpeciesSci = tree_branchmat),
                       method = "REML", 
                       data = dat_rr)


cvr.Full.exp.d <- rma.mv(yi, VCV_cvr, mods = ~-1 + WildLabRear,
                        random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                        R = list(SpeciesSci = tree_branchmat),
                        method = "REML", 
                        data = dat_cvr)


summary(rr.Full.exp.d)
summary(cvr.Full.exp.d)
save(rr.Full.exp.d, file = "./models/rr.Full.exp.d.Rdata")
save(cvr.Full.exp.d, file = "./models/cvr.Full.exp.d.Rdata")



###########################_################################

##SENSITIVITY ANALYSIS: DATA SOURCE ----
#Excluding non-systematic data sources (lnRR)
dat_rr_sen1 <- subset(dat_rr, Source != "CD_Additional studies") 

#VCV matrix for dat_rr_sen1
VCV_rr_sen1 <- matrix(0, nrow = dim(dat_rr_sen1)[1], ncol = dim(dat_rr_sen1)[1])
rownames(VCV_rr_sen1) <- dat_rr_sen1[, "EffectID"]
colnames(VCV_rr_sen1) <- dat_rr_sen1[, "EffectID"]

shared_coord <- which(dat_rr_sen1[, "SharedGroup"] %in% dat_rr_sen1[duplicated(dat_rr_sen1[, "SharedGroup"]), "SharedGroup"] == TRUE)
combinations <- do.call("rbind", tapply(shared_coord, dat_rr_sen1[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2)))) 
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- (dat_rr_sen1[p1, "Sharedsd"])^2 / 
      ((dat_rr_sen1[p1, "SharedN"]) * ((dat_rr_sen1[p1, "Sharedmean"])^2))
   VCV_rr_sen1[p1, p2] <- p1_p2_cov
   VCV_rr_sen1[p2, p1] <- p1_p2_cov
} 
diag(VCV_rr_sen1) <- dat_rr_sen1[, "vi"] 

#full phylogenetic model
rr.sen1 <- rma.mv(yi, VCV_rr_sen1, 
                  random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                  R = list(SpeciesSci = tree_branchmat),
                  method = "REML", 
                  data = dat_rr_sen1)

summary(rr.sen1)
save(rr.sen1, file = "./models/rr.sen1.RData")


#Excluding non-systematic data sources (lnCVR)
dat_cvr_sen1 <- subset(dat_cvr, Source != "CD_Additional studies") 

#VCV matrix for dat_cvr_sen1
VCV_cvr_sen1 <- matrix(0, nrow = dim(dat_cvr_sen1)[1], ncol = dim(dat_cvr_sen1)[1])
rownames(VCV_cvr_sen1) <- dat_cvr_sen1[, "EffectID"]
colnames(VCV_cvr_sen1) <- dat_cvr_sen1[, "EffectID"]

shared_coord <- which(dat_cvr_sen1[, "SharedGroup"] %in% dat_cvr_sen1[duplicated(dat_cvr_sen1[, "SharedGroup"]), "SharedGroup"] == TRUE)
combinations <- do.call("rbind", tapply(shared_coord, dat_cvr_sen1[shared_coord, "SharedGroup"], function(x) t(utils::combn(x, 2)))) 
for (i in 1:dim(combinations)[1]) {
   p1 <- combinations[i, 1]
   p2 <- combinations[i, 2]
   p1_p2_cov <- (dat_cvr_sen1[p1, "Sharedsd"])^2 / 
      ((dat_cvr_sen1[p1, "SharedN"]) * ((dat_cvr_sen1[p1, "Sharedmean"])^2))
   VCV_cvr_sen1[p1, p2] <- p1_p2_cov
   VCV_cvr_sen1[p2, p1] <- p1_p2_cov
} 
diag(VCV_cvr_sen1) <- dat_cvr_sen1[, "vi"] 

#full phylogenetic model
cvr.sen1 <- rma.mv(yi, VCV_cvr_sen1, 
                  random = list(~1 | SpeciesSci, ~1 | SpeciesCom, ~1 | StudyID, ~1 | ExperimentID, ~1 | EffectID),
                  R = list(SpeciesSci = tree_branchmat),
                  method = "REML", 
                  data = dat_cvr_sen1)

summary(cvr.sen1)
save(cvr.sen1, file = "./models/cvr.sen1.RData")

