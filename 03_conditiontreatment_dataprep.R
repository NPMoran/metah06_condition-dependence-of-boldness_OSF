##########_____Meta-analysis project (NC3 DO5)_____#########
##TITLE: The Condition Dependence of Risk-Taking Behaviours: Systematic review and meta-analysis


library(metafor); library(dplyr); library(stringr); library(ape)
library(matrixcalc); library(rotl); library(treebase)


###########################_################################

###DATA IMPORT & PRELIMINARY DATA PROCESSING ----
setwd("~/metah06_condition-dependence-of-boldness")
extractdat <- read.csv("CD_extractdat.csv", strip.white = TRUE) 
studydat <- read.csv("CD_studydat.csv", strip.white = TRUE) 
speciesdat <- read.csv("CD_speciesdat.csv", strip.white = TRUE) 


#Deleting duplicated or irrelevant columns and merging dataframes
extractdat <- select(extractdat, -c(ManipLifeStage., ManipNotes, ExpAge, ExpRepeated, ExpNotes, Extraction.notes))
studydat <- select(studydat, -c(Title, Year, Journal, AuthorsAll, CorrAuthName, CorrAuthEmail, DOI, Species, Notes, UnderReportedData, AuthorContacted,
                               DataReceived, FirstContact, FirstDate, SecondaryContact, SecondaryDate, ResponseReceived,
                               RepliedtoResponse, FollowupContact, FollowupDate, ResponseReceivedFollowup, RepliedtoResponseFollowup))
speciesdat <- select(speciesdat, -c(SpeciesCom, Class, MaxLongevityRef, MaxLongevityNotes, ManipLifeStage., ManipLifeStageGuideline, DeterminedManipLifeStage, ManipLifeStageGuidelineRef))
dat <- merge(studydat, extractdat, by = "StudyID", all.x = TRUE)


#Removing 2 preliminarily excluded studies for which data was also published in another included study
dat <- subset(subset(dat, StudyID != "CD110"), StudyID != "CD179")
n_distinct(dat$StudyID) 
#qualitatively included studies = 146


#Combining species life history data with the main dataset using a new matching variable "LongevityMatch"
speciesdat <- transform(speciesdat, LongevityMatch=paste(StudyID, SpeciesSci, WildLabRear, Sex))
speciesdat <- select(speciesdat, -c(StudyID, SpeciesSci, WildLabRear, Sex))
dat <- transform(dat, LongevityMatch=paste(StudyID, SpeciesSci, WildLabRear, Sex))
dat <- merge(dat, speciesdat, by = "LongevityMatch", all.x = FALSE)
dat <- select(dat, -c(LongevityMatch)) #clearing matching variable

summary(subset(dat, Exclude == "exclude_all")$ExclusionReason)
#Removing preliminarily excluded effect sizes
summary(subset(dat, Exclude == "exclude_all")$ExclusionReason) 
##Summary of exclusion reasons:
#        data cannot be used to calculate a sampling variance: 3 rows
#        excluded behavioural response variable: 91 rows
#        finer scale data extracted: 96 rows
#        higher quality data extracted: 2 rows
#        insufficient data available: 263 rows
#        test statistic only: 10 rows
#Exclusion Note: estimated effect sizes from test statistics will not been included due to the small number of effect sizes impacted (F-stat: ES1137, ES1138, ES1139, ES0384, ES1119, ES1116, ES1115; t-stat: ES0397, ES0166, ES0778)
dat <- subset(dat, Exclude != "exclude_all") 
n_distinct(dat$StudyID)
#quantitatively included studies = 128



###########################_################################

###ADJUSTING MODERATORS ----
##Creating composite/transformed continuous moderators
#RelativeTimeFromTreatment, to include in Life-stage model as the time between the diet treatment and behavioural experimentation is potentially much longer, the additional factor 'ManipOffsetRelative' is included, as a measure of the time between the experimental treatment and beavhioural experiment relative to the maximum longevity of the species. 
dat$RelativeTimeFromTreatment <- (dat$ManipOffset)/(dat$MaxLongevity)
hist(dat$RelativeTimeFromTreatment)

#lnMaxLongevity, MaxLongevity includes a large number of short lived species and few long lived species, therefore the the natural log of lifespan is used in the Life History model to account for this structure and capture fine scale variation in the lifespan for short lived species.
hist(speciesdat$MaxLongevity)
dat$lnMaxLongevity <- log(dat$MaxLongevity)
hist(dat$lnMaxLongevity)

#RelativeManipDuration, for exploratory analysis to test if there is an effect of treatment duration as a proportion of the maximum lifespan.
dat$RelativeManipDuration <- (dat$ManipDuration)/(dat$MaxLongevity)

##Minor classification changes
#WildLabRear, subjects reared from wild caught eggs are catergorised as laboratory reared
dat$WildLabRear[dat$WildLabRear == "lab (wild caught eggs)"] <- "lab"
summary(dat$WildLabRear)

##RiskContext, combining lightdarktest with refuge use due the functionally similar designs and comparatively small sample sizes
#dat$RiskContext[dat$RiskContext == "novelenvironment_lightdarktest"] <- "novelenvironment_refugeuse"
#summary(dat$RiskContext)

#EffectSizesFromPublication, for analysis proportional only will be treated as yes for publication bias analysis
dat$EffectSizesFromPublication[dat$EffectSizesFromPublication == "proportional only"] <- "yes"
summary(dat$EffectSizesFromPublication)



###########################_################################

###BUILDING A PHYLOGENY ----
#extracting unique species names, converting to character for tnrs_match_names function
species_unique <- as.character(unique(dat$SpeciesSci))

#matching names with the Open Tree of Life database
species_unique_match <- tnrs_match_names(names = species_unique) 

dat$SpeciesSci <- as.character(dat$SpeciesSci)
dat$SpeciesSci[dat$SpeciesSci == "Cebus albifrons; Cebus apella"] <- "Cebus" #using genus only as comparable individuals from both species used
dat$SpeciesSci[dat$SpeciesSci == "Palaemon elegans; Palaemon serratus (only 5 of 320 were serratus)"] <- "Palaemon elegans" #changed to P. elegans as vast majority were one species
dat$SpeciesSci[dat$SpeciesSci == "Coelognathus helena (synonym Elaphe helena)"] <- "Coelognathus helena" #removing synonym
dat$SpeciesSci[dat$SpeciesSci == "Aedes triseriatus (synonym Ochlerotatus triseriatus)"] <- "Ochlerotatus triseriatus" #removing synonym
dat$SpeciesSci[dat$SpeciesSci == "Rhabdomys dilectus chakae"] <- "Rhabdomys dilectus" #subspecies, so changes to species name
dat$SpeciesSci[dat$SpeciesSci == "Rhithropanopeus harrisii tridentatus"] <- "Rhithropanopeus harrisii" #subspecies, so changes to species name
dat$SpeciesSci[dat$SpeciesSci == "Dineutes discolor"] <- "Dineutus discolor"  #changing to more common spelling
dat$SpeciesSci[dat$SpeciesSci == "Buccinanops globulosum"] <- "Buccinanops globulosus" #changing to more common spelling
dat$SpeciesSci[dat$SpeciesSci == "Oncorhyndtus kisutch"] <- "Oncorhynchus kisutch" #typo
dat$SpeciesSci <- as.factor(dat$SpeciesSci)

#re-extracting unique species names, converting to character for tnrs_match_names function
species_unique <- as.character(unique(dat$SpeciesSci))
#checking the lineage and genus for not found species
wardi <- tol_node_info(ott_id=3635611, include_lineage=TRUE) 
tnrs_match_names(names = ("Pomacentrus"))
#Pomacentrus wardi not placed in Open Tree, so Pomacentrus australis used as closely related species, then substituted back in
species_unique[species_unique == "Pomacentrus wardi"] <- "Pomacentrus australis" 

#matching names with the Open Tree of Life database
species_unique_match <- tnrs_match_names(names = species_unique) 
species_unique_match
#species names converted to synonyms in database search (so revert back to database name later)
     #hypoaspis aculeifer -> Gaeolaelaps aculeifer  
     #lithobates sylvaticus -> Rana sylvatica
     #buccinanops globulosus -> Buccinanops deformis
     #nephelopsis obscura -> Erpobdella obscura
     #pelophylax kl. esculentus -> Pelophylax esculentus
     #stizostedion vitreum -> Sander vitreus 
     #physa gyrina -> Physella gyrina
     #elysia clarki -> Elysia crispata
     #theragra chalcogramma -> Gadus chalcogrammus  
#species with names that require correcting in the tree
     #Gadus morhua (species in domain Eukaryota) -> Gadus morhua
     #Oncorhynchus mykiss (species in domain Eukaryota) -> Oncorhynchus mykiss
     #Pomacentrus australis -> Pomacentrus wardi
#species that require correcting in both
     #Cebus -> Cebus spp.

#extracting phylogenetic relationships between the species
tree <- tol_induced_subtree(ott_ids = species_unique_match[["ott_id"]], label_format = "name")
plot(tree)

#checking the labels for tips/species
tree$tip.label
tree$tip.label[tree$tip.label == "Gadus_morhua_(species_in_domain_Eukaryota)"] <- "Gadus_morhua"
tree$tip.label[tree$tip.label == "Oncorhynchus_mykiss_(species_in_domain_Eukaryota)"] <- "Oncorhynchus_mykiss"
tree$tip.label[tree$tip.label == "Pomacentrus_australis"] <- "Pomacentrus wardi"
tree$tip.label[tree$tip.label == "Cebus"] <- "Cebus spp."
tree$tip.label <- gsub("_", " ", tree$tip.label, perl = TRUE) 

#checking differences between species names, and editing tree to fit database
intersect(dat$SpeciesSci, tree$tip.label)
setdiff(dat$SpeciesSci, tree$tip.label)
setdiff(tree$tip.label, dat$SpeciesSci)
dat$SpeciesSci <- as.character(dat$SpeciesSci)
tree$tip.label[tree$tip.label == "Gaeolaelaps aculeifer"] <- "Hypoaspis aculeifer" #ToL synonym
tree$tip.label[tree$tip.label == "Rana sylvatica"] <- "Lithobates sylvaticus" #ToL synonym
tree$tip.label[tree$tip.label == "Buccinanops deformis"] <- "Buccinanops globulosus" #ToL synonym
tree$tip.label[tree$tip.label == "Erpobdella obscura"] <- "Nephelopsis obscura" #ToL synonym
tree$tip.label[tree$tip.label == "Pelophylax esculentus"] <- "Pelophylax kl. esculentus" #ToL synonym
tree$tip.label[tree$tip.label == "Sander vitreus"] <- "Stizostedion vitreum" #ToL synonym
tree$tip.label[tree$tip.label == "Physella gyrina"] <- "Physa gyrina" #ToL synonym
tree$tip.label[tree$tip.label == "Elysia crispata"] <- "Elysia clarki" #ToL synonym
tree$tip.label[tree$tip.label == "Gadus chalcogrammus"] <- "Theragra chalcogramma" #ToL synonym

dat$SpeciesSci[dat$SpeciesSci == "Cebus"] <- "Cebus spp." #formatting

#checking matches
dat$SpeciesSci <- as.factor(dat$SpeciesSci)
setdiff(dat$SpeciesSci, tree$tip.label) #database matches names with tree
setdiff(tree$tip.label, dat$SpeciesSci)
plot(tree)

#resolving politomies
is.binary.tree(tree) 
tree <- multi2di(tree,random=TRUE) #tree is non-binary, so politomies resolved at random

#building phylo matrix
tree$tip.label <- as.character(tree$tip.label) 
tree_branch <- compute.brlen(tree, method = "Grafen", power = 1) #computing branch lengths using "compute.brlen function in the R package ape (v. 5.1; Paradis and Schliep 2018))."
tree_branch
is.ultrametric(tree_branch) #checking the distance from root to each tip is equal
tree_branchmat <- vcv(tree_branch, cor = T) #vcv computes the expected variances and covariances of a continuous trait assuming it evolves under a given model
tree_branchmat

#Saving the tree
write.tree(tree, file="datphylo.tre")
plot.phylo(tree)
plot.phylo(tree, type = "cladogram")
plot.phylo(tree, type = "fan", cex = 0.5, node.depth = 1)
plot.phylo(tree, type = "fan", cex = 0.5, node.depth = 2, edge.width = 6) #producing images for presentation


#Saving the processed, pre-analysis dataset
write.csv(dat, 'CD_processed.csv')



###########################_################################
