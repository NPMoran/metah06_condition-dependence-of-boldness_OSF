##########_____Meta-analysis project (NC3 DO5)_____#########
#TITLE: The Condition Dependence of Risk-Taking Behaviours: Systematic review and meta-analysis


#install.packages("devtools")
#library(devtools)
#install_github("mjwestgate/revtools")
#install.packages("revtools")
library(revtools)
library(plyr)


###########################_################################

###INITIAL SEARCHES 10_11_2019 ----
#Databases: Web of Science Core Collection (http://www.webofknowledge.com); Scopus (https://www.scopus.com/)


#Web of Science Search 10_11_18 ----
#TS=("*nutrition*" OR "*nourish*" OR "calori*" OR "hunger" OR "starv*" OR (("diet*" OR "food") NEAR/10 ("quality" OR "restrict*" OR "depriv*" OR "enrich*" OR "availab*" OR "manipul*" OR "limit*")) OR "bod* state*" OR "bod* condition*" OR "condition depend*" OR "state depend*" OR "asset protection" residual reproductive value") 
#AND TS=("bold*" OR "explorat*" OR "risk*" OR "novel*" OR "predat*" OR "refuge" OR "neophobi*" OR "neophil*") 
#AND TS=("personalit*" OR "temperament*" OR "coping style*" OR "behavio* type*" OR "behavio* strateg*" OR "behavio* syndrome*" OR "risk taking behavio*" OR "developmental plasticity" OR "phenotypic plasticity" OR ("behavio*" NEAR/5 ("animal*" OR "plasticity" OR "flexibil*" OR "trait*" OR "phenotyp*" OR "response*" OR "decision*")))
#2729 Results
WoS1 <- read_bibliography("wos1.bib")
WoS2 <- read_bibliography("wos2.bib") 
WoS3 <- read_bibliography("wos3.bib") 
WoS4 <- read_bibliography("wos4.bib") 
WoS5 <- read_bibliography("wos5.bib") 
WoS6 <- read_bibliography("wos6.bib") 
WoS1.df <- as.data.frame(WoS1)
WoS2.df <- as.data.frame(WoS2)
WoS3.df <- as.data.frame(WoS3)
WoS4.df <- as.data.frame(WoS4)
WoS5.df <- as.data.frame(WoS5)
WoS6.df <- as.data.frame(WoS6)
reducing.fields <- c("label","title","author","journal","issn", "volume","number","pages","year","doi","abstract")
WoS1.df.reduced <- WoS1.df[,reducing.fields]
WoS2.df.reduced <- WoS2.df[,reducing.fields]
WoS3.df.reduced <- WoS3.df[,reducing.fields]
WoS4.df.reduced <- WoS4.df[,reducing.fields]
WoS5.df.reduced <- WoS5.df[,reducing.fields]
WoS6.df.reduced <- WoS6.df[,reducing.fields]
WoS <- rbind(WoS1.df.reduced,
             WoS2.df.reduced,
             WoS3.df.reduced,
             WoS4.df.reduced,
             WoS5.df.reduced,
             WoS6.df.reduced)
summary(WoS)

#Scopus Search 10_11_18 ----
#TITLE-ABS-KEY ( "*nutrition*"  OR  "*nourish*"  OR  "calori*"  OR  "hunger"  OR  "starv*"  OR  ( ( "diet*"  OR  "food" )  W/10  ( "quality"  OR  "restrict*"  OR  "depriv*"  OR  "enrich*"  OR  "availab*"  OR  "manipul*"  OR  "limit*" ) )  OR  "bod* state*"  OR  "bod* condition*"  OR  "condition depend*"  OR  "state depend*"  OR  "asset protection"  OR  "residual reproductive value" )  
#AND  TITLE-ABS-KEY ( "bold*"  OR  "explorat*"  OR  "risk*"  OR  "novel*"  OR  "predat*"  OR  "refuge"  OR  "neophobi*"  OR  "neophil*" )  
#AND  TITLE-ABS-KEY ( "personalit*"  OR  "temperament*"  OR  "coping style*"  OR  "behavio* type*"  OR  "behavio* strateg*"  OR  "behavio* syndrome*"  OR  "risk taking behavio*"  OR  "developmental plasticity"  OR  "phenotypic plasticity"  OR  ( "behavio*"  W/5  ( "animal*"  OR  "plasticity"  OR  "flexibil*"  OR  "trait*"  OR  "phenotyp*"  OR  "response*"  OR  "decision*" ) ) )
#4479 Results
Scopus1 <- read_bibliography("scopus1.bib")
Scopus2 <- read_bibliography("scopus2.bib")
Scopus3 <- read_bibliography("scopus3.bib")
Scopus4 <- read_bibliography("scopus4.bib")
Scopus5 <- read_bibliography("scopus5.bib")
Scopus1.df <- as.data.frame(Scopus1)
Scopus2.df <- as.data.frame(Scopus2)
Scopus3.df <- as.data.frame(Scopus3)
Scopus4.df <- as.data.frame(Scopus4)
Scopus5.df <- as.data.frame(Scopus5)
Scopus1.df.reduced <- Scopus1.df[,reducing.fields]
Scopus2.df.reduced <- Scopus2.df[,reducing.fields]
Scopus3.df.reduced <- Scopus3.df[,reducing.fields]
Scopus4.df.reduced <- Scopus4.df[,reducing.fields]
Scopus5.df.reduced <- Scopus5.df[,reducing.fields]
Scopus <- rbind(Scopus1.df.reduced,
                Scopus2.df.reduced,
                Scopus3.df.reduced,
                Scopus4.df.reduced,
                Scopus5.df.reduced)
summary(Scopus)


#Deduplication + Export ----
condition.search <- rbind(WoS,
                Scopus)
summary(condition.search)
#gives 7208 results total
library(plyr)
condition.search.duplicated <- find_duplicates(condition.search)
condition.search.deduplicated <- extract_unique_references(condition.search.duplicated)
nrow(condition.search.deduplicated)
summary(condition.search.deduplicated)
condition.search.deduplicated[c(1:5),c(1,3,5:8)]
#gives 5509 rows

inlinetestval <- 9
inlinetestvect <- c(8,1,2)

#exporting to csv
rayyan.example <- read.table("rayyan_csv_example.csv",header=TRUE,sep=",")
names(rayyan.example)
setdiff(names(rayyan.example),names(condition.search.deduplicated))
setdiff(names(condition.search.deduplicated),names(rayyan.example))
condition.search.deduplicated$issue <- ""
condition.search.deduplicated$publisher <- ""
condition.search.deduplicated$duplicate_group <- NULL
condition.search.deduplicated$n_duplicates <- NULL
setdiff(names(rayyan.example),names(condition.search.deduplicated))
setdiff(names(condition.search.deduplicated),names(rayyan.example))
condition.search.deduplicated.rayyan <- rename(condition.search.deduplicated, c("label"="key", "author"="authors", "doi"="url"))
key <- c(1:5509)
condition.search.deduplicated.rayyan$key <- key
names(condition.search.deduplicated.rayyan)
condition.search.deduplicated.rayyan <- condition.search.deduplicated.rayyan[,names(rayyan.example)]
write.csv(condition.search.deduplicated.rayyan,"screening_ref_data_rayyan.csv",row.names=FALSE)

#OLD DISCARDED CODE 
#if (dim(table(grepl(",  ",condition.search.deduplicated.rayyan$authors,fixed=T)))==2){
#  for(i in 1:nrow(condition.search.deduplicated.rayyan)){
#    if(grepl(",  ",condition.search.deduplicated.rayyan$authors[i],fixed=T)){
#      print(i) #any row number printed will need to be edited individually
#    }
#  }
#} else {
#  
#  print("you are ready to import the file screening_ref_data_rayyan.csv into rayyan")
#  write.csv(condition.search.deduplicated.rayyan,"screening_ref_data_rayyan.csv",row.names=FALSE)
#  
#}
#
#
#Exporting to bib for screening
#install.packages("devtools")
#library(devtools)
#install_github("mjwestgate/revtools")
#install.packages("revtools")
#write_bibliography(condition.search.deduplicated, "screening_ref_data_rayyan.bib", format="bib")
#
#screening_ref_data_rayyan.csv now ready for use in Rayyan


###########################_################################

#FINAL SEARCHES 4_02_2019 ----


#Web of Science Search 4_02_2019 ----
#You searched for: TS=("*nutrition*" OR "*nourish*" OR "calori*" OR "hunger" OR "starv*" OR (("diet*" OR "food") NEAR/10 ("quality" OR "restrict*" OR "depriv*" OR "enrich*" OR "availab*" OR "manipul*" OR "limit*")) OR "bod* state*" OR "bod* condition*" OR "condition depend*" OR "state depend*" OR "asset protection" OR "residual reproductive value") AND TS=("bold*" OR "explorat*" OR "risk*" OR "novel*" OR "predat*" OR "refuge" OR "neophobi*" OR "neophil*") AND TS=("personalit*" OR "temperament*" OR "coping style*" OR "behavio* type*" OR "behavio* strateg*" OR "behavio* syndrome*" OR "risk-taking behavio*" OR "developmental plasticity" OR "phenotypic plasticity" OR ("behavio*" NEAR/5 ("animal*" OR "plasticity" OR "flexibil*" OR "trait*" OR "phenotyp*" OR "response*" OR "decision*")))
#Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, CPCI-S, CPCI-SSH, BKCI-S, BKCI-SSH, ESCI, CCR-EXPANDED, IC.
#Using the WoS bib file produced above for comparison
#2813 Results
WoSFinal1 <- read_bibliography("wosfinal1.bib")
WoSFinal2 <- read_bibliography("wosfinal2.bib") 
WoSFinal3 <- read_bibliography("wosfinal3.bib") 
WoSFinal4 <- read_bibliography("wosfinal4.bib") 
WoSFinal5 <- read_bibliography("wosfinal5.bib") 
WoSFinal6 <- read_bibliography("wosfinal6.bib") 
WoSFinal1.df <- as.data.frame(WoSFinal1)
WoSFinal2.df <- as.data.frame(WoSFinal2)
WoSFinal3.df <- as.data.frame(WoSFinal3)
WoSFinal4.df <- as.data.frame(WoSFinal4)
WoSFinal5.df <- as.data.frame(WoSFinal5)
WoSFinal6.df <- as.data.frame(WoSFinal6)
WoSFinal1.df.reduced <- WoSFinal1.df[,reducing.fields]
WoSFinal2.df.reduced <- WoSFinal2.df[,reducing.fields]
WoSFinal3.df.reduced <- WoSFinal3.df[,reducing.fields]
WoSFinal4.df.reduced <- WoSFinal4.df[,reducing.fields]
WoSFinal5.df.reduced <- WoSFinal5.df[,reducing.fields]
WoSFinal6.df.reduced <- WoSFinal6.df[,reducing.fields]
WoSFinal <- rbind(WoSFinal1.df.reduced,
             WoSFinal2.df.reduced,
             WoSFinal3.df.reduced,
             WoSFinal4.df.reduced,
             WoSFinal5.df.reduced,
             WoSFinal6.df.reduced)
summary(WoSFinal)

WoSFinal.dedup <- rbind(WoS, WoSFinal)
search.duplicated.WoS <- find_duplicates(data = WoSFinal.dedup,
                                            match_variable = "title",
                                            group_variable = NULL,
                                            match_function = "exact")
WoS.unique <- extract_unique_references(WoSFinal.dedup,  search.duplicated.WoS)
WoS.additional.refs <- WoS.unique[WoS.unique$n_duplicates==1,] #extracing only additional records
WoS.additional.refs$n_duplicates <- NULL
summary(WoS.additional.refs)
#92 additional refs found

#Scopus Search 4_02_2019 ----
#Search using scopus updated search option (gives results loaded from dat0 10_11_2018)
#TITLE-ABS-KEY ( "*nutrition*"  OR  "*nourish*"  OR  "calori*"  OR  "hunger"  OR  "starv*"  OR  ( ( "diet*"  OR  "food" )  W/10  ( "quality"  OR  "restrict*"  OR  "depriv*"  OR  "enrich*"  OR  "availab*"  OR  "manipul*"  OR  "limit*" ) )  OR  "bod* state*"  OR  "bod* condition*"  OR  "condition depend*"  OR  "state depend*"  OR  "asset protection"  OR  "residual reproductive value" )  AND  TITLE-ABS-KEY ( "bold*"  OR  "explorat*"  OR  "risk*"  OR  "novel*"  OR  "predat*"  OR  "refuge"  OR  "neophobi*"  OR  "neophil*" )  AND  TITLE-ABS-KEY ( "personalit*"  OR  "temperament*"  OR  "coping style*"  OR  "behavio* type*"  OR  "behavio* strateg*"  OR  "behavio* syndrome*"  OR  "risk taking behavio*"  OR  "developmental plasticity"  OR  "phenotypic plasticity"  OR  ( "behavio*"  W/5  ( "animal*"  OR  "plasticity"  OR  "flexibil*"  OR  "trait*"  OR  "phenotyp*"  OR  "response*"  OR  "decision*" ) ) ) AND  ORIG-LOAD-DATE  >  20181110
#42 Results
ScopusFinal <- read_bibliography("scopusfinal.bib")
ScopusFinal.df <- as.data.frame(ScopusFinal)
Scopus.additional.refs <- ScopusFinal.df[,reducing.fields]
summary(Scopus.additional.refs)

#Deduplication + Export ----
condition.final <- rbind(Scopus.additional.refs,
                         WoS.additional.refs)
condition.final.duplicated <- find_duplicates(data = condition.final,
                                         match_variable = "title",
                                         group_variable = NULL,
                                         match_function = "fuzzdist")
condition.final.duplicated.unique <- extract_unique_references(condition.final, condition.final.duplicated)
nrow(condition.final.duplicated.unique)
#120 unique references, (note: fuzzdist and exact match produce 120 results)

rayyan.example <- read.table("rayyan_csv_example.csv",header=TRUE,sep=",")
names(rayyan.example)
setdiff(names(rayyan.example),names(condition.final.duplicated.unique))
setdiff(names(condition.final.duplicated.unique),names(rayyan.example))
condition.final.duplicated.unique$issue <- ""
condition.final.duplicated.unique$publisher <- ""
condition.final.duplicated.unique$duplicate_group <- NULL
condition.final.duplicated.unique$n_duplicates <- NULL
setdiff(names(rayyan.example),names(condition.final.duplicated.unique))
setdiff(names(condition.final.duplicated.unique),names(rayyan.example))
condition.final.duplicated.rayyan <- rename(condition.final.duplicated.unique, c("label"="key", "author"="authors", "doi"="url"))
key <- c(5510:5629)
condition.final.duplicated.rayyan$key <- key
names(condition.final.duplicated.rayyan)
condition.final.duplicated.rayyan <- condition.final.duplicated.rayyan[,names(rayyan.example)]
write.csv(condition.final.duplicated.rayyan,"screening_ref_data_rayyan_final.csv",row.names=FALSE)

###########################_################################