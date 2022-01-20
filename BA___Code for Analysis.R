######################################################Contents
##row 19        start
##row 92        preparation natalism instrument
##row 280       preparation demographics
##row 397       preparation personality
##row 545       preparation LFAIS and attitude towards abortion
##row 650       Correlations, t.tests, z.tests and regressions
        ##row 703 total sample
        ##row 1001 correlations separated by men and women
        ##row 1245 ztests
        ##row 1512 regressions
##row 1598 results for the appendices
        ##row 1603 results three factor solution of the natalism scale
        ##row 2461 results two factor solution of the correlates
      


#########################################################################
###################### 1. start
#########################################################################

# set working directory 
setwd("C:/Users/chiara/Documents/Uni/BA/r_dateien")

#remove possible datasets or variables that were loaded before

rm(list=ls())

#1.1 load packages
library(dplyr)
library(psych)
library(car)
library(GPArotation)
library(psychometric)
library(apaTables)
library(RcmdrMisc)
library(QuantPsyc)

#1.2 load datasets
datainstr <- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s1_instructions.csv")
datademo <- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s2_demographics.csv")
datanatal <- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s3_natalism.csv")
datareason<- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s4_reasons_ranking.csv")
dataperso <- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s5_personality.csv")
dataLFAIS <- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s6_LFAIS__abortion_contraception_socialnet.csv")
datatruecorona <- read.csv2("C:/Users/chiara/Documents/Uni/BA/r_dateien/s7_corona_vph.csv")

#1.3 merge datasets by session 
#-> those that did not finish until the last survey with question if answered truthfully 
#are deleated because session is not there (participants & testsessions)

datademo1 = inner_join(datainstr, datademo, by = "session") 
data_natalismfull = inner_join(datademo1, datanatal, by = "session") 
data_natalismfull2 = inner_join(data_natalismfull, datareason, by = "session")
data_natalismfull3 = inner_join(data_natalismfull2, dataperso, by = "session") 
data_natalismfull4 = inner_join(data_natalismfull3, dataLFAIS, by = "session") 
data_natalismfull5 = inner_join(data_natalismfull4, datatruecorona, by = "session") 

#1.4 remove sessions with no session code 
data_natalismfull5 = data_natalismfull5 %>% filter(session != "") 

#1.5 filter for complete 
data_natalismfull5 = data_natalismfull5 %>% filter(ended.y.y.y != "") 

#1.6 filter for testsessions: row 293 and 294 in full dataset 
data_natalismfull6 <- data_natalismfull5[-c(293,294), ] 

#1.7 check inclusion criteria
#children?
data_natalismfull7  = data_natalismfull6  %>% filter(children ==1) # 1 men in row 135

datanatalism8 <- data_natalismfull6[-c(135), ] 

#answered truthfully?
datanatalism9  = datanatalism8  %>% filter(yes_no_true ==2) # nobody answered not truthfully in the rest of the data

#final dataset
fulldata<-datanatalism8


#1.8 subsetten so that every survey is managable and smaller (original surveys)

natalism <- fulldata[,c(61:90)]
demographics <- fulldata[,c(9:60)]
personality <- fulldata[,c(124:176)]
LFAIS <- fulldata[,c(177:227)]
corona <- fulldata[,c(228:237)]

detach("package:dplyr") 

#######################################################################################
############################# 2. Preparation Natalism instrument
#######################################################################################


#2.1 delete columns not necessary
#column created modified, epxired und ended (1,2,3,4)

natalism2 <- natalism [,-c(1,2,3,4)]


#2.2 rename columns

names(natalism2)

#new dataset
natalism3 <- natalism2

#rename columns
colnames(natalism3) <- c("item1_parent_role","item2_endure_person","item3_own_children","item4_life_plan","item5_personal_idea","item6_fulfilled_life","item7_complete_family",            
                          "item8_childbearing_meaning","item9_prospective_parent","item10_central_lifegoal","item_11important_decisions",        
                         "item12_parenting","item13_love_affection","item14_reproduction","item15_happiness_parenting",     
                         "item16_resistance_parenting","item17_reaction_no_children","item18_responsability_society","item19_reaction_parentbehaviour",
                         "item20_meaningful_existence","item21_no_children_places","item22_living_area_family","item23_spending_time",              
                         "item24_media_parent_topics","item25_cancel_plans","item26_politics_measures")



#2.3 histograms of items (explorative)


hist(natalism3$item1_parent_role)
hist(natalism3$item2_endure_person)
hist(natalism3$item3_own_children)
hist(natalism3$item4_life_plan)
hist(natalism3$item5_personal_idea)
hist(natalism3$item6_fulfilled_life)
hist(natalism3$item7_complete_family)
hist(natalism3$item8_childbearing_meaning)
hist(natalism3$item9_prospective_parent)
hist(natalism3$item10_central_lifegoal)
hist(natalism3$item_11important_decisions)
hist(natalism3$item12_parenting)
hist(natalism3$item13_love_affection)
hist(natalism3$item14_reproduction)
hist(natalism3$item15_happiness_parenting)
hist(natalism3$item16_resistance_parenting)
hist(natalism3$item18_responsability_society)
hist(natalism3$item17_reaction_no_children)
hist(natalism3$item19_reaction_parentbehaviour)
hist(natalism3$item20_meaningful_existence)
hist(natalism3$item21_no_children_places)
hist(natalism3$item22_living_area_family)
hist(natalism3$item23_spending_time)
hist(natalism3$item24_media_parent_topics)
hist(natalism3$item25_cancel_plans)
hist(natalism3$item26_politics_measures)


#2.4 exploratory factor analysis, maximum likelihood, oblique rotation


#new dataset with items for the FA
itemsfa <- natalism3[,c(1:26)]


#check KMO
KMO(itemsfa) #good



#check bartlett test 
cortest.bartlett(itemsfa)


#number of factors to extract? 

fa.parallel(itemsfa, fm="ML", fa="fa", 
            main = "Parallel Analysis Scree Plots",
            n.iter=50)
#suggests 3 factors, one with eigen value <1.0 -> 2 and 3 Faktor solution 

#FA ML, oblique rotation, 3 Factors
fa.results <- fa(itemsfa, nfactors = 3, rotate = "oblimin", fm = "ML")


#sort coefficients, surpress loadings below .30

fa.sort(fa.results)
print.psych(fa.results, cut=0.3, sort=T)


#EFA with 2 Factors
fa.twofac <- fa(itemsfa, nfactors = 2, rotate = "oblimin", fm = "ML")
fa.sort(fa.twofac)
print.psych(fa.twofac, cut=0.3, sort=T)

#Loadings:
#Factor 1  Items 3,9,4,5,1,10,2,12,13,11,15,6,16,22,23,17,20,8,18,7
# Factor 2  Items: 25,24,19,21,26

#prefer 2 factor solution -> results for this, results for 3 factor solution presented in Appendix
#Factor 1: general attitude / value of children parenthood, 2: specific aspects e.g., adult only hotels

#exlude items 14,8,29 : double loadings / no loadings
#recode: items that load positively on Factor 1 and item 26

#descriptives before recoding items
psych::describe(natalism3)

#2.5 recoding items

#Item 3,9,5,1,2,13,11,22,17,23,18,7,26

natalism3$item3_own_children <- car::recode(natalism3$item3_own_children,"1=6;2=5;3=4;4=3;5=2;6=1") 
natalism3$item9_prospective_parent<- car::recode(natalism3$item9_prospective_parent,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item1_parent_role<- car::recode(natalism3$item1_parent_role,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item2_endure_person<- car::recode(natalism3$item2_endure_person,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item13_love_affection<- car::recode(natalism3$item13_love_affection,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item_11important_decisions<- car::recode(natalism3$item_11important_decisions,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item22_living_area_family<- car::recode(natalism3$item22_living_area_family,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item17_reaction_no_children<- car::recode(natalism3$item17_reaction_no_children,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item23_spending_time<- car::recode(natalism3$item23_spending_time,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item18_responsability_society<- car::recode(natalism3$item18_responsability_society,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item7_complete_family<- car::recode(natalism3$item7_complete_family,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item5_personal_idea<- car::recode(natalism3$item5_personal_idea,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism3$item26_politics_measures<- car::recode(natalism3$item26_politics_measures,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")



#2.6 reliabilities, item-total correlations

#add means of global natalism scale and sub factors

natalism3$submeanF1 <- rowMeans(natalism3[,c(3,9,4,5,1,10,2,12,13,11,15,6,16,22,23,17,20,18,7)])
natalism3$submeanF2 <- rowMeans(natalism3[,c(25,24,19,21)])
natalism3$mean <- rowMeans(natalism3[,c(26,8,14,25,24,19,21,3,9,4,5,2,10,1,12,13,11,15,6,16,22,23,17,20,18,7)])

#description

psych::describe(natalism3$mean)
psych::describe(natalism3$submeanF1)
psych::describe(natalism3$submeanF2)


#reliabilities and item-total correlations


#global scale
reliability(cov(natalism3[,c("item1_parent_role","item2_endure_person","item3_own_children","item4_life_plan","item5_personal_idea","item6_fulfilled_life","item7_complete_family",            
                             "item8_childbearing_meaning","item9_prospective_parent","item10_central_lifegoal","item_11important_decisions",        
                             "item12_parenting","item13_love_affection","item14_reproduction", "item15_happiness_parenting",     
                             "item16_resistance_parenting","item17_reaction_no_children","item18_responsability_society","item19_reaction_parentbehaviour",
                             "item20_meaningful_existence","item21_no_children_places","item22_living_area_family","item23_spending_time",              
                             "item24_media_parent_topics","item25_cancel_plans","item26_politics_measures")], use="complete.obs")) 



#Subdimension / Factor 1: 
reliability(cov(natalism3[,c("item1_parent_role","item2_endure_person","item3_own_children","item4_life_plan","item5_personal_idea","item6_fulfilled_life", "item7_complete_family",           
                             "item9_prospective_parent","item10_central_lifegoal","item_11important_decisions",        
                             "item12_parenting","item13_love_affection","item15_happiness_parenting", "item16_resistance_parenting",     
                             "item17_reaction_no_children", "item18_responsability_society",
                             "item20_meaningful_existence", "item22_living_area_family","item23_spending_time")], use="complete.obs")) 



#Subdimension /Factor 2: 
reliability(cov(natalism3[,c("item19_reaction_parentbehaviour",
                             "item21_no_children_places",             
                             "item24_media_parent_topics","item25_cancel_plans")], use="complete.obs")) 

 





#############################
#End  Natalism-Items
############################







#####################################################################
############################ 3. Preparation demographics
#####################################################################


#3.1 delete columns not necessary


#olumn created, modified, ended, expired (1,2,3,4)
demographics2 <- demographics [,-c(1,2,3,4)]
#column Ihrer, Ihre, Ihremm (24,25,26)
demographics3 <- demographics2 [,-c(24,25,26)]

names(demographics3)


#3.2 recode as factors

demographics3$gender <- as.factor(demographics3$gender)
demographics3$gender_ident <- as.factor(demographics3$gender_ident)
demographics3$siblings <- as.factor(demographics3$siblings)
demographics3$sex_orientation <- as.factor(demographics3$sex_orientation)
demographics3$children <- as.factor (demographics3$children)
demographics3$relationship_status <- as.factor(demographics3$relationship_status)
demographics3$partner_gender <- as.factor (demographics3$partner_gender)
demographics3$cohabit_partner <- as.factor (demographics3$cohabit_partner)
demographics3$desired_housing <- as.factor(demographics3$desired_housing)
demographics3$education_level <- as.factor(demographics3$education_level)
demographics3$jobs <- as.factor(demographics3$jobs)
demographics3$full_part_time <- as.factor(demographics3$full_part_time)
demographics3$living_situation <- as.factor(demographics3$living_situation)
demographics3$home_ownership <- as.factor(demographics3$home_ownership)
demographics3$desired_home <- as.factor(demographics3$desired_home)
demographics3$religion.x <- as.factor(demographics3$religion.x)


#3.3 rename columns


#show columns
names(demographics3)

#new dataset
demographics4 <- demographics3

#rename columns
colnames(demographics4) <- c("Geschlecht", "Geschlecht_Zusatz", "Geschlechtsidentitaet","GIdenitaet_Zusatz", "Alter",
                             "Geschwister_ja_nein", "Geschwister_Zahl", "Religion", "Religion_Zusatz", "Konfession", "Konfession_Zusatz", 
                             "Religioesitaet", "Religioesitaet_Zusatz", "sexuelle_Orientierung", "sexuelle_Orientierung_Zusatz", "Politik",
                             "Kinder_ja_nein", "Kinder_Anzahl", "Beziehungsstatus", "Beziehungstatus_Zusatz", "Anzahl_Beziehungen", "Partner_Geschlecht",
                             "Partner_Geschlecht_Zusatz","Wohnen_Partner", "Datum_Wohnen", "Plaene_Zusammenwohnen", "Tage_gemeinsam", "Naechte_gemeinsam",
                             "Beziehungszufriedenheit", "Naehe_Partner", "Bildungsjahre", "Bildungsabschluss", "Bildungsabschluss_Zusatz", "berufliche_Situation", 
                             "Berufl_Situation_Zusatz", "Voll_Teilzeit", "Arbeitsstunden", "Einkommen", "Bereit_Einkommen_ja_nein","selbsterbrachtes_Einkommen",
                             "staedtisch_laendlich", "Wohnsituation", "Wohnsituation_Zusatz", "Wohneigentumserwerb", "Postleitzahl")





#3.4 Recode:  Dummy-Coding


#Change NAs at siblings: those with NAs have 0 siblings
library(car)
demographicsOZ1 <- demographics4
demographicsOZ1$Geschwister_Zahl <- car::recode(demographicsOZ1$Geschwister_Zahl,"0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;9=9;NA=0")


#Dummy-Coding



#make new column out of column profession (double it, adding it at the end)
demographicsOZ1$beruflicheSituation2<-demographicsOZ1$berufliche_Situation
#recode: Student = 1, 0 = non-Student, 
demographicsOZ1$beruflicheSituation2<- car::recode(demographicsOZ1$beruflicheSituation2,"1=0;2=0;3=0;4=1;5=0;6=0;7=0;NA=NA")


#Siblings: with = 0, without  = 1
demographicsOZ1$Geschwister_ja_nein<- car::recode(demographicsOZ1$Geschwister_ja_nein, "1=0; 2=1; NA=NA")


#gender: female = 0, male = 1
demographicsOZ1$Geschlecht<- car::recode(demographicsOZ1$Geschlecht, "1=0; 2=1; NA=NA")

#recode political attitude 1-7 -> higher value more left orientated 
demographicsOZ1$Politik<- car::recode(demographicsOZ1$Politik,"1=7;2=6;3=5;4=4;5=3;6=2;7=1;NA=NA")



#3.5 description and tables
psych::describe(demographicsOZ1)

table(demographicsOZ1$Geschlecht, useNA = "ifany")
table(demographicsOZ1$Alter, useNA = "ifany")
table(demographicsOZ1$Geschwister_Zahl, useNA = "ifany")                             
table(demographicsOZ1$Beziehungsstatus, useNA = "ifany")
table(demographicsOZ1$Religion, useNA = "ifany")
table(demographicsOZ1$Konfession, useNA = "ifany")
table(demographicsOZ1$Religioesitaet, useNA = "ifany")
table(demographicsOZ1$sexuelle_Orientierung, useNA = "ifany")
table(demographicsOZ1$Politik, useNA = "ifany")
table(demographicsOZ1$Bildungsjahre, useNA = "ifany")
table(demographicsOZ1$Bildungsabschluss, useNA = "ifany")
table(demographicsOZ1$berufliche_Situation, useNA = "ifany")
table(demographicsOZ1$Einkommen, useNA = "ifany")
table(demographicsOZ1$staedtisch_laendlich, useNA = "ifany")
table(demographicsOZ1$Wohnsituation, useNA = "ifany")
table(demographicsOZ1$Voll_Teilzeit, useNA = "ifany")
table(demographicsOZ1$Geschwister_ja_nein, useNA = "ifany")
table(demographicsOZ1$beruflicheSituation2, useNA = "ifany")


############
#End Demographics
###########

###############################################################################################
######################### 4. Preparation Personality
##################################################################################################



#4.1 delete columns not necessary

#column created modified, epxired und ended (1,2,3,4)

personality2 <- personality [,-c(1,2,3,4)]


#4.2 rename columns


##show columns
names(personality2)

##new dataset
personality3 <- personality2

##rename columns
colnames(personality3) <- c("BFIK_extra01R" , "narq_1"        , "BFIK_agree01R"  ,"self_esteem_1"  ,"narq_2"        , "BFIK_consc01" , 
                            "self_esteem_2r", "BFIK_neuro01"  , "narq_3"        , "BFIK_open01"   , "self_esteem_3" , "BFIK_extra02"  , "narq_4"   ,     
                            "BFIK_agree02"  , "self_esteem_4" , "narq_5"  ,       "BFIK_consc02R" ,  "narq_6"      ,   "self_esteem_5r", "BFIK_neuro02R", 
                            "BFIK_open02",    "narq_7",        "self_esteem_6r", "BFIK_extra03R",  "narq_8",        "BFIK_agree03R",  "self_esteem_7", "BFIK_consc03"  , "narq_9"   ,      "BFIK_neuro03" ,  "BFIK_open03" ,   "self_esteem_8r" ,"BFIK_extra04" ,  "BFIK_agree04R" ,
                            "narq_10"   ,     "BFIK_consc04"  , "narq_11" ,       "BFIK_neuro04" ,  "narq_12"    ,    "narq_13"   ,     "BFIK_open04",   
                            "narq_14" ,       "self_esteem_9r", "narq_15"   ,     "BFIK_open05R"  , "narq_16" ,       "narq_17" ,       "self_esteem_10",
                            "narq_18" )


#4.3 recode inverted items


###Big Five
#Agreeableness: 1,3,4
personality3$BFIK_agree01R <- car::recode(personality3$BFIK_agree01R,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$BFIK_agree03R <- car::recode(personality3$BFIK_agree03R,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$BFIK_agree04R <- car::recode(personality3$BFIK_agree04R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

#Extraversion 1,3
personality3$BFIK_extra01R <- car::recode(personality3$BFIK_extra01R,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$BFIK_extra03R <- car::recode(personality3$BFIK_extra03R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

#Conscientiousness: 2
personality3$BFIK_consc02R <- car::recode(personality3$BFIK_consc02R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

#Neuroticis: 2
personality3$BFIK_neuro02R <- car::recode(personality3$BFIK_neuro02R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

#Openness: 5
personality3$BFIK_open05R <- car::recode(personality3$BFIK_open05R,"1=5;2=4;3=3;4=2;5=1;NA=NA")


###NARQ: none

###self-esteem: 2, 5, 6, 8, 9
personality3$self_esteem_2r <- car::recode(personality3$self_esteem_2r,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$self_esteem_5r <- car::recode(personality3$self_esteem_5r,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$self_esteem_6r <- car::recode(personality3$self_esteem_6r,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$self_esteem_9r <- car::recode(personality3$self_esteem_9r,"1=5;2=4;3=3;4=2;5=1;NA=NA")
personality3$self_esteem_8r <- car::recode(personality3$self_esteem_8r,"1=5;2=4;3=3;4=2;5=1;NA=NA")




#4.4 Descripitives

psych::describe(personality3)
names(personality3)


#build row-means 


###Big Five
#Extraversion 
personality3$MeanExtraversion <- rowMeans(personality3[,c(1,12,24,33)], na.rm=TRUE)
#Agreeableness
personality3$MeanVertraeglichkeit <- rowMeans(personality3[,c(3,14,26,34)], na.rm=TRUE)
#Conscientiousness
personality3$MeanGewissenhaftigkeit <- rowMeans(personality3[,c(6,17,28,36)], na.rm=TRUE)
#Openness
personality3$MeanOffenheit <- rowMeans(personality3[,c(10,21,31,41,45)], na.rm=TRUE)
#Neuroticism: 
personality3$MeanNeurotizismus <- rowMeans(personality3[,c(8,20,30,38)], na.rm=TRUE)

### NARQ: Admiration and Rivalry 
personality3$MeanAdmiration <- rowMeans(personality3[,c(2,5,9,16,22,25,44,46,49)], na.rm=TRUE)
personality3$MeanRivalry <- rowMeans(personality3[,c(13,18,29,35,37,39,40,42,47)], na.rm=TRUE)

##self-esteem all 10 Items -> global self-esteem
personality3$MeanSelfesteem <- rowMeans(personality3[,c(4,7,11,15,19,23,27,32,43,48)], na.rm=TRUE)


#Descriptives


psych::describe(personality3)
psych::describe(personality3$MeanAdmiration)
psych::describe(personality3$MeanRivalry)
psych::describe(personality3$MeanExtraversion)
psych::describe(personality3$MeanOffenheit)
psych::describe(personality3$MeanNeurotizismus)
psych::describe(personality3$MeanVertraeglichkeit)
psych::describe(personality3$MeanGewissenhaftigkeit)
psych::describe(personality3$MeanSelfesteem)




#4.5 Reliabilities and item-total correlations



##Big Five
#Extraversion
reliability(cov(personality3[,c("BFIK_extra01R","BFIK_extra02","BFIK_extra03R","BFIK_extra04")], use="complete.obs"))
#Agreeableness
reliability(cov(personality3[,c("BFIK_agree01R","BFIK_agree02","BFIK_agree03R","BFIK_agree04R")], use="complete.obs"))
#Conscientiousness
reliability(cov(personality3[,c("BFIK_consc01","BFIK_consc02R","BFIK_consc03","BFIK_consc04")], use="complete.obs"))
#Neuroticism
reliability(cov(personality3[,c("BFIK_neuro01","BFIK_neuro02R","BFIK_neuro03","BFIK_neuro04")], use="complete.obs"))
#Openness
reliability(cov(personality3[,c("BFIK_open01","BFIK_open02","BFIK_open03","BFIK_open04","BFIK_open05R")], use="complete.obs"))


###Narq
#Admiration 
reliability(cov(personality3[,c("narq_1","narq_2","narq_3","narq_8","narq_15","narq_5","narq_7","narq_16","narq_18")], use="complete.obs"))

#Rivalry
reliability(cov(personality3[,c("narq_13","narq_14","narq_17","narq_6","narq_9","narq_10","narq_4","narq_11","narq_12")], use="complete.obs"))


###self esteem: 
reliability(cov(personality3[,c("self_esteem_1","self_esteem_2r","self_esteem_3","self_esteem_4",
                                "self_esteem_5r","self_esteem_6r","self_esteem_7","self_esteem_8r",
                                "self_esteem_9r","self_esteem_10")], use="complete.obs"))

##############
#End Personality
##############



#############################################################################
################ 5. Preparation LFAIS, Attitude towards abortion
###################################################################################


#5.1 delete columns not necessary

#column created modified, epxired und ended (2,3,4,1) and repetitions (24,25,26,27)

LFAIS2 <- LFAIS [,-c(1,2,3,4,23,24,25,26,27)]



#5.2 rename columns


#show columns
names(LFAIS2)

#new dataset
LFAIS3 <- LFAIS2

#rename columns
colnames(LFAIS3) <- c(             "LFAIS_1_R"              ,    "LFAIS_2"         ,           "LFAIS_3_R"   ,              
                      "LFAIS_4"              ,      "LFAIS_5_R"               ,   "LFAIS_6"          ,          "LFAIS_7"      ,             
                      "LFAIS_8"              ,      "LFAIS_9_R"               ,   "LFAIS_10"         ,          "LFAIS_11_R"     ,           
                      "LFAIS_12"              ,     "LFAIS_13"     ,              "LFAIS_14"          ,         "LFAIS_15"        ,          
                      "LFAIS_16"         ,         "LFAIS_17"          ,        "Schwangerschaftsabbruch"       ,           "Verhuetung_ja_nein"  ,      
                      "Verhuetungsmethode"   ,   "sterilisation_selbst"      ,       "sterilisation_partner"            ,        "steril_special"   ,        
                      "contra_yes"             ,   "contracept_count"   ,       "contracept_special"     ,   "yesno_prosp"   ,           
                      "prospective_child_3_5"  ,   "prospective_child_6_10"   , "prospective_child_11_15"  , "prospective_child_ever" ,  
                      "social_marriage_friends"  , "social_children_friends"  , "no_mean_topics"        ,    "prospective_child_friends",
                      "content_friends"      ,     "friends_no_children"   ,    "age_friends"          ,    "friends_anti_children"  ,  
                      "plans_friends"         ,    "life_goal"           ,      "influence_friends") 

#5.3 recode inverted items

#LFAIS: 1, 3, 5, 9, 11, 



LFAIS3$LFAIS_1_R<- car::recode(LFAIS3$LFAIS_1_R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

LFAIS3$LFAIS_3_R<- car::recode(LFAIS3$LFAIS_3_R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

LFAIS3$LFAIS_5_R<- car::recode(LFAIS3$LFAIS_5_R,"1=5;2=4;3=3;4=2;5=1;NA=NA")    

LFAIS3$LFAIS_9_R<- car::recode(LFAIS3$LFAIS_9_R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

LFAIS3$LFAIS_11_R<- car::recode(LFAIS3$LFAIS_11_R,"1=5;2=4;3=3;4=2;5=1;NA=NA")

#recode item attitude tow. abortion -> higher value indicates rather liberal attitude
LFAIS3$Schwangerschaftsabbruch <- car::recode(LFAIS3$Schwangerschaftsabbruch, "1=5;2=4;3=3;4=2;5=1;NA=NA")



#5.4 descriptives

psych::describe(LFAIS3)



#row means LFAIS


#Gender roles: 1-9
#goals of feminism 10.17
LFAIS3$mean_genderroles_LFAIS <- rowMeans(LFAIS3[,1:9], na.rm = T)
hist(LFAIS3$mean_genderroles_LFAIS)
LFAIS3$mean_goalsoffeminism <- rowMeans(LFAIS3[,10:17], na.rm = T)
hist(LFAIS3$mean_goalsoffeminism)
hist(LFAIS3$Schwangerschaftsabbruch)




#descriptives

psych::describe(LFAIS3$mean_genderroles_LFAIS)
psych::describe(LFAIS3$mean_goalsoffeminism)
psych::describe(LFAIS3$Schwangerschaftsabbruch)



#5.5 reliabilities and item-total correlations

#gender roles
reliability(cov(LFAIS3[,c("LFAIS_1_R","LFAIS_2","LFAIS_3_R","LFAIS_4","LFAIS_5_R","LFAIS_6","LFAIS_7","LFAIS_8",
                          "LFAIS_9_R")], use="complete.obs"))


#goals of feminism
reliability(cov(LFAIS3[,c("LFAIS_10","LFAIS_11_R","LFAIS_12","LFAIS_13","LFAIS_14","LFAIS_15","LFAIS_16","LFAIS_17")], use="complete.obs")) 





################
#End LFAIS, attitude tow. abortion
################




##############################################################################################
################ 6. Correlations, t-tests and regressions
#################################################################################################



#6.1 build subdataset for sequential computings


itemsregmodeldemo <- demographicsOZ1[,c(1,5,6,12,16,31,46)]
itemsregmodelbfi1 <-personality3[,c(50,51,52,53,54,55,56,57)]
itemsregmodelmeannatalism <- natalism3[,c(27,28,29)]
itemsregmodelLFAIS <- LFAIS3[,c(18,43,44)]
itemsregmodelcorona <- corona[,c(5)]

# M, SD for Covid-19

psych::describe(corona)

#merge subdatasets in one dataset
itemsregmodel <- cbind(itemsregmodelbfi1, itemsregmodelmeannatalism, itemsregmodelLFAIS, itemsregmodeldemo, itemsregmodelcorona)

#6.2 rename columns
names(itemsregmodel)
colnames(itemsregmodel) <- c("MeanExtraversion"   ,     "MeanVertraeglichkeit",    "MeanGewissenhaftigkeit",  "MeanOffenheit" ,         
                      "MeanNeurotizismus"   ,    "MeanAdmiration"       ,   "MeanRivalry"            , "MeanSelfesteem" ,        
                     "MittelFaktor1"                  ,  "MittelFaktor2"              , "MittelwertNatalismus"          ,      
                      "Schwangerschaftsabbruch", "mean_genderroles_LFAIS" , "mean_goalsoffeminism"    ,"Geschlecht"       ,      
                      "Alter"                   ,"Geschwister_ja_nein"     ,"Religioesitaet"          ,"Politik"           ,     
                      "Bildungsjahre"           ,"Student_nichtStudent"    ,"EinflussCorona") 


#6.3 subsettings 

#Subsetten of itemsregmodel to male/female
itemsfemale <- itemsregmodel[ which(itemsregmodel$Geschlecht=='0'),]
itemsmale <- itemsregmodel[ which(itemsregmodel$Geschlecht=='1'),]
psych::describe(itemsfemale)
psych::describe(itemsmale)

###Subsetten of itemsregmodel to Student /non-Student
itemsstudent <- itemsregmodel [ which(itemsregmodel$Student_nichtStudent=='1'),]
itemsnichtstudent <- itemsregmodel [ which(itemsregmodel$Student_nichtStudent=='0'),]

##subsetten of itemsregmodel to siblings: yes/no 
itemsgeschwister <- itemsregmodel [ which(itemsregmodel$Geschwister_ja_nein=='0'),]
itemseinzelkind <- itemsregmodel [ which(itemsregmodel$Geschwister_ja_nein=='1'),]




#6.4 Correlations, t.tests, z tests and CIs

 
#####################################################Total Sample##############



##################################
#########global natalism scale
#################################


####Mean differences: :t.test

#gender
t.test(MittelwertNatalismus ~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
# M and SD of the Scores 
psych::describe(itemsfemale)
psych::describe(itemsmale)
#Cohens D
#(M1-M2)/Wurzel((SD^2+SD^2)/2)



(3.67- 3.16)/sqrt((1.210^2+0.84^2)/2)

  
#students vs. non-students
t.test(MittelwertNatalismus ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
#for M und Sd
psych::describe(itemsstudent)
psych::describe(itemsnichtstudent)
#cohens d
(3.79-3.26)/sqrt((1.26^2+0.93^2)/2)


#siblings / yes/ne
t.test(MittelwertNatalismus ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)

#for M un SD
psych::describe(itemsgeschwister)
psych::describe(itemseinzelkind)
#cohensd
(3.72 -3.48)/sqrt((1.15^2+1.13^2)/2)


########
#Correlations
########

##intercorrelations
#plain dataset with continous variables/ correlates for intercorrelation matrix
names(itemsregmodel)
itemsintercorrelation <- itemsregmodel[,c(16,18,20,19,1,2,3,4,5,6,7,8,13,14,12,22)]

#intercorrelation table
library(apaTables)
apa.cor.table(itemsintercorrelation, filename="Table5_APA.doc", table.number=1, show.conf.interval = FALSE)

#####Correlations globale natalism scale

#Age
cor.test(natalism3$mean, demographicsOZ1$Alter, na.rm = TRUE)
#Religiosity
cor.test(natalism3$mean, demographicsOZ1$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(natalism3$mean, demographicsOZ1$Politik, na.rm = TRUE)
#years of education
cor.test(natalism3$mean, demographicsOZ1$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(natalism3$mean, personality3$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(natalism3$mean, personality3$MeanVertraeglichkeit, na.rm = TRUE)
#conscientiousness
cor.test(natalism3$mean, personality3$MeanGewissenhaftigkeit, na.rm = TRUE)
#openness
cor.test(natalism3$mean, personality3$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(natalism3$mean, personality3$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(natalism3$mean, personality3$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(natalism3$mean, personality3$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(natalism3$mean, personality3$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(natalism3$mean, LFAIS3$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(natalism3$mean, LFAIS3$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(natalism3$mean, LFAIS3$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covi-19
cor.test(natalism3$mean, corona$corona_infl, na.rm = TRUE)



####################################
#################Factor 1
###################################

###t.tests

#gender
t.test(MittelFaktor1 ~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
#cohensd


(3.78-3.24)/sqrt((1.35^2+0.96^2)/2)

#students vs. non-students
t.test(MittelFaktor1 ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
#cohens d
(3.91-3.34)/sqrt((1.09^2+1.38^2)/2)


#siblings yes/no
t.test(MittelFaktor1 ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
#für cohens d
(3.86-3.57)/sqrt((1.30^2+1.26^2)/2)

###Correlations


#Age
cor.test(natalism3$submeanF1, demographicsOZ1$Alter, na.rm = TRUE)
#Religiosity
cor.test(natalism3$submeanF1, demographicsOZ1$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(natalism3$submeanF1, demographicsOZ1$Politik, na.rm = TRUE)
#years of education
cor.test(natalism3$submeanF1, demographicsOZ1$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(natalism3$submeanF1, personality3$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(natalism3$submeanF1, personality3$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(natalism3$submeanF1, personality3$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(natalism3$submeanF1, personality3$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(natalism3$submeanF1, personality3$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(natalism3$submeanF1, personality3$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(natalism3$submeanF1, personality3$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(natalism3$submeanF1, personality3$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(natalism3$submeanF1, LFAIS3$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(natalism3$submeanF1, LFAIS3$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(natalism3$submeanF1, LFAIS3$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covid-19
cor.test(natalism3$submeanF1, corona$corona_infl, na.rm = TRUE)


##################################
#################Factor 2 
##################################

###t.tests

#gender
t.test(MittelFaktor2 ~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
#Cohensd
(3.54-3.11)/sqrt((1.13^2+0.89^2)/2)

#students vs. non-students
t.test(MittelFaktor2 ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
#cohens d
(3.65-3.17)/sqrt((1.21^2+0.87^2)/2)


#siblings yes vs. no
t.test(MittelFaktor2 ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsregmodel)
(3.47-3.40)/sqrt((1.13^2+1.07^2)/2)


###Correlations

#Age
cor.test(natalism3$submeanF2, demographicsOZ1$Alter, na.rm = TRUE)
#Religiosity
cor.test(natalism3$submeanF2, demographicsOZ1$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(natalism3$submeanF2, demographicsOZ1$Politik, na.rm = TRUE)
#years of education
cor.test(natalism3$submeanF2, demographicsOZ1$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(natalism3$submeanF2, personality3$MeanExtraversion, na.rm = TRUE)
#agreeableness
cor.test(natalism3$submeanF2, personality3$MeanVertraeglichkeit, na.rm = TRUE)
#conscientiousness
cor.test(natalism3$submeanF2, personality3$MeanGewissenhaftigkeit, na.rm = TRUE)
#openness
cor.test(natalism3$submeanF2, personality3$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(natalism3$submeanF2, personality3$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(natalism3$submeanF2, personality3$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(natalism3$submeanF2, personality3$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(natalism3$submeanF2, personality3$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(natalism3$submeanF2, LFAIS3$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(natalism3$submeanF2, LFAIS3$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(natalism3$submeanF2, LFAIS3$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covid.19
cor.test(natalism3$submeanF2, corona$corona_infl, na.rm = TRUE)



#####CIs for the total sample


#Age
CIr(.31, 291, .95)
CIr(.32, 291, .95)
CIr(.23, 291, .95)

#Religiosity
CIr(-.34, 291, .95)
CIr(-.13, 291, .95)

#years of education
CIr(.12, 291, .95)
CIr(.14, 291, .95)
CIr(.07, 291, .95)

#politics
CIr(.06, 291, .95)
CIr(.09, 291, .95)
CIr(-.09, 291, .95)

#Extraversion
CIr(-.26, 291, .95)
CIr(-.27, 291, .95)
CIr(-.16, 291, .95)

#Agreeableness
CIr(-.33, 291, .95)
CIr(-.30, 291, .95)
CIr(-.35, 291, .95)

#Conscientiousness
CIr(-.20, 291, .95)
CIr(-.11, 291, .95)

#Openness
CIr(.04, 291, .95)
CIr(.06, 291, .95)
CIr(-.09, 291, .95)

#Neuroticism
CIr(.17, 291, .95)
CIr(.18, 291, .95)
CIr(.03, 291, .95)

#Admiration
CIr(-.09, 291, .95)
CIr(-.11, 291, .95)
CIr(.05, 291, .95)

#Rivalry
CIr(.20, 291, .95)
CIr(.13, 291, .95)
CIr(.40, 291, .95)

#Seflesteem
CIr(-.16, 291, .95)
CIr(-.15, 291, .95)
CIr(.00, 291, .95)

#Gender roles
CIr(.19, 291, .95)
CIr(.06, 291, .95)
CIr(.21, 291, .95)

#Goals of feminism
CIr(.18, 291, .95)
CIr(.19, 291, .95)
CIr(.14, 291, .95)

#Attitude towards abortion

CIr(.21, 291, .95)
CIr(.16, 291, .95)

#covid
CIr(-.07, 291, .95)
CIr(-.09, 291, .95)
CIr(-.04, 291, .95)




######################################################################
####################################correlations separated by men and women 
######################################################################



#######################
######Correlations for women 
#######################


###global natalism scale

#Age
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$Religioesitaet, na.rm = TRUE)
#Political Attitude
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$Politik, na.rm = TRUE)
#years of education
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$mean_goalsoffeminism, na.rm = TRUE)
#Attitude tow. abortion
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covid-19
cor.test(itemsfemale$MittelwertNatalismus, itemsfemale$EinflussCorona, na.rm = TRUE)





###Factor 1

#Age
cor.test(itemsfemale$MittelFaktor1, itemsfemale$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsfemale$MittelFaktor1, itemsfemale$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(itemsfemale$MittelFaktor1, itemsfemale$Politik, na.rm = TRUE)
#years of educations
cor.test(itemsfemale$MittelFaktor1, itemsfemale$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsfemale$MittelFaktor1, itemsfemale$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsfemale$MittelFaktor1, itemsfemale$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsfemale$MittelFaktor1, itemsfemale$mean_goalsoffeminism, na.rm = TRUE)
#Attitude tow. abortion
cor.test(itemsfemale$MittelFaktor1, itemsfemale$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covid.19
cor.test(itemsfemale$MittelFaktor1, itemsfemale$EinflussCorona, na.rm = TRUE)


###Factor 2


#Age
cor.test(itemsfemale$MittelFaktor2, itemsfemale$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsfemale$MittelFaktor2, itemsfemale$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(itemsfemale$MittelFaktor2, itemsfemale$Politik, na.rm = TRUE)
#years of education
cor.test(itemsfemale$MittelFaktor2, itemsfemale$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanExtraversion, na.rm = TRUE)
#agreeableness
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanVertraeglichkeit, na.rm = TRUE)
#conscientousness
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanGewissenhaftigkeit, na.rm = TRUE)
#openness
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanOffenheit, na.rm = TRUE)
#neuroticsim
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsfemale$MittelFaktor2, itemsfemale$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsfemale$MittelFaktor2, itemsfemale$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsfemale$MittelFaktor2, itemsfemale$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(itemsfemale$MittelFaktor2, itemsfemale$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covid-19
cor.test(itemsfemale$MittelFaktor2, itemsfemale$EinflussCorona, na.rm = TRUE)



#######################
###correlations for men
#######################



###global natalism scale


#Age
cor.test(itemsmale$MittelwertNatalismus, itemsmale$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsmale$MittelwertNatalismus, itemsmale$Religioesitaet, na.rm = TRUE)
#political attitude
cor.test(itemsmale$MittelwertNatalismus, itemsmale$Politik, na.rm = TRUE)
#years of education
cor.test(itemsmale$MittelwertNatalismus, itemsmale$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanVertraeglichkeit, na.rm = TRUE)
#conscientiousness
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsmale$MittelwertNatalismus, itemsmale$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsmale$MittelwertNatalismus, itemsmale$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsmale$MittelwertNatalismus, itemsmale$mean_goalsoffeminism, na.rm = TRUE)
#Attitude tow. abortion
cor.test(itemsmale$MittelwertNatalismus, itemsmale$Schwangerschaftsabbruch, na.rm = TRUE)
#influence of covid-19
cor.test(itemsmale$MittelwertNatalismus, itemsmale$EinflussCorona, na.rm = TRUE)



###factor 1


#age
cor.test(itemsmale$MittelFaktor1, itemsmale$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsmale$MittelFaktor1, itemsmale$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(itemsmale$MittelFaktor1, itemsmale$Politik, na.rm = TRUE)
#years of education 
cor.test(itemsmale$MittelFaktor1, itemsmale$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanVertraeglichkeit, na.rm = TRUE)
#conscientiousness
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsmale$MittelFaktor1, itemsmale$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsmale$MittelFaktor1, itemsmale$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsmale$MittelFaktor1, itemsmale$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(itemsmale$MittelFaktor1, itemsmale$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(itemsmale$MittelFaktor1, itemsmale$EinflussCorona, na.rm = TRUE)


###factor 2


#age
cor.test(itemsmale$MittelFaktor2, itemsmale$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsmale$MittelFaktor2, itemsmale$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(itemsmale$MittelFaktor2, itemsmale$Politik, na.rm = TRUE)
#years of education
cor.test(itemsmale$MittelFaktor2, itemsmale$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanExtraversion, na.rm = TRUE)
#agreeableness
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanVertraeglichkeit, na.rm = TRUE)
#conscientiousness
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanGewissenhaftigkeit, na.rm = TRUE)
#openness
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsmale$MittelFaktor2, itemsmale$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsmale$MittelFaktor2, itemsmale$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsmale$MittelFaktor2, itemsmale$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(itemsmale$MittelFaktor2, itemsmale$Schwangerschaftsabbruch, na.rm = TRUE)
#covid-19
cor.test(itemsmale$MittelFaktor2, itemsmale$EinflussCorona, na.rm = TRUE)




#####################
#z-tests if correlations of men and women are significantly different
####################


###Global Natalism scale

#Age
paired.r(.32, .28, yz=NULL, 204, 87,twotailed=TRUE)


CIr(.32, 204, .95)
CIr(.28, 87, .95)
#Religiosity
paired.r(-.38, -.27, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.38, 204, .95)
CIr(-.27, 87, .95)
#years of education
paired.r(.10, .11, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.10, 204, .95)
CIr(.11, 87, .95)
#Politics
paired.r(-.02, .04, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.02, 204, .95)
CIr(.04, 87,.95)
#Extraversion
paired.r(-.25, -.30, yz=NULL, 204, 87,twotailed=TRUE)
CIr(-.25, 204, .95)
CIr(-.30, 87, .95)
#Agreeableness
paired.r(-.36, -.30, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.36, 204, .95)
CIr(-.30, 87, .95)
#conscientiousness
paired.r(-.19, -.31, yz=NULL, 204, 87,twotailed=TRUE)
CIr(-.19, 204, .95)
CIr(-.31, 87, .95)
#Openness
paired.r(.04, -.03, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.04, 204, .95)
CIr(-.03, 87, .95)
#Neuroticism
paired.r(.10, .13, yz=NULL, 204, 87,twotailed=TRUE)
CIr(.10, 204, .95)
CIr(.13, 87, .95)
#Admiration
paired.r(-.03, -.13, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.03, 204, .95)
CIr(-.13, 87, .95)
#Rivalry
paired.r(.23, .25, yz=NULL, 204, 87,twotailed=TRUE)
CIr(.23, 204, .95)
CIr(.25, 87, .95)
#Self-esteem
paired.r(-.09, -.24, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.09, 204, .95)
CIr(-.24, 87, .95)
#gender role beliefs
paired.r(.27, -.15, yz=NULL, 204, 87,twotailed=TRUE)
CIr(.27, 204, .95)
CIr(-.15, 87, .95)
#opennness to feminism
paired.r(.22, -.10, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.22, 204, .95)
CIr(-.10, 87, .95)
#attitude towards abortion
paired.r(.27, -.03, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.27, 204, .95)
CIr(-.03, 87, .95)
#covid-19


paired.r(-.08, -.11, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.08, 204, .95)
CIr(-.11, 87, .95)


###Factor 1

#Age
paired.r(.33, .31, yz=NULL, 204, 87,twotailed=TRUE)

##CIs
CIr(.33, 204, .95)
CIr(.31, 87, .95)

#Religiosity
paired.r(-.39, -.22, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.39, 204, .95)
CIr(-.22, 87, .95)
#years of education
paired.r(.12, .12, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.12, 204, .95)
CIr(.12, 87, .95)
#politics
paired.r(-.00, .10, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.00, 204, .95)
CIr(.10, 87,.95)
#Extraversion
paired.r(-.25, -.27, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.25, 204, .95)
CIr(-.27, 87, .95)
#Agreeableness
paired.r(-.33, -.27, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.33, 204, .95)
CIr(-.27, 87, .95)
#Conscientiousness
paired.r(-.18, -.32, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.18, 204, .95)
CIr(-.32, 87, .95)
#Openness
paired.r(.05, .01, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.05, 204, .95)
CIr(.01, 87, .95)
#Neuroticism
paired.r(.12, .14, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.12, 204, .95)
CIr(.14, 87, .95)
#Admiration
paired.r(-.06, -.16, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.06, 204, .95)
CIr(-.16, 87, .95)
#Rivalry
paired.r(.16, .19, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.16, 204, .95)
CIr(.19, 87, .95)
#Self-esteem
paired.r(-.10, -.26, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.10, 204, .95)
CIr(-.26, 87, .95)
#gender role beliefs
paired.r(.30, -.13, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.30, 204, .95)
CIr(-.13, 87, .95)
#opennness to feminism
paired.r(.23, -.07, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.23, 204, .95)
CIr(-.07, 87, .95)
#attitude towards abortion
paired.r(.27, -.02, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.27, 204, .95)
CIr(-.02, 87, .95)
#covid-19
paired.r(-.10, -.08, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.10, 204, .95)
CIr(-.08, 87, .95)




###Factor 2

#Age
paired.r(.27, .08, yz=NULL, 204, 87,twotailed=TRUE)

##CIs
CIr(.27, 204, .95)
CIr(.08, 87, .95)
#Religiosity
paired.r(-.13, -.13, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.13, 204, .95)
CIr(-.13, 87, .95)
#years of education
paired.r(.05, .06, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.05, 204, .95)
CIr(.06, 87, .95)
#Politics
paired.r(-.12, -.26, yz=NULL, 204, 87,twotailed=TRUE)
CIr(-.12, 204, .95)
CIr(-.26, 87,.95)
#Extraversion
paired.r(-.10, -.30, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.10, 204, .95)
CIr(-.30, 87, .95)
#Agreeableness
paired.r(-.39, -.30, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.39, 204, .95)
CIr(-.30, 87, .95)
#Conscientiousness
paired.r(-.13, -.08, yz=NULL, 204, 87,twotailed=TRUE)
CIr(-.13, 204, .95)
CIr(-.08, 87, .95)
#Openness
paired.r(-.08, -.20, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.08, 204, .95)
CIr(-.20, 87, .95)
#Neuroticism
paired.r(-.09, .13, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.09, 204, .95)
CIr(.13, 87, .95)
#Admiration
paired.r(.12, -.01, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.12, 204, .95)
CIr(-.01, 87, .95)
#Rivalry
paired.r(.45, .39, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.45, 204, .95)
CIr(.39, 87, .95)
#Self-esteem
paired.r(.08, -.14, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.08, 204, .95)
CIr(-.14, 87, .95)
#gender role beliefs
paired.r(.04, -.10, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.04, 204, .95)
CIr(-.10, 87, .95)
#opennness to feminism
paired.r(.15, -.07, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.15, 204, .95)
CIr(-.07, 87, .95)
#attitude towards abortion
paired.r(.19, -.01, yz=NULL, 204, 87,twotailed=TRUE)

CIr(.19, 204, .95)
CIr(-.01, 87, .95)
#covid-19
paired.r(-.01, -.22, yz=NULL, 204, 87,twotailed=TRUE)

CIr(-.01, 204, .95)
CIr(-.22, 87, .95)











##############################################
######################## 7. Regressions
###################################################

#Ppackage for standardized regression coefficient
library(QuantPsyc)


#7.1 FA of the correlates (to reduce number of it)
#new dataset for factor analysis of the correlates


names(itemsregmodel)

itemscorfa <- itemsregmodel [,-c(9,10,11,15,16,17,20,21)]
names(itemscorfa)

#delete influence of covid-19 in the new dataset
itemscorfa2 <- itemscorfa [,-c(14)]

#check KMO
KMO(itemscorfa2)



#check Bartlett test
cortest.bartlett(itemscorfa2)
##Ist signfifikant


#How many factors should be extracted?
fa.parallel(itemscorfa2, fm="ML", fa="fa", 
            main = "Parallel Analysis Scree Plots",
            n.iter=50)
#Parallelanalyse suggessts excluding 4 factors, 2 with an eigen value <1.0 -> looking at the 4 factor solution and 2 factor solution 

#FA 4 factors, oblibque rotation, ML
fa.resultscor <- fa(itemscorfa2, nfactors = 4, rotate = "oblimin", fm = "ML")
fa.sort(fa.resultscor)
print.psych(fa.resultscor, cut=0.3, sort=T)


#Saving the factor scores
colnames(fa.resultscor$scores) <-  c('liberalism', 'positive_affect_achieve', 'selfpromotion', 'Agreeableness')
itemscorfa3 <- cbind(itemscorfa2, fa.resultscor$scores)

# descriptives of factor scores 
psych::describe(fa.resultscor$scores)



#7.2 dataset for regression (with natalism scales and covid-19)
itemsregressionfas <- itemscorfa3[,c(14,15,16,17)]
itemsregressionmeans <- itemsregmodel[,c(9,10,11,22)]
itemsregression <- cbind (itemsregressionfas, itemsregressionmeans)

#7.3 Regressions

###Model 1: 

Modell1 <- lm(MittelwertNatalismus ~ liberalism + positive_affect_achieve + selfpromotion + Agreeableness, data = itemsregression)

summary(Modell1)
lm.beta(Modell1)

apa.reg.table(Modell1, filename = "Table11_APA.doc", table.number = 11)



#Comparison with Covid-19: does anything change in the results in comparison with Model1 when including it?

###Modell2
Modellcorona1 <- lm(MittelwertNatalismus ~ EinflussCorona + liberalism + positive_affect_achieve + selfpromotion + Agreeableness, data = itemsregression)

summary(Modellcorona1)
lm.beta(Modellcorona1)
 

AIC(Modell1)
AIC(Modellcorona1)

#F-test / Anova

anova(Modell1, Modellcorona1)


######################################################
###########################8. Results for the Appendices            
#####################################################


######################
#8.1 Results for the three-factor solution of the natalism scale
#####################


#8.1.1 repeating factorial analysis

#new dataset without recoded items
natalism4 <- natalism2

#rename columns
colnames(natalism4) <- c("item1_parent_role","item2_endure_person","item3_own_children","item4_life_plan","item5_personal_idea","item6_fulfilled_life","item7_complete_family",            
                         "item8_childbearing_meaning","item9_prospective_parent","item10_central_lifegoal","item_11important_decisions",        
                         "item12_parenting","item13_love_affection","item14_reproduction","item15_happiness_parenting",     
                         "item16_resistance_parenting","item17_reaction_no_children","item18_responsability_society","item19_reaction_parentbehaviour",
                         "item20_meaningful_existence","item21_no_children_places","item22_living_area_family","item23_spending_time",              
                         "item24_media_parent_topics","item25_cancel_plans","item26_politics_measures")

#subset with only the natalism items
appendixfanatal <- natalism4[,c(1:26)]



#check KMO
KMO(appendixfanatal) #96 -> good

#check Bartlett
cortest.bartlett(appendixfanatal)

# how many factors should be extracted?
fa.parallel(appendixfanatal, fm="ML", fa="fa", 
            main = "Parallel Analysis Scree Plots",
            n.iter=50)
#suggests3 factors, 1 eigen value <1.0

#Results for the three factor solution
threefactorial<-fa(appendixfanatal, nfactors = 3, rotate = "oblimin", fm = "ML")


fa.sort(threefactorial)
print.psych(threefactorial, cut=0.3, sort=T)


#enforce one factor (factor loadings)

onefactorial<-fa(appendixfanatal, nfactors = 1, rotate = "oblimin", fm = "ML")
fa.sort(onefactorial)
print.psych(onefactorial, cut=0.3, sort=T)

#recode items that load positively on the global scale -> global scale and the factors measures antinatalism
natalism4$item3_own_children <- car::recode(natalism4$item3_own_children,"1=6;2=5;3=4;4=3;5=2;6=1") 
natalism4$item9_prospective_parent<- car::recode(natalism4$item9_prospective_parent,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item1_parent_role<- car::recode(natalism4$item1_parent_role,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item2_endure_person<- car::recode(natalism4$item2_endure_person,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item13_love_affection<- car::recode(natalism4$item13_love_affection,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item_11important_decisions<- car::recode(natalism4$item_11important_decisions,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item22_living_area_family<- car::recode(natalism4$item22_living_area_family,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item17_reaction_no_children<- car::recode(natalism4$item17_reaction_no_children,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item23_spending_time<- car::recode(natalism4$item23_spending_time,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item18_responsability_society<- car::recode(natalism4$item18_responsability_society,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item7_complete_family<- car::recode(natalism4$item7_complete_family,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item5_personal_idea<- car::recode(natalism4$item5_personal_idea,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")
natalism4$item26_politics_measures<- car::recode(natalism4$item26_politics_measures,"1=6;2=5;3=4;4=3;5=2;6=1;NA=NA")


#8.1.2 reliability and item total correlations




natalism4$Faktor1<- rowMeans(natalism3[,c(3,9,4,5,1,13,12,2,10,11,22,23,26)])
natalism4$Faktor2 <- rowMeans(natalism3[,c(25,24,19,21)])
natalism4$Faktor3 <- rowMeans(natalism3[,c(7,17,18)])

natalism4$Mittelwert <- rowMeans(natalism3[,c(26,8,14,25,24,19,21,3,9,4,5,2,10,1,12,13,11,15,6,16,22,23,17,20,18,7)])


psych::describe(natalism4$Mittelwert)
psych::describe(natalism4$Faktor1)
psych::describe(natalism4$Faktor2)
psych::describe(natalism4$Faktor3)
psych::describe(natalism4)



#global scale
reliability(cov(natalism3[,c("item1_parent_role","item2_endure_person","item3_own_children","item4_life_plan","item5_personal_idea","item6_fulfilled_life","item7_complete_family",            
                             "item8_childbearing_meaning","item9_prospective_parent","item10_central_lifegoal","item_11important_decisions",        
                             "item12_parenting","item13_love_affection","item14_reproduction", "item15_happiness_parenting",     
                             "item16_resistance_parenting","item17_reaction_no_children","item18_responsability_society","item19_reaction_parentbehaviour",
                             "item20_meaningful_existence","item21_no_children_places","item22_living_area_family","item23_spending_time",              
                             "item24_media_parent_topics","item25_cancel_plans","item26_politics_measures")], use="complete.obs")) 


#Factor 1: 
reliability(cov(natalism3[,c("item1_parent_role", "item2_endure_person", "item3_own_children","item4_life_plan","item5_personal_idea",           
                             "item9_prospective_parent","item10_central_lifegoal","item_11important_decisions",        
                             "item12_parenting","item13_love_affection", 
                             "item22_living_area_family","item23_spending_time", "item26_politics_measures")], use="complete.obs")) 



##Factor 2
reliability(cov(natalism3[,c("item19_reaction_parentbehaviour",
                             "item21_no_children_places",             
                             "item24_media_parent_topics","item25_cancel_plans")], use="complete.obs")) 

#Factor 3
reliability(cov(natalism3[,c("item7_complete_family", "item17_reaction_no_children", "item18_responsability_society")], use="complete.obs")) 





#8.1.3 t-test, correlations, z-tests and CIS  for the three-factor solution

#new subdata with global mean and means of the 3 factors



subdatanatalism<- natalism4[,c(27,28,29,30)]

#megre with datasets of correlates (see above)
itemsthreefactorsolution <- cbind(itemsregmodelbfi1, subdatanatalism, itemsregmodelLFAIS, itemsregmodeldemo, itemsregmodelcorona)

#rename columns
names(itemsthreefactorsolution)
colnames(itemsthreefactorsolution) <- c("MeanExtraversion"   ,     "MeanVertraeglichkeit",    "MeanGewissenhaftigkeit",  "MeanOffenheit" ,         
                             "MeanNeurotizismus"   ,    "MeanAdmiration"       ,   "MeanRivalry"            , "MeanSelfesteem" ,        
                             "Faktor1"                  ,  "Faktor2"  ,"Faktor3"            , "GlobalNatal"          ,      
                             "Schwangerschaftsabbruch", "mean_genderroles_LFAIS" , "mean_goalsoffeminism"    ,"Geschlecht"       ,      
                             "Alter"                   ,"Geschwister_ja_nein"     ,"Religioesitaet"          ,"Politik"           ,     
                             "Bildungsjahre"           ,"Student_nichtStudent"    ,"EinflussCorona") 


#subsetten

###divide into male and female
women <- itemsthreefactorsolution[ which(itemsregmodel$Geschlecht=='0'),]
men<- itemsthreefactorsolution[ which(itemsregmodel$Geschlecht=='1'),]
psych::describe(men)
psych::describe(women)

###divide into students and non-students
student <- itemsthreefactorsolution [ which(itemsregmodel$Student_nichtStudent=='1'),]
nichtstudent <- itemsthreefactorsolution [ which(itemsregmodel$Student_nichtStudent=='0'),]

###divide into only children and individuals with siblings
geschwister <- itemsthreefactorsolution[ which(itemsregmodel$Geschwister_ja_nein=='0'),]
einzelkind <- itemsthreefactorsolution[ which(itemsregmodel$Geschwister_ja_nein=='1'),]



#############################
#####t-tests and correlations
##############################



#######
###mean differences: t.tests
#######


#gender
t.test(GlobalNatal ~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor1~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor2~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor3~ Geschlecht, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)

#M and SD
psych::describe(men)
psych::describe(women)
#Cohens D
#(M1-M2)/Wurzel((SD^2+SD^2)/2)



(3.69- 3.16)/sqrt((1.210^2+0.84^2)/2)
(3.25- 2.76)/sqrt((1.56^2+1.13^2)/2)
(3.54- 3.11)/sqrt((1.13^2+0.89^2)/2)
(5.53- 4.81)/sqrt((.83^2+1.00^2)/2)



#Student vs. Non-student
t.test(GlobalNatal ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor1 ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor2 ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor3 ~ Student_nichtStudent, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)

#M and SD
psych::describe(student)
psych::describe(nichtstudent)
#cohens d
(3.79-3.26)/sqrt((1.26^2+0.93^2)/2)
(3.45-2.76)/sqrt((1.59^2+1.23^2)/2)
(3.66-3.18)/sqrt((1.21^2+0.87^2)/2)
(5.13-5.25)/sqrt((.89^2+0.94^2)/2)


##siblings (yes/no)
t.test(GlobalNatal ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor1 ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor2 ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)
t.test(Faktor3 ~ Geschwister_ja_nein, alternative = 'two.sided', conf.level = .95, data = itemsthreefactorsolution)

#M and SD
psych::describe(geschwister)
psych::describe(einzelkind)
#cohensd
(3.72 -3.48)/sqrt((1.15^2+1.13^2)/2)
(3.35 -3.04)/sqrt((1.52^2+1.14^2)/2)
(3.48 -3.40)/sqrt((1.13^2+1.07^2)/2)
(5.29 -5.17)/sqrt((.74^2+.95^2)/2)


#######
#Correlations
#######

##########################################total sample

#####################
####global scale
####################

#age
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$Politik, na.rm = TRUE)
#years of education
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanVertraeglichkeit, na.rm = TRUE)
#conscientiousness
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$mean_goalsoffeminism, na.rm = TRUE)
#atttiude tow. abortion
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$Schwangerschaftsabbruch, na.rm = TRUE)
#covid-19
cor.test(itemsthreefactorsolution$GlobalNatal, itemsthreefactorsolution$EinflussCorona, na.rm = TRUE)

#same CIs und r's as in the two factor solution (as Items stayed the same)


#######################
#Factor 1
#######################

#Age
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$Religioesitaet, na.rm = TRUE)
#Political attitude
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$Politik, na.rm = TRUE)
#years of education
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$mean_goalsoffeminism, na.rm = TRUE)
#atttiude tow. abortion
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$Schwangerschaftsabbruch, na.rm = TRUE)
#Covid.19
cor.test(itemsthreefactorsolution$Faktor1, itemsthreefactorsolution$EinflussCorona, na.rm = TRUE)

#CIs
CIr(.32, 291, .95)
CIr(-.34, 291, .95)
CIr(.10, 291, .95)
CIr(.04, 291, .95)
CIr(-.28, 291, .95)
CIr(-.30, 291, .95)
CIr(-.22, 291, .95)
CIr(.01, 291, .95)
CIr(.16, 291, .95)
CIr(-.10, 291, .95)
CIr(.18, 291, .95)
CIr(-.16, 291, .95)

CIr(.11, 291, .95)
CIr(.16, 291, .95)
CIr(-.10, 291, .95)

################
#Factor 2
################

#Age
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$Politik, na.rm = TRUE)
#years of education
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$Schwangerschaftsabbruch, na.rm = TRUE)
#Covid.19
cor.test(itemsthreefactorsolution$Faktor2, itemsthreefactorsolution$EinflussCorona, na.rm = TRUE)

#nothing changed values the same as in two factor solution, so CIs are the same, too


################
#Factor 3
###############


##Age
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$Alter, na.rm = TRUE)
#Religiosity
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$Politik, na.rm = TRUE)
#years of education
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(itemsthreefactorsolution$Faktor3, itemsthreefactorsolution$EinflussCorona, na.rm = TRUE)

#CIs

CIr(.11, 291, .95)
CIr(-.21, 291, .95)
CIr(.23, 291, .95)
CIr(.12, 291, .95)
CIr(-.03, 291, .95)
CIr(-.09, 291, .95)
CIr(-.04, 291, .95)
CIr(.17, 291, .95)
CIr(.12, 291, .95)
CIr(-.11, 291, .95)
CIr(-.19, 291, .95)
CIr(-.06, 291, .95)
CIr(.41, 291, .95)
CIr(.36, 291, .95)
CIr(.22, 291, .95)
CIr(-.11, 291, .95)


###########################################Correlations for men and women 


################
###global scale
#################


#age
cor.test(women$GlobalNatal, women$Alter, na.rm = TRUE)
#Religiosity
cor.test(women$GlobalNatal, women$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(women$GlobalNatal, women$Politik, na.rm = TRUE)
#years of education
cor.test(women$GlobalNatal, women$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(women$GlobalNatal, women$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(women$GlobalNatal, women$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(women$GlobalNatal, women$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(women$GlobalNatal, women$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(women$GlobalNatal, women$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(women$GlobalNatal, women$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(women$GlobalNatal, women$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(women$GlobalNatal, women$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(women$GlobalNatal, women$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(women$GlobalNatal, women$mean_goalsoffeminism, na.rm = TRUE)
#Attitude tow. abortion
cor.test(women$GlobalNatal, women$Schwangerschaftsabbruch, na.rm = TRUE)
#Covid.19
cor.test(women$GlobalNatal, women$EinflussCorona, na.rm = TRUE)

#Age
cor.test(men$GlobalNatal, men$Alter, na.rm = TRUE)
#Religiosity
cor.test(men$GlobalNatal, men$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(men$GlobalNatal, men$Politik, na.rm = TRUE)
#Bildungsjahre
cor.test(men$GlobalNatal, men$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(men$GlobalNatal, men$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(men$GlobalNatal, men$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(men$GlobalNatal, men$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(men$GlobalNatal, men$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(men$GlobalNatal, men$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(men$GlobalNatal, men$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(men$GlobalNatal, men$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(men$GlobalNatal, men$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(men$GlobalNatal, men$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(men$GlobalNatal, men$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(men$GlobalNatal, men$Schwangerschaftsabbruch, na.rm = TRUE)
#Covid.19
cor.test(men$GlobalNatal, men$EinflussCorona, na.rm = TRUE)

#r is the same as in 2 factor solution so CIs and z are the same, too

####################
###Factor 1
###################


#Age
cor.test(women$Faktor1, women$Alter, na.rm = TRUE)
#Religiosity
cor.test(women$Faktor1, women$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(women$Faktor1, women$Politik, na.rm = TRUE)
#years of education
cor.test(women$Faktor1, women$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(women$Faktor1, women$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(women$Faktor1, women$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(women$Faktor1, women$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(women$Faktor1, women$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(women$Faktor1, women$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(women$Faktor1, women$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(women$Faktor1, women$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(women$Faktor1, women$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(women$Faktor1, women$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(women$Faktor1, women$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(women$Faktor1, women$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(women$Faktor1, women$EinflussCorona, na.rm = TRUE)

#Age
cor.test(men$Faktor1, men$Alter, na.rm = TRUE)
#Religiosity
cor.test(men$Faktor1, men$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(men$Faktor1, men$Politik, na.rm = TRUE)
#years of education
cor.test(men$Faktor1, men$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(men$Faktor1, men$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(men$Faktor1, men$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(men$Faktor1, men$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(men$Faktor1, men$MeanOffenheit, na.rm = TRUE)
#Neuroticim
cor.test(men$Faktor1, men$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(men$Faktor1, men$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(men$Faktor1, men$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(men$Faktor1, men$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(men$Faktor1, men$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(men$Faktor1, men$mean_goalsoffeminism, na.rm = TRUE)
#atttiude tow. abortion
cor.test(men$Faktor1, men$Schwangerschaftsabbruch, na.rm = TRUE)
#covid-19
cor.test(men$Faktor1, men$EinflussCorona, na.rm = TRUE)

###CIs

CIr(.33, 204, .95)
CIr(-.38, 204, .95)
CIr(.08, 204, .95)
CIr(-.04, 204, .95)
CIr(-.28, 204, .95)
CIr(-.33, 204, .95)
CIr(-.21, 204, .95)
CIr(.00, 204, .95)
CIr(.12, 204, .95)
CIr(-.07, 204, .95)
CIr(.20, 204, .95)
CIr(-.11, 204, .95)
CIr(.24, 204, .95)
CIr(.18, 204, .95)
CIr(.25, 204, .95)
CIr(-.12, 204, .95)

CIr(.30, 87, .95)
CIr(-.23, 87, .95)
CIr(.30, 87, .95)
CIr(.12, 87, .95)
CIr(.03, 87, .95)
CIr(-.25, 87, .95)
CIr(-.24, 87, .95)
CIr(-.22, 87, .95)
CIr(-.30, 87, .95)
CIr(-.05, 87, .95)
CIr(.12, 87, .95)

CIr(-.11, 87, .95)
CIr(.24, 87, .95)
CIr(-.25, 87, .95)
CIr(-.28, 87, .95)
CIr(-.09, 87, .95)
CIr(-.03, 87, .95)
CIr(-.20, 87, .95)


#z test if correlations are significantly different
paired.r(.33, .30, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.38, -.23, yz=NULL, 204, 87,twotailed=TRUE)

paired.r(.08, .12, yz=NULL, 204, 87,twotailed=TRUE)

paired.r(-.04, .03, yz=NULL, 204, 87,twotailed=TRUE)

paired.r(-.28, -.25, yz=NULL, 204, 87,twotailed=TRUE)

paired.r(-.33, -.24, yz=NULL, 204, 87,twotailed=TRUE)

paired.r(-.21, -.30, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.00, -.05, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.12, .12, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.07, -.11, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.20, .24, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.11, -.25, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.24, -.28, yz=NULL, 204, 87,twotailed=TRUE)

paired.r(.18, -.20, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.25, -.09, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.12, -.03, yz=NULL, 204, 87,twotailed=TRUE)

#######################
###########Factor 2
#######################


#Age
cor.test(women$Faktor2, women$Alter, na.rm = TRUE)
#Religiosity
cor.test(women$Faktor2, women$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(women$Faktor2, women$Politik, na.rm = TRUE)
#years of education
cor.test(women$Faktor2, women$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(women$Faktor2, women$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(women$Faktor2, women$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(women$Faktor2, women$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(women$Faktor2, women$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(women$Faktor2, women$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(women$Faktor2, women$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(women$Faktor2, women$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(women$Faktor2, women$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(women$Faktor2, women$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(women$Faktor2, women$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(women$Faktor2, women$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(women$Faktor2, women$EinflussCorona, na.rm = TRUE)

#Age
cor.test(men$Faktor2, men$Alter, na.rm = TRUE)
#Religiosity
cor.test(men$Faktor2, men$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(men$Faktor2, men$Politik, na.rm = TRUE)
#years of education
cor.test(men$Faktor2, men$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(men$Faktor2, men$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(men$Faktor2, men$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(men$Faktor2, men$MeanGewissenhaftigkeit, na.rm = TRUE)
#openness
cor.test(men$Faktor2, men$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(men$Faktor2, men$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(men$Faktor2, men$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(men$Faktor2, men$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(men$Faktor2, men$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(men$Faktor2, men$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(men$Faktor2, men$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(men$Faktor2, men$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(men$Faktor2, men$EinflussCorona, na.rm = TRUE)


#nothing's changed, z and cis and r are the same



###############################
####################Factor 3
##############################


#Age
cor.test(women$Faktor3, women$Alter, na.rm = TRUE)
#Religiosity
cor.test(women$Faktor3, women$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(women$Faktor3, women$Politik, na.rm = TRUE)
#years of education
cor.test(women$Faktor3, women$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(women$Faktor3, women$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(women$Faktor3, women$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(women$Faktor3, women$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(women$Faktor3, women$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(women$Faktor3, women$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(women$Faktor3, women$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(women$Faktor3, women$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(women$Faktor3, women$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(women$Faktor3, women$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(women$Faktor3, women$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(women$Faktor3, women$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(women$Faktor3, women$EinflussCorona, na.rm = TRUE)

#Age
cor.test(men$Faktor3, men$Alter, na.rm = TRUE)
#Religiosity
cor.test(men$Faktor3, men$Religioesitaet, na.rm = TRUE)
#Politics
cor.test(men$Faktor3, men$Politik, na.rm = TRUE)
#years of education
cor.test(men$Faktor3, men$Bildungsjahre, na.rm = TRUE)
#Extraversion
cor.test(men$Faktor3, men$MeanExtraversion, na.rm = TRUE)
#Agreeableness
cor.test(men$Faktor3, men$MeanVertraeglichkeit, na.rm = TRUE)
#Conscientiousness
cor.test(men$Faktor3, men$MeanGewissenhaftigkeit, na.rm = TRUE)
#Openness
cor.test(men$Faktor3, men$MeanOffenheit, na.rm = TRUE)
#Neuroticism
cor.test(men$Faktor3, men$MeanNeurotizismus, na.rm = TRUE)
#Admiration
cor.test(men$Faktor3, men$MeanAdmiration, na.rm = TRUE)
#Rivalry
cor.test(men$Faktor3, men$MeanRivalry, na.rm = TRUE)
#selfesteem
cor.test(men$Faktor3, men$MeanSelfesteem, na.rm = TRUE)
#genderroles
cor.test(men$Faktor3, men$mean_genderroles_LFAIS, na.rm = TRUE)
#goalsoffeminism
cor.test(men$Faktor3, men$mean_goalsoffeminism, na.rm = TRUE)
#attitude tow. abortion
cor.test(men$Faktor3, men$Schwangerschaftsabbruch, na.rm = TRUE)
#covid.19
cor.test(men$Faktor3, men$EinflussCorona, na.rm = TRUE)

##CIs
CIr(.14, 204, .95)
CIr(-.28, 204, .95)
CIr(.11, 204, .95)
CIr(-.02, 204, .95)
CIr(-.11, 204, .95)
CIr(-.04, 204, .95)
CIr(.16, 204, .95)
CIr(.08, 204, .95)
CIr(-.14, 204, .95)
CIr(-.03, 204, .95)
CIr(.42, 204, .95)
CIr(.27, 204, .95)
CIr(.17, 204, .95)
CIr(-.13, 204, .95)

CIr(.04, 87, .95)
CIr(-.12, 87, .95)
CIr(.01, 87, .95)
CIr(.25, 87, .95)
CIr(-.01, 87, .95)
CIr(-.05, 87, .95)
CIr(-.07, 87, .95)
CIr(.12, 87, .95)
CIr(-.08, 87, .95)
CIr(-.13, 87, .95)
CIr(-.22, 87, .95)
CIr(.03, 87, .95)
CIr(.28, 87, .95)
CIr(.30, 87, .95)
CIr(.17, 87, .95)
CIr(-.14, 87, .95)

#z test if correlations are signfificantly different
paired.r(.14, .04, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.28, -.12, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.14, .01, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.11, .25, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.02, -.01, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.11, -.05, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.04, -.07, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.16, .12, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.08, -.08, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.04, -.13, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.14, -.22, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.03, .03, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.42, .28, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.27, .30, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(.17, .17, yz=NULL, 204, 87,twotailed=TRUE)
paired.r(-.13, -.14, yz=NULL, 204, 87,twotailed=TRUE)


###############################################
### 8.1.4 Regression for the three factor solution of the items (repetition, as items in the global scale stayed the same)
###############################################

##dataset for regression
itemsregressionfas <- itemscorfa3[,c(14,15,16,17)]
itemsthreefactormeans <- itemsthreefactorsolution[,c(9,10,11,12,23)]
regression2 <- cbind (itemsregressionfas, itemsthreefactormeans)


####Model3

Modell3 <- lm(GlobalNatal ~ liberalism + positive_affect_achieve + selfpromotion + Agreeableness, data = regression2)

summary(Modell3)
lm.beta(Modell3)

apa.reg.table(Modell3, filename = "Table11_APA.doc", table.number = 11)



#comarision with Model corona 2including influence of covid.19 on responses: does anything change?


corona2<-lm(GlobalNatal ~ EinflussCorona + liberalism + positive_affect_achieve + selfpromotion + Agreeableness, data = regression2)

summary(corona2)
lm.beta(corona2)
apa.reg.table(corona2, filename = "Table12_APA.doc", table.number = 12)


AIC(Modell1)
AIC(Modellcorona1)

#as expected nothing changed (as global scale contains the same items)





############################
#### 8.2 Regression Results 2 Factor solution of correlates, predicting global natalism scale
###########################



##Repeat explorator factor analysis
names(itemsregmodel)

#plain dataset of correlates 
itemscorfa <- itemsregmodel [,-c(9,10,11,15,16,17,20,21)]
names(itemscorfa)

#take influence of covid-19 away
itemscorfa2 <- itemscorfa [,-c(14)]

###check KMO
KMO(itemscorfa2)




##check Bartlett
cortest.bartlett(itemscorfa2)
##Ist signfifikant


## repeat: number of factors to be exluded
fa.parallel(itemscorfa2, fm="ML", fa="fa", 
            main = "Parallel Analysis Scree Plots",
            n.iter=50)
##looking here at 2 factor solution

###FA oblique rotation, 2 factors, ML
fa.cortwo <- fa(itemscorfa2, nfactors = 2, rotate = "oblimin", fm = "ML")
fa.sort(fa.cortwo)
print.psych(fa.cortwo, cut=0.3, sort=T)


#Saving the factor scores
colnames(fa.cortwo$scores) <-  c('liberalism', 'positive_affect_achieve')
cortwo <- cbind(itemscorfa2, fa.cortwo$scores)

# descriptives of factor scores 
psych::describe(fa.cortwo$scores)



##dataset for regression
correlatestwo <- cortwo[,c(14,15)]
itemsregressionmeans <- itemsregmodel[,c(9,10,11,22)]
regressiontwofactor <- cbind (correlatestwo, itemsregressionmeans)


####Model 1

correlatesmodel2 <- lm(MittelwertNatalismus ~ liberalism + positive_affect_achieve, data = regressiontwofactor)

summary(correlatesmodel2)
lm.beta(correlatesmodel2)

apa.reg.table(correlatesmodel2, filename = "Table11_APA.doc", table.number = 11)



#comparision with model including the influence of Covid-19

##
correlatesmodel3 <- lm(MittelwertNatalismus ~ liberalism + positive_affect_achieve + EinflussCorona, data = regressiontwofactor)

summary(correlatesmodel3)
lm.beta(correlatesmodel3)
apa.reg.table(correlatesmodel3, filename = "Table12_APA.doc", table.number = 12)
 

AIC(correlatesmodel2)
AIC(correlatesmodel3)

#F-test / Anova

anova(correlatesmodel2, correlatesmodel3)



####
#9. save workspace
####
save.image("C:/Users/chiara/Documents/Uni/BA/r_dateien/workspace_natalism.Rdata")



