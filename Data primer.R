library(tidyverse)
library(mice)

#Data set loading
setwd("D:/My_data/U/Andina/Epi/tesis/Data analysis")
if(!file.exists("COVIDiSTRESS_30May2020.csv")) {
  download.file("https://osf.io/cjxua/download", destfile = "COVIDiSTRESS_30May2020.csv")
} #downloads the latest dataframe from may 30th

World_data<-as.data.frame(read.csv("COVIDiSTRESS_30May2020.csv"))#loads the downloaded dataframe

#extraction of colombian Data  
Col_data<- filter(World_data, Country == "Colombia")

#deletion of survey filling data
Col_data<-Col_data[,-c(1:5, 11, 13)] #duration, answer all, date, individual, language, country and city

#inspection for people living outside colombia and deletion 
Col_data<-Col_data[-c(7,9,28,35,69,98), ]

#deletion of variables not applicable for the analysis
Col_data<-Col_data[, -c(49:64)] #deletion of Bosnia's specific items

#deletion of exploratory distress and exploratory coping and final open text
Col_data<-Col_data[, -c(88, 115, 122)]

#deletion of composite scores
Col_data<-Col_data[, -c(120:128)]

#deletion of asian disease problem
Col_data<-Col_data[, -c(13:15)]

#deletion of mom education level
Col_data<-Col_data[, -4]

#checking for missing values in the dependent variable (PSS-10)
PSS10_NAS<-(as.data.frame(cbind(Col_data$Scale_PSS10_UCLA_1, Col_data$Scale_PSS10_UCLA_2, Col_data$Scale_PSS10_UCLA_3,
                                Col_data$Scale_PSS10_UCLA_4, Col_data$Scale_PSS10_UCLA_5, Col_data$Scale_PSS10_UCLA_6,
                                Col_data$Scale_PSS10_UCLA_7, Col_data$Scale_PSS10_UCLA_8, Col_data$Scale_PSS10_UCLA_9,
                                Col_data$Scale_PSS10_UCLA_10))) 
PSS10_NAS<-as.data.frame(!is.na(PSS10_NAS))
PSS10_NAS$totalNAS<-rowSums(PSS10_NAS)

#removing rows for participants that answered less than 90% of the PSS10
Col_data<-Col_data[-which(PSS10_NAS$totalNAS<9), ]

#missing data
missing_data<-map(Col_data, ~mean(is.na(.))*100) %>%
  as.data.frame() %>%
  t () %>%
  as.data.frame() %>%
  mutate(Percentage = round(V1, 0),
         Variable = names(Col_data)) %>%
  select(Variable, Percentage) %>%
  filter(Percentage != 0) %>%
  ggplot(aes(x = reorder(Variable, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#56B4E9") + 
  theme_classic() +
  xlab("Variables") +
  ylab("Percentage of missing values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
missing_data


#Extraction of demographic data
Col_dem_data<-Col_data[, c(1:11)]


#categorical and ordinal variables
Col_dem_data$Dem_gender<-factor(Col_dem_data$Dem_gender, 
                                levels = c("Female", "Male", "Other/would rather not say"), 
                                ordered = F)

Col_dem_data$Dem_edu<-factor(Col_dem_data$Dem_edu, levels = c("Up to 6 years of school", "Up to 9 years of school",
                                                              "Up to 12 years of school", "Some College, short continuing education or equivalent",
                                                              "College degree, bachelor, master", "PhD/Doctorate"), 
                             ordered = T)

Col_dem_data$Dem_employment<-factor(Col_dem_data$Dem_employment, 
                                    levels = c("Student", "Self-employed", "Retired", 
                                               "Not employed", "Part time employed", "Full time employed"), 
                                    ordered = F)

Col_dem_data$Dem_Expat<-factor(Col_dem_data$Dem_Expat, 
                               levels = c("no", "yes"), 
                               ordered = F)

Col_dem_data$Dem_riskgroup<-factor(Col_dem_data$Dem_riskgroup, 
                                   levels = c("No", "Yes", "Not sure"), 
                                   ordered = F)

Col_dem_data$Dem_isolation<-factor(Col_dem_data$Dem_isolation, 
                                   levels = c("Isolated", "Isolated in medical facility of similar location", 
                                              "Life carries on as usual", "Life carries on with minor changes"), 
                                   ordered = F)

Col_dem_data$Dem_maritalstatus<-factor(Col_dem_data$Dem_maritalstatus,
                                       levels = c("Divorced/widowed", "Married/cohabiting", "Other or would rather not say", "Single"),
                                       ordered = F)



#Imputation 
Col_dem_data$Dem_isolation_adults[Col_dem_data$Dem_isolation_adults == 104]<-NA #removes the value of 104 (extreme value) for the variable

Col_dem_data_imp<-mice(Col_dem_data, method = "rf", m = 100, maxit = 1,
                       seed = 1, print = T)#imputation using random forest

Col_dem_data_imp<-complete(Col_dem_data_imp)

Col_dem_data_imp[,c(1,7,10,11)]<-sapply(Col_dem_data_imp[,c(1,7,10,11)], as.numeric)

#extraction of data from each psychometric instrument

#perceived stress scale
PSS10<-as.data.frame(cbind(Col_data$Scale_PSS10_UCLA_1, Col_data$Scale_PSS10_UCLA_2, Col_data$Scale_PSS10_UCLA_3,
                           Col_data$Scale_PSS10_UCLA_4, Col_data$Scale_PSS10_UCLA_5, Col_data$Scale_PSS10_UCLA_6,
                           Col_data$Scale_PSS10_UCLA_7, Col_data$Scale_PSS10_UCLA_8, Col_data$Scale_PSS10_UCLA_9,
                           Col_data$Scale_PSS10_UCLA_10))
names(PSS10)<-c("item_1", "item_2", "item_3", "item_4", "item_5", "item_6", "item_7", "item_8", "item_9","item_10")

#perceived loneliness scale
SLON<-as.data.frame(cbind(Col_data$Scale_SLON_1, Col_data$Scale_SLON_2, Col_data$Scale_SLON_3))
names(SLON)<-c("item_1", "item_2", "item_3")

#OECD trust
OECD_people<-as.data.frame(cbind(Col_data$OECD_people_1, Col_data$OECD_people_2))
names(OECD_people)<-c("OECD_people_item_1", "OECD_people_item_2")

OECD_institutions<-as.data.frame(cbind(Col_data$OECD_insititutions_1, Col_data$OECD_insititutions_2, Col_data$OECD_insititutions_3,
                                       Col_data$OECD_insititutions_4, Col_data$OECD_insititutions_5, Col_data$OECD_insititutions_6))
names(OECD_institutions)<-c("OECD_institutions_item_1", "OECD_institutions_item_2", "OECD_institutions_item_3", 
                            "OECD_institutions_item_4", "OECD_institutions_item_5", "OECD_institutions_item_6")

OECD_country_trust_measures<-as.data.frame(Col_data$Trust_countrymeasure)
names(OECD_country_trust_measures)<-c("OECD_country_trust_measures_item1")

OECD_trust<-cbind.data.frame(OECD_people, OECD_institutions, OECD_country_trust_measures)


#corona concerns
Corona_concerns<-as.data.frame(cbind(Col_data$Corona_concerns_1, Col_data$Corona_concerns_2, Col_data$Corona_concerns_3,
                                     Col_data$Corona_concerns_4, Col_data$Corona_concerns_5))
names(Corona_concerns)<-c("item_1", "item_2", "item_3", "item_4", "item_5")

#compliance with prevention measures
compliance<-as.data.frame(cbind(Col_data$Compliance_1, Col_data$Compliance_2, Col_data$Compliance_3,
                                Col_data$Compliance_4, Col_data$Compliance_5, Col_data$Compliance_6))
names(compliance)<-c("item_1", "item_2", "item_3", "item_4", "item_5", "item_6")

#BFI-S big five personality test
BFIS_neuroticism<-as.data.frame(cbind(Col_data$BFF_15_1, Col_data$BFF_15_2, Col_data$BFF_15_3))
names(BFIS_neuroticism)<-c("item_1", "item_2", "item_3")

BFIS_extraversion<-as.data.frame(cbind(Col_data$BFF_15_4, Col_data$BFF_15_5, Col_data$BFF_15_6))
names(BFIS_extraversion)<-c("item_4", "item_5", "item_6")

BFIS_openness<-as.data.frame(cbind(Col_data$BFF_15_7, Col_data$BFF_15_8, Col_data$BFF_15_9))
names(BFIS_openness)<-c("item_7", "item_8", "item_9")

BFIS_agreeableness<-as.data.frame(cbind(Col_data$BFF_15_10, Col_data$BFF_15_11, Col_data$BFF_15_12))
names(BFIS_agreeableness)<-c("item_10", "item_11", "item_12")

BFIS_conscientiousness<-as.data.frame(cbind(Col_data$BFF_15_13, Col_data$BFF_15_14, Col_data$BFF_15_15))
names(BFIS_conscientiousness)<-c("item_13", "item_14", "item_15")

#social provision scale 
SPS10_reliable_alliance<-as.data.frame(cbind(Col_data$SPS_1, Col_data$SPS_10))
names(SPS10_reliable_alliance)<-c("item_1", "item_10")

SPS10_social_integration<-as.data.frame(cbind(Col_data$SPS_2, Col_data$SPS_3))
names(SPS10_social_integration)<-c("item_2", "item_3")

SPS10_attachement<-as.data.frame(cbind(Col_data$SPS_4, Col_data$SPS_8))
names(SPS10_attachement)<-c("item_4", "item_8")

SPS10_reassurance_ofworth<-as.data.frame(cbind(Col_data$SPS_6, Col_data$SPS_9))
names(SPS10_reassurance_ofworth)<-c("item_6", "item_9")

SPS10_guidance<-as.data.frame(cbind(Col_data$SPS_5, Col_data$SPS_7))
names(SPS10_guidance)<-c("item_5", "item_7")

#exploratory coping
Expl_coping<-as.data.frame(cbind(Col_data$Expl_Coping_1, Col_data$Expl_Coping_2, Col_data$Expl_Coping_3, Col_data$Expl_Coping_4,
                                 Col_data$Expl_Coping_5, Col_data$Expl_Coping_6, Col_data$Expl_Coping_7, Col_data$Expl_Coping_8,
                                 Col_data$Expl_Coping_9, Col_data$Expl_Coping_10, Col_data$Expl_Coping_11, Col_data$Expl_Coping_12,
                                 Col_data$Expl_Coping_13, Col_data$Expl_Coping_14, Col_data$Expl_Coping_15, Col_data$Expl_Coping_16))
names(Expl_coping)<-c("item_1", "item_2", "item_3", "item_4", "item_5", "item_6", "item_7", "item_8", "item_9",
                      "item_10", "item_11", "item_12", "item_13", "item_14", "item_15", "item_16")

#exploratory media
Expl_media<-as.data.frame(cbind(Col_data$Expl_media_1, Col_data$Expl_media_2, Col_data$Expl_media_3, Col_data$Expl_media_4, 
                                Col_data$Expl_media_5, Col_data$Expl_media_6))
names(Expl_media)<-c("item_1", "item_2", "item_3", "item_4", "item_5", "item_6")

# grouping psychometric tests based on their previous validation status
not_previous_validation<-list(Corona_concerns, Expl_coping, Expl_media, compliance, OECD_trust)
names(not_previous_validation)<-c("Corona_concerns", "Expl_coping", "Expl_media", "compliance", "OECD_trust")

not_previous_validation<-rapply(not_previous_validation, as.integer, how = "replace")#convert items response to integers class


validated<-list(BFIS_neuroticism, BFIS_extraversion, BFIS_openness, BFIS_agreeableness, BFIS_conscientiousness,  
                PSS10, SLON, SPS10_attachement, SPS10_guidance, SPS10_reassurance_ofworth, SPS10_reliable_alliance, 
                SPS10_social_integration)

names(validated)<-c("BFIS_neuroticism", "BFIS_extraversion", "BFIS_openness", "BFIS_agreeableness", "BFIS_conscientiousness",  
                    "PSS10", "SLON", "SPS10_attachement", "SPS10_guidance", "SPS10_reassurance_ofworth", "SPS10_reliable_alliance", 
                    "SPS10_social_integration")

validated<-rapply(validated, as.integer, how = "replace")#convert items response to integers class


#multiple imputation with random forest

not_previous_validation_imp<-lapply(not_previous_validation, mice,  method = "rf", m = 100, maxit = 1,
                                    seed = 1, print = T) #multiple imputation with random forest

not_previous_validation_imp<-lapply(not_previous_validation_imp, complete) #imputed dataframe

not_previous_validation_imp<-rapply(not_previous_validation_imp, as.integer, how = "replace")#convert items response to integers class

validated_imp<-lapply(validated, mice,  method = "rf", m = 100, maxit = 1,
                      seed = 1, print = T) #multiple imputation with random forest

validated_imp<-lapply(validated_imp, complete) #imputed dataframe

validated_imp<-rapply(validated_imp, as.integer, how = "replace")#convert items response to integers class

# individual validated scales
BFIS_imp<-cbind.data.frame(validated_imp[1:5])
PSS10_imp<-as.data.frame(validated_imp[6])
SLON_imp<-as.data.frame(validated_imp[7])
SPS10_imp<-cbind.data.frame(validated_imp[8:12])

validated_imp<-list(BFIS_imp, PSS10_imp, SLON_imp, SPS10_imp)


#recoding of items which are inverted for the different test
##the following function returns the inverted score for the items that coded inversely 
##the item argument is the item that wants to be re coded while the scale argument is the range of values
##the likert scale can take
inverse_item_score<-function(item, scale) {
  mean(scale) - item + mean(scale)
}

PSS10_imp[,c(4,5,7,8)]<-inverse_item_score(item = PSS10_imp[,c(4,5,7,8)], 
                                           scale = c(1,2,3,4,5))

BFIS_imp$BFIS_neuroticism.item_3<-inverse_item_score(item =BFIS_imp$BFIS_neuroticism.item_3, 
                                                     scale= c(1,2,3,4,5,6))

BFIS_imp$BFIS_extraversion.item_6<-inverse_item_score(item =BFIS_imp$BFIS_extraversion.item_6, 
                                                      scale = c(1,2,3,4,5,6))

BFIS_imp$BFIS_agreeableness.item_10<-inverse_item_score(item =BFIS_imp$BFIS_agreeableness.item_10, 
                                                        scale = c(1,2,3,4,5,6))

BFIS_imp$BFIS_conscientiousness.item_14<-inverse_item_score(item =BFIS_imp$BFIS_conscientiousness.item_14, 
                                                            scale = c(1,2,3,4,5,6))


