library(tidyverse)
library(writexl)

score_corona_concerns_closeones<-rowSums(as.data.frame(not_previous_validation_imp["Corona_concerns"])[,c(1:3)])#score corona concerns close ones
score_corona_concerns_global<-rowSums(as.data.frame(not_previous_validation_imp["Corona_concerns"])[,c(4:5)])#score corona concerns global

score_OECD_trust_people<-rowSums(as.data.frame(not_previous_validation_imp["OECD_trust"])[,c(1:2)])#score trust in people
score_OECD_trust_institutions<-rowSums(as.data.frame(not_previous_validation_imp["OECD_trust"])[,c(3:7)])#score trust in institutions
score_OECD_trust_countrymeasures<-rowSums(as.data.frame(not_previous_validation_imp["OECD_trust"])[,c(8:9)])#score trust in country measures

score_SLON<-rowSums(SLON_imp)#score SLON

score_PSS10_coping<-rowSums(PSS10_imp[,c(4,5,7,8)])#score coping strategies with stress from PSS10
score_PSS10_stress<-rowSums(PSS10_imp[,c(1:3,6,9:10)])#score stress from PSS10

#all data from scales into dataframe
scales_scores<-data.frame(corona_concerns_closeones = score_corona_concerns_closeones,
                          corona_concerns_global = score_corona_concerns_global,
                          OECD_trust_people = score_OECD_trust_people,
                          OECD_trust_institutions = score_OECD_trust_institutions,
                          OECD_trust_countrymeasures = score_OECD_trust_countrymeasures,
                          SLON = score_SLON,
                          PSS10_coping = score_PSS10_coping,
                          PSS10_stress = score_PSS10_stress)

scales_scores<- scales_scores %>% 
  mutate(PSS10_stress_dico = ifelse(PSS10_stress >= 15, "high stress", "normal stress"))#new variable dichotomous stress level

scales_scores$PSS10_stress_dico<-factor(scales_scores$PSS10_stress_dico, 
                                        levels = c("normal stress", "high stress"), 
                                        ordered = F)#dichotomous stress level as factor


#all variables into a single dataframe
modelling_phase_data<-cbind.data.frame(Col_dem_data_imp, scales_scores)
save(modelling_phase_data, file = "modelling_phase_data.rda")
write_xlsx(modelling_phase_data, "modelling_phase_data.xlsx")


#corrplot
corplot_df<-modelling_phase_data %>%
  select(corona_concerns_closeones, corona_concerns_global, OECD_trust_people, OECD_trust_institutions,
         OECD_trust_countrymeasures, SLON, PSS10_coping, PSS10_stress)

names(corplot_df)<-c(
  "PSQ",
  "PEGP",
  "CP",
  "CI",
  "CMG",
  "SLON",
  "PSS10A",
  "PSS10D"
)
library(GGally)
ggcorr(corplot_df, label = TRUE)
