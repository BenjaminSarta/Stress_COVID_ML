library(psych)
library(parameters)
library(mvnormtest)
library(tidyverse)
library(MetBrewer)

#tests for not validated scales

#multivariate normality test
mvnormtest_Corona_concerns<-mshapiro.test(t(as.data.frame(not_previous_validation_imp["Corona_concerns"])))[2]#selects p value
mvnormtest_Expl_coping<-mshapiro.test(t(as.data.frame(not_previous_validation_imp["Expl_coping"])))[2]#selects p value
mvnormtest_Expl_media<-mshapiro.test(t(as.data.frame(not_previous_validation_imp["Expl_media"])))[2]#selects p value
mvnormtest_compliance<-mshapiro.test(t(as.data.frame(not_previous_validation_imp["compliance"])))[2]#selects p value
mvnormtest_OECD_trust<-mshapiro.test(t(as.data.frame(not_previous_validation_imp["OECD_trust"])))[2]#selects p value

mvnormtest_not_validated<-rbind.data.frame(mvnormtest_Corona_concerns, mvnormtest_Expl_coping, mvnormtest_Expl_media,
                                           mvnormtest_compliance, mvnormtest_OECD_trust)

#KMO for each scale 
KMO_not_validated<-t(as.data.frame(sapply(not_previous_validation_imp[c(1:5)], KMO)[1,]))

#Bartlett test of sphericity for each scale
Bartlett_not_validated<-as.data.frame(t(sapply(not_previous_validation_imp[c(1:5)], psych::cortest.bartlett)))
Bartlett_not_validated<-t(as.data.frame(Bartlett_not_validated$p.value))#selects p value


#Parallel analysis 
set.seed(1)
parallel_analysis_poly_notval<-sapply(not_previous_validation_imp[c(1:4)], 
                                      psych::fa.parallel, 
                                      plot = F, 
                                      fa = "fa", 
                                      cor = "poly")[7,]

parallel_analysis_pearson_OECDtrust<-psych::fa.parallel(as.data.frame(not_previous_validation_imp["OECD_trust"]),
                                      plot = F, 
                                      fa = "fa")[7]

parallel_analysis_notval<-t(cbind.data.frame(parallel_analysis_poly_notval, parallel_analysis_pearson_OECDtrust))
rownames(parallel_analysis_notval)<-c("Corona_concerns", "Expl_coping", "Expl_media", "compliance", "OECD_trust")

#statistics anlysis for EFA
assumptions_for_EFA_not_val<-cbind.data.frame(mvnormtest_not_validated, KMO_not_validated, 
                                              Bartlett_not_validated, parallel_analysis_notval)

assumptions_for_EFA_not_val<-sapply(assumptions_for_EFA_not_val, as.numeric)#all data as numeric
assumptions_for_EFA_not_val<-as.data.frame(assumptions_for_EFA_not_val)

names(assumptions_for_EFA_not_val)<-c("mutivariate normality (Shapiro-Wilk)", "KMO", 
                                      "Bartlett test", "number of factors (Parallel analysis)")

rownames(assumptions_for_EFA_not_val)<-c("Corona_concerns", "Expl_coping", "Expl_media", "compliance", "OECD_trust")

#EFA for each scale
EFA_Corona_concerns<-fa(as.data.frame(not_previous_validation_imp["Corona_concerns"]), 
                        nfactors = parallel_analysis_notval["Corona_concerns", ], 
                        rotate = "varimax",
                        fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for Corona_concerns scale

EFA_Expl_coping<-fa(as.data.frame(not_previous_validation_imp["Expl_coping"]), 
                    nfactors = parallel_analysis_notval["Expl_coping", ], 
                    rotate = "varimax",
                    fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for Expl_coping scale

EFA_Expl_media<-fa(as.data.frame(not_previous_validation_imp["Expl_media"]), 
                   nfactors = parallel_analysis_notval["Expl_media", ], 
                   rotate = "varimax",
                   fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for Expl_media scale

EFA_compliance<-fa(as.data.frame(not_previous_validation_imp["compliance"]), 
                   nfactors = parallel_analysis_notval["compliance", ], 
                   rotate = "varimax",
                   fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for compliance scale

EFA_OECD_trust<-fa(as.data.frame(not_previous_validation_imp["OECD_trust"]), 
                   nfactors = parallel_analysis_notval["OECD_trust", ], 
                   rotate = "varimax",
                   fm = "ml") #exploratory factor analysis (EFA) for OECD_trust scale

EFA_not_previous_validation<-list(EFA_Corona_concerns, EFA_Expl_coping, EFA_Expl_media, EFA_compliance, EFA_OECD_trust)
names(EFA_not_previous_validation)<-c("Corona_concerns","Expl_coping","Expl_media", "compliance")

#factor loadings
factor_loadings_not_previous_validation<-lapply(EFA_not_previous_validation, parameters, sort = TRUE, threshold = "max")
names(factor_loadings_not_previous_validation)<-c("Corona_concerns","Expl_coping","Expl_media", "compliance", "OECD_trust")



#tests for not validated scales


#multivariate normality test
mvnormtest_BFIS_imp<-mshapiro.test(t(BFIS_imp))[2]#selects p value
mvnormtest_PSS10_imp<-mshapiro.test(t(PSS10_imp))[2]#selects p value
mvnormtest_SLON_imp<-mshapiro.test(t(SLON_imp))[2]#selects p value
mvnormtest_SPS10_imp<-mshapiro.test(t(SPS10_imp))[2]#selects p value
mvnormtest_validated<-rbind.data.frame(mvnormtest_BFIS_imp, mvnormtest_PSS10_imp, mvnormtest_SLON_imp, mvnormtest_SPS10_imp)

#KMO for each scale 
KMO_validated<-t(as.data.frame(sapply(validated_imp, KMO)[1,]))

#Bartlett test of sphericity for each scale
Bartlett_validated<-as.data.frame(t(sapply(validated_imp, psych::cortest.bartlett)))
Bartlett_validated<-t(as.data.frame(Bartlett_validated$p.value))#selects p value


#Parallel analysis for validated scales
set.seed(1)
parallel_analysis_poly_val<-sapply(validated_imp, 
                                   psych::fa.parallel, 
                                   plot = F, 
                                   fa = "fa", 
                                   cor = "poly")[7,]

parallel_analysis_poly_val<-rbind.data.frame(parallel_analysis_poly_val)#parallel analysis results into a dataframe

parallel_analysis_poly_val<-sapply(parallel_analysis_poly_val, as.numeric)#all data as numeric


parallel_analysis_poly_val<-as.data.frame(parallel_analysis_poly_val)
rownames(parallel_analysis_poly_val)<-c("BFI", "PSS10", "SLON", "SPS10")

assumptions_for_EFA_val<-cbind.data.frame(mvnormtest_validated, KMO_validated, 
                                          Bartlett_validated, parallel_analysis_poly_val)

assumptions_for_EFA_val<-sapply(assumptions_for_EFA_val, as.numeric)#all data as numeric
assumptions_for_EFA_val<-as.data.frame(assumptions_for_EFA_val)

names(assumptions_for_EFA_val)<-c("mutivariate normality (Shapiro-Wilk)", "KMO", 
                                  "Bartlett test", "number of factors (Parallel analysis)")

rownames(assumptions_for_EFA_val)<-c("BFI", "PSS10", "SLON", "SPS10")

#EFA for each scale
EFA_BFI<-fa(BFIS_imp,
            nfactors = parallel_analysis_poly_val["BFI", ], 
            rotate = "varimax",
            fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for Corona_concerns scale

EFA_PSS10<-fa(PSS10_imp,
              nfactors = parallel_analysis_poly_val["PSS10", ], 
              rotate = "varimax",
              fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for Expl_coping scale

EFA_SLON<-fa(SLON_imp,
             nfactors = parallel_analysis_poly_val["SLON", ], 
             rotate = "varimax",
             fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for Expl_media scale

EFA_SPS10<-fa(SPS10_imp, 
              nfactors = parallel_analysis_poly_val["SPS10", ], 
              rotate = "varimax",
              fm = "ml", cor = "poly") #exploratory factor analysis (EFA) for compliance scale


EFA_previous_validation<-list(EFA_BFI, EFA_PSS10, EFA_SLON, EFA_SPS10)
names(EFA_previous_validation)<-c("BFI", "PSS10", "SLON", "SPS10")

#factor loadings
factor_loadings_previous_validation<-lapply(EFA_previous_validation, parameters, sort = TRUE, threshold = "max")
names(factor_loadings_previous_validation)<-c("BFI", "PSS10", "SLON", "SPS10")


#summary measures assumptions
assumptions_EFA_sum<-bind_rows(assumptions_for_EFA_not_val, assumptions_for_EFA_val)
assumptions_EFA_sum<-format(assumptions_EFA_sum, scientific = F)
assumptions_EFA_sum<-as.data.frame(sapply(assumptions_EFA_sum, as.numeric))
assumptions_EFA_sum<-as.data.frame(sapply(assumptions_EFA_sum, round, digits = 3))
assumptions_EFA_sum<-assumptions_EFA_sum %>% 
  mutate(Scale = c("Corona_concerns", "Expl_coping", "Expl_media", "compliance", "OECD_trust", "BFI", "PSS10", "SLON", "SPS10")) %>%
  select('Scale', 'mutivariate normality (Shapiro-Wilk)', 'KMO', 'Bartlett test', 'number of factors (Parallel analysis)')


#save data from EFA analysis
openxlsx::write.xlsx(assumptions_EFA_sum, "assumptions_EFA_sum.xlsx")
openxlsx::write.xlsx(factor_loadings_not_previous_validation, "psych_sum_not_validated.xlsx")
openxlsx::write.xlsx(factor_loadings_previous_validation, "psych_sum_validated.xlsx")



