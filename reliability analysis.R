library(psych)
library(tidyverse)
library(MetBrewer)


#Reliability analysis

#ordinal alpha for neuroticism
ord_alpha_neuroticism<-polychoric(BFIS_imp[,c(1:3)])
ord_alpha_neuroticism<-alpha(ord_alpha_neuroticism$rho)
ord_alpha_neuroticism<-ord_alpha_neuroticism$total$raw_alpha

#ordinal alpha for extraversion
ord_alpha_extraversion<-polychoric(BFIS_imp[,c(4:6)])
ord_alpha_extraversion<-alpha(ord_alpha_extraversion$rho)
ord_alpha_extraversion<-ord_alpha_extraversion$total$raw_alpha

#ordinal alpha for openness
ord_alpha_openness<-polychoric(BFIS_imp[,c(7:9)])
ord_alpha_openness<-alpha(ord_alpha_openness$rho)
ord_alpha_openness<-ord_alpha_openness$total$raw_alpha

#ordinal alpha for agreeableness
ord_alpha_agreeableness<-polychoric(BFIS_imp[,c(10:12)])
ord_alpha_agreeableness<-alpha(ord_alpha_agreeableness$rho)
ord_alpha_agreeableness<-ord_alpha_agreeableness$total$raw_alpha

#ordinal alpha for conscientiousness
ord_alpha_conscientiousness<-polychoric(BFIS_imp[,c(13:15)])
ord_alpha_conscientiousness<-alpha(ord_alpha_conscientiousness$rho)
ord_alpha_conscientiousness<-ord_alpha_conscientiousness$total$raw_alpha

#ordinal alpha for SLON
ord_alpha_SLON<-polychoric(SLON_imp)
ord_alpha_SLON<-alpha(ord_alpha_SLON$rho)
ord_alpha_SLON<-ord_alpha_SLON$total$raw_alpha

#ordinal alpha for PSS10 coping
ord_alpha_PSS10_coping<-polychoric(PSS10_imp [,c(4,5,7,8)])
ord_alpha_PSS10_coping<-alpha(ord_alpha_PSS10_coping$rho)
ord_alpha_PSS10_coping<-ord_alpha_PSS10_coping$total$raw_alpha

#ordinal alpha for PSS10 stress
ord_alpha_PSS10_stress<-polychoric(PSS10_imp [,c(1:3,6,9:10)])
ord_alpha_PSS10_stress<-alpha(ord_alpha_PSS10_stress$rho)
ord_alpha_PSS10_stress<-ord_alpha_PSS10_stress$total$raw_alpha

#ordinal alpha for SPS10 reliable alliance 
ord_alpha_SPS10_relalli<-polychoric(SPS10_imp [,c("SPS10_reliable_alliance.item_10", "SPS10_reliable_alliance.item_1")])
ord_alpha_SPS10_relalli<-alpha(ord_alpha_SPS10_relalli$rho)
ord_alpha_SPS10_relalli<-ord_alpha_SPS10_relalli$total$raw_alpha

#ordinal alpha for SPS10 social support (guidance, attachment, social integration) 
ord_alpha_SPS10_socialsupp<-polychoric(SPS10_imp [,c("SPS10_guidance.item_5", "SPS10_guidance.item_7", "SPS10_attachement.item_4", "SPS10_attachement.item_8", "SPS10_social_integration.item_2", "SPS10_social_integration.item_3")])
ord_alpha_SPS10_socialsupp<-alpha(ord_alpha_SPS10_socialsupp$rho)
ord_alpha_SPS10_socialsupp<-ord_alpha_SPS10_socialsupp$total$raw_alpha

#ordinal alpha for reassurance of worth
ord_alpha_SPS10_reasworth<-polychoric(SPS10_imp [,c("SPS10_reassurance_ofworth.item_6", "SPS10_reassurance_ofworth.item_9")])
ord_alpha_SPS10_reasworth<-alpha(ord_alpha_SPS10_reasworth$rho)
ord_alpha_SPS10_reasworth<-ord_alpha_SPS10_reasworth$total$raw_alpha

#ordinal alpha for the concern of the effect of the pandemic on oneself and close ones
ord_alpha_coronaconcern_close<-polychoric(as.data.frame(not_previous_validation_imp["Corona_concerns"])[,c(1:3)])
ord_alpha_coronaconcern_close<-alpha(ord_alpha_coronaconcern_close$rho)
ord_alpha_coronaconcern_close<-ord_alpha_coronaconcern_close$total$raw_alpha

#ordinal alpha for the concern of the global effect of the pandemic 
ord_alpha_coronaconcern_global<-polychoric(as.data.frame(not_previous_validation_imp["Corona_concerns"])[,c(4:5)])
ord_alpha_coronaconcern_global<-alpha(ord_alpha_coronaconcern_global$rho)
ord_alpha_coronaconcern_global<-ord_alpha_coronaconcern_global$total$raw_alpha

#ordinal alpha for coping strategy spending time with others and oneself
ord_alpha_explcoping_timeothers<-polychoric(as.data.frame(not_previous_validation_imp["Expl_coping"])[,c(2:5,13:14)])
ord_alpha_explcoping_timeothers<-alpha(ord_alpha_explcoping_timeothers$rho)
ord_alpha_explcoping_timeothers<-ord_alpha_explcoping_timeothers$total$raw_alpha

#ordinal alpha for coping strategy gathering information from the media and government 
ord_alpha_explcoping_information<-polychoric(as.data.frame(not_previous_validation_imp["Expl_coping"])[,c(1,6,9,12)])
ord_alpha_explcoping_information<-alpha(ord_alpha_explcoping_information$rho)
ord_alpha_explcoping_information<-ord_alpha_explcoping_information$total$raw_alpha

#ordinal alpha for coping strategy playing video games 
ord_alpha_explcoping_videogames<-polychoric(as.data.frame(not_previous_validation_imp["Expl_coping"])[,c(7,8)])
ord_alpha_explcoping_videogames<-alpha(ord_alpha_explcoping_videogames$rho)
ord_alpha_explcoping_videogames<-ord_alpha_explcoping_videogames$total$raw_alpha

#ordinal alpha for coping strategy helping others 
ord_alpha_explcoping_helpothers<-polychoric(as.data.frame(not_previous_validation_imp["Expl_coping"])[,c(10,11,15,16)])
ord_alpha_explcoping_helpothers<-alpha(ord_alpha_explcoping_helpothers$rho)
ord_alpha_explcoping_helpothers<-ord_alpha_explcoping_helpothers$total$raw_alpha

#ordinal alpha for contact with media
ord_alpha_explmedia<-polychoric(as.data.frame(not_previous_validation_imp["Expl_media"]))
ord_alpha_explmedia<-alpha(ord_alpha_explmedia$rho)
ord_alpha_explmedia<-ord_alpha_explmedia$total$raw_alpha

#ordinal alpha for compliance with measures to contain the virus 
#no alpha was calcultaed for the other dimension since it consists of only one item
ord_alpha_compliance<-polychoric(as.data.frame(not_previous_validation_imp["compliance"])[,c(1:3,5:6)])
ord_alpha_compliance<-alpha(ord_alpha_compliance$rho)
ord_alpha_compliance<-ord_alpha_compliance$total$raw_alpha

#cronbachs alpha for OECD trust in institutions
alpha_trust_institu<-alpha(as.data.frame(not_previous_validation_imp["OECD_trust"])[,c(3:7)])
alpha_trust_institu<-alpha_trust_institu$total$raw_alpha

#cronbachs alpha for OECD trust in country measures to control the pandemic
alpha_trust_measures<-alpha(as.data.frame(not_previous_validation_imp["OECD_trust"])[,c(8:9)])
alpha_trust_measures<-alpha_trust_measures$total$raw_alpha

#cronbachs alpha for OECD trust in people
alpha_trust_people<-alpha(as.data.frame(not_previous_validation_imp["OECD_trust"])[,c(1:2)])
alpha_trust_people<-alpha_trust_people$total$raw_alpha


#dataframe with cornbachs alphs
alpha<-as.data.frame(t(data_frame(ord_alpha_neuroticism, ord_alpha_extraversion, ord_alpha_openness, ord_alpha_agreeableness, ord_alpha_conscientiousness,
                                  ord_alpha_SLON, ord_alpha_PSS10_coping, ord_alpha_PSS10_stress, ord_alpha_SPS10_relalli, ord_alpha_SPS10_socialsupp, ord_alpha_SPS10_reasworth,
                                  ord_alpha_coronaconcern_close, ord_alpha_coronaconcern_global, ord_alpha_explcoping_timeothers, ord_alpha_explcoping_information,
                                  ord_alpha_explcoping_videogames, ord_alpha_explcoping_helpothers, ord_alpha_explmedia, ord_alpha_compliance, alpha_trust_institu,
                                  alpha_trust_measures, alpha_trust_people)))
alpha$V1<-alpha[order(alpha$V1, decreasing = FALSE),]  
alpha$V1<-round(alpha$V1, 3)
alpha$scales<-c("Neuroticism", "Extraversion", "Openness to experience", "Agreeableness", "Conscientiousness", "SLON", "PSS10 coping", "PSS10 general stress", 
                "SPS10 Relallible alliance", "SPS10 social support", "SPS10 reasurance of worth", "Coronavirus concerns Close ones", "Coronavirus global concerns", 
                "Coping time spent with others", "Coping gathering information", "Coping playing videogames", "Coping helping others", "Exposition to information related to the pandemic", 
                "Compliance", "Trust in institutions", "Trust in country measures", "Trust people")

alpha$scales_spanish<-c("Neuroticismo", "Extraversión", "Apertura a la experiencia", "Agradabilidad", "Conciencia", "SLON", "PSS10 afrontamiento", "PSS10 distrés general", 
                "SPS10 Alianza confiable", "SPS10 apoyo social", "SPS10 refuerzo de valía", "Preocupaciones por los seres queridos", "Preocupaciones por el efecto global de la pandemia", 
                "Disfrutar tiempo con otros o en actividades de entretenimiento", "Obtener información del gobierno o los medios de comunicación", "Jugar video juegos ", "Ayudar a otros", 
                "Exposición a información relacionada con la pandemia", "Cumplimiento de las medidas para evitar la propagación del virus", "Confianza en las instituciones", 
                "Confianza en las medidas del gobierno", "Confianza en las personas")

names(alpha)<-c("alpha", "scales", "Escalas")
alpha$scales<-factor(alpha$scales,levels = alpha$scales)
alpha$Escalas<-factor(alpha$Escalas,levels = alpha$Escalas)

hist_plot_alpha<-ggplot(data = alpha, aes(x = reorder(scales, -alpha), y = alpha, fill = scales)) + 
  geom_bar(stat = "identity") +
  labs(x= "Scales", y = "Cronbach's alpha") +
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values=met.brewer("OKeeffe1", n = 22,  type = "continuous")) +
  theme(legend.position = "none") + 
  geom_text(aes(label=alpha, fontface = "bold"), position=position_stack(vjust=0.5))


hist_plot_alpha_spanish<-ggplot(data = alpha, aes(x = reorder(Escalas, -alpha), y = alpha, fill = Escalas)) + 
  geom_bar(stat = "identity") +
  labs(x= "Escala", y = "Alfa de Cronbach") +
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values=met.brewer("OKeeffe1", n = 22,  type = "continuous")) +
  theme(legend.position = "none") + 
  geom_text(aes(label=alpha, fontface = "bold"), position=position_stack(vjust=0.5))

