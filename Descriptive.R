library(tidyverse)

#normality test
normality_modelling_data<-sapply(modelling_phase_data[,c(1,7,10:19)], shapiro.test)

normality_modelling_data<-as.data.frame(t(normality_modelling_data))[,c(1:2)] %>% #selects p-value and w statistic
  mutate(statistic = str_remove_all(statistic, pattern = "c\\(W \\=|\\)"), #removes strings
         p.value = format(p.value, scientific = F)) #removes scientific notation

normality_modelling_data<-as.data.frame(sapply(normality_modelling_data, as.numeric))#data as numeric

normality_modelling_data<-as.data.frame(sapply(normality_modelling_data, round, 3))#rounding values with 3 digits
  
normality_modelling_data$variable<-names(modelling_phase_data[,c(1,7,10:19)])

normality_modelling_data<-normality_modelling_data[,c(3,1,2)]


#descriptive statistics for categorical data

count_categorical<-list(
  table(modelling_phase_data$Dem_gender, modelling_phase_data$PSS10_stress_dico),
  table(modelling_phase_data$Dem_edu, modelling_phase_data$PSS10_stress_dico),
  table(modelling_phase_data$Dem_employment, modelling_phase_data$PSS10_stress_dico),
  table(modelling_phase_data$Dem_Expat, modelling_phase_data$PSS10_stress_dico),
  table(modelling_phase_data$Dem_maritalstatus, modelling_phase_data$PSS10_stress_dico),
  table(modelling_phase_data$Dem_riskgroup, modelling_phase_data$PSS10_stress_dico),
  table(modelling_phase_data$Dem_isolation, modelling_phase_data$PSS10_stress_dico)
  )
names(count_categorical)<-names(modelling_phase_data[c(2:6,8:9)])

percent_categorial<-lapply(count_categorical, prop.table, margin = 1)
percent_categorial<-lapply(percent_categorial, as.data.frame)
percent_categorial<-bind_rows(percent_categorial, .id = "row_label")
percent_categorial<-pivot_wider(percent_categorial, names_from = Var2, values_from = Freq)
percent_categorial$`high stress`<-round(percent_categorial$`high stress`*100, 3)
percent_categorial$`normal stress`<-round(percent_categorial$`normal stress`*100, 3)
names(percent_categorial)<-c("row_label","Var1","normal stress pct","high stress pct")


count_categorical<-lapply(count_categorical, as.data.frame)
count_categorical<-bind_rows(count_categorical, .id = "row_label")
count_categorical<-pivot_wider(count_categorical, names_from = Var2, values_from = Freq)
names(count_categorical)<-c("row_label","Var1","normal stress count","high stress count")

#descriptive statistics for numerical data

numerical_mean<-modelling_phase_data %>%
  group_by(PSS10_stress_dico) %>%
  summarise_at(
    vars(-c("Dem_gender", "Dem_edu", "Dem_employment", "Dem_Expat","Dem_maritalstatus", "Dem_riskgroup", "Dem_isolation")),
    mean
    ) %>%
  t %>%
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1)
names(numerical_mean)<-c("normal stress mean","high stress mean")

numerical_sd<-modelling_phase_data %>%
  group_by(PSS10_stress_dico) %>%
  summarise_at(
    vars(-c("Dem_gender", "Dem_edu", "Dem_employment", "Dem_Expat","Dem_maritalstatus", "Dem_riskgroup", "Dem_isolation")),
    sd
  ) %>%
  t %>%
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1)
names(numerical_sd)<-c("normal stress sd","high stress sd")

#descriptive table
descriptive_numerical<-cbind.data.frame(numerical_mean, numerical_sd) 
numerical_var_names<-rownames(descriptive_numerical)
descriptive_numerical<-sapply(descriptive_numerical, as.numeric)
descriptive_numerical<-as.data.frame(descriptive_numerical)

descriptive_numerical<- descriptive_numerical %>%
  mutate(`high stress mean` = round(`high stress mean`, 2),
         `high stress sd` = round(`high stress sd`, 2),
         `normal stress mean` = round(`normal stress mean`, 2),
         `normal stress sd` = round(`normal stress sd`, 2),
         high_stress = paste0(`high stress mean`, " (", `high stress sd`,")"),
         normal_stress = paste0(`normal stress mean`, " (", `normal stress sd`,")"),
         Variable = numerical_var_names
    ) %>%
  select(Variable, normal_stress, high_stress)


descriptive_categorical<-cbind.data.frame(percent_categorial, count_categorical)
descriptive_var_names<-rownames(descriptive_categorical)
descriptive_categorical<-as.data.frame(descriptive_categorical)
descriptive_categorical<- descriptive_categorical[,3:8] 

descriptive_categorical<- descriptive_categorical  %>%
  mutate(`high stress pct` = round(`high stress pct`, 2),
         `normal stress pct` = round(`normal stress pct`, 2),
         high_stress = paste0(`high stress count`, " (", `high stress pct`, "%",")"),
         normal_stress = paste0(`normal stress count`, " (", `normal stress pct`, "%",")")
  ) %>%
  select(row_label, Var1, normal_stress, high_stress)

writexl::write_xlsx(descriptive_numerical, "descriptive_numerical.xlsx")
writexl::write_xlsx(descriptive_categorical, "descriptive_categorical.xlsx")
