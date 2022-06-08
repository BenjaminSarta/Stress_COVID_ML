library(tidymodels)
library(tidyverse)
library(fastDummies)
library(glmnet)
library(tictoc)
library(probably)
library(epiR)
library(runway)
library(ClinicalUtilityRecal)

load("D:/My_data/U/Andina/Epi/tesis/Data analysis/Epi thesis/modelling_phase_data.Rda")

#Data resampling
set.seed(123)

data_dummy<-modelling_phase_data %>% 
  dummy_cols(select_columns = c("Dem_gender", "Dem_edu", "Dem_employment", "Dem_Expat", "Dem_maritalstatus", "Dem_riskgroup", "Dem_isolation"), remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
  select(-PSS10_stress) %>%
  mutate(Dem_gender_Female = ifelse(Dem_gender_Male==1, 0, 1)) %>%
  select(-Dem_gender_Male)

data_dummy$PSS10_stress_dico <- relevel(data_dummy$PSS10_stress_dico, ref = "high stress")

data_split_classifieres<-initial_split(data_dummy, prop = 0.7)
train_classifiers<-training(data_split_classifieres)
test_classifiers<-testing(data_split_classifieres)

data_cv_dummy<-vfold_cv(train_classifiers, v = 10)

logistic_recipe<-recipe(PSS10_stress_dico ~ ., data = train_classifiers) %>%
  step_zv(all_predictors(), -all_outcomes()) 

logistic_spec<-logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

logistic_lambda_grid <- grid_regular(penalty(), mixture(), levels = 50)

doParallel::registerDoParallel()


tic()

set.seed(125)
logistic_grid <- tune_grid(logistic_spec,
                           logistic_recipe,
                           resamples = data_cv_dummy,
                           grid = logistic_lambda_grid)

toc()

logisitc_best_roc_auc <- logistic_grid %>%
  select_best("roc_auc")

parameters_tunning_plot<-logistic_grid %>%
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>%
  mutate(mixture = format(mixture)) %>% 
  ggplot(aes(x = penalty, y = mean, color = mixture))+
  theme_minimal() +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  labs(x = "Penalidad", y = "Área bajo la curva ROC", title = "Hiperparámetros regresión logística")


wf_logistic<-workflow() %>%
  add_recipe(logistic_recipe)

logistic_model<-finalize_workflow(
  wf_logistic %>% add_model(logistic_spec),
  logisitc_best_roc_auc) %>%
  fit(data = train_classifiers)


coefficients_logistic<-logistic_model %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  mutate(estimate = estimate *-1)#since the regression is predicting normal stress the sign of the coefficients needs to be changed to predict highs stress

prediction_class_logistic_train<-predict(logistic_model, train_classifiers, type = "class")

prediction_prob_logistic_train<-predict(logistic_model, train_classifiers, type = "prob")

metrics_logistic_data_train<-train_classifiers %>%
  select(PSS10_stress_dico) %>%
  bind_cols(prediction_class_logistic_train, prediction_prob_logistic_train)
#---------------
#Threshold probability for discriminitaion based on:
#https://cran.r-project.org/web/packages/probably/vignettes/where-to-use.html#:~:text=Regarding%20placement%20in%20the%20modeling,model%20performance%20has%20been%20calculated.

threshold_prob_logistic<-metrics_logistic_data_train %>%
  threshold_perf(PSS10_stress_dico, ".pred_high stress", thresholds = seq(0.20, 1, by = 0.001)) %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2"
  ))

max_j_index_threshold_prob_logistic <- threshold_prob_logistic %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold) %>%
  min()

j_index_plot_logistic<-ggplot(threshold_prob_logistic, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = max_j_index_threshold_prob_logistic, alpha = .6, color = "grey30") +
  labs(
    x = "Alto estrés",
    y = "Metrica",
    title = "Discriminación de la regresión logísitca al variar el umbral de probabilidad",
    subtitle = "línea vertical = Valor máximo  del índice J"
  )
#----------------------------
#data set containing probabilities on the training and testing set.

metrics_logistic_data_train_new_threshold<-metrics_logistic_data_train %>% 
  mutate(`.pred_class` = make_two_class_pred(`.pred_high stress`, 
                                           levels(metrics_logistic_data_train$PSS10_stress_dico),
                                           threshold = max_j_index_threshold_prob_logistic),
         `.pred_class` = factor(.pred_class, levels = levels(metrics_logistic_data_train$PSS10_stress_dico)),
         PSS10_stress_dico = ifelse(PSS10_stress_dico == "high stress", 1, 0),
         .pred_class = ifelse(.pred_class == "high stress", 1, 0)) %>%
  select(-`.pred_normal stress`)

names(metrics_logistic_data_train_new_threshold)<-c("outcomes", "predicted outcomes","predictions")

#calibration model
other_metrics_logisitc_train<-rms::val.prob(metrics_logistic_data_train_new_threshold$predictions, metrics_logistic_data_train_new_threshold$outcomes)
other_metrics_logisitc_train<-as.data.frame(other_metrics_logisitc_train)
other_metrics_logisitc_train$metric<-rownames(other_metrics_logisitc_train)
  
calibrated_prob_logistic<-stdRecal(metrics_logistic_data_train_new_threshold$outcomes, metrics_logistic_data_train_new_threshold$predictions)

metrics_logistic_data_train_new_threshold$`calibrated predictions`<-calibrated_prob_logistic$p.std

#function to predict stress based on calibrated model
pred_logistic<-function(SLON, PSS10_coping) {
  p = exp(-0.241 + (0.0293*SLON) + (0.0390*PSS10_coping))/(1+exp(-0.241 + (0.0293*SLON) + (0.0390*PSS10_coping)))
  exp(-3.13151 + 10.83824 * log(p/(1-p)))/(1+exp(-3.13151 + 10.83824 * log(p/(1-p))))
}

#metrics on test data
metrics_logistic_data_test<-data.frame(outcomes = ifelse(test_classifiers$PSS10_stress_dico == "high stress", 1, 0),
                                       predictions = pred_logistic(SLON = test_classifiers$SLON, PSS10_coping = test_classifiers$PSS10_coping)) %>%
  mutate("predicted outcomes" = ifelse(predictions >= max_j_index_threshold_prob_logistic, 1, 0))


#metrics of discrimination
conf_mat_logistic_results_train<-table(metrics_logistic_data_train_new_threshold$`predicted outcomes`, #columns
                                       metrics_logistic_data_train_new_threshold$outcomes) #rows
conf_mat_logistic_results_train<-conf_mat_logistic_results_train[2:1,2:1]

conf_mat_logistic_results_test<-table(metrics_logistic_data_test$`predicted outcomes`, #columns
                                  metrics_logistic_data_test$outcomes) #rows
conf_mat_logistic_results_test<-conf_mat_logistic_results_test[2:1,2:1]


#discrimination metrics train
discrim_metrics_logistic_train<-epi.tests(conf_mat_logistic_results_train)
sens_logistic_train<-discrim_metrics_logistic_train$detail$se
spec_logistic_train<-discrim_metrics_logistic_train$detail$sp
pvp_logistic_train<-discrim_metrics_logistic_train$detail$pv.pos
pvn_logistic_train<-discrim_metrics_logistic_train$detail$pv.neg
discrim_metrics_logistic_train<-rbind.data.frame(round(sens_logistic_train, 3), 
                                                 round(spec_logistic_train, 3),
                                                 round(pvp_logistic_train, 3), 
                                                 round(pvn_logistic_train, 3)) %>%
  mutate(medida = c("sensibilidad", "especificidad", "valor predictivo positivo","valor predictivo negativo"),
         entrenamiento =  paste0(est, "(IC 95% ", lower,"-", upper, ")")) %>%
  select(medida, entrenamiento)
roc_logistic_train<-pROC::roc(metrics_logistic_data_train_new_threshold$outcomes, metrics_logistic_data_train_new_threshold$`calibrated predictions`)
ci_roc_logistic_train<-pROC::ci.auc(roc_logistic_train)


#discrimination metrics test  
discrim_metrics_logistic_test<-epi.tests(conf_mat_logistic_results_test)
sens_logistic_test<-discrim_metrics_logistic_test$detail$se
spec_logistic_test<-discrim_metrics_logistic_test$detail$sp
pvp_logistic_test<-discrim_metrics_logistic_test$detail$pv.pos
pvn_logistic_test<-discrim_metrics_logistic_test$detail$pv.neg
discrim_metrics_logistic_test<-rbind.data.frame(round(sens_logistic_test, 3), 
                                                round(spec_logistic_test, 3),
                                                round(pvp_logistic_test, 3), 
                                                round(pvn_logistic_test, 3)) %>%
  mutate(medida = c("sensibilidad", "especificidad", "valor predictivo positivo","valor predictivo negativo"),
         prueba =  paste0(est, "(IC 95% ", lower,"-", upper, ")")) %>%
  select(medida, prueba)
roc_logistic_test<-pROC::roc(metrics_logistic_data_test$outcomes, metrics_logistic_data_test$predictions)
ci_roc_logistic_test<-pROC::ci.auc(roc_logistic_test)

#discrimination metrics logistic
discrim_metrics_logistic<-full_join(discrim_metrics_logistic_train, discrim_metrics_logistic_test, by = "medida")

discrim_metrics_logistic<-discrim_metrics_logistic %>% add_case(medida = "ROC", 
                                                                entrenamiento = paste0(round(ci_roc_logistic_train[2],3), "(IC 95% ", round(ci_roc_logistic_train[1],3),"-", round(ci_roc_logistic_train[3],3), ")"),
                                                                prueba = paste0(round(ci_roc_logistic_test[2],3), "(IC 95% ", round(ci_roc_logistic_test[1],3),"-", round(ci_roc_logistic_test[3],3), ")"))

#calibration and general performance metrics training set
other_metrics_logisitc_train_cal<-rms::val.prob(metrics_logistic_data_train_new_threshold$`calibrated predictions`, metrics_logistic_data_train_new_threshold$outcomes)
other_metrics_logisitc_train_cal<-as.data.frame(other_metrics_logisitc_train_cal)
other_metrics_logisitc_train_cal$metric<-rownames(other_metrics_logisitc_train_cal)
other_metrics_logisitc_train_cal<-other_metrics_logisitc_train_cal %>% 
  filter(metric == "Intercept" | metric == "Slope" | metric == "Brier")
names(other_metrics_logisitc_train_cal)<-c("entrenamiento", "medida")


#calibration and general performance metrics testing set
other_metrics_logisitc_test<-rms::val.prob(metrics_logistic_data_test$predictions, metrics_logistic_data_test$outcomes)
other_metrics_logisitc_test<-as.data.frame(other_metrics_logisitc_test)
other_metrics_logisitc_test$metric<-rownames(other_metrics_logisitc_test)
other_metrics_logisitc_test<-other_metrics_logisitc_test %>% 
  filter(metric == "Intercept" | metric == "Slope" | metric == "Brier")
names(other_metrics_logisitc_test)<-c("prueba", "medida")

#calibration metrics logistic
calibration_metrics_logistic<-full_join(other_metrics_logisitc_train_cal, other_metrics_logisitc_test, by = "medida")
calibration_metrics_logistic$entrenamiento<-round(calibration_metrics_logistic$entrenamiento, 3)
calibration_metrics_logistic$prueba<-round(calibration_metrics_logistic$prueba, 3)

#performance logistic
performance_logistic<-rbind.data.frame(discrim_metrics_logistic, calibration_metrics_logistic)

#decision curve analysis
#----------
#function from decisioncurveanalysis.org

dca <- function(data, outcome, predictors, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  # data MUST BE A DATA FRAME
  if (class(data)!="data.frame") {
    stop("Input data must be class data.frame")
  }
  
  #ONLY KEEPING COMPLETE CASES
  data=data[complete.cases(data[append(outcome,predictors)]),append(outcome,predictors)]
  
  # outcome MUST BE CODED AS 0 AND 1
  if (max(data[[outcome]])>1 | min(data[[outcome]])<0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  if (xstart<0 | xstart>1) {
    stop("xstart must lie between 0 and 1")
  }
  
  # xstop IS BETWEEN 0 AND 1
  if (xstop<0 | xstop>1) {
    stop("xstop must lie between 0 and 1")
  }
  
  # xby IS BETWEEN 0 AND 1
  if (xby<=0 | xby>=1) {
    stop("xby must lie between 0 and 1")
  }
  
  # xstart IS BEFORE xstop
  if (xstart>=xstop) {
    stop("xstop must be larger than xstart")
  }
  
  #STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n=length(predictors)
  
  #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
  if (length(probability)>0 & pred.n!=length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  
  #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm)>0 & pred.n!=length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm)==0) {
    harm=rep(0,pred.n)
  }
  if (length(probability)==0) {
    probability=rep(TRUE,pred.n)
  }
  
  
  #CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO, 
  #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  #IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  for(m in 1:pred.n) { 
    if (probability[m]!=TRUE & probability[m]!=FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
      stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))  
    }
    if(probability[m]==FALSE) {
      model=NULL
      pred=NULL
      model=glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family=binomial("logit"))
      pred=data.frame(model$fitted.values)
      pred=data.frame(pred)
      names(pred)=predictors[m]
      data=cbind(data[names(data)!=predictors[m]],pred)
      print(paste(predictors[m],"converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.",sep=" "))
    }
  }
  
  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors=="all" | predictors=="none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }  
  
  #########  CALCULATING NET BENEFIT   #########
  N=dim(data)[1]
  event.rate=colMeans(data[outcome])
  
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb=data.frame(seq(from=xstart, to=xstop, by=xby))
  names(nb)="threshold"
  interv=nb
  
  nb["all"]=event.rate - (1-event.rate)*nb$threshold/(1-nb$threshold)
  nb["none"]=0
  
  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for(m in 1:pred.n){
    for(t in 1:length(nb$threshold)){
      # COUNTING TRUE POSITIVES AT EACH THRESHOLD
      tp=mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome])*sum(data[[predictors[m]]]>=nb$threshold[t])
      # COUNTING FALSE POSITIVES AT EACH THRESHOLD
      fp=(1-mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome]))*sum(data[[predictors[m]]]>=nb$threshold[t])
      #setting TP and FP to 0 if no observations meet threshold prob.
      if (sum(data[[predictors[m]]]>=nb$threshold[t])==0) {
        tp=0
        fp=0
      }
      
      # CALCULATING NET BENEFIT
      nb[t,predictors[m]]=tp/N - fp/N*(nb$threshold[t]/(1-nb$threshold[t])) - harm[m]
    }
    interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
  }
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  for(m in 1:pred.n) {
    if (smooth==TRUE){
      lws=loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
      
      lws=loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
    }
  }
  
  # PLOTTING GRAPH IF REQUESTED
  if (graph==TRUE) {
    require(graphics)
    
    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if(intervention==TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      
      #getting maximum number of avoided interventions
      ymax=max(interv[predictors],na.rm = TRUE)
      
      #INITIALIZING EMPTY PLOT WITH LABELS
      plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
      
      #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
        } else {
          lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
        }
        
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      
      #getting maximum net benefit
      ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
      
      # inializing new benfit plot with treat all option
      plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
      # adding treat none option
      lines(x=nb$threshold, y=nb$none,lwd=2)
      #PLOTTING net benefit FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=m,lty=2) 
        } else {
          lines(nb$threshold,data.matrix(nb[predictors[m]]),col=m,lty=2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
    
  }
  
  #RETURNING RESULTS
  results=list() 
  results$N=N
  results$predictors=data.frame(cbind(predictors,harm,probability))
  names(results$predictors)=c("predictor","harm.applied","probability")
  results$interventions.avoided.per=interventionper
  results$net.benefit=nb
  results$interventions.avoided=interv
  
  return(results)
  
}  

#----------
#metrics train data
dca_logistic_train<-dca(data = metrics_logistic_data_train_new_threshold,
       outcome = "outcomes",
       predictors = "calibrated predictions",
       xstart = 0.0,
       xstop = 1,
       xby = 0.001)

netbenef_logistic_train_cal_df<-dca_logistic_train$net.benefit

netbenf_logistic_train_cal<-netbenef_logistic_train_cal_df %>%
  filter(threshold == 0.598) %>%
  select(`calibrated predictions`) %>%
  as.numeric() %>%
  round(3) %>%
  as.character()

netbenef_logistic_train_cal_diff<-netbenef_logistic_train_cal_df %>%
  filter(threshold == 0.598) %>%
  summarise("evaluar a todos vs modelo" = `calibrated predictions`- all) %>%
  as.numeric() %>%
  round(3) %>%
  as.character()
  
netbenef_logistic_train_cal_interv_avoided<-dca_logistic_train$interventions.avoided %>%
  filter(threshold == 0.598) %>%
  select(`calibrated predictions`) %>%
  as.numeric() %>%
  round(3) %>%
  as.character()


#metrics test data
dca_logistic_test<-dca(data = metrics_logistic_data_test,
                        outcome = "outcomes",
                        predictors = "predictions",
                        xstart = 0.0,
                        xstop = 1,
                        xby = 0.001)

netbenef_logistic_test_df<-dca_logistic_test$net.benefit

netbenf_logistic_test<-netbenef_logistic_test_df %>%
  filter(threshold == 0.598) %>%
  select(predictions) %>%
  as.numeric() %>%
  round(3) %>%
  as.character()

netbenef_logistic_test_diff<-netbenf_logistic_test_cal<-netbenef_logistic_test_df %>%
  filter(threshold == 0.598) %>%
  summarise("evaluar a todos vs modelo" = predictions- all) %>%
  as.numeric() %>%
  round(3) %>%
  as.character()

netbenef_logistic_test_interv_avoided<-dca_logistic_test$interventions.avoided %>%
  filter(threshold == 0.598) %>%
  select(predictions)%>%
  as.numeric() %>%
  round(3) %>%
  as.character()


#performance logistic
performance_logistic<-performance_logistic %>%
  add_case(medida = c("Beneficio neto", "Beneficio neto (evaluar a todos vs modelo)", "Evaluaciones evitadas"),
           entrenamiento = c(netbenf_logistic_train_cal, netbenef_logistic_train_cal_diff, netbenef_logistic_train_cal_interv_avoided),
           prueba = c(netbenf_logistic_test, netbenef_logistic_test_diff, netbenef_logistic_test_interv_avoided))

names(performance_logistic)<-c("medida", "entrenamiento logistic", "prueba logistic")
