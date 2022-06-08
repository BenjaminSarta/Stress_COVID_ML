#code based on https://dnield.com/posts/tidymodels-intro/

library(tidymodels)
library(tidyverse)
library(fastDummies)
library(glmnet)
library(tictoc)
library(vip)
library(patchwork)


load("D:/My_data/U/Andina/Epi/tesis/Data analysis/Epi thesis/modelling_phase_data.Rda")

#Data resampling
set.seed(123)

data_dummy<-modelling_phase_data %>% 
  dummy_cols(select_columns = c("Dem_gender", "Dem_edu", "Dem_employment", "Dem_Expat", "Dem_maritalstatus", "Dem_riskgroup", "Dem_isolation"), remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
  select(-PSS10_stress_dico) %>%
  mutate(Dem_gender_Female = ifelse(Dem_gender_Male==1, 0, 1)) %>%
  select(-Dem_gender_Male)

data_split_linear<-initial_split(data_dummy, prop = 0.7)
train_linear<-training(data_split_linear)
test_linear<-testing(data_split_linear)

  
data_cv_linear<-vfold_cv(train_linear, v = 10)

#Linear model
linear_recipe<-recipe(PSS10_stress ~ ., data = train_linear) %>%
  step_zv(all_predictors(), -all_outcomes())

linear_spec<-linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

lambda_grid_linear <- grid_regular(penalty(), mixture(), levels = 50)

doParallel::registerDoParallel()

tic()

set.seed(125)
linear_grid <- tune_grid(linear_spec,
                         linear_recipe,
                         resamples = data_cv_linear,
                         grid = lambda_grid_linear)

toc()

lowest_rmse <- linear_grid %>%
  select_best("rmse")

parameters_tunning_plot<-linear_grid %>%
  collect_metrics() %>% 
  filter(.metric == "rmse") %>%
  mutate(mixture = format(mixture)) %>% 
  ggplot(aes(x = penalty, y = mean, color = mixture))+
  theme_minimal() +
  geom_line() +
  geom_point() 


wf_linear<-workflow() %>%
  add_recipe(linear_recipe)

linear_model <- finalize_workflow(
  wf_linear %>% add_model(linear_spec),
  lowest_rmse) %>%
  fit(data = train_linear)

coefficients<-linear_model %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0)

metrics_data_train_linear<-data.frame(pred = predict(linear_model, train_linear),
                                      pss10 = train_linear$PSS10_stress)

metrics_train_linear<-metrics(data = metrics_data_train_linear, estimate = .pred, truth = pss10)

metrics_data_test_linear<-data.frame(pred = predict(linear_model, test_linear),
                      pss10 = test_linear$PSS10_stress)

metrics_test_linear<-metrics(data = metrics_data_test_linear, estimate = .pred, truth = pss10)


tidy_coefs<-linear_model$fit$fit$fit %>% 
  broom::tidy() %>% 
  filter(term != "(Intercept)") %>% 
  select(-step, -dev.ratio)

delta <- abs(tidy_coefs$lambda - lowest_rmse$penalty)
lambda_opt<-tidy_coefs$lambda[which.min(delta)]

label_coefs <- tidy_coefs %>% 
  mutate(abs_estimate = abs(estimate)) %>% 
  filter(abs_estimate >= 0.01) %>% 
  distinct(term) %>% 
  inner_join(tidy_coefs, by = "term") %>% 
  filter(lambda == lambda_opt)


coefficients_plot<-linear_model$fit$fit$fit %>% 
  tidy() %>% 
  filter(term != "(Intercept)") %>% 
  select(-step, -dev.ratio) %>% 
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  theme_minimal() +
  geom_vline(xintercept = lambda_opt, lty = 3) +
  geom_line(alpha = .4) +
  theme(legend.position = "none") +
  scale_x_log10() +
  ggrepel::geom_text_repel(data = label_coefs)


performance_linear_reg<-cbind.data.frame(metrics_train_linear$.metric,
                                         metrics_train_linear$.estimate,
                                         metrics_test_linear$.estimate)
names(performance_linear_reg)<-c("metrica", "set entrenamiento", "set prueba")
writexl::write_xlsx(performance_linear_reg, "performance_linear.xlsx")
