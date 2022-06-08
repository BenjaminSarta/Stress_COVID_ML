comparative_model_performance<-full_join(performance_logistic, performance_rf, by = "medida")
comparative_model_performance<-full_join(comparative_model_performance, performance_svm, by = "medida")

writexl::write_xlsx(comparative_model_performance, "comparative_model_performance.xlsx")

metrics_logistic_data_test_2<-metrics_logistic_data_test %>%
  select(outcomes, predictions) %>%
  mutate(model_name = c(rep("Regresión logística", 53)))

metrics_rf_data_test_2<-metrics_rf_data_test %>%
  select(outcomes, predictions) %>%
  mutate(model_name = c(rep("Bosque aleatorio", 53)))

metrics_svm_data_test_2<-metrics_svm_data_test %>%
  select(outcomes, predictions) %>%
  mutate(model_name = c(rep("Maquina de soporte a vectores", 53)))

comparative_model_metrics<-rbind.data.frame(metrics_logistic_data_test_2, metrics_rf_data_test_2, metrics_svm_data_test_2)


#-----
#Function from runway edited to change axis labels to spanish
calib_plot<-function (df, outcome, prediction, n_bins = 10, show_loess = FALSE, 
                      plot_title = "", ...) 
{
  g1 <- dplyr::mutate(df, bin = dplyr::ntile(!!rlang::parse_expr(prediction), 
                                             n_bins)) %>% dplyr::group_by(bin) %>% dplyr::mutate(n = dplyr::n(), 
                                                                                                 bin_pred = mean(!!rlang::parse_expr(prediction), na.rm = TRUE), 
                                                                                                 bin_prob = mean(as.numeric(as.character(!!rlang::parse_expr(outcome))), 
                                                                                                                 na.rm = TRUE), se = sqrt((bin_prob * (1 - bin_prob))/n), 
                                                                                                 ul = bin_prob + 1.96 * se, ll = bin_prob - 1.96 * se) %>% 
    dplyr::mutate_at(dplyr::vars(ul, ll), . %>% scales::oob_squish(range = c(0, 
                                                                             1))) %>% dplyr::ungroup() %>% ggplot2::ggplot() + 
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 
                                                               1, by = 0.1)) + ggplot2::scale_x_continuous(limits = c(0, 
                                                                                                                      1), breaks = seq(0, 1, by = 0.1)) + ggplot2::geom_abline(linetype = "dashed")
  if (n_bins > 0) {
    g1 = g1 + ggplot2::geom_point(ggplot2::aes(x = bin_pred, 
                                               y = bin_prob, ymin = ll, ymax = ul), size = 2, color = "black") + 
      ggplot2::geom_errorbar(ggplot2::aes(x = bin_pred, 
                                          y = bin_prob, ymin = ll, ymax = ul), size = 0.5, 
                             color = "black", width = 0.02)
  }
  if (show_loess) {
    g1 = g1 + ggplot2::geom_smooth(ggplot2::aes(x = !!rlang::parse_expr(prediction), 
                                                y = as.numeric(!!rlang::parse_expr(outcome))), color = "black", 
                                   se = TRUE, method = "loess")
  }
  g1 = g1 + ggplot2::xlab("Probabilidad predicha") + ggplot2::ylab("Riesgo predicho") + 
    ggplot2::theme_minimal() + ggplot2::theme(aspect.ratio = 1) + 
    ggplot2::ggtitle(plot_title)
  g2 <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::parse_expr(prediction))) + 
    ggplot2::geom_histogram(fill = "black", bins = 100) + 
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 
                                                               1, by = 0.1)) + ggplot2::xlab("") + ggplot2::ylab("") + 
    ggplot2::theme_minimal() + ggeasy::easy_remove_y_axis() + 
    ggeasy::easy_remove_x_axis() + ggplot2::theme_void() + 
    ggplot2::theme(aspect.ratio = 0.1)
  layout = c(patchwork::area(t = 1, b = 10, l = 1, r = 10), 
             patchwork::area(t = 11, b = 12, l = 1, r = 10))
  g1/g2
}

library(patchwork)


log_calplot<-calib_plot(metrics_logistic_data_test,
                        outcome = 'outcomes',
                        prediction = 'predictions',
                        n_bins = 5)  + 
  labs(y = "Beneficio neto", x = "Umbral de probabilidad") 

rf_calplot<-calib_plot(metrics_rf_data_test,
                       outcome = 'outcomes',
                       prediction = 'predictions',
                       n_bins = 5)  + 
  labs(y = "Beneficio neto", x = "Umbral de probabilidad") 

svm_calplot<-calib_plot(metrics_svm_data_test,
                        outcome = 'outcomes',
                        prediction = 'predictions',
                        n_bins = 5)  + 
  labs(y = "Beneficio neto", x = "Umbral de probabilidad") 

plots<- wrap_plots(log_calplot, rf_calplot, svm_calplot) + 
  plot_annotation(
    title = 'Curvas de calibraci�n de los modelos',
    tag_levels = 'A'
  )

runway::roc_plot_multi(comparative_model_metrics, 
                       outcome = 'outcomes', 
                       prediction = 'predictions', 
                       model = 'model_name',
                       ci = F,
                       plot_title = NULL) +
  labs(x = "Especificidad", y = "Sensibilidad")+
  ggplot2::scale_color_brewer(name = "Modelo", palette = "Set1") + 
  ggplot2::scale_fill_brewer(name = "Modelo", palette = "Set1") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12))

#decision curve plot
decision_curve_threshold<-c(netbenef_logistic_test_df$threshold)
decision_curve_all<-c(netbenef_logistic_test_df$all)
decision_curve_logistic<-c(netbenef_logistic_test_df$predictions)
decision_curve_rf<-c(netbenef_rf_test_df$predictions)
decision_curve_svm<-c(netbenef_svm_test_df$predictions)

decision_curve_umbral<-c(decision_curve_threshold, decision_curve_threshold, decision_curve_threshold, decision_curve_threshold)
decision_curve_beneficioneto<-c(decision_curve_all, decision_curve_logistic, decision_curve_rf, decision_curve_svm)
decision_curve_modelo<-c(rep("Asumir que todos tienen el desenlace", 1001),
                        rep("Regresión logística", 1001),
                        rep("Bosque aleatorio", 1001),
                        rep("Maquina de soporte a vectores",1001))

decision_curve_df<-data.frame(Umbral = decision_curve_umbral,
                              Beneficio = decision_curve_beneficioneto,
                              Modelo = decision_curve_modelo)

decision_plot<-ggplot(data = decision_curve_df, aes(x = `Umbral`, y = `Beneficio`, group = `Modelo`))  +
    geom_line(aes(color=Modelo)) + 
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-0.85, 1)) +
    theme_minimal() + 
    labs(y = "Beneficio neto", x = "Umbral de probabilidad")z+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text=element_text(size=12)