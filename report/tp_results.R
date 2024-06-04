
# settings ----
options(knitr.kable.NA = '', scipen = 999)
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(stringr)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(kableExtra)))
suppressWarnings(suppressMessages(library(patchwork)))
suppressWarnings(suppressMessages(library(ggbreak)))
source(file = file.path("analysis", "model_analysis", "tp_bin_helpers.R"))
source(file = file.path("analysis", "model_analysis", "analysis_helpers.R"))
source(file = file.path("report", "functions", "report_helpers.R"))

SEED <- 517
task_path <- file.path("output",
                       "2024-05-16_tp")
res_path <- file.path("output",
                      "2024-05-17_results")

# Test data set
test_data <- readRDS(file.path("no_vc", "cache", "sample_test.Rds"))

# Result files
res_files <- list.files(task_path,
                        full.names = TRUE) %>%
  str_subset(., "_res")
res_gamma <- readRDS(res_files[1])
res_weibull <- readRDS(res_files[2])
res_ggamma <- readRDS(res_files[3])

res <- list(res_gamma,
            res_weibull,
            res_ggamma)
names(res) <- c("gamma", "weibull", "ggamma")

limit_ck <- readRDS(file.path(task_path, "limit_ck.Rds"))
limit_te <- readRDS(file.path(task_path, "limit_te.Rds"))

# SISU-model ----
sisu_res <- data.frame(
  "model" = "SISU-model",
  "stat" = "mean",
  "rmse" = calcRMSE(test_data$toimtuki_simul, test_data$perustt),
  "ks_test" = suppressWarnings(ks.test(test_data$toimtuki_simul,
                                       test_data$perustt)$statistic))

dummy_estimate <- rep(0, nrow(test_data))
dummy_class <- data.frame(dummy_te = calcPropIncorrect(dummy_estimate,
                                                         test_data$data_kylla))

sisu_class <- data.frame(sisu_te = calcPropIncorrect(test_data$simul_kylla,
                                                        test_data$perustt_kylla),
                         sisu_fn = calcFalseNeg(test_data$simul_kylla,
                                                  test_data$perustt_kylla),
                         sisu_fp = calcFalsePos(test_data$simul_kylla,
                                                  test_data$perustt_kylla))

# LPPD ----
lppd_tp <- Reduce(bind_cols, lapply(res, \(x) x$loglik$lppd_tp))
colnames(lppd_tp) <- c("gamma", "weibull", "ggamma")

res_lppd <- calcLogLikDiffsTP(df = lppd_tp)
res_lppd <- res_lppd %>%
  arrange(desc(lppd))
res_lppd$model <- c("Generalized gamma",
                     "Weibull",
                     "Gamma")

lppd_table_pres <- kableExtra::kable(res_lppd,
                                     booktabs = TRUE,
                                     digits = 2,
                                     row.names = FALSE,
                                     col.names = c("Model", "LPPD", "LPPD_diff", "SE_diff")) %>%
  footnote(paste("N = ", nrow(test_data))) %>% 
  kable_styling(font_size = 11,
                html_font = "Source Sans Pro",
                htmltable_class = "lightable-classic")

saveRDS(lppd_table_pres,
        file.path(res_path, "tables", "table-res_lppd.Rds"))

lppd_table <- kableExtra::kable(res_lppd,
                                format = "latex",
                                escape = FALSE,
                                booktabs = TRUE,
                                digits = 2,
                                label = "res_lppd",
                                row.names = FALSE,
                                col.names = c("Model", "LPPD", "$\\text{LPPD}_{\\text{diff}}$", "$\\text{\\hat{SE}}_{\\text{diff}}$"),
                                caption = "LPPD, LPPD differences ($\\text{LPPD}_{\\text{diff}}$), and standard error of the LPPD differences ($\\text{SE}_{\\text{diff}}$) of the two-part model with varying continuous distribution assumption (Model) of total annual social assistance. The differences in LPPD have been calculated with respect to the model with the highest LPPD. The estimates have been calculated from posterior predictive samples using the test data set.",
) %>% kable_styling(font_size = 11)

cat(lppd_table, file = file.path(res_path, "tables", "table-res_lppd.txt"))

# RMSE and KS-test ----

criteria_src <- Reduce(bind_rows, lapply(res, \(x) x$criteria))
criteria_src <- criteria_src %>%
  pivot_wider(names_from = "stat",
              values_from = "est")

# Add SISU-results
sisu_print <- sisu_res %>%
  pivot_longer(cols = c("rmse", "ks_test"),
               names_to = "criteria",
               values_to = "estimate") %>%
  rename(dist = model)
sisu_print$class_stat <- NA
sisu_print$lwr_ci <- NA
sisu_print$upr_ci <- NA
sisu_print <- sisu_print %>%
  select(dist, class_stat, criteria, estimate, lwr_ci, upr_ci)
criteria_src <- bind_rows(criteria_src, sisu_print)

criteria_src$dist <- factor(criteria_src$dist, levels = c("SISU-model", "gamma", "weibull", "ggamma"))
criteria_src$class_stat <- factor(criteria_src$class_stat, levels = c(NA, "none", "cohens_kappa", "test_error"))
criteria_src <- criteria_src %>% 
  arrange(dist, class_stat)
index <- with(criteria_src, table(dist))
names(index) <- c("SISU model", "Gamma", "Weibull", "Generalized gamma")
# index <- with(criteria_src, table(dist, class_stat)) %>% as.vector()
# newnames <- expand.grid(dist = c("Gamma", "Weibull", "Generalized gamma"),
#                         class_stat = c("Cohen's kappa", "Test error")) %>%
#   arrange(dist) %>%
#   apply(., 1, paste, collapse = ", ")
# names(index) <- newnames

criteria_src <- criteria_src %>% 
  mutate(estimate = ifelse(criteria == "rmse",
                           format(round(estimate, 2), nsmall = 2, trim = TRUE),
                           format(round(estimate, 3), nsmall = 3, trim = TRUE)))
         

criteria_print <- criteria_src %>%
  group_by(dist, class_stat) %>%
  mutate(class_stat = as.character(class_stat),
         class_stat = replace(class_stat, duplicated(class_stat), '')) %>%
  ungroup() %>%
  select(!c(dist))
  
# class_print$class_stat <- factor(class_print$class_stat,
#                                  levels = "")

# Change labels for pretty printing
criteria_print <- criteria_print %>%
  mutate(
  ci_95 = ifelse(is.na(lwr_ci),
                 "",
                 ifelse(criteria == "rmse",
                        paste0("[", format(round(lwr_ci, 2), nsmall = 2, trim = T), ", ",
                               format(round(upr_ci, 2), nsmall = 2, trim = T), "]"),
                        paste0("[", format(round(lwr_ci, 3), nsmall = 3, trim = T), ", ",
                               format(round(upr_ci, 3), nsmall = 3, trim = T), "]"))),
    criteria = case_when(
      criteria == "rmse" ~ "RMSE",
      criteria == "ks_test" ~ "KS test $d$",
      .default = criteria
    ),
  class_stat = case_when(
    class_stat == "cohens_kappa" ~ "Kappa-optimised",
    class_stat == "test_error" ~ "Misclassification rate-optimised",
    class_stat == "none" ~ "Posterior predictive",
    .default = class_stat
  ),
  ) %>%
  select(!c(lwr_ci, upr_ci))
criteria_print

criteria_table <- kbl(criteria_print,
    digits = 3,
    col.names = c("\\quad Classifier", "Criteria", "Estimate", "95\\% CI"),
    align = c("l", "l", "r", "r"),
    booktabs = T,
    format = "latex",
    label = "res_criteria",
    caption = "Mean and 95\\% credible intervals of the root mean squared error (RMSE) and Kolmogorov-Smirnov (KS) test
      $d$-statistic for each of the standard two-part models according to the continuous distribution assumption and
      the classification method, and the corresponding point estimates of the SISU model. The estimates have been calculated
      using the test data set, and the two-part model the predictions have been simulated using 3000 posterior samples.",
    escape = FALSE) %>%
  pack_rows(index = index) %>%
  kable_styling(font_size = 11)

cat(criteria_table, file = file.path(res_path,
                                     "tables",
                                     paste0("table-res_criteria.txt")))

# Classification ----

res_class <- readRDS(file.path(res_path, "src", "class_data.R"))
res_class$model <- "Two-part model"
res_class <- res_class %>%
  mutate(across(where(is.character), as.factor))

support_class <- data.frame(
  model = factor(rep(c("SISU model", "Dummy"), each = 3)),
  optim_stat = "",
  statistic = factor(c("test_error",
                "false_pos",
                "false_neg")),
  estimate = c(sisu_class$sisu_te,
               sisu_class$sisu_fp,
               sisu_class$sisu_fn,
               dummy_class$dummy_te,
               NA,
               NA)
  )
res_class <- bind_rows(res_class, support_class)
res_class$optim_stat <- factor(res_class$optim_stat,
                               levels = c("", "none", "cohens_kappa", "test_error"))
res_class$model <- factor(res_class$model, levels = c("Dummy", "SISU model", "Two-part model"))

class_prop  <- res_class %>%
  group_by(model, optim_stat, statistic) %>%
  summarise(mean = mean(estimate),
            lwr_ci = quantile(estimate, 0.025, na.rm = TRUE),
            upr_ci = quantile(estimate, 0.975, na.rm = TRUE))

class_print <- class_prop %>% 
  mutate(across(where(is.numeric), \(x) x*100))

class_print <- class_print %>%
  filter(!c(model == "Dummy" & statistic %in% c("false_neg", "false_pos"))) %>%
  mutate(
    lwr_ci = ifelse(model %in% c("Dummy", "SISU model"), NA, lwr_ci),
    upr_ci = ifelse(model %in% c("Dummy", "SISU model"), NA, upr_ci),
    statistic = case_when(
      statistic == "false_neg" ~ "False negative rate",
      statistic == "false_pos" ~ "False positive rate",
      statistic == "test_error" ~ "Misclassification rate",
    ),
    ci_95 = ifelse(!is.na(lwr_ci),
                   paste0("[", format(lwr_ci, digits = 3, trim = T), ", ",
                          format(upr_ci, digits = 3, trim = T), "]"),
                   "")
    ) %>%
  ungroup()

index <- table(class_print$model)
class_print$optim_stat <- c(NA, NA, "", "",
                            "Posterior predictive", "", "",
                            "Kappa-optimised", "", "",
                            "Misclassification rate-optimised", "", "")
class_print <- class_print %>% select(!c(model, lwr_ci, upr_ci))
  
colnames(class_print) <- c("\\quad Classifier", "Statistic", "Estimate (\\%)", "95\\% CI")
class_print

linesep <- function(table, groups) {
  sep_index <- rep("", nrow(table))
  sep_index[cumsum(groups)] <- "\\addlinespace"
  return(sep_index)
}

class_table <- kbl(class_print,
                   digits = 2,
                   booktabs = T,
                   align = c("l", "l", "r", "r"),
                   format = "latex",
                   label = "classacc",
                   linesep = linesep(class_print, c(7, 3)),
                   caption = "Mean and 95\\% credible intervals of the false negative rate, false positive rate
                   and misclassification rate for the standard two-part model according to the classification method,
                   and the corresponding point estimates of the dummy model and the SISU model. The estimates have been calculated using the test data set, and
                   the two-part model the predictions have been simulated using 3000 posterior samples.",
                      escape = FALSE) %>%
  pack_rows(index = index) %>%
  column_spec(1, width = "5.6cm") %>%
  column_spec(1, width = "3.5cm") %>%
  column_spec(3, width = "1.7cm") %>%
  column_spec(4, width = "2.2cm") %>% 
  kable_styling(font_size = 11)

cat(class_table, file = file.path(res_path,
                                     "tables",
                                     paste0("table-res_classaccuracy.txt")))


support <- c(sisu_class, dummy_class)
class_plot <- classPlot(res_class, support)
class_plot
ggsave(plot = class_plot,
       filename = file.path(res_path, "plots", "plot-res_classacc.pdf"),
       device = "pdf",
       units = "px",
       height = 3508*0.25,
       width = 2480*0.6)

# ROC ----

errorratios <- readRDS(file.path(res_path, "src", "roc_data.R"))

# Points
sisu_roc <- data.frame("tprate" = truePosRate(train_data$simul_kylla, train_data$perustt_kylla),
                "fprate" = falsePosRate(train_data$simul_kylla, train_data$perustt_kylla))
sisu_roc$Classifier <- c("SISU model")

er_ck <- errorratios %>% 
  filter(l == limit_ck)
ck_roc <- data.frame("tprate" = mean(er_ck$proptp),
            "fprate" = mean(er_ck$propfp),
            "Classifier" = "Kappa-\noptimised")

er_te <- errorratios %>% 
  filter(l == limit_te)
te_roc <- data.frame("tprate" = mean(er_te$proptp),
            "fprate" = mean(er_te$propfp),
            "Classifier" = "Misclassification rate-\noptimised")

er_pp <- readRDS(file.path(res_path, "src", "postpred_data.R"))
pp_roc <- data.frame("tprate" = mean(er_pp$tprate),
                     "fprate" = mean(er_pp$fprate),
                     "Classifier" = "Posterior\npredictive")

newpoints <- bind_rows(sisu_roc, ck_roc, te_roc, pp_roc)
newpoints$Classifier <- factor(newpoints$Classifier,
                          levels = c("SISU model", "Kappa-\noptimised", "Misclassification rate-\noptimised", "Posterior\npredictive"))

roc_plot <- ROCPlot(errorratios,
                    points = newpoints)
roc_plot
ggsave(plot = roc_plot,
       filename = file.path(res_path, "plots", "plot-res_roc.pdf"),
       device = "pdf",
       units = "px",
       height = 3508*0.25,
       width = 2480*0.7)

  
# Bayes p-values ----

bayesp <- Reduce(bind_rows, lapply(res, \(x) x$bayesp))

bayesp$class_stat <- factor(bayesp$class_stat,
                            levels = c("none", "cohens_kappa", "test_error"))
bayesp$dist <- factor(bayesp$dist,
                      levels = c("ggamma", "gamma", "weibull"))
bayesp <- bayesp %>% 
  filter(class_stat != "none")

y <- test_data$toimtuki_data
yrepsisu <- test_data$toimtuki_simul
ysisu <- test_data$perustt

bpmean_plot <- ggplot(bayesp %>% filter(stat == "mean"), aes(x = est, fill = dist)) +
  geom_density(alpha = 0.6) +
  labs(fill = "Distribution") + 
  scale_fill_discrete(labels = c("Generalized gamma", "Gamma", "Weibull"),
                      type =  c("black", "dark gray", "azure1")) +
  # scale_fill_grey(labels = c("Generalized gamma", "Gamma", "Weibull")) +
                      #type =  c("#F0E442", "#0072B2", "#D55E00")) +
  scale_x_continuous(expand = rep(2e-2, 2)) +
  geom_vline(aes(xintercept = mean(y)),
             linetype = "solid") +
  geom_vline(aes(xintercept = mean(yrepsisu)),
             linetype = "dashed") +
  geom_vline(aes(xintercept = mean(ysisu)),
             linetype = "dotted") +
    scale_color_manual(
      name = "Test",
      values = c("black" = "black", "blue" = "blue"),
      labels = c("Observed", "SISU-model")
      ) +
    facet_grid(cols = vars(class_stat),
               labeller = labeller(class_stat = c("cohens_kappa" = "Kappa-optimised",
                                                  "test_error" = "Misclassification rate-optimised"))) +
    labs(x = "Mean annual social assistance (€/year)",
         y = "Density",
         tag = "i") +
    theme_bw() +
    theme(legend.position = "top")

bptotal_plot <- ggplot(bayesp %>% filter(stat == "total"), aes(x = est/1000, fill = dist)) +
  geom_density(alpha = 0.6) +
  labs(fill = "Distribution") + 
  scale_fill_discrete(labels = c("Generalized gamma", "Gamma", "Weibull"),
                      type =  c("black", "dark gray", "azure1")) +
  scale_x_continuous(expand = rep(2e-2, 2)) +
    geom_vline(aes(xintercept = sum(y)/1000),
               linetype = "solid") +
    geom_vline(aes(xintercept = sum(yrepsisu)/1000),
               linetype = "dashed") +
    geom_vline(aes(xintercept = sum(ysisu)/1000),
               linetype = "dotted") +
  facet_grid(cols = vars(class_stat),
             labeller = labeller(class_stat = c("cohens_kappa" = "Kappa-optimised",
                                                "test_error" = "Misclassification rate-optimised"))) +
    labs(x = "Total annual social assistance (1000€/year)",
         y = "Density") +
  theme_bw() +
  theme(legend.position = "top")
  
  bpvar_plot <- ggplot(bayesp %>% filter(stat == "var"), aes(x = est/1000, fill = dist)) +
    geom_density(alpha = 0.6) +
    labs(fill = "Distribution") + 
    scale_fill_discrete(labels = c("Generalized gamma", "Gamma", "Weibull"),
                        type =  c("black", "dark gray", "azure1")) +
    scale_x_continuous(expand = rep(2e-2, 2)) +
    geom_vline(aes(xintercept = var(y)/1000),
               linetype = "solid") +
    geom_vline(aes(xintercept = var(yrepsisu)/1000),
               linetype = "dashed") +
    geom_vline(aes(xintercept = var(ysisu)/1000),
               linetype = "dotted") +
    facet_grid(cols = vars(class_stat),
               labeller = labeller(class_stat = c("cohens_kappa" = "Kappa-optimised",
                                                  "test_error" = "Misclassification rate-optimised"))) +
    labs(x = "Variance of annual social assistance (1000€/year)",
         y = "Density") +
    theme_bw() +
    theme(legend.position = "top")
  
  bpq99_plot <- ggplot(bayesp %>% filter(stat == "q99"), aes(x = est, fill = dist)) +
    geom_density(alpha = 0.6) +
    labs(fill = "Distribution") + 
    scale_fill_discrete(labels = c("Generalized gamma", "Gamma", "Weibull"),
                        type =  c("black", "dark gray", "azure1")) +
    scale_x_continuous(expand = rep(2e-2, 2)) +
    geom_vline(aes(xintercept = quantile(y, 0.99)),
               linetype = "solid") +
    geom_vline(aes(xintercept = quantile(yrepsisu, 0.99)),
               linetype = "dashed") +
    geom_vline(aes(xintercept = quantile(ysisu, 0.99)),
               linetype = "dotted") +
    facet_grid(cols = vars(class_stat),
               labeller = labeller(class_stat = c("cohens_kappa" = "Kappa-optimised",
                                                  "test_error" = "Misclassification rate-optimised"))) +
    labs(x = "99% quantile of annual social assistance (€/year)",
         y = "Density") +
    theme_bw() +
    theme(legend.position = "top")
  
  bp_plot <- (bpmean_plot / bptotal_plot / bpvar_plot / bpq99_plot) +
    plot_layout(guides = "collect") &
    plot_annotation(tag_levels = "i") &
    theme(text = element_text(size = 8.5),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          strip.text = element_text(size = 7.4),
          plot.margin = unit(c(0, 0.15, 0.05, 0.05), "cm"),
          legend.position = "bottom")
    
  
  bp_plot
  ggsave(bp_plot,
         filename = file.path(res_path, "plots",
                              paste0("plot-res_bayesp.pdf")),
         device = "pdf",
         units = "cm",
         height = 16.02,
         width = 12.13)
  
  
bayesp <- Reduce(bind_rows, lapply(res, \(x) x$bayesp))
  
bayesp$class_stat <- factor(bayesp$class_stat,
                              levels = c("none", "cohens_kappa", "test_error"))
bayesp$dist <- factor(bayesp$dist,
                        levels = c("ggamma", "gamma", "weibull"))
  
bayesp %>%
  group_by(dist, class_stat) %>%
  filter(stat == "mean") %>%
  summarise(meanbp = mean(est > mean(test_data$toimtuki_data)))

bayesp %>%
  group_by(dist, class_stat) %>%
  filter(stat == "total") %>%
  summarise(totalbp = mean(est > sum(test_data$toimtuki_data)))

bayesp %>%
  group_by(dist, class_stat) %>%
  filter(stat == "var") %>%
  summarise(varbp = mean(est > var(test_data$toimtuki_data)))

bayesp %>%
  group_by(dist, class_stat) %>%
  filter(stat == "q99") %>%
  summarise(meanbp = mean(est > quantile(test_data$toimtuki_data, 0.99)))
  