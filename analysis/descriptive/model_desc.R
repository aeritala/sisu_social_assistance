
library(dplyr)
library(brms)
library(kableExtra)

res_path <- file.path("output",
                      "2024-05-17_results",
                      "tables")
test_data <- readRDS(file.path("no_vc", "cache", "sample_test.Rds"))

binreg <- readRDS(file.path("output",
                            "2024-01-31_tp",
                            "fit_tp01.Rds"))

# Covariates ----

covariates <- data.frame("Variable" = c(
  "Intercept",
  "Age",
  "Sex",
  "House-dwel. structure",
  "Education level",
  "Housing tenure",
  "Immigrant status",
  "Municip. class",
  "Communal house-dwel.",
  "Unemp. duration",
  "Income decile",
  "Capital income",
  "Labour market subsidy",
  "Basic unemp.",
  "Earnings-rel. unemp.",
  "Student aid",
  "SISU soc.assist. cont.",
  "SISU soc.assist. categ.",
  "Housing allowance",
  "Family's benefits",
  "Other social benefits",
  "Pension",
  "Main activity",
  "Salary"),
  "Description" = c("Intercept, reference classes marked with (ref.)",
                    "Age of the reference person",
                    "Sex of the reference person",
                    "House-dwelling unit structure",
                    "Education level of the reference person",
                    "Housing tenure",
                    "Immigrant status",
                    "Municipality class",
                    "Communal house-dwelling unit status, indicator whether persons aged 18 or over that are not the spouse of the reference person live in the house-dwelling unit",
                    "Unemployment duration of the reference person",
                    "Income decile",
                    "Capital income",
                    "Duration of labour market subsidy receipt",
                    "Duration of basic unemployment allowance receipt",
                    "Duration of earnings-related unemployment allowance receipt",
                    "Duration of reference person's student aid receipt",
                    "Amount of annual basiv social assistance predicted by the SISU model",
                    "Categorical amount of annual social assistance predicted by the SISU model",
                    "Amount of general housing allowance, housing supplement for students and housing allowance for pensioners",
                    "Child benefit, maternity grant and child maintenance allowance",
                    "Total of other social benefits",
                    "Amount of national pension, survivor's pension, inc. guarantee pension",
                    "Main activity of the reference person over the course of the year",
                    "Amount of annual salary"),
  "Unit" = c("",
             "under 18, 18--24 (ref.), 25--34, 35--44, 45--64, 65 or older",
             "male (ref.), female",
             "single (ref.), single with children, couple, couple with children, other, unknown",
             "comprehensive school (ref.), upper secondary school, post-secondary non-tertiary education, lowest tertiary education, bachelor's or equivalent, master's or equivalent and second stage of tertiary education",
             "rented (ref.), owner-occupied, part-ownership, other",
             "no (ref.), yes",
             "Helsinki (ref.), other Helsinki metropolitan area, middle-sized cities, other municipalities",
             "no (ref.), yes",
             "number of months, 0--12",
             "ordinal number, 0--9",
             "€/year, total across all recipients in the house-dwelling unit",
             "number of days, total across all recipients in the house-dwelling unit",
             "number of days, total across all recipients in the house-dwelling unit",
             "number of days, total across all recipients in the house-dwelling unit",
             "number of months, 0--12",
             "€/year",
             "0--580 €/year, 581--2424 €/year, 2425--5482 €/year, more than 5483 €/year",
             "€/year, total across all recipients in the house-dwelling unit",
             "€/year, total across all recipients in the house-dwelling unit",
             "€/year, total across all recipients in the house-dwelling unit",
             "€/year, total across all recipients in the house-dwelling unit",
             "employed (ref.), entrepreneur, on pension, student, other",
             "1000€/year divided by the number of 18-year-olds or over in the house-dwelling unit"
             )
  )

cov_table <- kbl(covariates,
                 format = "latex",
                 booktabs = TRUE,
                 escape = FALSE,
                 linesep = c(""),
                 longtable = TRUE,
                 label = "app_cov",
                 caption = "Descriptions of the covariates used in the best-performing logistic, gamma, Weibull and generalized gamma regression models.") %>% 
  column_spec(1, bold = TRUE, width = "3cm") %>% 
  column_spec(2, width = "5.56cm") %>% 
  column_spec(3, width = "5.56cm") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("hold_position", "repeat_header"))

cat(cov_table, 
    file = file.path(res_path, "table-app_covariates.txt"))

# Logistic regression ----

binreg_sum <- summary(binreg)
binreg_print <- binreg_sum$fixed
binreg_print$variable <- c("Intercept",
                           rep("Age", 5),
                           "Sex",
                           rep("House-dwel. structure", 5),
                           rep("Education level", 5),
                           rep("Housing tenure", 3),
                           "Immigrant status",
                           rep("Municip. class", 3),
                           "Communal house-dwel.",
                           "Unemp. duration",
                           "Income decile",
                           "Capital income",
                           "Labour market subsidy",
                           "Basic unemp.",
                           "Earnings-rel. unemp.",
                           "Student aid",
                           "SISU soc.assist. cont.",
                           "Housing allowance",
                           "Family's benefits",
                           "Other social benefits",
                           "Pension",
                           rep("Main activity", 4),
                           "Salary")
binreg_print$coefficient <- c("Intercept",
                              "Under 18",
                              "25--34",
                              "35--44",
                              "45--65",
                              "65 or older",
                              "Female",
                              "Couple with children",
                              "Couple",
                              "Unknown",
                              "Other",
                              "Single with children",
                              "Upper second.",
                              "Post-second. non-tert.",
                              "Lowest tertiary",
                              "Bachelor's or equiv.",
                              "Master's or equiv. and second stage of tert.",
                              "Owner-occupied",
                              "Part-ownership",
                              "Rented dwelling",
                              "Yes",
                              "Other Helsinki metrop. area",
                              "Middle-sized cities",
                              "Other municipalities",
                              "Yes",
                              rep("", 12),
                              "On pension",
                              "Other",
                              "Student",
                              "Entrepreneur",
                              "")

binreg_print$variable <- factor(binreg_print$variable,
                                levels = c("Intercept",
                                           "Age",
                                           "Sex",
                                           "House-dwel. structure",
                                           "Main activity",
                                           "Housing tenure",
                                           "Communal house-dwel.",
                                           "Education level",
                                           "Municip. class",
                                           "Immigrant status",
                                           "Unemp. duration",
                                           "Income decile",
                                           "Salary",
                                           "Capital income",
                                           "Housing allowance",
                                           "Labour market subsidy",
                                           "Basic unemp.",
                                           "Earnings-rel. unemp.",
                                           "Pension",
                                           "Student aid",
                                           "SISU soc.assist. cont.",
                                           "Family's benefits",
                                           "Other social benefits"
                                           )
                                )
binreg_print <- binreg_print[order(binreg_print$variable),]
rownames(binreg_print) <- NULL
binreg_print$variable <- as.character(binreg_print$variable)
binreg_print <- binreg_print %>% 
  select(variable, coefficient, Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS)
binreg_print <- binreg_print %>% 
  group_by(variable) %>% 
  mutate(variable = replace(variable, duplicated(variable), ''),
         Rhat = format(round(Rhat, 2), nsmall = 2))

binreg_table <- kbl(binreg_print,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l", "l", rep("r", 5)),
    linesep = c(""),
    digits = 2,
    longtable = TRUE,
    label = "app_binreg",
    col.names = c("Variable", "Levels", "Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
    caption = "Posterior means, standard errors, $\\hat{R}$-values, bulk-ESS and tail-ESS values of the regression coefficients from the logistic regression model
    which was developed as the binary component of the two-part model. The star (*) denotes an interaction term. Detailed covariate descriptions are provided in the Appendix \\ref{tab:app_cov}.") %>% 
  column_spec(1, bold = TRUE, width = "3cm") %>% 
  column_spec(2, width = "3.5cm") %>% 
  column_spec(4:5, width = "1cm") %>% 
  column_spec(c(3, 6, 7), width = "1.5cm") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(binreg_table,
    file = file.path(res_path, "table-app_binreg.txt"))

# binreg_epred <- fitted(binreg,
#                  newdata = test_data)
# binreg_epred2 <- posterior_epred(binreg,
#                                  newdata = test_data)
# 
# set.seed(105)
# i <- sample(1:nrow(binreg_epred2), 30)
# 
# epred_sample <- binreg_epred2[i,]
# epred_sample <- t(epred_sample)
# colnames(epred_sample) <- paste0("S", 1:ncol(epred_sample))
# epred_sample <- as.data.frame(epred_sample) %>%
#   pivot_longer(cols = everything(),
#                names_to = "sample",
#                values_to = "vals")
# epred_sample$sample <- factor(epred_sample$sample)
# 
# binreg_epred_df <- as.data.frame(binreg_epred)
# 
# ggplot(binreg_epred_df, aes(x = Estimate)) +
#   geom_histogram(bins = 60) +
#   theme_bw() +
#   labs(y = "Count",
#        x = "Estimate",
#        caption = paste("N = ", nrow(test_data)))
# 
# ggsave(plot = last_plot(),
#        filename = file.path("2024-05-17_results",
#                             "plots",
#                             "fitted_test.pdf"),
#        width = 5,
#        height = 4)

# Gamma regression ----

gammareg <- readRDS(file.path("output",
                               "2024-01-31_tp",
                               "fit_tp02.Rds"))
gammareg_sum <- summary(gammareg)
gamma_print <- gammareg_sum$fixed
gamma_shape <- gammareg_sum$spec_pars

gamma_print$variable <- c("Intercept",
                           rep("Age", 5),
                           "Sex",
                           rep("House-dwel. structure", 5),
                           rep("Education level", 5),
                           rep("Housing tenure", 3),
                           "Immigrant status",
                           rep("Municip. class", 3),
                           "Communal house-dwel.",
                           "Unemp. duration",
                           "Income decile",
                           "Capital income",
                           "Labour market subsidy",
                           "Basic unemp.",
                           "Earnings-rel. unemp.",
                           "Student aid",
                           "Housing allowance",
                           "Family's benefits",
                           "Other social benefits",
                           "Pension",
                           rep("SISU soc.assist. categ.", 3),
                           rep("Main activity", 4),
                           "Salary",
                          rep("House-dwel. structure * Salary", 5)
                          )
gamma_print$coefficient <- c("Intercept",
                             "Under 18",
                             "25--34",
                             "35--44",
                             "45--65",
                             "65 or older",
                             "Female",
                             "Couple with children",
                             "Couple",
                             "Unknown",
                             "Other",
                             "Single with children",
                             "Upper second.",
                             "Post-second. non-tert.",
                             "Lowest tertiary",
                             "Bachelor's or equiv.",
                             "Master's or equiv. and second stage of tert.",
                             "Owner-occupied",
                             "Part-ownership",
                             "Rented dwelling",
                             "Yes",
                             "Other Helsinki metrop. area",
                             "Middle-sized cities",
                             "Other municipalities",
                             "Yes",
                             rep("", 11),
                             "581--2424",
                             "2425---5482",
                             "more than 5483",
                             "On pension",
                             "Other",
                             "Student",
                             "Entrepreneur",
                             "",
                             "Salary * Couple with children",
                             "Salary * Couple",
                             "Salary * Unknown",
                             "Salary * Other",
                             "Salary * Single with children"
                             )


gamma_print$variable <- factor(gamma_print$variable,
                                levels = c("Intercept",
                                           "Age",
                                           "Sex",
                                           "House-dwel. structure",
                                           "Main activity",
                                           "Housing tenure",
                                           "Communal house-dwel.",
                                           "Education level",
                                           "Municip. class",
                                           "Immigrant status",
                                           "Unemp. duration",
                                           "Income decile",
                                           "Salary",
                                           "Capital income",
                                           "Housing allowance",
                                           "Labour market subsidy",
                                           "Basic unemp.",
                                           "Earnings-rel. unemp.",
                                           "Pension",
                                           "Student aid",
                                           "SISU soc.assist. categ.",
                                           "Family's benefits",
                                           "Other social benefits",
                                           "House-dwel. structure * Salary"
                                )
)
gamma_print <- gamma_print[order(gamma_print$variable),]
rownames(gamma_print) <- NULL
gamma_print$variable <- as.character(gamma_print$variable)
gamma_print <- gamma_print %>% 
  select(variable, coefficient, Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS)
gamma_print <- gamma_print %>% 
  group_by(variable) %>% 
  mutate(variable = replace(variable, duplicated(variable), ''),
         Rhat = format(round(Rhat, 2), nsmall = 2)
)

gamma_table <- kbl(gamma_print,
                   format = "latex",
                   booktabs = TRUE,
                   escape = FALSE,
                   align = c("l", "l", rep("r", 5)),
                   linesep = c(""),
                   digits = 2,
                   longtable = TRUE,
                   label = "app_gamma",
                   col.names = c("Variable", "Levels", "Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
                   caption = "Posterior means, standard errors, $\\hat{R}$-values, bulk-ESS and tail-ESS values of the regression coefficients from the gamma regression model
    which was developed as the continuous component of the two-part model. Estimates are on the scale of the linear predictor (log). The star (*) denotes an interaction term. Detailed covariate descriptions are provided in the Appendix \\ref{tab:app_cov}.") %>% 
  column_spec(1, bold = TRUE, width = "3cm") %>% 
  column_spec(2, width = "3.5cm") %>% 
  column_spec(4:5, width = "1cm") %>% 
  column_spec(c(3, 6, 7), width = "1.5cm") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(gamma_table,
    file = file.path(res_path, "table-app_gamma.txt"))

# Shape
gamma_shape <- gamma_shape %>% 
  select(Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS) %>% 
  mutate(Rhat = format(round(Rhat, 2), nsmall = 2))
rownames(gamma_shape) <- "Shape"

gammashape_table <- kbl(gamma_shape,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = c(""),
    digits = 2,
    longtable = TRUE,
    label = "app_gammashape",
    col.names = c("Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
    caption = "Posterior mean, standard error, $\\hat{R}$-value, bulk-ESS and tail-ESS value of the shape parameter from the gamma regression model
    which was developed as the continuous component of the two-part model.") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(gammashape_table,
    file = file.path(res_path, "table-app_gammashape.txt"))


# Weibull regression ----

weibullreg <- readRDS(file.path("output",
                               "2024-01-31_tp",
                               "fit_tp03.Rds"))

weibullreg_sum <- summary(weibullreg)
weibull_print <- weibullreg_sum$fixed
weibull_shape <- weibullreg_sum$spec_pars

weibull_print$variable <- c("Intercept",
                          rep("Age", 5),
                          "Sex",
                          rep("House-dwel. structure", 5),
                          rep("Education level", 5),
                          rep("Housing tenure", 3),
                          "Immigrant status",
                          rep("Municip. class", 3),
                          "Communal house-dwel.",
                          "Unemp. duration",
                          "Income decile",
                          "Capital income",
                          "Labour market subsidy",
                          "Basic unemp.",
                          "Earnings-rel. unemp.",
                          "Student aid",
                          "Housing allowance",
                          "Family's benefits",
                          "Other social benefits",
                          "Pension",
                          rep("SISU soc.assist. categ.", 3),
                          rep("Main activity", 4),
                          "Salary",
                          rep("House-dwel. structure * Salary", 5)
)
weibull_print$coefficient <- c("Intercept",
                             "Under 18",
                             "25--34",
                             "35--44",
                             "45--65",
                             "65 or older",
                             "Female",
                             "Couple with children",
                             "Couple",
                             "Unknown",
                             "Other",
                             "Single with children",
                             "Upper second.",
                             "Post-second. non-tert.",
                             "Lowest tertiary",
                             "Bachelor's or equiv.",
                             "Master's or equiv. and second stage of tert.",
                             "Owner-occupied",
                             "Part-ownership",
                             "Rented dwelling",
                             "Yes",
                             "Other Helsinki metrop. area",
                             "Middle-sized cities",
                             "Other municipalities",
                             "Yes",
                             rep("", 11),
                             "581--2424",
                             "2425---5482",
                             "more than 5483",
                             "On pension",
                             "Other",
                             "Student",
                             "Entrepreneur",
                             "",
                             "Salary * Couple with children",
                             "Salary * Couple",
                             "Salary * Unknown",
                             "Salary * Other",
                             "Salary * Single with children"
)


weibull_print$variable <- factor(weibull_print$variable,
                               levels = c("Intercept",
                                          "Age",
                                          "Sex",
                                          "House-dwel. structure",
                                          "Main activity",
                                          "Housing tenure",
                                          "Communal house-dwel.",
                                          "Education level",
                                          "Municip. class",
                                          "Immigrant status",
                                          "Unemp. duration",
                                          "Income decile",
                                          "Salary",
                                          "Capital income",
                                          "Housing allowance",
                                          "Labour market subsidy",
                                          "Basic unemp.",
                                          "Earnings-rel. unemp.",
                                          "Pension",
                                          "Student aid",
                                          "SISU soc.assist. categ.",
                                          "Family's benefits",
                                          "Other social benefits",
                                          "House-dwel. structure * Salary"
                               )
)
weibull_print <- weibull_print[order(weibull_print$variable),]
rownames(weibull_print) <- NULL
weibull_print$variable <- as.character(weibull_print$variable)
weibull_print <- weibull_print %>% 
  select(variable, coefficient, Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS)
weibull_print <- weibull_print %>% 
  group_by(variable) %>% 
  mutate(variable = replace(variable, duplicated(variable), ''),
         Rhat = format(round(Rhat, 2), nsmall = 2))

weibull_print

weibull_table <- kbl(weibull_print,
                   format = "latex",
                   booktabs = TRUE,
                   escape = FALSE,
                   align = c("l", "l", rep("r", 5)),
                   linesep = c(""),
                   digits = 2,
                   longtable = TRUE,
                   label = "app_weibull",
                   col.names = c("Variable", "Levels", "Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
                   caption = "Posterior means, standard errors, $\\hat{R}$-values, bulk-ESS and tail-ESS values of the regression coefficients from the Weibull regression model
    which was developed as the continuous component of the two-part model. Estimates are on the scale of the linear predictor (log). The star (*) denotes an interaction term. Detailed covariate descriptions are provided in the Appendix \\ref{tab:app_cov}.") %>% 
  column_spec(1, bold = TRUE, width = "3cm") %>% 
  column_spec(2, width = "3.5cm") %>% 
  column_spec(4:5, width = "1cm") %>% 
  column_spec(c(3, 6, 7), width = "1.5cm") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(weibull_table,
    file = file.path(res_path, "table-app_weibull.txt"))

weibull_shape <- weibull_shape %>% 
  select(Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS) %>% 
  mutate(Rhat = format(round(Rhat, 2), nsmall = 2))
row.names(weibull_shape) <- "Shape"

weibullshape_table <- kbl(weibull_shape,
                        format = "latex",
                        booktabs = TRUE,
                        escape = FALSE,
                        linesep = c(""),
                        digits = 2,
                        longtable = TRUE,
                        label = "app_weibullshape",
                        col.names = c("Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
                        caption = "Posterior mean, standard error, $\\hat{R}$-value, bulk-ESS and tail-ESS value of the shape parameter from the Weibull regression model
    which was developed as the continuous component of the two-part model.") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(weibullshape_table,
    file = file.path(res_path, "table-app_weibullshape.txt"))

# Generalized gamma regression ----

ggammareg <- readRDS(file.path("output",
                               "2024-01-31_tp",
                               "fit_tp04.Rds"))

ggammareg_sum <- summary(ggammareg)
ggamma_print <- ggammareg_sum$fixed
ggamma_shape <- ggammareg_sum$spec_pars

ggamma_print$variable <- c("Intercept",
                            rep("Age", 5),
                            "Sex",
                            rep("House-dwel. structure", 5),
                            rep("Education level", 5),
                            rep("Housing tenure", 3),
                            "Immigrant status",
                            rep("Municip. class", 3),
                            "Communal house-dwel.",
                            "Unemp. duration",
                            "Income decile",
                            "Capital income",
                            "Labour market subsidy",
                            "Basic unemp.",
                            "Earnings-rel. unemp.",
                            "Student aid",
                            "Housing allowance",
                            "Family's benefits",
                            "Other social benefits",
                            "Pension",
                            rep("SISU soc.assist. categ.", 3),
                            rep("Main activity", 4),
                            "Salary",
                            rep("House-dwel. structure * Salary", 5)
)
ggamma_print$coefficient <- c("Intercept",
                               "Under 18",
                               "25--34",
                               "35--44",
                               "45--65",
                               "65 or older",
                               "Female",
                               "Couple with children",
                               "Couple",
                               "Unknown",
                               "Other",
                               "Single with children",
                               "Upper second.",
                               "Post-second. non-tert.",
                               "Lowest tertiary",
                               "Bachelor's or equiv.",
                               "Master's or equiv. and second stage of tert.",
                               "Owner-occupied",
                               "Part-ownership",
                               "Rented dwelling",
                               "Yes",
                               "Other Helsinki metrop. area",
                               "Middle-sized cities",
                               "Other municipalities",
                               "Yes",
                               rep("", 11),
                               "581--2424",
                               "2425---5482",
                               "more than 5483",
                               "On pension",
                               "Other",
                               "Student",
                               "Entrepreneur",
                               "",
                               "Salary * Couple with children",
                               "Salary * Couple",
                               "Salary * Unknown",
                               "Salary * Other",
                               "Salary * Single with children"
)


ggamma_print$variable <- factor(ggamma_print$variable,
                                 levels = c("Intercept",
                                            "Age",
                                            "Sex",
                                            "House-dwel. structure",
                                            "Main activity",
                                            "Housing tenure",
                                            "Communal house-dwel.",
                                            "Education level",
                                            "Municip. class",
                                            "Immigrant status",
                                            "Unemp. duration",
                                            "Income decile",
                                            "Salary",
                                            "Capital income",
                                            "Housing allowance",
                                            "Labour market subsidy",
                                            "Basic unemp.",
                                            "Earnings-rel. unemp.",
                                            "Pension",
                                            "Student aid",
                                            "SISU soc.assist. categ.",
                                            "Family's benefits",
                                            "Other social benefits",
                                            "House-dwel. structure * Salary"
                                 )
)
ggamma_print <- ggamma_print[order(ggamma_print$variable),]
rownames(ggamma_print) <- NULL
ggamma_print$variable <- as.character(ggamma_print$variable)
ggamma_print <- ggamma_print %>% 
  select(variable, coefficient, Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS)
ggamma_print <- ggamma_print %>% 
  group_by(variable) %>% 
  mutate(variable = replace(variable, duplicated(variable), ''),
         Rhat = format(round(Rhat, 2), nsmall = 2))

ggamma_print

ggamma_table <- kbl(ggamma_print,
                    format = "latex",
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", "l", rep("r", 5)),
                    linesep = c(""),
                    digits = 2,
                    longtable = TRUE,
                    col.names = c("Variable", "Levels", "Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
                    caption = "Posterior means, standard errors, $\\hat{R}$-values, bulk-ESS and tail-ESS values of the regression coefficients from the generalized gamma regression model
    which was developed as the continuous component of the two-part model. Estimates are on the scale of the linear predictor (log). Detailed covariate descriptions are provided in the Appendix \\ref{tab:app_cov}.") %>% 
  column_spec(1, bold = TRUE, width = "3cm") %>% 
  column_spec(2, width = "3.5cm") %>% 
  column_spec(4:5, width = "1cm") %>% 
  column_spec(c(3, 6, 7), width = "1.5cm") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(ggamma_table,
    file = file.path(res_path, "table-app_ggamma.txt"))

ggamma_shape <- ggamma_shape %>% 
  select(Estimate, Est.Error, Rhat, Bulk_ESS, Tail_ESS) %>% 
  mutate(Rhat = format(round(Rhat, 2), nsmall = 2))
rownames(ggamma_shape) <- c("$\\beta$", "$\\delta$")

ggammashape_table <- kbl(ggamma_shape,
                          format = "latex",
                          booktabs = TRUE,
                          escape = FALSE,
                          linesep = c(""),
                          digits = 2,
                          longtable = TRUE,
                          label = "app_gammashape",
                          col.names = c("Estimate", "SE", "$\\hat{R}$", "Bulk-ESS", "Tail-ESS"),
                          caption = "Posterior mean, standard error, $\\hat{R}$-value, bulk-ESS and tail-ESS value of the scale ($\\beta$) and shape ($\\delta$) parameters from the generalized gamma regression model
    which was developed as the continuous component of the two-part model.") %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(ggammashape_table,
    file = file.path(res_path, "table-app_ggammashape.txt"))
