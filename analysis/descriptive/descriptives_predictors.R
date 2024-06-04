
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(kableExtra)

# load data set ----
reg19 <- readRDS(file.path("no_vc",
                           "cache",
                           "sisu_est_register_data_2019_tidy.Rds"))

# relevel factors for visualisation ----

reg19$fikavuv <- relevel(reg19$fikavuv, ref = "0-18") # age
reg19$paatoim <- factor(reg19$paatoim,
                        levels = c("tyollinen",
                                   "pitkaaik_tyoton",
                                   "opiskelija",
                                   "elakelainen",
                                   "yrittaja",
                                   "muu"))
reg19$frake <- factor(reg19$frake,
                      levels = c("yksinasuva",
                                 "yksi_aik_ja_lapsia",
                                 "2_aikuista",
                                 "2_aik_ja_lapsia",
                                 "ei_tietoa",
                                 "muu"))

reg19$fpalkat <- with(reg19,
                     cut(palkat,
                         breaks = c(min(palkat), 500, 3000, 7000, 10000, 20000, max(palkat)),
                         include.lowest = T,
                         right = F,
                         dig.lab = 10)) %>% factor()

reg19$ftmmktukipv2 <- with(reg19,
                          cut(tmmktukipv_data_sum,
                              breaks = c(min(tmmktukipv_data_sum), 30, 60, 100, 200, 300, max(tmmktukipv_data_sum)),
                              include.lowest = T,
                              right = F,
                              dig.lab = 10)) %>% factor()

reg19$fasumtuet <- with(reg19,
                       cut(asumtuet_data,
                           breaks = c(min(asumtuet_data), 500, 1000, 3000, 5000, max(asumtuet_data)),
                           include.lowest = T,
                           right = F,
                           dig.lab = 10)) %>% factor()

# data sets ----

# osadata, jossa vain 2019 rekisteriaineiston mukaan toimeentulotukea saaneet
reg19_tt <- subset(reg19, reg19$data_kylla == 1)


# helper functions ----

calcProps <- function(data, var, d = 200) {
  df <- data %>% 
  group_by({{ var }}) %>%
  summarise(os = ((sum(data_kylla)/nrow(data))*100),
            q3 = quantile(toimtuki_data[toimtuki_data > 0], 0.75),
            q1 = quantile(toimtuki_data[toimtuki_data > 0], 0.25),
            upr_whisker = q3+(q3-q1)*1.5) %>% 
    ungroup() %>%
    as.data.frame()
  df <- df %>%
    mutate(y = upr_whisker[which.max(upr_whisker)]+d,
           ylim = y+300) %>%
    mutate_if(is.numeric, ~round(., 2))
  
  return(df) 
}

# local attributes ----
table_path <- file.path("output",
                        "2024-05-17_results",
                        "tables")
plot_path <- file.path("output",
                        "2024-05-17_results",
                        "plots")
label_size <- 2.5
linewidth <- 0.3
padding <- 0.15
xsize <- 6.5
yexpand <- c(0.03, 0.08)
xexpand <- 0.5
angle <- 40

# the sex of the reference person ----

sp_os <- calcProps(reg19, sp, d = 400)
sp_os$os <- format(sp_os$os, digits = 3)

sp_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = sp)) +
  geom_boxplot(
    #position = position_dodge(width = 1.5),
    width = 0.9,
    outlier.shape = NA,
    linewidth = linewidth) +
  geom_label(data = sp_os,
             mapping = aes(label = os, y = y),
             size = label_size,
             label.padding = unit(padding, "lines")) +
  scale_y_continuous(limits = c(0, sp_os$ylim[1]),
                     breaks = seq(0, sp_os$ylim[1], 2500),
                     labels = paste0(seq(0, sp_os$ylim[1]/1000, 2.5), "k"),
                     expand = expansion(mult = yexpand)
                     # expand = rep(0.02, 2)
                     ) +
  scale_x_discrete(labels = c("male", "female"),
                   expand = expansion(add = xexpand)
                   # expand = rep(0.25, 2)
                   ) +
  labs(#y = "Social assistance\n(€/year)",
       x = "Sex of the reference person") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize),
        axis.text.x = element_text(angle = angle, vjust = 1, hjust = 1),
        axis.title.y = element_blank())

# the age of the reference person ----

ika_os <- calcProps(reg19, fikavuv, d = 700)
ika_os$os <- format(ika_os$os, digits = 2)

ika_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = fikavuv)) +
  geom_boxplot(outlier.shape = NA,
               linewidth = linewidth) +
  geom_label(data = ika_os,
             mapping = aes(label = os, y = y),
             size = label_size,
             label.padding = unit(padding, "lines")) +
  scale_y_continuous(limits = c(0, ika_os$ylim[1]),
                     breaks = seq(0, ika_os$ylim[1], 2500),
                     labels = paste0(seq(0, ika_os$ylim[1]/1000, 2.5), "k"),
                     expand = expansion(mult = yexpand)
                     # expand = rep(0.01, 2)
                     ) +
  scale_x_discrete(labels = c("[0, 18)",
                              "[18, 25)",
                              "[25, 35)",
                              "[35, 45)",
                              "[45, 65)",
                              "[>65]"),
                   expand = expansion(add = xexpand)
                   # expand = rep(0.08, 2)
                   ) +
  labs(y = "Social assistance\n(€/year)",
       x = "Age of the reference person") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize),
        axis.text.x = element_text(angle = angle, vjust = 1.1, hjust=1))

# the main activity of the reference person ----

paatoim_os <- calcProps(reg19, paatoim, d = 1100)
paatoim_os$os <- format(paatoim_os$os, digits = 2)

paatoim_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = paatoim)) +
  geom_boxplot(outlier.shape = NA,
               linewidth = linewidth) +
  geom_label(data = paatoim_os,
             mapping = aes(label = os, y = y),
             size = label_size,
             label.padding = unit(padding, "lines")) +
  scale_y_continuous(limits = c(0, paatoim_os$ylim[1]),
                     breaks = seq(0, paatoim_os$ylim[1], 2500),
                     labels = paste0(seq(0, paatoim_os$ylim[1]/1000, 2.5), "k"),
                     expand = expansion(mult = yexpand)) +
  scale_x_discrete(labels = c(
    "employed",
    "long-term\nunemployed",
    "student",
    "on pension",
    "entre-\npreneur",
    "other"
  ),
    expand = expansion(add = xexpand)) +
  labs(y = "Social assistance\n(€/year)",
       x = "Main activity of the reference person\non the last day of the year") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize))

# family structure ----

rake_os <- calcProps(reg19, frake, d = 1000)
rake_os$os <- format(rake_os$os, digits = 2)

rake_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = frake)) +
  geom_boxplot(outlier.shape = NA,
               linewidth = linewidth) +
  geom_label(data = rake_os,
            mapping = aes(label = os, y = y),
            size = label_size,
            label.padding = unit(padding, "lines")) +
  scale_y_continuous(limits = c(0, rake_os$ylim[1]),
                     breaks = seq(0, rake_os$ylim[1], 2500),
                     labels = paste0(seq(0, rake_os$ylim[1]/1000, 2.5), "k"),
                     expand = expansion(mult = yexpand)
                     ) +
  scale_x_discrete(labels = c(
    "single",
    "single with\nchildren",
    "couple",
    "couple with\nchildren",
    "other",
    "unknown"
    ),
    expand = expansion(add = xexpand)
    ) +
  labs(y = "Social assistance\n(€/year)",
       x = "House-dwelling unit structure") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize),
        axis.text.x = element_text(angle = angle, vjust = 1, hjust = 1))

# labour market support ----

tmmtuki_os <- calcProps(reg19, ftmmktukipv2, d = 1500)
tmmtuki_os$os <- format(tmmtuki_os$os, digits = 2)

tmmtuki_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = ftmmktukipv2)) +
  geom_boxplot(outlier.shape = NA,
               linewidth = linewidth) +
  geom_label(data = tmmtuki_os,
             mapping = aes(label = os, y = y),
             size = label_size,
             label.padding = unit(padding, "lines")) +
  scale_y_continuous(limits = c(0, tmmtuki_os$ylim[1]),
                     breaks = seq(0, tmmtuki_os$ylim[1], 2500),
                     labels = paste0(seq(0, tmmtuki_os$ylim[1]/1000, 2.5), "k"),
                     expand = expansion(mult = yexpand)) +
  scale_x_discrete(labels = c("[<30)",
                              "[30, 60)",
                              "[60, 100)",
                              "[100, 200)",
                              "[200, 300)",
                              "[>300]"),
                   expand = expansion(add = xexpand)) +
  labs(#y = "Social assistance\n(1000€/year)",
       x = "Total labour market subsidy per\nhouse-dwelling unit (number of days)") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize),
        axis.text.x = element_text(angle = angle, vjust = 1.1, hjust=1),
        axis.title.y = element_blank())

# housing allowance ----

asumtuet_os <- calcProps(reg19, fasumtuet, d = 1300)
asumtuet_os$os <- format(asumtuet_os$os, digits = 2)

asumtuet_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = fasumtuet)) +
  geom_boxplot(outlier.shape = NA,
               linewidth = linewidth) +
  geom_label(data = asumtuet_os,
             mapping = aes(label = os, y = y),
             size = label_size,
             label.padding = unit(padding, "lines")) +
  scale_y_continuous(limits = c(0, asumtuet_os$ylim[1]),
                     breaks = seq(0, asumtuet_os$ylim[1], 2500),
                     labels = paste0(seq(0, asumtuet_os$ylim[1]/1000, 2.5), "k"),
                     expand = expansion(mult = yexpand)) +
  scale_x_discrete(labels = c("[0, 0.5k)",
                              "[0.5k, 1k)",
                              "[1k, 3k)",
                              "[3k, 5k)",
                              "[>5k]"),
                   expand = expansion(add = xexpand)) +
  labs(y = "Social assistance\n(1000€/year)",
       x = "Total housing allowance per\nhouse-dwelling unit (1000€/year)") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize),
        axis.text.x = element_text(angle = angle, vjust = 1.1, hjust=1))

# salary ----

palkat_os <- calcProps(reg19, fpalkat, d = 1200)
palkat_os$os <- format(palkat_os$os, digits = 2)

palkat_plot <- ggplot(reg19_tt, aes(y = toimtuki_data, x = fpalkat)) +
  geom_boxplot(outlier.shape = NA,
               linewidth = linewidth) +
  geom_label(
    data = palkat_os,
    mapping = aes(label = os, y = y),
    size = label_size,
    label.padding = unit(padding, "lines")) +
  scale_y_continuous(
    limits = c(0, palkat_os$ylim[1]),
    breaks = seq(0, palkat_os$ylim[1], 2500),
    labels = paste0(seq(0, palkat_os$ylim[1]/1000, 2.5), "k"),
    expand = expansion(mult = yexpand)) +
  scale_x_discrete(
    labels = c("[0, 0.5k)",
               "[0.5k, 3k)",
               "[3k, 7k)",
               "[7k, 10k)",
               "[10k, 20k)",
               "[>20k]"),
    expand = expansion(add = xexpand)) +
  labs(#y = "Social assistance\n(1000€/year)",
       x = "Total salary per\nhouse-dwelling unit (1000€/year)") +
  theme_bw() +
  theme(axis.text = element_text(size = xsize),
        axis.text.x = element_text(angle = angle, vjust = 1.1, hjust=1),
        axis.title.y = element_blank())


# report ----

desc_plot <-  paatoim_plot / (rake_plot + sp_plot) / (ika_plot + palkat_plot) / (asumtuet_plot + tmmtuki_plot) &
  theme(axis.title = element_text(size = 8),
        axis.text.x = element_text(lineheight = 0.7),
        plot.margin = unit(c(0, 0.12, 0, 0.05), "cm")) &
  plot_annotation(tag_levels = "i") 

ggsave(desc_plot,
       file = file.path(plot_path, "plot-desc_preds.pdf"),
       device = "pdf",
       units = "cm",
       height = 17.02,
       width = 12.13)

# appendix tables ----

formatForAppendix <- function(newdata) {
  
  app_data <- newdata %>% 
    select(
      sp,
      fikavuv,
      paatoim,
      frake,
      fpalkat,
      ftmmktukipv2,
      fasumtuet,
      koulasv,
      faslaji,
      fmuuttovvi,
      toimtuki_data,
      data_kylla)
  app_data <- app_data %>% 
    mutate(sp = factor(sp, levels = c("mies", "nainen"),
                       labels = c("Male", "Female")),
           fikavuv = factor(fikavuv, levels = levels(app_data$fikavuv),
                            labels = c("[0, 18)",
                                       "[18, 25)",
                                       "[25, 35)",
                                       "[35, 45)",
                                       "[45, 65)",
                                       "[>65]")),
                            #labels = c("0--18", "18--24", "25--34", "35--44", "45--64", "65 or older")),
           paatoim = factor(paatoim, levels = levels(app_data$paatoim),
                            labels = c("Employed", "Long-term unemployed", "Student", "On pension",
                                       "Entrepreneur", "Other")),
           fpalkat = factor(fpalkat, levels = levels(app_data$fpalkat),
                            labels = c("[0, 0.5k)",
                                       "[0.5k, 3k)",
                                       "[3k, 7k)",
                                       "[7k, 10k)",
                                       "[10k, 20k)",
                                       "[>20k]")),
                            #labels = c("0--499", "500--2999", "3000--6999", "7000--9999", "10000--19999", "20000 or more")),
           ftmmktukipv2 = factor(ftmmktukipv2, levels = levels(app_data$ftmmktukipv2),
                                 labels = c("[<30)",
                                            "[30, 60)",
                                            "[60, 100)",
                                            "[100, 200)",
                                            "[200, 300)",
                                            "[>300]")),
                                 #labels = c("30 or fewer", "30--59", "60--99", "100--199", "200--299", "300 or more")),
           fasumtuet = factor(fasumtuet, levels = levels(app_data$fasumtuet),
                              labels = c("[0, 0.5k)",
                                         "[0.5k, 1k)",
                                         "[1k, 3k)",
                                         "[3k, 5k)",
                                         "[>5k]")),
                              #labels = c("0--499", "500--999", "1000--2999", "3000--4999", "5000 or more")),
           koulasv = factor(koulasv, levels = levels(app_data$koulasv),
                            labels = c("Comprehensive", "Upper secondary", "Post-secondary non-tertiary",
                                       "Lowest tertiary", "Bachelor's or equiv.", "Masters or equiv. and second stage of tertiary")),
           faslaji = factor(faslaji, levels = c("omistusasunto", "osaomistus", "vuokra-asunto", "muu"),
                            labels = c("Owner-occupied", "Part-ownership", "Rented-dwelling", "Other")),
           fmuuttovvi = factor(fmuuttovvi, levels = levels(app_data$fmuuttovvi),
                               labels = c("No", "Yes")),
           frake = factor(frake, levels = levels(app_data$frake),
                          labels = c("Single",
                                     "Single with children",
                                     "Couple with children",
                                     "Couple",
                                     "Other",
                                     "Unknown"))
    )
  
  app_data <- app_data %>% 
    rename(`Sex of the reference person` = "sp",
           `Age of the reference person` = "fikavuv",
           `Main activity of the reference person on the last day of the year` = "paatoim",
           `House-dwelling unit structure` = "frake",
           `Total salary per house-dwelling unit (1000€/year)` = "fpalkat",
           `Total labour market subsidy per house-dwelling unit (number of days)` = "ftmmktukipv2",
           `Total housing allowance per house-dwelling unit (€/year)` = "fasumtuet",
           `Education level of the reference person` = "koulasv",
           `Housing tenure` = "faslaji",
           `Immigration status of the reference person` = "fmuuttovvi",
           `Annual soc. assist.` = toimtuki_data,
           `Soc. assist. recipient` = data_kylla)
  app_data <- app_data %>% 
    pivot_longer(cols = !c(`Annual soc. assist.`, `Soc. assist. recipient`),
                 values_to = "Level",
                 names_to = "Variable")
  
  app_data$Variable <- factor(app_data$Variable,
                              levels = c("Sex of the reference person",
                                         "Age of the reference person",
                                         "House-dwelling unit structure",
                                         "Housing tenure",
                                         "Main activity of the reference person on the last day of the year",
                                         "Education level of the reference person",
                                         "Immigration status of the reference person",
                                         "Total salary per house-dwelling unit (1000€/year)",
                                         "Total housing allowance per house-dwelling unit (€/year)",
                                         "Total labour market subsidy per house-dwelling unit (number of days)"))
  
  return(app_data)
  
}

app_data_sa <- formatForAppendix(reg19_tt)

app_data_sum <- app_data_sa %>%
  mutate(variable = factor(Variable)) %>% 
  group_by(Variable, Level) %>% 
  summarise(mean = mean(`Annual soc. assist.`),
            q1 = quantile(`Annual soc. assist.`, 0.25),
            q3 = quantile(`Annual soc. assist.`, 0.75)
            )

app_data_reg <- formatForAppendix(reg19)
app_data_p <- app_data_reg %>%
  mutate(variable = factor(Variable)) %>% 
  group_by(Variable, Level) %>% 
  summarise(p = (sum(`Soc. assist. recipient`)/nrow(reg19))*100)

app_data <- left_join(app_data_sum, app_data_p, by = c("Variable", "Level"))
#print(app_data, n = 49)

index <- table(app_data$Variable)

pred_table <- kbl(app_data %>% ungroup()  %>% select(!c(Variable)),
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = c(""),
    digits = 2,
    longtable = TRUE,
    label = "app_preds",
    col.names = c("Variable", "Mean (€/year)", "Q1 (€/year)", "Q3 (€/year)", "Prop. of SA recipients (\\%)"),
    caption = "Mean, first quantile (Q1), third quantile (Q3) of annual social assistance, and proportion of social assistance recipients in groups
    defined by attributes of the house-dwelling units. Housing allowance refers to general housing allowance, housing
    allowance for students and housing allowance for pensioners.") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(5, width = "2.3cm") %>% 
  pack_rows(index = index) %>% 
  kable_styling(
    font_size = 10,
    latex_options = c("repeat_header", "hold_position"))

cat(pred_table,
    file = file.path(table_path, "table-app_predictors.txt"))
