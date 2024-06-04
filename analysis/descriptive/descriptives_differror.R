
library(dplyr)
library(ggplot2)
library(ggthemes)

calcPlotDimPx <- function(pagedim, s) {
  if (pagedim == "width") {
    res <- 2480*s
  }
  if (pagedim == "height") {
    res <- 3508*s
  }
  return(res)
}

res_path <- file.path("output",
                      "2024-05-17_results",
                      "plots")

# data sets ----

reg19 <- readRDS(file.path("no_vc",
                           "cache",
                           "sisu_est_register_data_2019_tidy.Rds"))


predictors <- list(
  knro = c("knro"),
  erotus = c("erotus"),
  data_kylla = c("data_kylla"),
  luokka = c("luokka"),
  ika = c("fikavuv"),
  sp = c("sp"),
  askunt_rake = c("frake"),
  sosek_asema = c("paatoim",
                  "fpaasoss"),
  koulu_aste = c("koulasv"),
  aslaji = c("faslaji"),
  muutto_vuosi = c("fmuuttovvi"),
  kuntaryhma = c("kuntar"),
  yhteis_tal = c("fmuuaik"),
  tyoton_kk = c("tyot"),
  tuloluokka = c("desmod"),
  palkka = c("palkat"),
  paaomatulo = c("paaomatulo"),
  tmmktuki = c("tmmktukipv_data_sum"),
  peruspr = c("perusprpv_data_sum"),
  ansiopr = c("ansioprpv_data_sum"),
  asumtuki = c("asumtuet_simul"),
  lapsilisa = c("lapsip_data"),
  elake = c("kansel_perhel_data"),
  opintotuki = c("vopintukikk"),
  muutsoset = c("muusoset_data_sum"),
  totu_simul = c("toimtuki_simul"),
  totu_perustt = c("perustt"),
  perustt_kylla = c("perustt_kylla"))

reg19 <- reg19 %>% select(all_of(unlist(predictors)),
                                     toimtuki_data)
reg19 <- reg19 %>% 
  group_by(knro) %>% 
  mutate(etuus_summa = sum(palkka, paaomatulo, tmmktuki,
                           peruspr, ansiopr, asumtuki, lapsilisa,
                           elake, opintotuki, muutsoset)
  )

# Perustoimeentulotukea saaneet
reg19_tp <- subset(reg19, reg19$perustt_kylla == 1)
# Toimeentulotukea saaneet
reg19_tt <- subset(reg19, reg19$data_kylla == 1)


# Osadata, josta poistettu ei toimeentulotuen saajat sekä aineistossa että simuloidussa datassa
reg19_pos <- subset(reg19, reg19$luokka != "ei_tukea")
reg19_pos$luokka <- factor(reg19_pos$luokka,
                    levels = c("vain_data", "data_simul", "vain_simul", "ei_tukea"))

# Osadata, jossa vain toimeentulotuen saajat
totu_tp <- subset(reg19, reg19$data_kylla == 1)


# Toimeentulotuki ----

sa_dist <- ggplot(reg19_tt, aes(x = toimtuki_data)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(0, 15000),
                     breaks = seq(0, 15000, 2500),
                     labels = paste0(seq(0, 15, 2.5), "k"),
                     expand = expansion(add = 0)
                     ) +
  scale_y_continuous(
    expand = expansion(mult = 0.01)
  ) +
  labs(y = "Number of\nhouse-dwelling units",
       x = "Social assistance (1000€/year)",
       caption = paste(nrow(reg19_tt) - sum(reg19_tt$toimtuki_data > 15000), "observations")) +
  theme_bw(base_size = 8) +
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        plot.caption = element_text(size = 8),
        plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))
sa_dist

ggsave(sa_dist,
       filename = file.path(res_path, "plot-desc_sisu-totudis.pdf"),
       device = "pdf",
       units = "px",
       height = calcPlotDimPx("height", 0.2),
       width = calcPlotDimPx("width", 0.45))

# Aineiston ja simuloidun toimeentulotuen erotus ----

diff_conf_plot <- ggplot(reg19_pos, aes(x = erotus, fill = luokka)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(
    limits = c(-15000, 15000),
    breaks = seq(-15000, 15000, 5000),
    labels = paste0(seq(-15, 15, 5), "k"),
    expand = rep(0.01, 2)) +
  labs(y = "Density",
       x = "The difference between the register data set and the SISU model's\nestimated basic social assistance (1000€/year)",
       caption = paste(nrow(reg19_tp) - sum(reg19_pos$erotus < -15000 | reg19_pos$erotus > 15000), "observations")) +
  scale_fill_discrete(name = "Classification\noutcome", 
                      labels = c("False\nnegative", "True\npositive", "False\npositive"),
                      type =  c("black", "dark gray", "azure1")) +
  # scale_fill_grey(name = "Classification\noutcome",
  #                       labels = c("False\nnegative", "True\npositive", "False\npositive")
  #                 ) +
  # scale_linetype_manual(name = "Classification\noutcome",
  #                       values = c(1, 2, 3),
  #                       labels = c("True\npositive", "False\nnegative", "False\npositive")) +
  theme_bw() +
  theme(
    text = element_text(size = 8.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8),
    legend.position = "top",
    plot.margin = unit(c(0, 0.7, 0, 0.2), "cm")) #TRBL

diff_conf_plot
ggsave(diff_conf_plot,
       filename = file.path(res_path, "plot-desc_sisu-diff.pdf"),
       device = "pdf",
       units = "px",
       height = calcPlotDimPx("height", 0.25),
       width = calcPlotDimPx("width", 0.45))


# Poistetaan lapset ja ei-etuuksia saavat
reg19_pos$vulner <- ifelse(reg19_pos$etuus_summa > 0 & reg19_pos$aslaji != "muu" & reg19_pos$sosek_asema2 != "tyoton_muu", 1, 0)
reg19_pos$vulner <- factor(reg19_pos$vulner)
reg19_pos2 <- reg19_pos %>% filter(luokka == "vain_simul")

summary(reg19_pos2)

diff_conf_plot2 <- ggplot(reg19_pos2, aes(x = erotus, fill = vulner)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(
    limits = c(-30000, 30000),
    breaks = seq(-30000, 30000, 10000),
    labels = paste0(seq(-30, 30, 10), "k")) +
  labs(y = "Density",
       x = "Difference of the register data set and the SISU-model's\nestimated basic social assistance (1000€/year)",
       caption = paste0("Source: 2019 register data set (N = ", nrow(reg19), ")")) +
  theme_bw() +
  scale_fill_colorblind(name = "group",
                        labels = c("marginal", "non-marginal")) +
  theme(legend.position = "top")

diff_conf_plot2

reg19_pos3 <- reg19_pos %>%
  filter(etuus_summa <= 0)


t <- qt(0.975, nrow(toimtuki)-1)
diff_desc <- toimtuki %>% 
  summarise(mean = mean(erotus),
            median = median(erotus),
            sd = sd(erotus),
            lwr_ci = mean - t*sd,
            upr_ci = mean + t*sd)
diff_desc
# Eli keskimäärin toimeentulotuki aineistossa on isompaa kuin simuloitu toimeentulotuki

quantile(toimtuki$erotus)
# Suurinosa virheistä on alle 1800 euroa

diff_desc_luokat <- toimtuki %>% group_by(luokka) %>% summarise(median = median(erotus),
                                                                mean = mean(erotus),
                                                                sd = sd(erotus))
diff_desc_luokat

# Ikä ----

ggplot(toimtuki, aes(x = erotus, fill = luokka)) +
  geom_density(alpha = 0.6) +
  facet_grid(rows = vars(fikavuv))

# Asuntokunnan rakenne ------

ggplot(toimtuki, aes(x = erotus, fill = luokka)) +
  geom_density(alpha = 0.6) +
  facet_grid(rows = vars(frake))

# Asunnon  hallintaperuste ----

ggplot(toimtuki, aes(x = erotus, fill = luokka)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~faslaji)

# Päätoimeentulo ----

ggplot(toimtuki, aes(x = erotus, fill = luokka)) +
  geom_density(alpha = 0.6) +
  facet_grid(rows = vars(paatoim))

# Havaitaan, että työttömillä, ketkä eivät saa ensisijaisia tukia
# on paljon yksinasuvia, ja lisäksi ryhmä joiden kotitalouden rakennetta
# ei tunneta. Ko. ryhmä näyttäisi siis selittävän hyvin toista erotuksen huippua
table(toimtuki$paatoim, toimtuki$frake)

# Työmarkkinatuki päivät per asuntokunnan aikuinen ----

ggplot(toimtuki, aes(x = tmmktukipv_kt_ka, y = erotus, color = luokka)) +
  geom_point() +
  labs(y = "Tuki aineisto - Tuki simuloitu",
       x = "Työmarkkinatukipäivät / asuntokunnan aikuinen")

# Ansiopäiväraha päivät per asuntokunnan aikuinen ----

ggplot(toimtuki, aes(x = ansiopv_pv, y = erotus, color = luokka)) +
  geom_point() +
  labs(y = "Tuki aineisto - Tuki simuloitu",
       x = "Ansiopäiväraha päivät / asuntokunnan aikuinen")

# Peruspäiväraha päivät per asuntokunnan aikuinen ----

ggplot(toimtuki, aes(x = peruspr_pv, y = erotus, color = luokka)) +
  geom_point() +
  labs(y = "Tuki aineisto - Tuki simuloitu",
       x = "Ansiopäiväraha päivät / asuntokunnan aikuinen")

# Asumistuki per asuntokunta ----

ggplot(toimtuki, aes(x = ASUMTUET_DATA, y = erotus, color = luokka)) +
  geom_point(alpha = 0.4) +
  labs(y = "Tuki aineisto - Tuki simuloitu",
       x = "Asumistuki / asuntokunnan aikuinen")

# Muut sosiaalietuudet yhteensä / asuntokunnnan aikuinen ----

ggplot(toimtuki, aes(x = muusoset, y = erotus, color = luokka)) +
  geom_point() +
  labs(y = "Tuki aineisto - Tuki simuloitu",
       x = "Asumistuki / asuntokunnan aikuinen")
