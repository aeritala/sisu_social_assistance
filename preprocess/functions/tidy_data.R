# Tidy data set

suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))

# Set up ----
sisu_data <- readRDS(file.path(
  "no_vc",
  "raw",
  "sisu_est_register_data_2019.Rds"
))
register_data <- readRDS(file.path(
  "no_vc",
  "raw",
  "register_data_2019.Rds"
))

# Helper functions ----
calcBreaks <- function(mydata, myvar, filtervar = NULL, q = 0.1, dig = 0) {
  
  if (length(filtervar) > 0) {
    qt_data <- mydata %>%
      filter(.data[[filtervar]] != "ei_tukea")
  } else {
    qt_data <- mydata
  }
  
  my_qt <- qt_data[[myvar]] %>%
    quantile(seq(0, 1, q),
             names = F) %>%
    round(., digits = dig) %>% 
    unique()
  if (my_qt[length(my_qt)] < max(mydata[[myvar]])) my_qt[length(my_qt)] <- max(mydata[[myvar]])
  if (my_qt[1] > min(mydata[[myvar]])) my_qt <- c(min(mydata[[myvar]]), my_qt)
  
  return(my_qt)
}

createFactor <- function(mydata, myvar, filtervar = NULL, q = 0.1, right = FALSE,
                         dig = 0, diglab = 10, labs = NULL) {
  
  if (is.null(labs)) {
    qt <- calcBreaks(mydata, myvar, filtervar, q, dig)
  } else {
    qt <- labs
  }
  
  myfactor <- cut(mydata[[myvar]],
                  qt,
                  dig.lab = diglab,
                  right = right,
                  include.lowest = TRUE) %>% factor()
  
  return(myfactor)
}


# Alkuasetukset ----

# Numeric variables NA:s to zero, because in the data set NA means 0
sisu_data <- sisu_data %>% mutate(across(where(is.numeric, ~replace(., is.na(.), 0))))

# Indicator variables to integers
sisu_data <- sisu_data %>%
  mutate_at(c("knro"), as.integer) %>% 
  mutate_if(vars(all(. %in% 0:1)), as.integer)

# Toimeentulotuki
sisu_data$TOIMTUKI_DATA <- ifelse(sisu_data$TOIMTUKI_DATA < 1, 0, sisu_data$TOIMTUKI_DATA)

# Aineiston ja simuloidun toimeentulotuen erotus ----

# N.B. SISU-malli estimoi vain perustoimeentulotuen!
perustt <- register_data %>% 
  select(knro, perustt) %>% 
  group_by(knro) %>% 
  summarise(perustt = sum(perustt))
perustt <- perustt %>%
  mutate(perustt = ifelse(perustt < 1, 0, perustt),
         perustt_kylla = factor(ifelse(perustt != 0, 1, 0)))

sisu_data <- left_join(sisu_data, perustt, by = "knro")
sisu_data$erotus <- sisu_data$perustt - sisu_data$TOIMTUKI_SIMUL

rm(perustt)

# Sekaannusluokittelu ----

sisu_data$data_kylla <- ifelse(sisu_data$TOIMTUKI_DATA != 0, 1, 0) %>% as.integer()
sisu_data$fdata_kylla <- factor(sisu_data$data_kylla)
sisu_data$simul_kylla <- ifelse(sisu_data$TOIMTUKI_SIMUL != 0, 1, 0) %>% as.integer()
sisu_data$fsimul_kylla <- factor(sisu_data$simul_kylla)

sisu_data <- sisu_data %>%
  mutate(
    luokka = case_when(
      perustt_kylla == 1 & simul_kylla == 0 ~ "vain_data",
      perustt_kylla == 0 & simul_kylla == 1 ~ "vain_simul",
      perustt_kylla == 1 & simul_kylla == 1 ~ "data_simul",
      perustt_kylla == 0 & simul_kylla == 0 ~ "ei_tukea"
    )
  )
sisu_data$luokka <- factor(sisu_data$luokka)

# Indikaattorimuuttuja onko SISU-malli ennustanut oikein ----

sisu_data$simul_cor <- ifelse(sisu_data$luokka == "ei_tukea" |
                                sisu_data$luokka == "data_simul", 1, 0) %>%
  factor

# Viitehenkilon ika ----

sisu_data$fikavuv <- cut(sisu_data$ikavuv,
                         c(0, 18, 25, 35, 50, 65, Inf),
                         c("0-18",
                           "18-25",
                           "25-35",
                           "35-45",
                           "45-65",
                           ">65"), 
                         include.lowest=T) %>% factor
sisu_data$fikavuv <- relevel(sisu_data$fikavuv, ref = "18-25")

# Viitehenkilon sukupuoli ----

sukup <- register_data %>%
  select(knro, asko, sp) %>%
  filter(asko == 1)
sukup$sp <- factor(sukup$sp,
                   levels = c(1, 2),
                   labels = c("mies", "nainen"))

sisu_data <- left_join(sisu_data, sukup, by = "knro")

rm(sukup)

# Asuntokunnan rakenne ----

sisu_data <- sisu_data %>%
  mutate(frake = case_when(
    rake == 10 ~ "yksinasuva",
    rake %in% seq(21, 61, 10) ~ "yksi_aik_ja_lapsia",
    rake == 22 ~ "2_aikuista",
    rake %in% seq(32, 62, 10) ~ "2_aik_ja_lapsia",
    rake %in% c(33, 43, 44, 53, 54, 55, 63, 64, 65, 66) ~ "muu",
    rake == 99 ~ "ei_tietoa"
  ))
sisu_data$frake <- factor(sisu_data$frake)
sisu_data$frake <- relevel(sisu_data$frake, ref = "yksinasuva")

# Sosioekonominen asema ----

sisu_data <- sisu_data %>% 
  mutate(paatoim_sisu = case_when(
    paasoss %in% c(10, 20) ~ "yrittaja",
    paasoss %in% c(30, 40, 50) ~ "tyollinen",
    paasoss == 60 ~ "opiskelija",
    paasoss == 70 ~ "elakelainen",
    paasoss == 80 ~ "muu")
  )
sisu_data$paatoim_sisu <- factor(sisu_data$paatoim_sisu)
sisu_data$paatoim_sisu <- relevel(sisu_data$paatoim_sisu, ref = "tyollinen")

sisu_data$fpaasoss <- factor(sisu_data$paasoss,
                             levels = seq(10, 80, 10),
                             labels = c("maatalouden yrittaja",
                                        "pienyrittaja",
                                        "ylempi toimihenkilo",
                                        "alempi toimihenkilo",
                                        "tyontekija",
                                        "opiskelija",
                                        "elakelainen",
                                        "tyoton_muu")
)
sisu_data$fpaasoss <- relevel(sisu_data$fpaasoss, ref = "tyontekija")

# Koulutusaste ----

sisu_data$koulasv <- ifelse(sisu_data$koulasv == 8, 7, sisu_data$koulasv)
sisu_data$koulasv <- factor(sisu_data$koulasv)

# Aikuisten lukumaara ----

register_data$aikuinen <- ifelse(register_data$ikavu >= 18, 1, 0)

n_aikuinen <- register_data %>%
  group_by(knro) %>%
  summarise(n_aikuinen = sum(aikuinen))

sisu_data <- left_join(sisu_data, n_aikuinen, by = "knro")
sisu_data <- sisu_data %>% 
  mutate(fn_aikuinen = case_when(
    n_aikuinen == 0 ~ "0",
    n_aikuinen == 1 ~ "1",
    n_aikuinen == 2 ~ "2",
    n_aikuinen == 3 ~ "3",
    n_aikuinen >= 4 ~ "4+",
  ))
sisu_data$fn_aikuinen <- factor(sisu_data$fn_aikuinen)

rm(n_aikuinen)

register_data$lapsi <- ifelse(register_data$ikavu < 18, 1, 0)

n_lapsi <- register_data %>%
  group_by(knro) %>%
  summarise(n_lapsi = sum(lapsi))

sisu_data <- left_join(sisu_data, n_lapsi, by = "knro")

rm(n_lapsi)

# Lasten lukumaara ----


# Yhteistalous indikaattori

register_data <- register_data %>% 
  mutate(muu_aik = ifelse(!(asko %in% 1:2) & aikuinen == 1,
                          1,
                          0))

n_muuaik <- register_data %>% 
  group_by(knro) %>% 
  summarise(n_muuaik = sum(muu_aik))
n_muuaik <- n_muuaik %>% 
  mutate(muuaik = ifelse(n_muuaik > 0,
                         1,
                         0),
         fmuuaik = factor(muuaik,
                          levels = c(0,1),
                          labels = c("ei", "kylla"))) %>% 
  select(knro, muuaik, fmuuaik)

sisu_data <- left_join(sisu_data, n_muuaik, by = "knro")

rm(n_muuaik)

# Maahanmuuttaja status indikaattori ----

register_data <- register_data %>% 
  mutate(muuttovvi = ifelse(is.na(muuttovv),
                            0,
                            1))

muuttov <- register_data %>%
  select(knro, asko, muuttovvi) %>% 
  group_by(knro) %>% 
  filter(asko == 1) %>% 
  select(knro, muuttovvi) %>% 
  mutate(fmuuttovvi = factor(muuttovvi,
                             levels = c(0,1),
                             labels = c("ei", "kylla")))

sisu_data <- left_join(sisu_data, muuttov, by = "knro")

rm(muuttov)

# Maakunta ----

# Pohjana muuttuja asuntotuen kuntaryhma vuonna 2015

# Jokaisella asuntokunnalla on vain yksi kuntaryhma
# register_data %>% select(knro, astukikr_l15) %>% group_by(knro) %>% summarise(count = n_distinct(astukikr_l15)) %>% nrow()

maakunta <- register_data %>%
  select(knro, astukikr_l15) %>%
  group_by(knro) %>% 
  summarise(kuntar = min(astukikr_l15))
maakunta$kuntar <- factor(maakunta$kuntar,
                          levels = 1:4,
                          labels = c("helsinki",
                                     "muu_pkseutu",
                                     "keskisuuret_kaup",
                                     "muut_kunnat"))
sisu_data <- left_join(sisu_data, maakunta, by = "knro")

rm(maakunta)

# Asunnon hallintaperuste ----

# Jokaisella asuntokunnalla on vain yksi asumisen muoto.
# register_data %>% select(knro, aslaji) %>% group_by(knro) %>% summarise(count = n_distinct(aslaji)) %>% nrow()

aslaji <- register_data %>%
  select(knro, aslaji) %>%
  group_by(knro) %>%
  summarise(aslaji = min(aslaji))
aslaji <- aslaji %>%
  mutate(faslaji = case_when(
    aslaji %in% 1:3 ~ "omistusasunto",
    aslaji %in% 4:5 ~ "vuokra-asunto",
    aslaji == 6 ~ "osaomistus",
    aslaji == 7 ~ "palveluasunto",
    aslaji == 8 ~ "muu"
  ))
aslaji$faslaji <- factor(aslaji$faslaji)

sisu_data <- left_join(sisu_data, aslaji, by = "knro")

rm(aslaji)

# Tyottomyyskuukaudet ----

# Viitehenkilon tyottomyyskuukaudet

tyotkkv <- register_data %>%
  filter(asko == 1) %>% 
  select(knro, tyot) %>% 
  mutate(tyot = as.double(tyot))
sisu_data <- left_join(sisu_data, tyotkkv, by = "knro")

rm(tyotkkv)

# Kotitalouden aikuisten tyottomyyskuukaudet per kotitalouden aikuisten lukumaara
tyotkk_kt <- register_data %>%
  select(knro, tyot, aikuinen) %>%
  group_by(knro) %>%
  summarise(tyotkk_kt_ka = ifelse(sum(aikuinen) != 0,
                                  sum(tyot)/sum(aikuinen),
                                  sum(tyot)))

# Huom. kotitaloudessa 2291176 on alle 18 vuotias, jolle merkattu 4 kuukautta
# ty?tt?myyskuukausia. Nuoren sosioekonominen asema on kuitenkin opiskelija, eik?
# h?n ole saanut ty?tt?myysturvaa, joten oletetaan tyottomyyskuukausiksi 0.
# https://kela.fi/tyottomyysturvan-ehtoja-alle-25-vuotiaille-hakijoille
# register_data %>%
#   select(knro, tyot, aikuinen, soss, ikavu, tmtukimk, tmtukipv, htyotper, VVVMKQ) %>%
#   filter(knro == 2291176)
#
tyotkk_kt$tyotkk_kt_ka[tyotkk_kt$knro == 2291176] <- 12

#sisu_data$tyotkk_kt <- tyotkk_kt$tyotkk_kt_ka
sisu_data <- left_join(sisu_data, tyotkk_kt, by = "knro")

rm(tyotkk_kt)

# Tulokymmenys ----

sisu_data$desmod <- as.numeric(sisu_data$desmod)

# Kaytettavissa olevat rahatulot ----

# Estimoidut kaytettavissa olevat rahatulot
sisu_data$kaytrahatulo_simul2 <- sisu_data$KAYTRAHATULO_SIMUL

# Estimoidut kaytettavissa olevat rahatulot per kulutusyksikko
sisu_data$kaytrahatulo_simul_mod <- ifelse(sisu_data$kaytrahatulo_simul2 != 0,
                                           sisu_data$kaytrahatulo_simul2 / sisu_data$modoecd,
                                           sisu_data$kaytrahatulo_simul2)
sisu_data$fkaytrahatulo_simul_mod <- createFactor(sisu_data,
                                                  "kaytrahatulo_simul_mod",
                                                  "luokka",
                                                  q = 0.2)

# kaytrahatulot_simul == 0 indikaattori
sisu_data$ikaytrahatulo_simul <- ifelse(sisu_data$kaytrahatulo_simul2 == 0,
                                        1,
                                        0) %>% factor

# Palkkatulot ----

# Palkat faktorina
sisu_data$fpalkat <- createFactor(sisu_data,
                                  "PALKAT",
                                  "luokka",
                                  q = 0.2)

# Palkkatulot per kulutusyksikko
sisu_data$palkat_aik <- ifelse(sisu_data$n_aikuinen != 0,
                               sisu_data$PALKAT / sisu_data$n_aikuinen,
                               sisu_data$PALKAT)
sisu_data$palkat1k_aik <- sisu_data$palkat_aik / 1000

# Palkkatulojen logaritmi
sisu_data <- sisu_data %>%
  mutate(logp1_palkat = log(PALKAT + 1))

# Paaomatulot ----

sisu_data$paaomatulo <- sisu_data$YRIT_POTULO + # Yritystulot ansiotuloina yhteens?
  sisu_data$SEKAL_POTULO + # Sekalaisia p??omatuloja yhteens?
  sisu_data$OSVEROVAP_DATA + # Verottomat osingot yhteens?
  sisu_data$OSINGOTA_DATA + # Ansiotulo-osingot yhteens?
  sisu_data$OSINGOTP_DATA # P??omatulo-osingot yhteens?

# Alkuperaiset paaomatulot
sisu_data$fpaaomatulo <- createFactor(sisu_data,
                                      "paaomatulo",
                                      q = 0.2)

sisu_data$fpaaomatulo2 <- cut(sisu_data$paaomatulo,
                              c(0, 10, 1000, 5000, 9200000),
                              dig.lab = 10,
                              right = FALSE,
                              include.lowest = TRUE) %>% factor()

# Paaomatulot per kulutusyksikko
sisu_data$paaomatulo_mod <- ifelse(sisu_data$modoecd != 0,
                                   sisu_data$paaomatulo / sisu_data$modoecd,
                                   sisu_data$paaomatulo)
sisu_data$fpaaomatulo_mod <- createFactor(sisu_data,
                                          "paaomatulo_mod",
                                          q = 0.2)

sisu_data$fpaaomatulo_mod2 <- cut(sisu_data$paaomatulo_mod,
                                  c(0, 10, 1000, 5000, 9200000),
                                  dig.lab = 10,
                                  right = FALSE,
                                  include.lowest = TRUE) %>% factor()


# paaomatulo == 0 indikaattori
sisu_data$ipaaomatulo <- ifelse(sisu_data$paaomatulo == 0,
                                1,
                                0) %>% factor

# Tyottomyysturva ----

# Tyomarkkinatuki

# HUOM. Tyomarkkinatuken maara voi saada negatiivisia arvoja ja tyomarkkinatuen paivat voi saada
# 365 paivaa suurempia arvoja

# Asuntokunnan saama tyomarkkinatuki (euroina vuodessa) per asuntokunnan aikuisten lukumaara
# ja asuntokunnan aikuisten saamien tyomarkkinatukipaivat per asuntokunnan aikuisten lukumaara
tmmktuki_kt <- register_data %>%
  select(knro, tmtukimk, tmtukipv, aikuinen) %>% 
  group_by(knro) %>%
  summarise(
    aik_sum = sum(aikuinen),
    tmmktuki_data_sum = sum(tmtukimk),
    tmmktuki_data_aik = ifelse(aik_sum > 0,
                               tmmktuki_data_sum / aik_sum,
                               tmmktuki_data_sum),
    tmmktukipv_data_sum = sum(tmtukipv),
    tmmktukipv_data_aik = ifelse(aik_sum > 0,
                                 tmmktukipv_data_sum / aik_sum,
                                 tmmktukipv_data_sum)) %>% 
  select(knro,
         tmmktuki_data_sum,
         tmmktuki_data_aik,
         tmmktukipv_data_sum,
         tmmktukipv_data_aik) %>% 
  mutate(tmmktukipv_data_sum = as.double(tmmktukipv_data_sum))

sisu_data <- left_join(sisu_data, tmmktuki_kt, by = "knro")

sisu_data <- sisu_data %>% 
  mutate(tmmktuki_simul_aik = ifelse(n_aikuinen != 0,
                                     TMTUKI_SIMUL / n_aikuinen,
                                     TMTUKI_SIMUL))

sisu_data$ftmmktukipv <- createFactor(sisu_data,
                                      "tmmktukipv_data_aik",
                                      "luokka",
                                      q = 0.05)
rm(tmmktuki_kt)

# Peruspaivaraha per asuntokunnan aikuinen
# ja peruspaivarahapaivat per asuntokunnan aikuinen

peruspr <- register_data %>% 
  select(knro, htyotper, palkm, aikuinen) %>%
  group_by(knro) %>%
  summarise(
    aik_sum = sum(aikuinen),
    peruspr_data_sum = sum(htyotper),
    peruspr_data_aik = ifelse(aik_sum > 0,
                              peruspr_data_sum/aik_sum,
                              peruspr_data_sum),
    perusprpv_data_sum = sum(palkm),
    perusprpv_data_aik = ifelse(aik_sum > 0,
                                perusprpv_data_sum/aik_sum,
                                perusprpv_data_sum)) %>% 
  select(knro,
         peruspr_data_sum,
         peruspr_data_aik,
         perusprpv_data_sum,
         perusprpv_data_aik) %>% 
  mutate(perusprpv_data_sum = as.double(perusprpv_data_sum))

sisu_data <- left_join(sisu_data, peruspr, by = "knro")

sisu_data$fperuspr_pv <- createFactor(sisu_data,
                                      "perusprpv_data_aik",
                                      "luokka",
                                      q = 0.03)

sisu_data <- sisu_data %>% 
  mutate(peruspr_simul_aik = ifelse(n_aikuinen != 0,
                                    PERUSPR_SIMUL / n_aikuinen,
                                    PERUSPR_SIMUL))
rm(peruspr)

# Ansiopaivaraha per kotitalouden aikuinen
# ja ansiopaivarhapaivat per kotitalouden aikuinen

ansiopr <- register_data %>%
  select(knro, VVVMKQ, VVVPVTQ, aikuinen) %>%
  group_by(knro) %>%
  summarise(
    aik_sum = sum(aikuinen),
    ansiopr_data_sum = sum(VVVMKQ),
    ansiopr_data_aik = ifelse(aik_sum > 0, 
                              ansiopr_data_sum / aik_sum,
                              ansiopr_data_sum),
    ansioprpv_data_sum = sum(VVVPVTQ),
    ansioprpv_data_aik = ifelse(aik_sum > 0, 
                                ansioprpv_data_sum / aik_sum,
                                ansioprpv_data_sum)) %>% 
  select(knro,
         ansiopr_data_sum,
         ansiopr_data_aik,
         ansioprpv_data_sum,
         ansioprpv_data_aik) %>% 
  mutate(ansioprpv_data_sum = as.double(ansioprpv_data_sum))

sisu_data <- left_join(sisu_data, ansiopr, by = "knro")

sisu_data$fansiopr_pv <- createFactor(sisu_data,
                                      "ansioprpv_data_aik",
                                      q = 0.05)

sisu_data <- sisu_data %>% 
  mutate(ansiopr_simul_aik = ifelse(n_aikuinen != 0,
                                    ANSIOPR_SIMUL / n_aikuinen,
                                    ANSIOPR_SIMUL))
rm(ansiopr)

# Asumistuki ----

# Opintotuen asumislisa, yleinen asumistuki, eläkkeensaajan asumistuki, palautukset vähennetty


# Rekisteriaineiston asumistuet per kulutustyksikko
sisu_data$asumtuet_data_mod <- ifelse(sisu_data$modoecd != 0,
                                      sisu_data$ASUMTUET_DATA / sisu_data$modoecd,
                                      sisu_data$ASUMTUET_DATA)
sisu_data$fasumtuet_data_mod <- createFactor(sisu_data,
                                             "asumtuet_data_mod",
                                             "luokka",
                                             q = 0.2)

sisu_data$asumtuet_simul_mod <- ifelse(sisu_data$modoecd != 0,
                                       sisu_data$ASUMTUET_SIMUL / sisu_data$modoecd,
                                       sisu_data$ASUMTUET_SIMUL)

# ASUMTUET_DATA == 0 indikaattori
sisu_data$iasumtuet_data <- ifelse(sisu_data$ASUMTUET_DATA == 0,
                                   1,
                                   0) %>% factor

# Lapsilisat, aitiysavustus, ja elatustuki yhteensa ----


# Aineiston lapsilisat, aitiysavustus, ja elatustuki yhteensa per lapsien lukumÃ¤Ã¤rÃ¤
sisu_data$lapsip_data_lkm <- ifelse(sisu_data$n_lapsi > 0,
                                    sisu_data$LAPSIP_DATA / sisu_data$n_lapsi,
                                    sisu_data$LAPSIP_DATA)

# Estimoidut lapsilisat, aitiysavustus, ja elatustuki yhteensa per lapsien lukumÃ¤Ã¤rÃ¤
sisu_data$lapsip_simul_lkm <- ifelse(sisu_data$n_lapsi > 0,
                                     sisu_data$LAPSIP_SIMUL / sisu_data$n_lapsi,
                                     sisu_data$LAPSIP_SIMUL)

# Aineiston lapsilisat, aitiysavustus, ja elatustuki yhteensa per kulutusyksikko
sisu_data$lapsip_data_mod <- ifelse(sisu_data$modoecd != 0,
                                    sisu_data$LAPSIP_DATA / sisu_data$modoecd,
                                    sisu_data$LAPSIP_DATA)
sisu_data$flapsip_data_mod <- createFactor(sisu_data,
                                           "lapsip_data_mod",
                                           "luokka",
                                           q = 0.1)

# Estimoidut lapsilisat, aitiysavustus, ja elatustuki yhteensa per kulutusyksikko
sisu_data$lapsip_simul_mod <- ifelse(sisu_data$modoecd != 0,
                                     sisu_data$LAPSIP_SIMUL / sisu_data$modoecd,
                                     sisu_data$LAPSIP_SIMUL)
sisu_data$flapsip_simul_mod <- createFactor(sisu_data,
                                            "lapsip_simul_mod",
                                            "luokka",
                                            q = 0.1)


# Estimoitu lapsilisa, aitiysavustus, ja elatustuki yhteensa osuutena
# estimoiduista kaytettavissa olevista rahatuloista
# N.B. jÃ¤rjettÃ¶miÃ¤ arvoja, joille osuus > 1
sisu_data$lapsip_simul_os <- ifelse(sisu_data$kaytrahatulo_simul2 != 0,
                                    sisu_data$LAPSIP_SIMUL / sisu_data$kaytrahatulo_simul2,
                                    sisu_data$LAPSIP_SIMUL)
sisu_data$flapsip_simul_os <- createFactor(sisu_data,
                                           "lapsip_simul_os",
                                           "luokka",
                                           q = 0.1)

# Elake ----

# Rekisteriaineiston kansanelakkeet ja perhe-elakkeet per saajien lukumÃ¤Ã¤rÃ¤
sisu_data <- sisu_data %>% 
  mutate(elake_data_mod = ifelse(modoecd > 0, 
                                 KANSEL_PERHEL_DATA / modoecd,
                                 KANSEL_PERHEL_DATA),
         elake_simul_mod  = ifelse(modoecd > 0, 
                                   KANSEL_PERHEL_SIMUL / modoecd,
                                   KANSEL_PERHEL_SIMUL))

# SISU-mallin estimoidut kansanelakkeet ja perhe-elakkeet per saajien lukumÃ¤Ã¤rÃ¤


# Rekisteriaineiston kansanelakkeet ja perhe-elakkeet yhteensa
sisu_data$felake_data <- createFactor(sisu_data,
                                      "KANSEL_PERHEL_DATA",
                                      "luokka",
                                      q = 0.05)

# Estimoidut kansanelakkeet ja perhe-elakkeet yhteensa
sisu_data$felake_simul <- createFactor(sisu_data,
                                       "KANSEL_PERHEL_SIMUL",
                                       "luokka",
                                       q = 0.05)

# Opintotuki ----

# Opintotuen mÃ¤Ã¤rÃ¤ ja skaalaus opintotuen saajien lukumÃ¤Ã¤rÃ¤llÃ¤, ja
# opintotukikuukausien lukumÃ¤Ã¤rÃ¤ ja skaalaus opintotuen saajien lukumÃ¤Ã¤rÃ¤llÃ¤
opintuki <- register_data %>% 
  select(knro, optukk, tkopira) %>%
  mutate(opintukikk_saaja = ifelse(optukk > 0, 1, 0),
         opintukimaara_saaja = ifelse(tkopira > 0, 1, 0))

opintuki <- opintuki %>% 
  group_by(knro) %>% 
  summarise(
    opintukikk_saaja_n = sum(opintukikk_saaja),
    opintukikk_sum = sum(optukk),
    opintukikk_saaja = ifelse(opintukikk_saaja_n > 0,
                              opintukikk_sum / opintukikk_saaja_n,
                              opintukikk_sum),
    opintukimaara_saaja_n = sum(opintukimaara_saaja),
    opintukimaara_sum = sum(tkopira),
    opintukimaara_saaja = ifelse(opintukimaara_saaja_n > 0,
                                 opintukimaara_sum / opintukimaara_saaja_n,
                                 opintukimaara_sum)
  ) %>% 
  select(knro,
         opintukikk_sum,
         opintukikk_saaja,
         opintukimaara_sum,
         opintukimaara_saaja)

sisu_data <- left_join(sisu_data, opintuki, by = "knro")

vopintuki <- register_data %>% 
  select(knro, asko, optukk) %>%
  filter(asko == 1) %>% 
  mutate(vopintukikk = as.numeric(optukk)) %>% 
  select(knro, vopintukikk)

sisu_data <- left_join(sisu_data, vopintuki, by = "knro")

rm(opintuki,
   vopintuki)

# Muut etuudet ----

# Muiden ensisijaisten sosiaalietuuksien summa
# Sisaltaa: elake, opintotuki, sairauspaivaraha, kuntoutusraha,
# vanhempainpaivaraha, lasten kotihoidon tuki

# Rekisteriaineisto
sisu_data$muusoset_data_sum <- sisu_data %>%
  select(VEROTT_KANSEL_DATA, # Verottomat elakelisat ja vammaistuet yhteensa
         SAIRVAK_DATA, # Sairauspaivaraha, sisaltaa vanhempainpaivarahan vakuutetulle (haiprva)
         KOTIHTUKI_DATA, # Lasten kotihoidon tuki
         SEKAL_PRAHAT) %>% # Tapaturma ja liikennevakuutuspaivaraha, kuntoutusraha, vastuuvakuutuksenpaivaraha, henkilovakuutuspaivaraha
  rowSums(na.rm = T) 

# Estimaatit
sisu_data$muusoset_simul_sum <- sisu_data %>%
  select(VEROTT_KANSEL_SIMUL, # Verottomat elakelisat ja vammaistuet yhteensa
         SAIRVAK_SIMUL, # Sairauspaivaraha, sisaltaa vanhempainpaivarahan vakuutetulle (haiprva)
         KOTIHTUKI_SIMUL, # Lasten kotihoidon tuki
         SEKAL_PRAHAT) %>% # Tapaturma ja liikennevakuutuspaivaraha, kuntoutusraha, vastuuvakuutuksenp?iv?raha, henkil?vakuutusp?iv?raha
  rowSums(na.rm = T) 


# Estimoidut sosiaalietuudet yhteensa per kulutusyksikko
sisu_data$muusoset_simul_mod <- ifelse(sisu_data$modoecd != 0,
                                       sisu_data$muusoset_simul_sum / sisu_data$modoecd,
                                       sisu_data$muusoset_simul_sum)
sisu_data$fmuusoset_simul_mod <- createFactor(sisu_data,
                                              "muusoset_simul_mod",
                                              "luokka",
                                              q = 0.1)

sisu_data$muusoset_data_mod <- ifelse(sisu_data$modoecd != 0,
                                      sisu_data$muusoset_data_sum / sisu_data$modoecd,
                                      sisu_data$muusoset_data_sum)
sisu_data$fmuusoset_data_mod <- createFactor(sisu_data,
                                             "muusoset_data_mod",
                                             "luokka",
                                             q = 0.1)

# Estimoidut sosiaalietuudet yhteensa osuutena estimoiduista kaytettavissa olevista rahatuloista
sisu_data$muusoset_simul_os <- ifelse(sisu_data$kaytrahatulo_simul2 != 0,
                                      sisu_data$muusoset_simul_sum / sisu_data$kaytrahatulo_simul2,
                                      sisu_data$muusoset_simul_sum)
sisu_data$fmuusoset_simul_os <- createFactor(sisu_data,
                                             "muusoset_simul_os",
                                             "luokka",
                                             q = 0.01)

# Indikaattorimuuttuja saako sosiaalietuuksia ----

# N.B. rekisteriaineistossa poislukien toimeentulotuki
sisu_data$etuus_data <- ifelse((sisu_data$VERONAL_TULOT_DATA +
                                  sisu_data$VEROTT_TULOT_DATA -
                                  sisu_data$TOIMTUKI_DATA) <= 0,
                               1,
                               0) %>% factor()

sisu_data$etuus_simul <- ifelse((sisu_data$VERONAL_TULOT_SIMUL +
                                   sisu_data$VEROTT_TULOT_SIMUL) <= 0,
                                1,
                                0) %>% factor()



# Paatoimeentulo ----

new_paasoss <- register_data %>% 
  select(knro, sose, paasoss, asko) %>% 
  filter(asko == 1) %>% 
  mutate(paatoim = case_when(
    sose %in% c(10, 20) ~ "yrittaja",
    sose %in% c(31:34, 41:44, 51:54) ~ "tyollinen",
    sose == 60 ~ "opiskelija",
    sose == 70 ~ "elakelainen",
    sose == 81 ~ "pitkaaik_tyoton", 
    sose %in% c(82, 99) ~ "muu"
  ))

new_paasoss <- new_paasoss %>% select(knro, paatoim)
new_paasoss$paatoim <- factor(new_paasoss$paatoim)

sisu_data <- left_join(sisu_data, new_paasoss, by = "knro")

rm(new_paasoss)

# Simuloitu toimeentulotuki ----

# Kategorisoitu simuloitu toimeentulotuki
# sisu_data$fTOIMTUKI_SIMUL <- createFactor(sisu_data,
#                                           "TOIMTUKI_SIMUL",
#                                           "luokka",
#                                           q = 0.2)
sisu_data$fTOIMTUKI_SIMUL <- cut(sisu_data$TOIMTUKI_SIMUL,
                                  c(0, 581, 2425, 5483, 53502),
                                  dig.lab = 10,
                                  right = FALSE,
                                  include.lowest = TRUE) %>% factor()


sisu_data$fTOIMTUKI_SIMUL2 <- cut(sisu_data$TOIMTUKI_SIMUL,
                                  c(0, 200, 2000, 4000, 45000),
                                  dig.lab = 10,
                                  right = FALSE,
                                  include.lowest = TRUE) %>% factor()

# Estimoitu toimeentulotuki jaettuna kulutusyksikÃ¶llÃ¤
sisu_data$toimtuki_simul_mod <- ifelse(sisu_data$modoecd != 0,
                                       sisu_data$TOIMTUKI_SIMUL / sisu_data$modoecd,
                                       sisu_data$TOIMTUKI_SIMUL)
sisu_data$ftoimtuki_simul_mod <- cut(sisu_data$toimtuki_simul_mod,
                                     c(0, 100, 300, 500, 1000, 5000),
                                     dig.lab = 10,
                                     right = FALSE,
                                     include.lowest = TRUE) %>% factor()

# Estimoitu toimeentulotuki osuutena estimoiduista kÃ¤yt.rahatuloista
sisu_data <- sisu_data %>% mutate(toimtuki_simul_os = 
                                    ifelse(kaytrahatulo_simul2 != 0,
                                           TOIMTUKI_SIMUL / kaytrahatulo_simul2,
                                           0))

q <- quantile(sisu_data$toimtuki_simul_os[sisu_data$luokka != "ei_tukea"],
              names = F,
              seq(0, 1, 0.1)) %>%
  round(., 2) %>%
  unique %>% 
  append(., 1.00, after = which(. == max(.))-1)
sisu_data$ftoimtuki_simul_os <- createFactor(sisu_data,
                                             "toimtuki_simul_os",
                                             right = TRUE,
                                             labs = q)

# Jussin ehdotus: Jos estimoitu toimeentulotuki / estimoiduilla kÃ¤yt.rahatuloilla > 1,
# tai ei mÃ¤Ã¤ritelty, niin osuus on 1.
sisu_data <- sisu_data %>% mutate(toimtuki_simul_os2 = 
                                    ifelse(kaytrahatulo_simul2 != 0,
                                           TOIMTUKI_SIMUL / kaytrahatulo_simul2,
                                           1))
q <- quantile(sisu_data$toimtuki_simul_os2[sisu_data$luokka != "ei_tukea"],
              names = F,
              seq(0, 1, 0.1)) %>%
  round(., 2) %>%
  unique %>% 
  append(., 1.00, after = which(. == max(.))-1)
sisu_data$ftoimtuki_simul_os2 <- createFactor(sisu_data,
                                              "toimtuki_simul_os2",
                                              right = TRUE,
                                              labs = q)

# Jatkuvat muunnokset
sisu_data <- sisu_data %>%
  mutate(sTOIMTUKI_SIMUL = sqrt(TOIMTUKI_SIMUL),
         lTOIMTUKI_SIMUL = log(TOIMTUKI_SIMUL + 1))

# Muut asetukset ----

# Faktorit
faktoriksi <- c("paasoss", "elivtu", "rake", "maakunta", "DESMOD_MALLI")
faktoriksi <- faktoriksi[faktoriksi %in% names(sisu_data)]
sisu_data <- sisu_data %>% mutate(across(all_of(faktoriksi), as.factor))
rm(faktoriksi)

names(sisu_data) <- tolower(names(sisu_data))

saveRDS(sisu_data,
        file = file.path(
          "no_vc",
          "cache",
          "sisu_est_register_data_2019_tidy.Rds"
        ))

