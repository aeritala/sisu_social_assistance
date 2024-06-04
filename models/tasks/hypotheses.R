
firstf <- "toimtuki_data ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + toimtuki_simul + asumtuet_simul + lapsip_simul + muusoset_simul_sum + kansel_perhel_simul"
firstf_bern <- "data_kylla ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + toimtuki_simul + asumtuet_simul + lapsip_simul + muusoset_simul_sum + kansel_perhel_simul"

hypotheses <- list(
  c("-asumtuet_simul +asumtuet_data -lapsip_simul +lapsip_data -muusoset_simul_sum +muusoset_data_sum -kansel_perhel_simul +kansel_perhel_data"),
  c("-toimtuki_simul", "-toimtuki_simul +ftoimtuki_simul"),
  c("-paatoim +paatoim_sisu"),
  c("-palkat +logp1_palkat", "-palkat +palkat1k_aik"),
  c("+sp*frake", "+asumtuet_data*frake", "+PALKAT*frake",
       "+sp*frake +asumtuet_data*frake", "+sp*frake +PALKAT*frake",
       "+asumtuet_data*frake +PALKAT*frake")
)
