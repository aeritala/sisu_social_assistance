Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="weibull" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + toimtuki_simul + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data" --update.f="-toimtuki_simul" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_weibull\fit_weibull03.Rds" --overwrite=FALSE --seed=109 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="weibull" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + toimtuki_simul + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data" --update.f="-toimtuki_simul +ftoimtuki_simul" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_weibull\fit_weibull04.Rds" --overwrite=FALSE --seed=109 
