Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="generalized_gamma" --prior="informative" --reg.par="alpha" --f="toimtuki_data ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + toimtuki_simul + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data" --update.f="-toimtuki_simul" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-10_ggamma_alpha\fit_ggamma_alpha03.Rds" --overwrite=FALSE --seed=110 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="generalized_gamma" --prior="informative" --reg.par="alpha" --f="toimtuki_data ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + toimtuki_simul + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data" --update.f="-toimtuki_simul +ftoimtuki_simul" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-10_ggamma_alpha\fit_ggamma_alpha04.Rds" --overwrite=FALSE --seed=110 
