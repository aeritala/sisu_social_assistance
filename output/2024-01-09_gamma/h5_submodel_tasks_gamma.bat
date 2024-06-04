Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="gamma" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul + paatoim_sisu + palkat1k_aik" --update.f="+sp*frake" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_gamma\fit_gamma08.Rds" --overwrite=FALSE --seed=109 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="gamma" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul + paatoim_sisu + palkat1k_aik" --update.f="+asumtuet_data*frake" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_gamma\fit_gamma09.Rds" --overwrite=FALSE --seed=109 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="gamma" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul + paatoim_sisu + palkat1k_aik" --update.f="+palkat1k_aik*frake" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_gamma\fit_gamma10.Rds" --overwrite=FALSE --seed=109 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="gamma" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul + paatoim_sisu + palkat1k_aik" --update.f="+sp*frake +asumtuet_data*frake" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_gamma\fit_gamma11.Rds" --overwrite=FALSE --seed=109 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="gamma" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul + paatoim_sisu + palkat1k_aik" --update.f="+sp*frake +palkat1k_aik*frake" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_gamma\fit_gamma12.Rds" --overwrite=FALSE --seed=109 
Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="gamma" --prior="informative" --f="toimtuki_data ~ fikavuv + sp + frake + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul + paatoim_sisu + palkat1k_aik" --update.f="+asumtuet_data*frake +palkat1k_aik*frake" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-09_gamma\fit_gamma13.Rds" --overwrite=FALSE --seed=109 
