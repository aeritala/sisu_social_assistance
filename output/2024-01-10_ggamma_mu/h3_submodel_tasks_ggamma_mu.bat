Rscript --no-save --no-restore models\functions\fit_submodel.R --data="no_vc\cache\sample_realtrain_standard.Rds" --dist="generalized_gamma" --prior="informative" --reg.par="mu" --f="toimtuki_data ~ fikavuv + sp + frake + paatoim + koulasv + faslaji + fmuuttovvi + kuntar + fmuuaik + tyot + desmod + palkat + paaomatulo + tmmktukipv_data_sum + perusprpv_data_sum + ansioprpv_data_sum + vopintukikk + asumtuet_data + lapsip_data + muusoset_data_sum + kansel_perhel_data + ftoimtuki_simul" --update.f="-paatoim +paatoim_sisu" --warmup=1000 --iter=2000 --chains=3 --file="output\2024-01-10_ggamma_mu\fit_ggamma_mu05.Rds" --overwrite=FALSE --seed=110 
