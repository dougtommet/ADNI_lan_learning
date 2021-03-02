

# adni.survival.cn.mci <- readRDS(file=path(r_objects_folder, "025_adni_survival_cn_mci.rds"))
# adni.survival.cn <- readRDS(file=path(r_objects_folder, "025_adni_survival_cn.rds"))
adni.survival.mci <- readRDS(file=path(r_objects_folder, "030_adni_survival_mci.rds"))

# adni.survival.cn.apoe <- adni.survival.cn %>%
#   filter(!is.na(apoe4))
# my_surv_object_cn_apoe <- with(adni.survival.cn.apoe, Surv(time_in_adni, event))

adni.survival.mci.apoe <- adni.survival.mci %>%
  filter(!is.na(apoe4))
my_surv_object_mci_apoe <- with(adni.survival.mci.apoe, Surv(time_in_adni, event))

# adni.survival.cn.csf <- adni.survival.cn %>%
#   filter(!is.na(cluster_mixtmodel))
# my_surv_object_cn_csf <- with(adni.survival.cn.csf, Surv(time_in_adni, event))

adni.survival.mci.csf <- adni.survival.mci %>%
  filter(!is.na(cluster_mixtmodel))
my_surv_object_mci_csf <- with(adni.survival.mci.csf, Surv(time_in_adni, event))

# adni.survival.cn.ent <- adni.survival.cn %>%
#   filter(!is.na(entorhinal_z))
# my_surv_object_cn_ent <- with(adni.survival.cn.ent, Surv(time_in_adni, event))

adni.survival.mci.ent <- adni.survival.mci %>%
  filter(!is.na(entorhinal_z))
my_surv_object_mci_ent <- with(adni.survival.mci.ent, Surv(time_in_adni, event))

# adni.survival.cn.hcv <- adni.survival.cn %>%
#   filter(!is.na(hcv_z))
# my_surv_object_cn_hcv <- with(adni.survival.cn.hcv, Surv(time_in_adni, event))

adni.survival.mci.hcv <- adni.survival.mci %>%
  filter(!is.na(hcv_z))
my_surv_object_mci_hcv <- with(adni.survival.mci.hcv, Surv(time_in_adni, event))



foo.ind <- tribble(
  ~name,             ~funcs,                                                                                  
                             ~models_mci_apoe,                                                                                   
                             ~models_mci_csf,                                                                                               
                             ~models_mci_ent,                                                                                           
                             ~models_mci_hcv,                                                                                               
  "biomarker",      'coxph', 
                             formula = my_surv_object_mci_apoe ~  agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_csf  ~  agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_ent  ~  agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_mci_hcv  ~  agebl + ptsex + pteducat + hcv_z,         
  "animal_raw",     'coxph', formula = my_surv_object_mci_apoe ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_csf  ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_ent  ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_mci_hcv  ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat,                                   
  "ffluency_raw",   'coxph', formula = my_surv_object_mci_apoe ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_csf  ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_ent  ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_mci_hcv  ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat,                                  
  "confront",       'coxph', formula = my_surv_object_mci_apoe ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_csf  ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_ent  ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_mci_hcv  ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat,                                   
  
  "bnt",            'coxph', formula = my_surv_object_mci_apoe ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_csf  ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_ent  ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_mci_hcv  ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat,                                   
  
  "confront_wo_bnt",'coxph', formula = my_surv_object_mci_apoe ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_csf  ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_ent  ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_mci_hcv  ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat,                                   
  
  "other_lan",      'coxph', formula = my_surv_object_mci_apoe ~ others_q1    + others_q3    + agebl + ptsex + pteducat,         
                             formula = my_surv_object_mci_csf  ~ others_q1    + others_q3    + agebl + ptsex + pteducat,                     
                             formula = my_surv_object_mci_ent  ~ others_q1    + others_q3    + agebl + ptsex + pteducat,                 
                             formula = my_surv_object_mci_hcv  ~ others_q1    + others_q3    + agebl + ptsex + pteducat,                                   
  "animal_raw_biomarker",  'coxph', 
                             formula = my_surv_object_mci_apoe ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_csf  ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_ent  ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_mci_hcv  ~ anim_raw_q1  + anim_raw_q3  + agebl + ptsex + pteducat + hcv_z,        
  "ffluency_raw_biomarker", 'coxph', 
                             formula = my_surv_object_mci_apoe ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat + apoe4, 
                             formula = my_surv_object_mci_csf  ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat + cluster_mixtmodel, 
                             formula = my_surv_object_mci_ent  ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat + entorhinal_z,  
                             formula = my_surv_object_mci_hcv  ~ f_raw_q1     + f_raw_q3     + agebl + ptsex + pteducat + hcv_z,      
  "confront_biomarker",      'coxph', 
                             formula = my_surv_object_mci_apoe ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat + apoe4,          
                             formula = my_surv_object_mci_csf  ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat + cluster_mixtmodel,                     
                             formula = my_surv_object_mci_ent  ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat + entorhinal_z,                   
                             formula = my_surv_object_mci_hcv  ~ confront_q1  + confront_q3  + agebl + ptsex + pteducat + hcv_z,                                      
  
  "bnt_biomarker",      'coxph', 
                             formula = my_surv_object_mci_apoe ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat + apoe4,          
                             formula = my_surv_object_mci_csf  ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat + cluster_mixtmodel,                     
                             formula = my_surv_object_mci_ent  ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat + entorhinal_z,                   
                             formula = my_surv_object_mci_hcv  ~ bnt_q1  + bnt_q3  + agebl + ptsex + pteducat + hcv_z,                                       
  
  "confront_wo_bnt_biomarker", 'coxph', 
                             formula = my_surv_object_mci_apoe ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat + apoe4,          
                             formula = my_surv_object_mci_csf  ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat + cluster_mixtmodel,                     
                             formula = my_surv_object_mci_ent  ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat + entorhinal_z,                   
                             formula = my_surv_object_mci_hcv  ~ confront_wo_bnt_q1  + confront_wo_bnt_q3  + agebl + ptsex + pteducat + hcv_z,                                       
  
  "other_biomarker",         'coxph', 
                             formula = my_surv_object_mci_apoe ~ others_q1    + others_q3    + agebl + ptsex + pteducat + apoe4,          
                             formula = my_surv_object_mci_csf  ~ others_q1    + others_q3    + agebl + ptsex + pteducat + cluster_mixtmodel,                     
                             formula = my_surv_object_mci_ent  ~ others_q1    + others_q3    + agebl + ptsex + pteducat + entorhinal_z,                   
                             formula = my_surv_object_mci_hcv  ~ others_q1    + others_q3    + agebl + ptsex + pteducat + hcv_z,             
  
)
foo.all <- tribble(
  ~name,             ~funcs,  ~models_mci_apoe,                                                                                                                                                                      
                              ~models_mci_csf,                                                                                                                                                                                 
                              ~models_mci_ent,                                                                                                                                                                             
                              ~models_mci_hcv,   
  "all_raw",           "coxph", formula = my_surv_object_mci_apoe  ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat,         
                                formula = my_surv_object_mci_csf   ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat,                     
                                formula = my_surv_object_mci_ent   ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat,                
                                formula = my_surv_object_mci_hcv   ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat,         
  "all_raw_biomarker", "coxph", formula = my_surv_object_mci_apoe  ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat + apoe4, 
                                formula = my_surv_object_mci_csf   ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat + cluster_mixtmodel, 
                                formula = my_surv_object_mci_ent   ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat + entorhinal_z, 
                                formula = my_surv_object_mci_hcv   ~ anim_raw_q1  + anim_raw_q3  + f_raw_q1 + f_raw_q3 + confront_q1 + confront_q3 +  others_q1  + others_q3  +  agebl + ptsex + pteducat + hcv_z, 
)
foo.data <- tribble(
  ~n, ~data_mci_apoe,         ~data_mci_csf,          ~data_mci_ent,         ~data_mci_hcv,        
  1,  adni.survival.mci.apoe, adni.survival.mci.csf,  adni.survival.mci.ent, adni.survival.mci.hcv,
  
)

foo <- foo.ind %>%
  bind_rows(foo.all) %>%
  mutate(n=1) %>%
  left_join(foo.data, by = "n") %>%
  select(-n)

fitted.cox.models <- foo %>%
  filter(!str_detect(name, "ffluency")) %>%
  filter(!str_detect(name, "all")) %>%
  mutate(params_mci_apoe = pmap(list(models_mci_apoe, data_mci_apoe), list),
         params_mci_csf  = pmap(list(models_mci_csf, data_mci_csf), list),
         params_mci_ent  = pmap(list(models_mci_ent, data_mci_ent), list),
         params_mci_hcv  = pmap(list(models_mci_hcv, data_mci_hcv), list),
         fit_mci_apoe = invoke_map(funcs, params_mci_apoe),
         fit_mci_csf  = invoke_map(funcs, params_mci_csf),
         fit_mci_ent  = invoke_map(funcs, params_mci_ent),
         fit_mci_hcv  = invoke_map(funcs, params_mci_hcv),
         tidy_mci_apoe = map(fit_mci_apoe, tidy, exp = TRUE, conf.int = TRUE),
         tidy_mci_csf  = map(fit_mci_csf, tidy, exp = TRUE, conf.int = TRUE),
         tidy_mci_ent  = map(fit_mci_ent, tidy, exp = TRUE, conf.int = TRUE),
         tidy_mci_hcv  = map(fit_mci_hcv, tidy, exp = TRUE, conf.int = TRUE),
         glance_mci_apoe = map(fit_mci_apoe, glance),
         glance_mci_csf  = map(fit_mci_csf, glance),
         glance_mci_ent  = map(fit_mci_ent, glance),
         glance_mci_hcv  = map(fit_mci_hcv, glance))

saveRDS(fitted.cox.models,   file=path(r_objects_folder, "155_fitted_cox_models.rds"))  

fitted.cox.models %>%
  select(name, tidy_mci_apoe) %>%
  unnest(c(tidy_mci_apoe)) %>%
  select(term, name, estimate, p.value, conf.low, conf.high) %>%
  gather(-c(1:2), key = "statistic", value = "value") %>%
  unite("xyz", c(statistic, name)) %>%
  spread(key = xyz, value = value)

fitted.cox.models %>%
  select(name, glance_mci_apoe) %>%
  unnest(c(glance_mci_apoe)) %>%
  select(name, n)

