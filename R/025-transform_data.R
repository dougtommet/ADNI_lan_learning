rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "002-folder_paths_and_options.R"))

# It's input is the following R objects:
adni.retest <- readRDS(file=path(r_objects_folder, "020_adni_retest.rds"))
adni.filtered <- readRDS(file=path(r_objects_folder, "020_adni_filtered.rds"))
adni_lan <- readRDS(file=path(r_objects_folder, "010_adni_lan.rds"))

adni_lan_recoded <- adni_lan %>%
  mutate(animalsc_raw_change_quantile   = ntile(animalsc_raw_change, 4),
         animalsc_cat_change_quantile   = ntile(animalsc_cat_change, 4),
         ffluency_raw_change_quantile   = ntile(ffluency_raw_change, 4),
         ffluency_cat_change_quantile   = ntile(ffluency_cat_change, 4),
         confront_change_quantile       = ntile(confront_change, 4),
         others_change_quantile         = ntile(others_change, 4),
         animalsc_raw_change_quantile   = factor(animalsc_raw_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         animalsc_cat_change_quantile   = factor(animalsc_cat_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         ffluency_raw_change_quantile   = factor(ffluency_raw_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         ffluency_cat_change_quantile   = factor(ffluency_cat_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         confront_change_quantile       = factor(confront_change_quantile,      levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
         others_change_quantile         = factor(others_change_quantile,        levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE))

adni_lan_recoded_quantile <- adni_lan_recoded %>%
  filter(bl_dx_factor %in% c("MCI")) 




# Left censoring subjects if they convert at the 6M visit
adni.survival.cn.mci <- adni.filtered %>%
  filter(bl_dx_factor %in% c("Normal", "MCI")) %>%
  group_by(rid) %>%
  mutate(event = case_when(bl_dx==1 & dxcurren %in% c(2, 3)  ~ 1 ,
                           bl_dx==2 & dxcurren==3  ~ 1 ,
                           TRUE ~ 0),
         time_in_adni = lubridate::as.duration(examdate - examdate_bl) / lubridate::dyears(1),
         event_ever = max(event)) %>%
  arrange(rid, desc(event), visnum) %>%
  mutate(n = row_number(),
         first_event = case_when(event==1 & n==1 ~ 1,
                                 TRUE ~ 0),
         first_event_left_censored = case_when(first_event==1 & visnum==6 ~ 1)) %>%
  select(-n) %>%
  arrange(rid, desc(visnum)) %>%
  mutate(n = row_number(),
         censored = case_when(event_ever==0 & n==1 ~ 1,
                                 TRUE ~ 0)) %>%
  fill(first_event_left_censored, .direction = "downup") %>%
  filter(is.na(first_event_left_censored)) %>%
  select(-n) %>%
  arrange(rid, visnum) %>%
  ungroup() %>%
  filter(first_event==1 | censored==1) %>%
  select(rid, bl_dx_factor, time_in_adni, event, phase_bl) 


# Choosing the covariates at the specific visits to use in the analysis
# baseline covariates
adni.bl <- adni.filtered %>%
  filter(visnum==0) %>%
  select(rid, 
         agebl, ptsex, pteducat, pthisp, ptrace3c,
         bl_dx_factor, apoe4, cluster_mixtmodel)

# 6M covariates
adni.6M <- adni.filtered %>%
  filter(visnum == 6) %>%
  select(rid, 
         st24ta_d, st83ta_d, hcv, hcv3) %>%
  mutate(entorhinal = (st24ta_d + st83ta_d)/2,
         entorhinal_z = (entorhinal-mean(entorhinal, na.rm = TRUE))/(2*sd(entorhinal, na.rm = TRUE)),
         hcv_z = (hcv-mean(hcv, na.rm = TRUE))/(2*sd(hcv, na.rm = TRUE)),
         hcv3_z = (hcv3-mean(hcv3, na.rm = TRUE))/(2*sd(hcv3, na.rm = TRUE)))

# Creating the merged data set
# Data set will be nonmissing for retest and also not convert to MCI/AD prior to 6M visit
adni.survival.cn.mci <- adni.survival.cn.mci %>%
  inner_join(adni_lan_recoded_quantile, by = c("rid", "bl_dx_factor")) %>%
  left_join(adni.bl, by = c("rid" = "rid", "bl_dx_factor" = "bl_dx_factor")) %>%
  left_join(adni.6M, by = c("rid" = "rid"))

# Creating dummy variables for the quantiles
adni.survival.cn.mci <- adni.survival.cn.mci %>%
  mutate(anim_raw_q1 = case_when(animalsc_raw_change_quantile=="1 (best)" ~ 1,
                                 animalsc_raw_change_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         anim_raw_q2 = case_when(animalsc_raw_change_quantile=="2" ~ 1,
                                 animalsc_raw_change_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         anim_raw_q3 = case_when(animalsc_raw_change_quantile=="3" ~ 1,
                                 animalsc_raw_change_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         anim_raw_q4 = case_when(animalsc_raw_change_quantile=="4 (worst)" ~ 1,
                                 animalsc_raw_change_quantile %in% c("1 (best)", "2", "3") ~ 0),
         anim_cat_q1 = case_when(animalsc_cat_change_quantile=="1 (best)" ~ 1,
                                 animalsc_cat_change_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         anim_cat_q2 = case_when(animalsc_cat_change_quantile=="2" ~ 1,
                                 animalsc_cat_change_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         anim_cat_q3 = case_when(animalsc_cat_change_quantile=="3" ~ 1,
                                 animalsc_cat_change_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         anim_cat_q4 = case_when(animalsc_cat_change_quantile=="4 (worst)" ~ 1,
                                 animalsc_cat_change_quantile %in% c("1 (best)", "2", "3") ~ 0),
         
         f_raw_q1 = case_when(ffluency_raw_change_quantile=="1 (best)" ~ 1,
                              ffluency_raw_change_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         f_raw_q2 = case_when(ffluency_raw_change_quantile=="2" ~ 1,
                              ffluency_raw_change_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         f_raw_q3 = case_when(ffluency_raw_change_quantile=="3" ~ 1,
                              ffluency_raw_change_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         f_raw_q4 = case_when(ffluency_raw_change_quantile=="4 (worst)" ~ 1,
                              ffluency_raw_change_quantile %in% c("1 (best)", "2", "3") ~ 0),
         f_cat_q1 = case_when(ffluency_cat_change_quantile=="1 (best)" ~ 1,
                              ffluency_cat_change_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         f_cat_q2 = case_when(ffluency_cat_change_quantile=="2" ~ 1,
                              ffluency_cat_change_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         f_cat_q3 = case_when(ffluency_cat_change_quantile=="3" ~ 1,
                              ffluency_cat_change_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         f_cat_q4 = case_when(ffluency_cat_change_quantile=="4 (worst)" ~ 1,
                              ffluency_cat_change_quantile %in% c("1 (best)", "2", "3") ~ 0),
         
         confront_q1 = case_when(confront_change_quantile=="1 (best)" ~ 1,
                                 confront_change_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         confront_q2 = case_when(confront_change_quantile=="2" ~ 1,
                                 confront_change_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         confront_q3 = case_when(confront_change_quantile=="3" ~ 1,
                                 confront_change_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         confront_q4 = case_when(confront_change_quantile=="4 (worst)" ~ 1,
                                 confront_change_quantile %in% c("1 (best)", "2", "3") ~ 0),
         
         others_q1 = case_when(others_change_quantile=="1 (best)" ~ 1,
                               others_change_quantile %in% c("2", "3", "4 (worst)") ~ 0),
         others_q2 = case_when(others_change_quantile=="2" ~ 1,
                               others_change_quantile %in% c("1 (best)", "3", "4 (worst)") ~ 0),
         others_q3 = case_when(others_change_quantile=="3" ~ 1,
                               others_change_quantile %in% c("1 (best)", "2", "4 (worst)") ~ 0),
         others_q4 = case_when(others_change_quantile=="4 (worst)" ~ 1,
                               others_change_quantile %in% c("1 (best)", "2", "3") ~ 0)
         
         )

# Separating the data into CN at baseline and MCI at baseline subsets
# adni.survival.cn <- adni.survival.cn.mci %>%
#   filter(bl_dx_factor %in% c("Normal"))

adni.survival.mci <- adni.survival.cn.mci %>%
  filter(bl_dx_factor %in% c("MCI"))


saveRDS(adni_lan_recoded,   file=path(r_objects_folder, "025_adni_lan_recoded.rds"))  
saveRDS(adni_lan_recoded_quantile,   file=path(r_objects_folder, "025_adni_lan_recoded_quantile.rds"))
saveRDS(adni.survival.cn.mci,   file=path(r_objects_folder, "025_adni_survival_cn_mci.rds"))
# saveRDS(adni.survival.cn,   file=path(r_objects_folder, "025_adni_survival_cn.rds"))
saveRDS(adni.survival.mci,   file=path(r_objects_folder, "025_adni_survival_mci.rds"))


