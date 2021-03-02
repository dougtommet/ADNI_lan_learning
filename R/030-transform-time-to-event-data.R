rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "002-folder_paths_and_options.R"))

# It's input is the following R objects:
adni.retest <- readRDS(file=path(r_objects_folder, "020_adni_retest.rds"))
adni.filtered <- readRDS(file=path(r_objects_folder, "020_adni_filtered.rds"))
adni_lan_recoded_quantile <- readRDS(file=path(r_objects_folder, "025_adni_lan_recoded_quantile.rds"))  

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


# Separating the data into CN at baseline and MCI at baseline subsets
# adni.survival.cn <- adni.survival.cn.mci %>%
#   filter(bl_dx_factor %in% c("Normal"))

adni.survival.mci <- adni.survival.cn.mci %>%
  filter(bl_dx_factor %in% c("MCI"))



saveRDS(adni.survival.mci,   file=path(r_objects_folder, "030_adni_survival_mci.rds"))
