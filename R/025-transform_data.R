rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "002-folder_paths_and_options.R"))

# It's input is the following R objects:
adni_lan <- readRDS(file=path(r_objects_folder, "010_adni_lan.rds"))

adni_lan_recoded <- adni_lan %>%
  mutate(animalsc_raw_change_quantile    = cut(animalsc_raw_change,    breaks = c(-Inf, -3.9, 3.9,  Inf), labels = FALSE),
         ffluency_raw_change_quantile    = cut(ffluency_raw_change,    breaks = c(-Inf, -3.9, 3.9,  Inf), labels = FALSE),
         confront_change_quantile        = cut(confront_change,        breaks = c(-Inf, -.05, .05, Inf), labels = FALSE),
         others_change_quantile          = cut(others_change,          breaks = c(-Inf, -.05, .05, Inf), labels = FALSE),
         bnt_change_quantile             = cut(bnt_change,             breaks = c(-Inf, -2.9, 2.9, Inf),     labels = FALSE),
         confront_wo_bnt_change_quantile = cut(confront_wo_bnt_change, breaks = c(-Inf, -.05, .05, Inf), labels = FALSE),
         animalsc_raw_change_quantile    = factor(animalsc_raw_change_quantile,    levels = c(3:1), labels = c("1 (improve)", "2 (no change)", "3 (decline)"), ordered = TRUE),
         ffluency_raw_change_quantile    = factor(ffluency_raw_change_quantile,    levels = c(3:1), labels = c("1 (improve)", "2 (no change)", "3 (decline)"), ordered = TRUE),
         confront_change_quantile        = factor(confront_change_quantile,        levels = c(3:1), labels = c("1 (improve)", "2 (no change)", "3 (decline)"), ordered = TRUE),
         others_change_quantile          = factor(others_change_quantile,          levels = c(3:1), labels = c("1 (improve)", "2 (no change)", "3 (decline)"), ordered = TRUE),
         bnt_change_quantile             = factor(bnt_change_quantile,             levels = c(3:1), labels = c("1 (improve)", "2 (no change)", "3 (decline)"), ordered = TRUE),
         confront_wo_bnt_change_quantile = factor(confront_wo_bnt_change_quantile, levels = c(3:1), labels = c("1 (improve)", "2 (no change)", "3 (decline)"), ordered = TRUE))


# adni_lan_recoded <- adni_lan %>%
#   filter(bl_dx_factor %in% c("MCI")) %>%
#   mutate(animalsc_raw_change_quantile   = ntile(animalsc_raw_change, 4),
#          animalsc_cat_change_quantile   = ntile(animalsc_cat_change, 4),
#          ffluency_raw_change_quantile   = ntile(ffluency_raw_change, 4),
#          ffluency_cat_change_quantile   = ntile(ffluency_cat_change, 4),
#          confront_change_quantile       = ntile(confront_change, 4),
#          others_change_quantile         = ntile(others_change, 4),
#          animalsc_raw_change_quantile   = factor(animalsc_raw_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
#          animalsc_cat_change_quantile   = factor(animalsc_cat_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
#          ffluency_raw_change_quantile   = factor(ffluency_raw_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
#          ffluency_cat_change_quantile   = factor(ffluency_cat_change_quantile,  levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
#          confront_change_quantile       = factor(confront_change_quantile,      levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE),
#          others_change_quantile         = factor(others_change_quantile,        levels = c(4:1), labels = c("1 (best)", "2", "3", "4 (worst)"), ordered = TRUE))

adni_lan_recoded_quantile <- adni_lan_recoded %>%
  mutate(anim_raw_q1 = case_when(animalsc_raw_change_quantile=="1 (improve)" ~ 1,
                                 animalsc_raw_change_quantile %in% c("2 (no change)", "3 (decline)") ~ 0),
         anim_raw_q2 = case_when(animalsc_raw_change_quantile=="2 (no change)" ~ 1,
                                 animalsc_raw_change_quantile %in% c("1 (improve)", "3 (decline)") ~ 0),
         anim_raw_q3 = case_when(animalsc_raw_change_quantile=="3 (decline)" ~ 1,
                                 animalsc_raw_change_quantile %in% c("1 (improve)", "2 (no change)") ~ 0),

         f_raw_q1 = case_when(ffluency_raw_change_quantile=="1 (improve)" ~ 1,
                              ffluency_raw_change_quantile %in% c("2 (no change)", "3 (decline)") ~ 0),
         f_raw_q2 = case_when(ffluency_raw_change_quantile=="2 (no change)" ~ 1,
                              ffluency_raw_change_quantile %in% c("1 (improve)", "3 (decline)") ~ 0),
         f_raw_q3 = case_when(ffluency_raw_change_quantile=="3 (decline)" ~ 1,
                              ffluency_raw_change_quantile %in% c("1 (improve)", "2 (no change)") ~ 0),

         confront_q1 = case_when(confront_change_quantile=="1 (improve)" ~ 1,
                                 confront_change_quantile %in% c("2 (no change)", "3 (decline)") ~ 0),
         confront_q2 = case_when(confront_change_quantile=="2 (no change)" ~ 1,
                                 confront_change_quantile %in% c("1 (improve)", "3 (decline)") ~ 0),
         confront_q3 = case_when(confront_change_quantile=="3 (decline)" ~ 1,
                                 confront_change_quantile %in% c("1 (improve)", "2 (no change)") ~ 0),

         others_q1 = case_when(others_change_quantile=="1 (improve)" ~ 1,
                               others_change_quantile %in% c("2 (no change)", "3 (decline)") ~ 0),
         others_q2 = case_when(others_change_quantile=="2 (no change)" ~ 1,
                               others_change_quantile %in% c("1 (improve)", "3 (decline)") ~ 0),
         others_q3 = case_when(others_change_quantile=="3 (decline)" ~ 1,
                               others_change_quantile %in% c("1 (improve)", "2 (no change)") ~ 0),
         
         bnt_q1 = case_when(bnt_change_quantile=="1 (improve)" ~ 1,
                            bnt_change_quantile %in% c("2 (no change)", "3 (decline)") ~ 0),
         bnt_q2 = case_when(bnt_change_quantile=="2 (no change)" ~ 1,
                            bnt_change_quantile %in% c("1 (improve)", "3 (decline)") ~ 0),
         bnt_q3 = case_when(bnt_change_quantile=="3 (decline)" ~ 1,
                            bnt_change_quantile %in% c("1 (improve)", "2 (no change)") ~ 0),
         
         confront_wo_bnt_q1 = case_when(confront_wo_bnt_change_quantile=="1 (improve)" ~ 1,
                                        confront_wo_bnt_change_quantile %in% c("2 (no change)", "3 (decline)") ~ 0),
         confront_wo_bnt_q2 = case_when(confront_wo_bnt_change_quantile=="2 (no change)" ~ 1,
                                        confront_wo_bnt_change_quantile %in% c("1 (improve)", "3 (decline)") ~ 0),
         confront_wo_bnt_q3 = case_when(confront_wo_bnt_change_quantile=="3 (decline)" ~ 1,
                                        confront_wo_bnt_change_quantile %in% c("1 (improve)", "2 (no change)") ~ 0),

  )




saveRDS(adni_lan_recoded,   file=path(r_objects_folder, "025_adni_lan_recoded.rds"))  
saveRDS(adni_lan_recoded_quantile,   file=path(r_objects_folder, "025_adni_lan_recoded_quantile.rds"))


