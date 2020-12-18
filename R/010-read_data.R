

load(path(adni_lan_folder, "1 animal raw", ".RData"))
ADNIanimal_raw_wide <- ADNIsub.animal %>%
  as_tibble() %>%
  filter(visnum %in% c(0, 6)) %>%
  mutate(test = str_c("animalsc_raw", visnum)) %>%
  select(rid, test, catanimsc) %>%
  pivot_wider(id_cols = "rid", names_from = test, values_from = catanimsc)
adni_animal_raw <- THEMATRIXM %>%
  as_tibble() %>%
  left_join(ADNIanimal_raw_wide, by = "rid") %>%
  mutate(phase = as.numeric(phase)) %>%
  select(-catanimsc) %>%
  rename(animalsc_raw_change = diff)

load(path(adni_lan_folder, "2 animal category", ".RData"))
ADNIanimal_cat_wide <- ADNIsub.change %>%
  as_tibble() %>%
  filter(visnum %in% c(0, 6)) %>%
  mutate(test = str_c("animalsc_cat", visnum)) %>%
  select(rid, test, catanimsc) %>%
  pivot_wider(id_cols = "rid", names_from = test, values_from = catanimsc)
adni_animal_cat <- THEMATRIXM %>%
  as_tibble() %>%
  left_join(ADNIanimal_cat_wide, by = "rid") %>%
  select(-score0, -score6, -catanimsc) %>%
  rename(animalsc_cat_change = diff)

load(path(adni_lan_folder, "3 letter f raw", ".RData"))
ADNIffluency_raw_wide <- ADNIsub %>%
  as_tibble() %>%
  filter(visnum %in% c(0, 6)) %>%
  mutate(test = str_c("ffluency_raw", visnum)) %>%
  select(rid, test, ffluency) %>%
  pivot_wider(id_cols = "rid", names_from = test, values_from = ffluency)
adni_f_raw <- THEMATRIXM %>%
  as_tibble() %>%
  left_join(ADNIffluency_raw_wide, by = "rid") %>%
  select(-score0, -score6, -ffluency) %>%
  mutate(phase = as.numeric(phase)) %>%
  rename(ffluency_raw_change = diff)

load(path(adni_lan_folder, "4 letter f category", ".RData"))
ADNIffluency_cat_wide <- ADNIsub.change %>%
  as_tibble() %>%
  filter(visnum %in% c(0, 6)) %>%
  mutate(test = str_c("ffluency_cat", visnum)) %>%
  select(rid, test, ffluency) %>%
  pivot_wider(id_cols = "rid", names_from = test, values_from = ffluency)
adni_f_cat <- THEMATRIXM %>%
  as_tibble() %>%
  left_join(ADNIffluency_cat_wide, by = "rid") %>%
  select(-score0, -score6, -ffluency) %>%
  rename(ffluency_cat_change = diff)

load(path(adni_lan_folder, "5 confrontational", ".RData"))
ADNIconfront_wide <- ADNI.confront %>%
  as_tibble() %>%
  filter(visnum %in% c(0, 6)) %>%
  mutate(test = str_c("confront", visnum)) %>%
  select(rid, test, t1) %>%
  pivot_wider(id_cols = "rid", names_from = test, values_from = t1)
adni_confront <- THEMATRIXM %>%
  as_tibble() %>%
  left_join(ADNIconfront_wide, by = "rid") %>%
  select(-score0, -score6, -t1) %>%
  rename(confront_change = diff)

load(path(adni_lan_folder, "6 others", ".RData"))
ADNIothers_wide <- ADNI.lan.other %>%
  as_tibble() %>%
  filter(visnum %in% c(0, 6)) %>%
  mutate(test = str_c("others", visnum)) %>%
  select(rid, test, t1) %>%
  pivot_wider(id_cols = "rid", names_from = test, values_from = t1)
adni_others <- THEMATRIXM %>%
  as_tibble() %>%
  left_join(ADNIothers_wide, by = "rid") %>%
  select(-score0, -score6, -t1) %>%
  rename(others_change = diff)

adni_lan <- adni_animal_raw %>%
  left_join(adni_animal_cat, by = c("rid", "visnum", "phase", "bl_dx", "dxcurren", "bl_dx_factor")) %>%
  left_join(adni_f_raw, by = c("rid", "visnum", "phase", "bl_dx", "dxcurren", "bl_dx_factor")) %>%
  left_join(adni_f_cat, by = c("rid", "visnum", "phase", "bl_dx", "dxcurren", "bl_dx_factor")) %>%
  left_join(adni_confront, by = c("rid", "visnum", "phase", "bl_dx", "dxcurren", "bl_dx_factor")) %>%
  left_join(adni_others, by = c("rid", "visnum", "phase", "bl_dx", "dxcurren", "bl_dx_factor")) %>%
  select(rid, visnum, phase, bl_dx, bl_dx_factor, everything()) %>%
  select(-dxcurren)

saveRDS(adni_lan,   file=path(r_objects_folder, "010_adni_lan.rds"))

# If connected to the network drive, 
# else skip to the next file and read in the saved rds
if(fs::dir_exists(network_projectfolder)) {
  # File was created by Elizabeth Sanders and shared via dropbox on 2019-08-08
  adni <- read_dta(path(network_derivedfolder, "ADNI_LE_190808.dta"))
  
  adni <- adni %>%
    filter(!viscode %in% c("sc", "scmri")) %>%
    filter(phase != "") %>%
    select(-IDENTIFIERS, -DEMOGRAPHICS, -DX, -NEUROPSYCH_created) %>%
    mutate(VSP_score = as.numeric(VSP_score),
           LAN_score = as.numeric(LAN_score),
           bl_dx_factor = factor(bl_dx, levels = 1:3, labels = c("Normal", "MCI", "AD"), ordered = TRUE))
    
  adni <- adni %>%
    group_by(rid) %>%
    mutate(phase_bl = case_when(viscode=="bl" ~ phase),
           phase_bl = factor(phase_bl, levels = c("ADNI1", "ADNIGO", "ADNI2", "ADNI3")),
           phase_bl = fct_collapse(phase_bl, 
                                   "ADNI1" = c("ADNI1"),
                                   "ADNIGO/2" = c("ADNIGO", "ADNI2"),
                                   "ADNI3" = c("ADNI3")),
           examdate = lubridate::as_date(examdate, origin = as.Date("1960-01-01")),
           examdate_bl = case_when(viscode=="bl" ~ examdate)) %>%
    fill(phase_bl, .direction = "down") %>%
    fill(examdate_bl, .direction = "down") %>%
    ungroup()
  
  
  adni.bondi <- read_spss(path(network_derivedfolder, "ADNI_newDX_1.18.19_robustm72_LONG.sav"))
  
  goo <-  adni.bondi %>%
    rename_all(tolower) %>%
    select(rid, visit, mci_comp, mci_comp_subtype)
  
  hoo <- adni %>%
    left_join(goo, by = c("rid" = "rid", "visnum" = "visit")) %>%
    select(rid, visnum, mci_comp, mci_comp_subtype, dxcurren, everything())
  
  # Tried to recreate mci_comp according to syntax from SPSS file. 
  # Need more information on how the variables should be scaled.  
  # Using the provided mci_comp variable.
  # xxx <- adni.bondi %>%
  #   rename_all(tolower) %>%
  #   filter(visit==0) %>%
  #   select(rid, age, pteducat, ptgender, avdel30min, avlt_recog, catanimsc, bnttotal, traascor, trabscor, limmtotal, ldeltotal) %>%
  #   mutate(age = as.numeric(age),
  #          pteducat = as.numeric(pteducat),
  #          ptgender = as.numeric(ptgender),
  #          avdel30min = as.numeric(avdel30min),
  #          avlt_recog = as.numeric(avlt_recog),
  #          catanimsc = as.numeric(catanimsc),
  #          bnttotal = as.numeric(bnttotal),
  #          traascor = as.numeric(traascor),
  #          trabscor = as.numeric(trabscor),
  #          limmtotal = as.numeric(limmtotal),
  #          ldeltotal = as.numeric(ldeltotal),
  #          traascor_revcoded = traascor * -1,
  #          trabscor_revcoded = trabscor * -1,
  #          predicted_avdel30min        =  6.259 + ( 0.006 * age) + (1.564 * pteducat) + ( 1.231 * ptgender),
  #          predicted_avlt_recog        =  8.493 + (-0.063 * age) + (1.260 * pteducat) + (-0.852 * ptgender),
  #          predicted_catanimsc         =  6.647 + (-0.002 * age) + (0.193 * pteducat) + ( 0.722 * ptgender),
  #          predicted_bnttotal          = -0.693 + (-0.007 * age) + (0.107 * pteducat) + ( 0.053 * ptgender),
  #          predicted_traascor_revcoded = -2.542 + (-0.009 * age) + (0.507 * pteducat) + ( 0.046 * ptgender),
  #          predicted_trabscor_revcoded = 13.640 + (-0.059 * age) + (1.606 * pteducat) + ( 1.189 * ptgender),
  #          predicted_limmtotal         = -0.523 + ( 0.015 * age) + (0.064 * pteducat) + ( 0.045 * ptgender),
  #          predicted_ldeltotal         = -0.523 + ( 0.015 * age) + (0.064 * pteducat) + ( 0.045 * ptgender),
  #          z_avdel30min        = (avdel30min        - predicted_avdel30min       ) / 3.790,
  #          z_avlt_recog        = (avlt_recog        - predicted_avlt_recog       ) / 3.555,
  #          z_catanimsc         = (catanimsc         - predicted_catanimsc        ) / 1.565,
  #          z_bnttotal          = (bnttotal          - predicted_bnttotal         ) / 0.292,
  #          z_traascor_revcoded = (traascor_revcoded - predicted_traascor_revcoded) / 1.235,
  #          z_trabscor_revcoded = (trabscor_revcoded - predicted_trabscor_revcoded) / 3.375,
  #          z_limmtotal         = (limmtotal         - predicted_limmtotal        ) / 0.563,
  #          z_ldeltotal         = (ldeltotal         - predicted_ldeltotal        ) / 0.563,
  #          z_avdel30min_impaired        = case_when(z_avdel30min       < -1 ~ 1,
  #                                                   z_avdel30min       >= -1 ~ 0),
  #          z_avlt_recog_impaired        = case_when(z_avlt_recog       < -1 ~ 1,
  #                                                   z_avlt_recog       >= -1 ~ 0),
  #          z_catanimsc_impaired         = case_when(z_catanimsc        < -1 ~ 1,
  #                                                   z_catanimsc        >= -1 ~ 0),
  #          z_bnttotal_impaired          = case_when(z_bnttotal         < -1 ~ 1,
  #                                                   z_bnttotal         >= -1 ~ 0),
  #          z_traascor_revcoded_impaired = case_when(z_traascor_revcoded< -1 ~ 1,
  #                                                   z_traascor_revcoded>= -1 ~ 0),
  #          z_trabscor_revcoded_impaired = case_when(z_trabscor_revcoded< -1 ~ 1,
  #                                                   z_trabscor_revcoded>= -1 ~ 0),
  #          z_limmtotal_impaired         = case_when(z_limmtotal        < -1 ~ 1,
  #                                                   z_limmtotal        >= -1 ~ 0),
  #          z_ldeltotal_impaired         = case_when(z_ldeltotal        < -1 ~ 1,
  #                                                   z_ldeltotal        >= -1 ~ 0),
  #          
  #          mem_impaired = case_when(z_avdel30min_impaired + z_avlt_recog_impaired >=2 ~ 1,
  #                                   z_avdel30min_impaired + z_avlt_recog_impaired <=2 ~ 0),
  #          lang_impaired = case_when(z_catanimsc_impaired + z_bnttotal_impaired >=2 ~ 1,
  #                                    z_catanimsc_impaired + z_bnttotal_impaired <=2 ~ 0),
  #          attnexec_impaired = case_when(z_traascor_revcoded_impaired + z_trabscor_revcoded_impaired >=2 ~ 1,
  #                                        z_traascor_revcoded_impaired + z_trabscor_revcoded_impaired <=2 ~ 0),
  #          mci_comp = case_when(mem_impaired + lang_impaired + attnexec_impaired >=1 ~ 1,
  #                               mem_impaired + lang_impaired + attnexec_impaired ==0 ~ 0),
  #          mci_comp_subtype = case_when(mem_impaired + lang_impaired + attnexec_impaired ==0 ~ 0,
  #                                       mem_impaired==1 & lang_impaired==0 & attnexec_impaired==0 ~ 1,
  #                                       mem_impaired==0 & (lang_impaired + attnexec_impaired ==1) ~ 2,
  #                                       mem_impaired==1 & (lang_impaired + attnexec_impaired ==2) ~ 3,
  #                                       mem_impaired==0 & (lang_impaired + attnexec_impaired ==2) ~ 4),
  #          mci_comp_subtype = factor(mci_comp_subtype, levels = c(0:4), 
  #                                    labels = c("None", "Amnestic, single domain", 
  #                                               "Non-Amnestic, single domain", 
  #                                               "Amnestic, multiple domain", 
  #                                               "Non-Amnestic, multiple domain")))
  # 
 
  saveRDS(adni,   file=path(r_objects_folder, "010_adni.rds"))
}


