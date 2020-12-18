rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "002-folder_paths_and_options.R"))

# It's input is the following R objects:
adni <- readRDS(file=path(r_objects_folder, "010_adni.rds"))

# Who are the people missing viscode/visnum?
# Keeping only ADNI1 and ADNIGO/2
adni.filtered <- adni %>%
  filter(!viscode %in% c("sc", "scmri")) %>%
  filter(!viscode %in% c("m03")) %>%
  filter(!is.na(visnum)) %>%
  filter(phase_bl!="ADNI3") %>%
  filter(blonly==0)


# Using only the 6M followup for retest
adni.retest <- adni.filtered %>%
  group_by(rid) %>%
  arrange(rid, visnum) %>%
  mutate(nrows = n()) %>%
  filter(nrows > 1) %>%
  filter(visnum %in% c(0, 6)) %>%
  ungroup()
  
saveRDS(adni.filtered,   file=path(r_objects_folder, "020_adni_filtered.rds"))
saveRDS(adni.retest,   file=path(r_objects_folder, "020_adni_retest.rds"))  
