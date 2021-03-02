knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)


set.seed(2017)


network_projectfolder <- fs::path(wftree_network_path(), "STUDIES", "ADNI_PSYCHOMETRICS", "PROJECTS", 
                                  "ADNI_PSYCH_LEARNING")
network_datafolder <- fs::path(network_projectfolder, "POSTED", "DATA")
network_derivedfolder <- fs::path(network_datafolder, "DERIVED")

here_directory_split <- fs::path_split(here::here())
up_one_level <- fs::path_join(here_directory_split[[1]][-length(here_directory_split[[1]])])
r_objects_folder <- fs::path(up_one_level, "R_OBJECTS")
adni_lan_folder <- fs::path(up_one_level, "ADNI-lan-by-flavor")
adni_lan_folder2 <- fs::path(up_one_level, "ADNI-lan-bnt-and-confrontation")
rm(here_directory_split)
analysisfolder <- here::here("R")



# create folder to save mplus output that is ignored by github
if (!fs::dir_exists(here::here("mplus_ignore"))) {
  fs::dir_create(here::here("mplus_ignore"))
}
# create folder to save mplus output that will be on github
if (!fs::dir_exists(here::here("mplus_output"))) {
  fs::dir_create(here::here("mplus_output"))
}

