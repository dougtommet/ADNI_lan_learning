
library(tidyverse)
library(here)
library(fs)
library(haven)
library(kableExtra)
library(rlang)
library(gghighlight)
library(survival)
library(survminer)
library(broom)
library(gt)
library(glue)

path_to_source <- path_home("documents", "dwork", "r", "rsource")
if (file.exists(path(path_to_source, "ggplot-theme.r"))) {
  source(path(path_to_source, "ggplot-theme.r"))
}

wftree_network_path <- function(windows_path = fs::path("R:"), mac_path = fs::path("/Volumes", "Research")) {
  # Need to add a condition for linux users
  windows_machine <- ifelse(grepl("windows", Sys.info()['sysname'], ignore.case=T), 1, 0)
  if(windows_machine==1) {
    # For Windows machines
    network_start <- windows_path
  } else {
    # For Macs
    network_start <- mac_path
  }
  networkpath <- fs::path(network_start, "BM_QuantitativeSciencesPrg")
  networkpath
}

