
# renv::init()

renv::snapshot()


rmarkdown::render(here::here("R", "000-master.Rmd"))


fs::file_move(here::here("R", "000-master.html"), 
              here::here("Reports", stringr::str_c("ADNI_lang_retest_", Sys.Date(),".html")))

