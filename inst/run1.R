remove( list = ls() )

library(antadraft)
library(magrittr)

rep_path <- "D:/transparency_repo/staging_area/incoming/A-CONSOMMATION/A01-Consommation_réalisée"
load_db <- rep_path %>% import_load_db()
load_db <- load_db %>% filter(year > 2014)

db <- build_db(raw_db = load_db )

db_erros <- qualcon(db, yaml_rules = "inst/validation_rules.yml")
sum_db_erros <- fortify_qualcon(db_erros) %>%
  mutate( duration = end - start )

sum_db_erros %>% arrange(country, start, end)

xx <- extract_raw_data(load_db, db_erros)

Sys.setenv("R_ZIPCMD" = "D:/programmes/Rtools/bin/zip.exe")
for(i in unique( xx$country ) ){
  dat <- xx[xx$country %in% i, ]
  openxlsx::write.xlsx(x = dat, file = paste0("output/", i, ".xlsx"))
}

