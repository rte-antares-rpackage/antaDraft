remove( list = ls() )
library(antadraft)

rep_path <- "D:/transparency_repo/staging_area/incoming/A-CONSOMMATION/A01-Consommation_réalisée"
load_db <- rep_path %>% import_load_db() %>% filter(year > 2014)

db <- build_db(raw_db = load_db )


db_erros <- get_invalidated_data(db, yaml_rules = "inst/validation_rules.yml")
sum_db_erros <- fortify_invalidated_data(db_erros)

xx <- extract_raw_data(load_db, db_erros)

for(i in unique( xx$country ) ){
  dat <- xx[xx$country %in% i, ]
  openxlsx::write.xlsx(x = dat, file = paste0("output/", i, ".xlsx"))
}

