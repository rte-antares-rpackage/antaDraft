library(antadraft)
library(dplyr)
library(lubridate)

rep_path <- "/Users/davidgohel/Documents/consulting/RTE/load"
do_calc <- FALSE

if( do_calc && !exists("load_db")){
  load_db <- rep_path %>% read_load_files() %>% fake_raw_data()
  fst::write.fst(load_db, "load_db.fst")
} else if( !do_calc && !exists("load_db") ) {
  load_db <- fst::read.fst("load_db.fst")
  load_db$DateTime <- as_datetime(load_db$DateTime )
}

db <- fortify_from_rules(raw_db = load_db )

db_errors <- qualcon(db)
errors_summary <- fortify_qualcon(db_errors)

db_issues <- extract_raw_data(raw_db = load_db, issues_db = db_errors)

yaml_correct <- system.file(package = "antadraft", "yaml_data/correction/cty_correct.yml" )
all_report <- report_errors_summary( errors_summary, db_issues, dir = "reports" )

