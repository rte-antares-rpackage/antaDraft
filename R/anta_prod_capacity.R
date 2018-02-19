read_prod_capacity <- function( data_dir = NULL, join_class, production_file){
  stopifnot(dir.exists(data_dir))

  id_vars <- c("DateTime", "AreaTypeCode", "MapCode", "ProductionType_Name")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  drops <- c("year", "month", "day", "AreaName")
  data <- entsoe_dir_reader(dir = data_dir, datetime_col = time_vars,
                            submissin_col = submission_time_var,
                            drops = drops,
                            id_vars = id_vars)

  setnames(data, "AggregatedInstalledCapacity", "installed_capacity")
  setnames(data, "ProductionType_Name", "production_type")

  data <- data[installed_capacity>0]

  data <- ref_join_class(x = data, classobj = join_class,
                         date_time = time_vars, production_file)

  # data <- ref_join_class(x = data, classobj = join_class, date_time = time_vars)
  data
}


