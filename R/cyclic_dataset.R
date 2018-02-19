cyclic_dataset <- function(x, y = "TotalLoadValue",
                           gp_col = "DateTime",
                           group_col = "country",
                           measures_col = "AreaTypeCode" ){
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[[measures_col]])
  # browser()
  cyclic_computations <- dimensions[!dimensions$simple_type,]
  # print(cyclic_computations)
  cyclic_computations$id <- seq_along(cyclic_computations[[group_col]])
  cyclic_computations <- as.data.frame(cyclic_computations)
  pivot_data <- unique(cyclic_computations[, c("rel_ctry", "rel", "prod", "id") ])
  pivot_data <- as.data.frame(pivot_data)


  add_db <- merge( as.data.table(x), pivot_data,
                   by.x = c(group_col, measures_col),
                   by.y = c("rel_ctry", "rel"),
                   all = FALSE)
  add_db[[y]] <- add_db[[y]] * add_db$prod
  setnames(add_db, y, "TotalLoadValue")

  # na.rm = TRUE pour noter les aggregations avec donnees manquantes
  add_db <- add_db[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
                   by=c("id", gp_col)]
  setnames(add_db, "TotalLoadValue", y)
  add_db <- merge( add_db, cyclic_computations[, c(group_col, measures_col, "id") ],
                   by = "id", all = FALSE)
  add_db$id <- NULL

  add_db
}

