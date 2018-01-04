library(antaDraft)

production_channel <- anta_prod_channel(
  production_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B01-Production_réalisée_par_filière",
  capacity_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B06-Capacité_installée_par_filière"
  )
production_channel <- augment_validation(production_channel)

ccc <- qualcon(production_channel)
render_quality(ccc, "coco" )


prop.table(table(production_channel$IS_OBS))
prop.table(table(production_channel$IS_FINITE_OUTPUT))
prop.table(table(production_channel$IS_NOT_NEG_OUTPUT))
prop.table(table(production_channel$OUTPUT_LT_CAPACITY))
