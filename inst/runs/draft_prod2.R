library(antaDraft)

production_channel <- anta_prod_channel(
  production_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B01-Production_réalisée_par_filière",
  capacity_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B06-Capacité_installée_par_filière"
  )
production_channel <- augment_validation(production_channel)

# library(dplyr)
# library(tidyr)
# production_channel %>%
#   as_tibble() %>%
#   filter( is.na(generation_output) ) %>%
#   group_by(country, production_type) %>%
#   tally() %>%
#   spread(production_type, n) %>%
#   View


ccc <- qualcon(production_channel)
render_quality(ccc, "coco" )

prod_agg <- aggregate_prod_with_rules(production_channel)
prod_agg <- augment_validation(prod_agg)

ddd <- qualcon(prod_agg)
render_quality(ddd, "tutu" )




prod_group_data <- anta_prod_group(production_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B02-Production_réalisée_par_groupe")
