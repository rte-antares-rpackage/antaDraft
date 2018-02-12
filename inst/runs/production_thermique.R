library(antaDraft)
library(magrittr)

# Production thermique par types -----
main_dir <- "quality/thermique"
dir.create(main_dir, recursive = TRUE, showWarnings = TRUE)

production_dir <- "/Users/davidgohel/Documents/consulting/RTE/prod_20180115/B01"
capacity_dir <- "/Users/davidgohel/Documents/consulting/RTE/prod_20180115/B06"

global_options <- getOption("global_options")
p_thermique_file <- global_options$thermal_production_per_country

PTT <- read_prod_type(
  production_dir = production_dir,
  capacity_dir = capacity_dir,
  production_file = p_thermique_file) %>%
  augment_validation()

qualcon(PTT) %>% render_quality( file.path(main_dir, "types_raw") )

PTTA <- agg_data(PTT) %>% augment_validation()

qualcon(PTTA) %>% render_quality( file.path(main_dir, "types_agg") )

# Production thermique par groupes -----

production_dir <- "/Users/davidgohel/Documents/consulting/RTE/prod_20180115/B02"
PTG <- read_prod_group(
  production_dir = production_dir,
  production_file = p_thermique_file) %>%
  augment_validation()

qualcon(PTG) %>% render_quality( file.path(main_dir, "groupes_raw") )

# Comparaison Production thermique par groupes et types -----

comp_data <- prod_compare(PTT, PTG) %>%
  augment_validation()
qualcon(comp_data) %>% render_quality( file.path(main_dir, "cta_prod") )
