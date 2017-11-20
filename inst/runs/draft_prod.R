library(readr)
library(tidyverse)
library(stringr)

production_channel <- anta_prod_channel(
  data_dir = "/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/B01-Production_réalisée_par_filière")


production <- list.files("/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/B01-Production_réalisée_par_filière",
                 pattern = "\\.csv", full.names = TRUE) %>%
  map_df(function(path){
    read_delim(path, delim = "\t")
  }) %>%
  mutate(ProductionType_Name = str_trim(ProductionType_Name) ) %>%
  select(-year, -month, -day, -SubmissionTS) %>%
  filter(ProductionType_Name %in%
           c(charbon="Fossil Hard coal",
             charbonpdtpargaz = "Fossil Coal-derived gas",
             lignite = "Fossil Brown coal/Lignite",
             nucleaire = "Nuclear", diesel = "Fossil Oil",
             gaz = "Fossil Gas", tourbe = "Fossil Peat",
             kerogene = "Fossil Oil shale"
         ) )
library(antaDraft)
dimensions <- antaDraft:::get_rules( add_complex = FALSE )
dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
production <- inner_join(production, dimensions, by = c("MapCode", "AreaTypeCode"))

head(production)
View(sample_n(production, size = 200))


zz <- list.files("/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/B02-Production_réalisée_par_groupe",
           pattern = "\\.csv", full.names = TRUE) %>%
  map_df(function(path){
    read_delim(path, delim = "\t")
  }) %>%
  mutate(ProductionTypeName = str_trim(ProductionTypeName) ) %>%
  filter(ProductionTypeName %in% c(charbon="Fossil Hard coal",
                                  charbonpdtpargaz = "Fossil Coal-derived gas",
                                  lignite = "Fossil Brown coal/Lignite",
                                  nucleaire = "Nuclear", diesel = "Fossil Oil",
                                  gaz = "Fossil Gas", tourbe = "Fossil Peat",
                                  kerogene = "Fossil Oil shale"
       ) )


View(sample_n(zz, size = 200))

do_ts_plot <- function(x, PowerSystemResourceName, ProductionTypeName){
  ggplot(x, aes(x = DateTime, y = InstalledGenCapacity)) +
    geom_line() + scale_y_continuous(limits = c(0, NA)) + scale_x_datetime(labels = scales::date_format("%Y-%m-%d")) +
    labs(title = PowerSystemResourceName, subtitle = ProductionTypeName ) + theme_minimal()
}

all_plots <- zz %>% select(PowerSystemResourceName, ProductionTypeName, DateTime, InstalledGenCapacity) %>%
  group_by(PowerSystemResourceName, ProductionTypeName) %>% nest() %>%
  ungroup() %>%
  mutate(
    gg = pmap( list(x=data,
                    PowerSystemResourceName = PowerSystemResourceName,
                    ProductionTypeName = ProductionTypeName),
               do_ts_plot) )

# pdf()
# walk(all_plots$gg, function(x) (print(x)))
# dev.off()





capaciteannuelle <- list.files("/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/B06-Capacité_installée_par_filière",
                 pattern = "\\.csv", full.names = TRUE) %>%
  map_df(function(path){
    read_delim(path, delim = "\t")
  })




