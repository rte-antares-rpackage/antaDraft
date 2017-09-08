data(load_example)
cty_rules <- system.file(package = "entsoe",
                         "templates/cty_rules.yaml")
val_rules <- system.file(package = "entsoe",
                         "templates/raw/validate.yml")
fp_rules <- system.file(package = "entsoe",
                        "templates/raw/false_positives.yml")
correct_rules <- system.file(package = "entsoe", "templates/aggregated/correct.yml")
raw_db <- augment_rules(load_example, file_rules = cty_rules)
raw_db <- augment_validation(data = raw_db, val_rules = val_rules,
                             fp_rules = fp_rules)

val_rules <- system.file(package = "entsoe",
                         "templates/aggregated/validation.yml")
fp_rules <- system.file(package = "entsoe",
                        "templates/aggregated/false_positives.yml")
agg_data <- aggregate_with_rules(raw_db)

agg_data <- agg_data[agg_data$country %in% "FRANCE", ]
agg_data <- agg_data[1:4,]
agg_data$BZN <- agg_data$CTA <- agg_data$CTY
row.names(agg_data)<- NULL

agg_data$CTA[1] <- NA
agg_data$CTA[2] <- 69640
agg_data$CTA[3] <- 65960
agg_data$BZN[4] <- agg_data$CTA[4] <- agg_data$CTY[3] * .4
fr_agg <- agg_data
devtools::use_data(fr_agg, overwrite = TRUE)
