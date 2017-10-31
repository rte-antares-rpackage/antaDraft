<!-- README.md is generated from README.Rmd. Please edit that file -->
antadata
========

Le package contient un ensemble de fonctions pour vérifier et corriger
des données entsoe.

Example
-------

### Préparation des données brutes

``` r
library(antadata)
library(magrittr)
library(flextable)
#> Loading required package: officer
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names
library(randomForest)
#> randomForest 4.6-12
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Attaching package: 'randomForest'
#> The following object is masked from 'package:dplyr':
#> 
#>     combine

load_data <- anta_load_read( data_dir = Sys.getenv("LOAD_DIR") ) %>% 
  augment_validation()
head(load_data)
#> # A tibble: 6 x 10
#>     DateTime country AreaTypeCode AreaName  MapCode TotalLoadValue
#>       <dttm>   <chr>        <chr>    <chr>    <chr>          <dbl>
#> 1 2014-12-01 AUSTRIA          BZN DE-AT-LU DE_AT_LU       11239.13
#> 2 2014-12-01 BELGIUM          CTA  Elia CA       BE        9100.34
#> 3 2014-12-01 BELGIUM          CTY  Belgium       BE        9100.34
#> 4 2014-12-01 BELGIUM          BZN  Elia BZ       BE        9100.34
#> 5 2014-12-01  FRANCE         <NA>     <NA>     <NA>             NA
#> 6 2014-12-01 GERMANY          CTY  Germany       DE       11239.13
#> # ... with 4 more variables: observed <lgl>, IS_OBS <lgl>,
#> #   IS_FINITE <lgl>, IS_POS <lgl>
```

### Préparation des données agrégées

``` r

agg_db <- aggregate_with_rules(load_data) %>% 
  augment_validation() 
head(agg_db)
#>   country            DateTime      BZN CTA CTY CTY_NA CTA_NA BZN_NA
#> 1 AUSTRIA 2014-12-01 00:00:00 11239.13  NA  NA  FALSE  FALSE   TRUE
#> 2 AUSTRIA 2014-12-01 01:00:00 10922.59  NA  NA  FALSE  FALSE   TRUE
#> 3 AUSTRIA 2014-12-01 02:00:00 10996.24  NA  NA  FALSE  FALSE   TRUE
#> 4 AUSTRIA 2014-12-01 03:00:00 11184.66  NA  NA  FALSE  FALSE   TRUE
#> 5 AUSTRIA 2014-12-01 04:00:00 12234.47  NA  NA  FALSE  FALSE   TRUE
#> 6 AUSTRIA 2014-12-01 05:00:00 14838.97  NA  NA  FALSE  FALSE   TRUE
#>   CTY_IS_POS CTA_IS_POS BZN_IS_POS CTY_CTA_EQUAL CTY_BZN_EQUAL
#> 1       TRUE       TRUE       TRUE          TRUE          TRUE
#> 2       TRUE       TRUE       TRUE          TRUE          TRUE
#> 3       TRUE       TRUE       TRUE          TRUE          TRUE
#> 4       TRUE       TRUE       TRUE          TRUE          TRUE
#> 5       TRUE       TRUE       TRUE          TRUE          TRUE
#> 6       TRUE       TRUE       TRUE          TRUE          TRUE
#>   CTA_BZN_EQUAL CTY_CTA_DIFF_LT_05 CTY_BZN_DIFF_LT_05 CTA_BZN_DIFF_LT_05
#> 1          TRUE               TRUE               TRUE               TRUE
#> 2          TRUE               TRUE               TRUE               TRUE
#> 3          TRUE               TRUE               TRUE               TRUE
#> 4          TRUE               TRUE               TRUE               TRUE
#> 5          TRUE               TRUE               TRUE               TRUE
#> 6          TRUE               TRUE               TRUE               TRUE
#>   CTY_CTA_DIFF_LT_10 CTY_BZN_DIFF_LT_10 CTA_BZN_DIFF_LT_10 CTY_LAG_LT_50
#> 1               TRUE               TRUE               TRUE          TRUE
#> 2               TRUE               TRUE               TRUE          TRUE
#> 3               TRUE               TRUE               TRUE          TRUE
#> 4               TRUE               TRUE               TRUE          TRUE
#> 5               TRUE               TRUE               TRUE          TRUE
#> 6               TRUE               TRUE               TRUE          TRUE
#>   CTA_LAG_LT_50 BZN_LAG_LT_50
#> 1          TRUE         FALSE
#> 2          TRUE          TRUE
#> 3          TRUE          TRUE
#> 4          TRUE          TRUE
#> 5          TRUE          TRUE
#> 6          TRUE          TRUE
```

### Correction mécanique des données agrégées

``` r
agg_db <- data_correct_with_rules(agg_db) 
```

### Vérification des flags sur données corrigées, originales

``` r
agg_db <- augment_process_summary(agg_db) 
agg_db %>%
  group_by(country, summary ) %>%
  tally() %>% ungroup() %>%
  tidyr::complete(country, summary, fill = list(n=0) ) %>%
  tidyr::spread(summary, n) %>%
  regulartable() %>%
  theme_booktabs() %>% autofit()
#> # A tibble: 13 x 4
#>          country corrected invalid original
#>  *         <chr>     <dbl>   <dbl>    <dbl>
#>  1       AUSTRIA         0   22162        0
#>  2       BELGIUM        26     167    21706
#>  3        FRANCE         0       3    21774
#>  4       GERMANY         0   22140       22
#>  5       IRELAND         0   20639      595
#>  6         ITALY         0      50    20890
#>  7    LUXEMBOURG         0   22162        0
#>  8   NETHERLANDS         3     886    21271
#>  9 NORTH_IRELAND       144   21108        0
#> 10      PORTUGAL         0    1048    20944
#> 11         SPAIN         0     777    20900
#> 12   SWITZERLAND         0     301    21236
#> 13            UK         0   17965     3534
```

### Préparation des jeux de données en vue de la modélisation

``` r
agg_db <- augment_holiday(agg_db) %>% 
  augment_seasons_id() %>% 
  augment_daylight() %>% 
  augment_daily(col = "CTY", decay = 1) %>% 
  augment_daily(col = "CTY", decay = 2)

CTY_H1 <- agg_db %>%
  select(country, DateTime, CTY) %>%
  group_by(country) %>%
  transmute(CTY_H1 = CTY, DateTime = DateTime - 60*60) %>%
  ungroup()
#> Adding missing grouping variables: `country`
agg_db <- agg_db %>% left_join(CTY_H1, by = c("DateTime", "country") )


training_datasets <- agg_db %>%
  filter( year.iso %in% c(2015, 2016), summary %in% "original" ) %>%
  group_by_at("country") %>%
  tidyr::nest() 

selected_ctry <- agg_db %>% filter( 
  year.iso %in% c(2015, 2016), summary %in% "original" ) %>%
  group_by_at("country") %>% tally() %>% 
  ungroup() %>% filter(n > 16000)

rf_formula <- CTY ~ week.iso + day.iso + hour.iso +
  light_time + is_off + likely_off + AVG_CTY_D1 + 
  MIN_CTY_D1 + MAX_CTY_D1 + AVG_CTY_D2 + MIN_CTY_D2 + 
  MAX_CTY_D2 + CTY_H1

library(randomForest)
rt_by_country <- training_datasets %>% 
  semi_join(selected_ctry) %>% 
  mutate(model = map(data, function(x){
  randomForest(formula = rf_formula, data = x, na.action = na.omit)
}))
#> Joining, by = "country"


rt_by_country %>% 
  mutate( r.sq = map_dbl(model, function(x) mean(x$rsq) ) )
#> # A tibble: 7 x 4
#>       country                   data                      model      r.sq
#>         <chr>                 <list>                     <list>     <dbl>
#> 1     BELGIUM <tibble [17,398 x 39]> <S3: randomForest.formula> 0.9817431
#> 2      FRANCE <tibble [17,617 x 39]> <S3: randomForest.formula> 0.9877654
#> 3       ITALY <tibble [17,091 x 39]> <S3: randomForest.formula> 0.9905407
#> 4 NETHERLANDS <tibble [16,790 x 39]> <S3: randomForest.formula> 0.9880920
#> 5    PORTUGAL <tibble [17,096 x 39]> <S3: randomForest.formula> 0.9872155
#> 6       SPAIN <tibble [17,060 x 39]> <S3: randomForest.formula> 0.9858113
#> 7 SWITZERLAND <tibble [17,390 x 39]> <S3: randomForest.formula> 0.9680295
unique(agg_db$country)
#>  [1] "AUSTRIA"       "BELGIUM"       "FRANCE"        "GERMANY"      
#>  [5] "IRELAND"       "ITALY"         "LUXEMBOURG"    "NETHERLANDS"  
#>  [9] "NORTH_IRELAND" "PORTUGAL"      "SPAIN"         "SWITZERLAND"  
#> [13] "UK"

country_datasets <- agg_db %>%
  filter( year.iso %in% c(2015, 2016) ) %>%
  group_by_at("country") %>%
  tidyr::nest() %>% 
  left_join(rt_by_country[,-2]) %>% 
  mutate( data = map2(model, data, function(model, data){
    data <- data[ data$summary %in% "invalid",]
    if( !is.null(model) && !inherits(model, "try-error"))
      prev <- as.vector(predict(model, newdata = data))
    else prev <- rep(NA_real_, nrow(data) )
    data$CTY <- prev
    data$summary <- "mod_rf"
    data[!is.na(prev),]
  } ) ) %>% 
  select(-model) %>% 
  tidyr::unnest(data) 
#> Joining, by = "country"
```
