<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/rte-antares-rpackage/antaDraft.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaDraft) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/antaDraft?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/antaDraft) [![version](http://www.r-pkg.org/badges/version/antaDraft)](https://CRAN.R-project.org/package=antaDraft) [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![codecov](https://codecov.io/gh/rte-antares-rpackage/antaDraft/branch/master/graph/badge.svg)](https://codecov.io/gh/rte-antares-rpackage/antaDraft)

antaDraft
=========

Le package contient un ensemble de fonctions pour vérifier et corriger des données entsoe.

L'objectif du package est de permettre la création d'un jeu de données complet pour deux années de données et pour plusieurs pays.

Les données peuvent être manquantes ou imparfaites. Il s'agit de permettre :

-   l'identification des problèmes,
-   leur reporting afin de permettre leur correction par l'entsoe,
-   leur correction si la donnée n'a pu être corrigée par l'entsoe.

La procédure est pour l'instant implémentée pour les données de consommation.

Utilisation
-----------

On va charger l'ensemble des données archivées sur archive.org à l'URL suivante : <https://archive.org/details/RTE_load>

Le zip à charger est le suivant: <https://archive.org/download/RTE_load/load.zip>.

``` r
load_dir <- file.path(tempdir(), "load_files" )
load_zip <- file.path(tempdir(), "load.zip" )

local_zip <- "/Users/davidgohel/Documents/consulting/RTE/load.zip"
if( file.exists(local_zip))
  file.copy(local_zip, load_zip, overwrite = TRUE )
#> [1] TRUE

if( !file.exists(load_zip) )
  download.file(url = "https://archive.org/download/RTE_load/load.zip", 
                destfile = load_zip )

if( dir.exists(load_dir) )
  unlink(load_dir, recursive = TRUE, force = TRUE)

utils::unzip(load_zip, exdir = load_dir )
```

Les données sont disponibles dans le répertoire /var/folders/51/6jygptvs3bb4njv0t6x7br900000gn/T//RtmpdzmTvb/load\_files. Celui ci contient les fichiers suivants :

``` r
csv_files <- list.files(load_dir, full.names = TRUE, pattern = "\\.csv$")
csv_infos <- file.info(csv_files)[, c(1, 3) ]
row.names(csv_infos) <- NULL
csv_infos$file <- basename(csv_files)

kable(csv_infos)
```

|      size|  mode| file                          |
|---------:|-----:|:------------------------------|
|   4818386|   644| 2014\_12\_ActualTotalLoad.csv |
|  11754185|   644| 2015\_1\_ActualTotalLoad.csv  |
|  11875151|   644| 2015\_10\_ActualTotalLoad.csv |
|  11310245|   644| 2015\_11\_ActualTotalLoad.csv |
|  11781045|   644| 2015\_12\_ActualTotalLoad.csv |
|  10726041|   644| 2015\_2\_ActualTotalLoad.csv  |
|  11786273|   644| 2015\_3\_ActualTotalLoad.csv  |
|  11696767|   644| 2015\_4\_ActualTotalLoad.csv  |
|  12125893|   644| 2015\_5\_ActualTotalLoad.csv  |
|  11541557|   644| 2015\_6\_ActualTotalLoad.csv  |
|  11875512|   644| 2015\_7\_ActualTotalLoad.csv  |
|  11475232|   644| 2015\_8\_ActualTotalLoad.csv  |
|  11390834|   644| 2015\_9\_ActualTotalLoad.csv  |
|  11418000|   644| 2016\_1\_ActualTotalLoad.csv  |
|  11872808|   644| 2016\_10\_ActualTotalLoad.csv |
|  11450892|   644| 2016\_11\_ActualTotalLoad.csv |
|  11911508|   644| 2016\_12\_ActualTotalLoad.csv |
|  10714419|   644| 2016\_2\_ActualTotalLoad.csv  |
|  11895715|   644| 2016\_3\_ActualTotalLoad.csv  |
|  11049161|   644| 2016\_4\_ActualTotalLoad.csv  |
|  11393888|   644| 2016\_5\_ActualTotalLoad.csv  |
|  11050699|   644| 2016\_6\_ActualTotalLoad.csv  |
|  11617278|   644| 2016\_7\_ActualTotalLoad.csv  |
|  11567217|   644| 2016\_8\_ActualTotalLoad.csv  |
|  11134715|   644| 2016\_9\_ActualTotalLoad.csv  |
|  11780627|   644| 2017\_1\_ActualTotalLoad.csv  |
|  12195737|   644| 2017\_10\_ActualTotalLoad.csv |
|   6721301|   644| 2017\_11\_ActualTotalLoad.csv |
|  10569835|   644| 2017\_2\_ActualTotalLoad.csv  |
|  11947416|   644| 2017\_3\_ActualTotalLoad.csv  |
|  11574205|   644| 2017\_4\_ActualTotalLoad.csv  |
|  11942550|   644| 2017\_5\_ActualTotalLoad.csv  |
|  11595339|   644| 2017\_6\_ActualTotalLoad.csv  |
|  11714825|   644| 2017\_7\_ActualTotalLoad.csv  |
|  11457734|   644| 2017\_8\_ActualTotalLoad.csv  |
|  11212010|   644| 2017\_9\_ActualTotalLoad.csv  |

### Les données brutes

Avant de dégager une série de consommation par pays

``` r
library(antaDraft)
load_data <- anta_load_read(data_dir = load_dir )
```

### Validation des données brutes

L'opération va ajouter autant de colonnes qu'il y a de tests exprimés dans le fichier `config/load/raw_validate.yml`.

Ce fichier décrit les règles de validation de chaque ligne de donnée.

<!--html_preserve-->
<pre>rules:

- expr: observed==TRUE
  name: IS_OBS
- expr: is.finite(TotalLoadValue)
  name: IS_FINITE
- expr: sign(TotalLoadValue)&gt;0
  name: IS_POS
</pre>
<!--/html_preserve-->
``` r
load_data <- augment_validation(load_data)
head(load_data)
#>    MapCode AreaTypeCode   DateTime AreaName TotalLoadValue country
#> 1 DE_AT_LU          BZN 2014-12-01 DE-AT-LU       11239.13 AUSTRIA
#> 2       BE          BZN 2014-12-01  Elia BZ        9100.34 BELGIUM
#> 3       BE          CTA 2014-12-01  Elia CA        9100.34 BELGIUM
#> 4       BE          CTY 2014-12-01  Belgium        9100.34 BELGIUM
#> 5     <NA>         <NA> 2014-12-01     <NA>             NA  FRANCE
#> 6       DE          CTY 2014-12-01  Germany       11239.13 GERMANY
#>   observed IS_OBS IS_FINITE IS_POS
#> 1     TRUE   TRUE      TRUE   TRUE
#> 2     TRUE   TRUE      TRUE   TRUE
#> 3     TRUE   TRUE      TRUE   TRUE
#> 4     TRUE   TRUE      TRUE   TRUE
#> 5    FALSE  FALSE      TRUE   TRUE
#> 6     TRUE   TRUE      TRUE   TRUE
```

### Les données agrégées

On va produire les données agrégées avec la fonction `aggregate_with_rules`. Les règles sont exprimées dans le fichier `config/load/cty_rules.yml`.

<!--html_preserve-->
<pre>FRANCE:
  CTY:
    - FR
  CTA:
    - FR
  BZN:
    - FR

BELGIUM:
  CTY:
    - BE
  CTA:
    - BE
  BZN:
    - BE

SWITZERLAND:
  CTY:
    - CH
  CTA:
    - CH
  BZN:
    - CH

SPAIN:
  CTY:
    - ES
  CTA:
    - ES
  BZN:
    - ES

NETHERLANDS:
  CTY:
    - NL
  CTA:
    - NL
  BZN:
    - NL

PORTUGAL:
  CTY:
    - PT
  CTA:
    - PT
  BZN:
    - PT

ITALY:
  CTY:
    - IT
  CTA:
    - IT
  BZN:
    - IT_CNOR
    - IT_CSUD
    - IT_NORD
    - IT_SARD
    - IT_SICI
    - IT_SUD

GERMANY:
  CTY:
    - DE
  CTA:
    - DE_TenneT_GER
    - DE_TransnetBW
    - DE_Amprion
    - DE_50HzT
  BZN:
    - DE_AT_LU
    - "!CTY|AUSTRIA"
    - "!CTY|LUXEMBOURG"

AUSTRIA:
  CTY:
    - AT
  CTA:
    - AT
  BZN:
    - DE_AT_LU
    - "!CTY|GERMANY"
    - "!CTY|LUXEMBOURG"

UK:
  CTY:
    - GB
    - "CTA|NORTH_IRELAND"
  CTA:
    - GB
  BZN:
    - GB

IRELAND:
  CTY:
    - IE
  CTA:
    - IE
  BZN:
    - IE_SEM
    - "!CTA|NORTH_IRELAND"

NORTH_IRELAND:
  CTY:
    - NIE
  CTA:
    - NIE
  BZN:
    - IE_SEM
    - "!CTY|IRELAND"

LUXEMBOURG:
  CTY:
    - LU
  CTA:
    - LU
  BZN:
    - DE_AT_LU
    - "!CTY|GERMANY"
    - "!CTY|AUSTRIA"
</pre>
<!--/html_preserve-->
La fonction prend des données de *load* comme argument, c'est à dire obtenue avec la fonction `anta_load_read()`.

``` r
aggregated_db <- aggregate_with_rules(load_data)
```

### Validation des données agrégées

Comme pour les données brutes, l'opération va ajouter autant de colonnes qu'il y a de tests exprimés dans le fichier `agg_validate.yml`.

<!--html_preserve-->
<pre>rules:

- expr: is.finite(CTY)
  name: CTY_NA
- expr: is.finite(CTA)
  name: CTA_NA
- expr: is.finite(BZN)
  name: BZN_NA

- expr: CTY &gt; 0
  name: CTY_IS_POS
- expr: CTA &gt; 0
  name: CTA_IS_POS
- expr: BZN &gt; 0
  name: BZN_IS_POS

- expr: abs(CTY-CTA) &lt; 1
  name: CTY_CTA_EQUAL
- expr: abs(CTY-BZN) &lt; 1
  name: CTY_BZN_EQUAL
- expr: abs(CTA-BZN) &lt; 1
  name: CTA_BZN_EQUAL


- expr: abs(1 - (CTY / CTA) ) &lt; .05
  name: CTY_CTA_DIFF_LT_05
- expr: abs(1 - (CTY / BZN) ) &lt; .05
  name: CTY_BZN_DIFF_LT_05
- expr: abs(1 - (CTA / BZN) ) &lt; .05
  name: CTA_BZN_DIFF_LT_05

- expr: abs(1 - (CTY / CTA) ) &lt; .1
  name: CTY_CTA_DIFF_LT_10
- expr: abs(1 - (CTY / BZN) ) &lt; .1
  name: CTY_BZN_DIFF_LT_10
- expr: abs(1 - (CTA / BZN) ) &lt; .1
  name: CTA_BZN_DIFF_LT_10


- expr: ((CTY - lag(CTY)) / CTY) &lt; .5 &amp; ((lag(CTY) - CTY) / lag(CTY)) &lt; .5
  name: CTY_LAG_LT_50
- expr: ((CTA - lag(CTA)) / CTA) &lt; .5 &amp; ((lag(CTA) - CTA) / lag(CTA)) &lt; .5
  name: CTA_LAG_LT_50
- expr: ((BZN - lag(BZN)) / BZN)  &lt; .5 &amp; ((lag(BZN) - BZN) / lag(BZN)) &lt; .5
  name: BZN_LAG_LT_50
</pre>
<!--/html_preserve-->
``` r
aggregated_db <- augment_validation(aggregated_db)
```

### Correction mécanique des données agrégées

``` r
aggregated_db <- data_correct_with_rules(aggregated_db)
```

### Qualification résumée des lignes agrégées

``` r
aggregated_db <- augment_process_summary(aggregated_db)

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
aggregated_db %>% 
  group_by_at(c( "country", "summary") ) %>% 
  tally() %>% 
  spread(summary, n) %>% 
  kable()
```

| country        |  corrected|  invalid|  original|
|:---------------|----------:|--------:|---------:|
| AUSTRIA        |      14010|       NA|     11990|
| BELGIUM        |          9|      268|     25723|
| FRANCE         |          1|      389|     25610|
| GERMANY        |      14009|       NA|     11991|
| IRELAND        |        141|     2528|     23331|
| ITALY          |         24|      759|     25217|
| LUXEMBOURG     |      14224|       NA|     11776|
| NETHERLANDS    |         28|       NA|     25972|
| NORTH\_IRELAND |      23472|     2528|        NA|
| PORTUGAL       |       1047|      169|     24784|
| SPAIN          |        772|      480|     24748|
| SWITZERLAND    |        288|      577|     25135|
| UK             |      21308|      664|      4028|
