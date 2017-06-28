readme
================

Goal of the project
-------------------

Construction d'une base de données publique contenant des données ANTARES provenant du SI transparency.

Les objectifs premiers du logiciel sont :

-   identifier les sources problématiques :
    -   y a t-il des données manquantes et où sont-elles?
    -   y a t-il des données nulles et où sont-elles?
-   quelles données sont mauvaises
-   Substituer de façon mécanique quand c'est possible des données manquantes par d'autres données supposées valides et échangeable.
-   comment estimer la données identifiée mauvaise

Installation
------------

This package is only available on Github. To install the last development version:

``` r
devtools::install_github("rte-antares-rpackage/antaDraft")
```

Usage
-----

> Pour l'instant, seules les données de consommation sont traitées.

### Importation des données brutes

``` r
library(antadraft)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
rep_path <- "D:/transparency_repo/staging_area/incoming/A-CONSOMMATION/A01-Consommation_réalisée"
load_db <- rep_path %>% read_load_files()
load_db <- load_db %>% dplyr::filter(year > 2014)

head(load_db)
```

    ## # A tibble: 6 x 9
    ##    year month   day            DateTime AreaTypeCode       AreaName
    ##   <int> <int> <int>              <dttm>        <chr>          <chr>
    ## 1  2015     1     7 2015-01-07 23:00:00          CTY Czech Republic
    ## 2  2015     1     8 2015-01-08 22:00:00          CTY Czech Republic
    ## 3  2015     1     8 2015-01-08 19:00:00          CTY Czech Republic
    ## 4  2015     1     8 2015-01-08 07:00:00          CTY Czech Republic
    ## 5  2015     1     8 2015-01-08 06:00:00          CTY Czech Republic
    ## 6  2015     1     8 2015-01-08 12:00:00          CTY Czech Republic
    ## # ... with 3 more variables: MapCode <chr>, TotalLoadValue <dbl>,
    ## #   SubmissionTS <chr>

### Préparation au format *antares*

``` r
db <- fortify_from_rules(raw_db = load_db )
head(db)
```

    ## # A tibble: 6 x 5
    ##   country            DateTime   CTY   CTA   BZN
    ##     <chr>              <dttm> <dbl> <dbl> <dbl>
    ## 1  FRANCE 2015-01-01 00:00:00 70929 70929 70929
    ## 2  FRANCE 2015-01-01 01:00:00 69773 69773 69773
    ## 3  FRANCE 2015-01-01 02:00:00 66417 66417 66417
    ## 4  FRANCE 2015-01-01 03:00:00 64182 64182 64182
    ## 5  FRANCE 2015-01-01 04:00:00 63859 63859 63859
    ## 6  FRANCE 2015-01-01 05:00:00 63921 63921 63921

### Recherche des anomalies

La fonction `qualcon` permet de tester les cohérences définies dans le fichier `validation_rules`. Il s'agit d'un fichier yaml exprimant les expressions R représentant ces tests.

La fonction retourne un data.frame décrivant chaque invalidité, par pays et période entière.

``` r
db_erros <- qualcon(db)
db_erros
```

    ## # A tibble: 80,625 x 14
    ##    `CTY must not be missing` `CTA must not be missing`
    ##                        <lgl>                     <lgl>
    ##  1                      TRUE                     FALSE
    ##  2                      TRUE                     FALSE
    ##  3                      TRUE                     FALSE
    ##  4                      TRUE                     FALSE
    ##  5                      TRUE                     FALSE
    ##  6                      TRUE                     FALSE
    ##  7                      TRUE                     FALSE
    ##  8                      TRUE                     FALSE
    ##  9                      TRUE                     FALSE
    ## 10                      TRUE                     FALSE
    ## # ... with 80,615 more rows, and 12 more variables: `BZN must not be
    ## #   missing` <lgl>, `CTY and CTA must be equals if not missing` <lgl>,
    ## #   `CTY and BZN must be equals if not missing` <lgl>, `CTA and BZN must
    ## #   be equals if not missing` <lgl>, `CTY and CTA difference is not
    ## #   greater than 5%` <lgl>, `CTY and BZN difference is not greater than
    ## #   5%` <lgl>, `CTA and BZN difference is not greater than 5%` <lgl>, `CTY
    ## #   and CTA difference is not greater than 10%` <lgl>, `CTY and BZN
    ## #   difference is not greater than 10%` <lgl>, `CTA and BZN difference is
    ## #   not greater than 10%` <lgl>, DateTime <dttm>, country <chr>

Une vue synthétique peut être obtenue avec la fonction `fortify_qualcon`.

``` r
erros_summary <- fortify_qualcon(db_erros)
erros_summary %>% arrange(country, start, end)
```

    ## # A tibble: 7,312 x 5
    ##    country                                 validator time_frame
    ##      <chr>                                     <chr>      <int>
    ##  1 AUSTRIA                   BZN must not be missing          1
    ##  2 BELGIUM                   CTA must not be missing          1
    ##  3 BELGIUM                   CTA must not be missing          2
    ##  4 BELGIUM                   CTA must not be missing          3
    ##  5 BELGIUM                   CTA must not be missing          4
    ##  6 BELGIUM                   CTA must not be missing          5
    ##  7 BELGIUM                   CTA must not be missing          6
    ##  8 BELGIUM CTA and BZN must be equals if not missing          1
    ##  9 BELGIUM CTY and CTA must be equals if not missing          1
    ## 10 BELGIUM                   CTA must not be missing          7
    ## # ... with 7,302 more rows, and 2 more variables: start <dttm>, end <dttm>

### Détails des anomalies

Les données brutes attachées à un problème identifié peuvent être obtenues avec la fonction `extract_raw_data`.

``` r
extract_raw_data(load_db, db_erros)
```

    ## # A tibble: 284,303 x 22
    ##    country  year month   day            DateTime AreaTypeCode AreaName
    ##      <chr> <int> <int> <int>              <dttm>        <chr>    <chr>
    ##  1  FRANCE  2016     9    19 2016-09-19 10:00:00          CTY   France
    ##  2 BELGIUM  2015     3     7 2015-03-07 23:00:00          CTY  Belgium
    ##  3 BELGIUM  2015     3    29 2015-03-29 02:00:00          CTY  Belgium
    ##  4 BELGIUM  2015     3     8 2015-03-08 00:00:00          CTY  Belgium
    ##  5 BELGIUM  2015     3     8 2015-03-08 06:00:00          CTY  Belgium
    ##  6 BELGIUM  2015     3     8 2015-03-08 01:00:00          CTY  Belgium
    ##  7 BELGIUM  2015     3     8 2015-03-08 07:00:00          CTY  Belgium
    ##  8 BELGIUM  2015     3     8 2015-03-08 08:00:00          CTY  Belgium
    ##  9 BELGIUM  2015     3     8 2015-03-08 03:00:00          CTY  Belgium
    ## 10 BELGIUM  2015     3     8 2015-03-08 02:00:00          CTY  Belgium
    ## # ... with 284,293 more rows, and 15 more variables: MapCode <chr>,
    ## #   TotalLoadValue <dbl>, SubmissionTS <chr>, `CTY must not be
    ## #   missing` <lgl>, `CTA must not be missing` <lgl>, `BZN must not be
    ## #   missing` <lgl>, `CTY and CTA must be equals if not missing` <lgl>,
    ## #   `CTY and BZN must be equals if not missing` <lgl>, `CTA and BZN must
    ## #   be equals if not missing` <lgl>, `CTY and CTA difference is not
    ## #   greater than 5%` <lgl>, `CTY and BZN difference is not greater than
    ## #   5%` <lgl>, `CTA and BZN difference is not greater than 5%` <lgl>, `CTY
    ## #   and CTA difference is not greater than 10%` <lgl>, `CTY and BZN
    ## #   difference is not greater than 10%` <lgl>, `CTA and BZN difference is
    ## #   not greater than 10%` <lgl>
