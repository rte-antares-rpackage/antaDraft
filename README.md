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

> Scope is only on load data for now. Goal is to extend scope to production data.

### Raw data importation

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
rep_path <- system.file(package = "antadraft", "files", "load")
load_db <- read_load_files(rep_path)
head(load_db)
```

    ## # A tibble: 6 x 9
    ##    year month   day            DateTime AreaTypeCode           AreaName
    ##   <int> <int> <int>              <dttm>        <chr>              <chr>
    ## 1  2017     1    15 2017-01-15 20:00:00          BZN             NO5 BZ
    ## 2  2017     1    15 2017-01-15 04:00:00          BZN             NO5 BZ
    ## 3  2017     1    15 2017-01-15 22:00:00          BZN             NO5 BZ
    ## 4  2017     1     2 2017-01-02 20:00:00          BZN Ireland - (SEM) BZ
    ## 5  2017     1    18 2017-01-18 08:00:00          BZN Ireland - (SEM) BZ
    ## 6  2017     1    12 2017-01-12 12:00:00          CTA      Cyprus TSO CA
    ## # ... with 3 more variables: MapCode <chr>, TotalLoadValue <dbl>,
    ## #   SubmissionTS <chr>

### Préparation au format *antares*

``` r
db <- fortify_from_rules(raw_db = load_db )
head(db)
```

    ##       country   DateTime      CTY      CTA      BZN
    ## 1      FRANCE 2017-01-01 73330.00 73330.00 73330.00
    ## 2     BELGIUM 2017-01-01  9970.44  9970.44  9970.44
    ## 3 SWITZERLAND 2017-01-01  6536.40  6536.40  6536.40
    ## 4       SPAIN 2017-01-01 23393.00 23393.00 23393.00
    ## 5 NETHERLANDS 2017-01-01 10903.00 10903.00 10903.00
    ## 6    PORTUGAL 2017-01-01  5076.70  5076.70  5076.70

### Recherche des anomalies

La fonction `qualcon` permet de tester les cohérences définies dans le fichier `validation_rules`. Il s'agit d'un fichier yaml exprimant les expressions R représentant ces tests.

La fonction retourne un data.frame décrivant chaque invalidité, par pays et période entière.

``` r
db_erros <- qualcon(db)
db_erros
```

    ## # A tibble: 22,816 x 20
    ##               DateTime       country `BZN is positive`
    ##                 <dttm>         <chr>             <lgl>
    ##  1 2017-01-01 00:00:00       AUSTRIA              TRUE
    ##  2 2017-01-01 00:00:00       GERMANY              TRUE
    ##  3 2017-01-01 00:00:00       IRELAND              TRUE
    ##  4 2017-01-01 00:00:00    LUXEMBOURG              TRUE
    ##  5 2017-01-01 00:00:00 NORTH_IRELAND              TRUE
    ##  6 2017-01-01 00:00:00            UK              TRUE
    ##  7 2017-01-01 01:00:00       AUSTRIA              TRUE
    ##  8 2017-01-01 01:00:00       GERMANY              TRUE
    ##  9 2017-01-01 01:00:00       IRELAND              TRUE
    ## 10 2017-01-01 01:00:00    LUXEMBOURG              TRUE
    ## # ... with 22,806 more rows, and 17 more variables: `BZN measure is less
    ## #   than half its previous value` <lgl>, `BZN must not be missing` <lgl>,
    ## #   `CTA and BZN difference is not greater than 10%` <lgl>, `CTA and BZN
    ## #   difference is not greater than 5%` <lgl>, `CTA and BZN must be equals
    ## #   if not missing` <lgl>, `CTA is positive` <lgl>, `CTA measure is less
    ## #   than half its previous value` <lgl>, `CTA must not be missing` <lgl>,
    ## #   `CTY and BZN difference is not greater than 10%` <lgl>, `CTY and BZN
    ## #   difference is not greater than 5%` <lgl>, `CTY and BZN must be equals
    ## #   if not missing` <lgl>, `CTY and CTA difference is not greater than
    ## #   10%` <lgl>, `CTY and CTA difference is not greater than 5%` <lgl>,
    ## #   `CTY and CTA must be equals if not missing` <lgl>, `CTY is
    ## #   positive` <lgl>, `CTY measure is less than half its previous
    ## #   value` <lgl>, `CTY must not be missing` <lgl>

Une vue synthétique peut être obtenue avec la fonction `fortify_qualcon`.

``` r
erros_summary <- fortify_qualcon(db_erros)
erros_summary %>% arrange(country, start, end)
```

    ## # A tibble: 986 x 4
    ##    country                                      validator
    ##      <chr>                                          <chr>
    ##  1 AUSTRIA CTA and BZN difference is not greater than 10%
    ##  2 AUSTRIA  CTA and BZN difference is not greater than 5%
    ##  3 AUSTRIA      CTA and BZN must be equals if not missing
    ##  4 AUSTRIA CTY and BZN difference is not greater than 10%
    ##  5 AUSTRIA  CTY and BZN difference is not greater than 5%
    ##  6 AUSTRIA      CTY and BZN must be equals if not missing
    ##  7 AUSTRIA                        CTA must not be missing
    ##  8 AUSTRIA                        CTY must not be missing
    ##  9 AUSTRIA                        BZN must not be missing
    ## 10 BELGIUM                        BZN must not be missing
    ## # ... with 976 more rows, and 2 more variables: start <dttm>, end <dttm>

### Détails des anomalies

Les données brutes attachées à un problème identifié peuvent être obtenues avec la fonction `extract_raw_data`.

``` r
extract_raw_data(load_db, db_erros)
```

    ## # A tibble: 74,924 x 28
    ##        country  year month   day            DateTime AreaTypeCode
    ##          <chr> <int> <int> <int>              <dttm>        <chr>
    ##  1 SWITZERLAND  2017     1    16 2017-01-16 15:00:00          CTY
    ##  2       ITALY  2017     1     2 2017-01-02 19:00:00          CTY
    ##  3       ITALY  2017     1     2 2017-01-02 03:00:00          CTY
    ##  4       ITALY  2017     1     2 2017-01-02 22:00:00          CTY
    ##  5       ITALY  2017     1     2 2017-01-02 14:00:00          CTY
    ##  6       ITALY  2017     1     2 2017-01-02 04:00:00          CTY
    ##  7       ITALY  2017     1     2 2017-01-02 01:00:00          CTY
    ##  8       ITALY  2017     1     2 2017-01-02 06:00:00          CTY
    ##  9       ITALY  2017     1     2 2017-01-02 16:00:00          CTY
    ## 10       ITALY  2017     1     2 2017-01-02 13:00:00          CTY
    ## # ... with 74,914 more rows, and 22 more variables: AreaName <chr>,
    ## #   MapCode <chr>, TotalLoadValue <dbl>, SubmissionTS <chr>, `BZN is
    ## #   positive` <lgl>, `BZN measure is less than half its previous
    ## #   value` <lgl>, `BZN must not be missing` <lgl>, `CTA and BZN difference
    ## #   is not greater than 10%` <lgl>, `CTA and BZN difference is not greater
    ## #   than 5%` <lgl>, `CTA and BZN must be equals if not missing` <lgl>,
    ## #   `CTA is positive` <lgl>, `CTA measure is less than half its previous
    ## #   value` <lgl>, `CTA must not be missing` <lgl>, `CTY and BZN difference
    ## #   is not greater than 10%` <lgl>, `CTY and BZN difference is not greater
    ## #   than 5%` <lgl>, `CTY and BZN must be equals if not missing` <lgl>,
    ## #   `CTY and CTA difference is not greater than 10%` <lgl>, `CTY and CTA
    ## #   difference is not greater than 5%` <lgl>, `CTY and CTA must be equals
    ## #   if not missing` <lgl>, `CTY is positive` <lgl>, `CTY measure is less
    ## #   than half its previous value` <lgl>, `CTY must not be missing` <lgl>
