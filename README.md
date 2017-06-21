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
-   comment estimer la données identifiée mauvaise

Installation
------------

This package is only available on Github.

To install the last development version:

``` r
devtools::install_github("rte-antares-rpackage/antaDraft")
```

    ## Installation failed: Failure when receiving data from the peer

Usage
-----

> Pour l'instant, seules les données de consommation sont traitées.

### Importation des données brutes

``` r
library(antadraft)

system.time(
  load_db <- import_load_db("D:/transparency_repo/staging_area/incoming/A-CONSOMMATION/A01-Consommation_réalisée")
)
```

    ##    user  system elapsed 
    ##   48.28    0.97   49.40

``` r
head(load_db)
```

    ## # A tibble: 6 x 9
    ##    year month   day            DateTime AreaTypeCode   AreaName MapCode
    ##   <int> <int> <int>              <dttm>        <chr>      <chr>   <chr>
    ## 1  2014    12    23 2014-12-23 20:00:00          CTY    Austria      AT
    ## 2  2014    12    23 2014-12-23 12:00:00          CTA Fingrid CA      FI
    ## 3  2014    12    21 2014-12-21 22:00:00          BZN     NO1 BZ     NO1
    ## 4  2014    12    21 2014-12-21 06:00:00          BZN     NO1 BZ     NO1
    ## 5  2014    12    29 2014-12-29 15:00:00          BZN     NO3 BZ     NO3
    ## 6  2014    12    29 2014-12-29 12:00:00          BZN     NO3 BZ     NO3
    ## # ... with 2 more variables: TotalLoadValue <dbl>, SubmissionTS <chr>

### Préparation au format *antares*

``` r
db <- build_db(raw_db = load_db )
head(db)
```

    ## # A tibble: 6 x 5
    ##   country            DateTime   CTY   CTA   BZN
    ##     <chr>              <dttm> <dbl> <dbl> <dbl>
    ## 1  FRANCE 2014-12-15 09:00:00 72729 72729 72729
    ## 2  FRANCE 2014-12-15 10:00:00 73107 73107 73107
    ## 3  FRANCE 2014-12-15 11:00:00 73077 73077 73077
    ## 4  FRANCE 2014-12-15 12:00:00 71859 71859 71859
    ## 5  FRANCE 2014-12-15 13:00:00 70910 70910 70910
    ## 6  FRANCE 2014-12-15 14:00:00 68965 68965 68965

### Recherche des anomalies

``` r
knitr::kable( extract_nonvalid_data(db) )
```

| country     | validator              | start               | end                 |
|:------------|:-----------------------|:--------------------|:--------------------|
| AUSTRIA     | BZN is a finite real   | 2014-12-01 23:00:00 | 2017-06-11 07:00:00 |
| AUSTRIA     | CTY and CTA are equals | 2014-12-07 22:00:00 | 2014-12-07 22:00:00 |
| BELGIUM     | CTA and BZN are equals | 2016-02-16 04:00:00 | 2016-03-27 20:00:00 |
| BELGIUM     | CTA is a finite real   | 2015-03-07 23:00:00 | 2016-11-09 07:00:00 |
| BELGIUM     | CTY and CTA are equals | 2016-02-16 04:00:00 | 2016-03-27 20:00:00 |
| FRANCE      | CTA is a finite real   | 2016-09-19 10:00:00 | 2016-09-19 10:00:00 |
| GERMANY     | CTA and BZN are equals | 2014-12-08 00:00:00 | 2017-06-11 07:00:00 |
| GERMANY     | CTY and BZN are equals | 2014-12-08 00:00:00 | 2017-06-11 07:00:00 |
| GERMANY     | CTY and CTA are equals | 2014-12-08 00:00:00 | 2017-06-11 07:00:00 |
| ITALY       | BZN is a finite real   | 2017-01-01 23:00:00 | 2017-05-30 21:00:00 |
| NETHERLANDS | CTA and BZN are equals | 2014-12-09 16:00:00 | 2016-03-31 21:00:00 |
| NETHERLANDS | CTA is a finite real   | 2014-12-30 23:00:00 | 2014-12-30 23:00:00 |
| NETHERLANDS | CTY and CTA are equals | 2014-12-09 16:00:00 | 2016-03-31 21:00:00 |
| PORTUGAL    | CTY and BZN are equals | 2014-12-08 00:00:00 | 2015-01-20 14:00:00 |
| PORTUGAL    | CTY and CTA are equals | 2014-12-08 00:00:00 | 2015-01-20 14:00:00 |
| SPAIN       | CTA and BZN are equals | 2016-04-24 23:00:00 | 2016-10-05 13:00:00 |
| SPAIN       | CTY and BZN are equals | 2014-12-19 09:00:00 | 2015-01-20 14:00:00 |
| SPAIN       | CTY and CTA are equals | 2014-12-19 09:00:00 | 2016-10-05 13:00:00 |
| SWITZERLAND | BZN is a finite real   | 2016-04-26 22:00:00 | 2016-09-19 21:00:00 |
| SWITZERLAND | CTA is a finite real   | 2016-04-26 22:00:00 | 2016-11-02 22:00:00 |
| UK          | CTA and BZN are equals | 2015-01-01 00:00:00 | 2017-06-11 07:00:00 |
| UK          | CTA is a finite real   | 2015-03-29 01:00:00 | 2017-06-01 03:00:00 |
| UK          | CTY and BZN are equals | 2015-01-01 00:00:00 | 2017-06-11 07:00:00 |
| UK          | CTY and CTA are equals | 2015-01-01 00:00:00 | 2017-06-11 07:00:00 |
