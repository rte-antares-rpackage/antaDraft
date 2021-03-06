% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anta_load_read.R, R/prod_compare.R,
%   R/read_prod_group.R, R/read_prod_type.R, R/validate.R
\name{augment_validation.load_raw}
\alias{augment_validation.load_raw}
\alias{augment_validation.aggregated}
\alias{augment_validation.type_group_prod}
\alias{augment_validation.prod_by_group}
\alias{augment_validation.prod_by_type}
\alias{augment_validation.aggprod_by_type}
\alias{augment_validation}
\title{evaluate validation rules for each line of a dataset}
\usage{
\method{augment_validation}{load_raw}(data)

\method{augment_validation}{aggregated}(data)

\method{augment_validation}{type_group_prod}(data)

\method{augment_validation}{prod_by_group}(data)

\method{augment_validation}{prod_by_type}(data)

\method{augment_validation}{aggprod_by_type}(data)

augment_validation(data)
}
\arguments{
\item{data}{dataset}
}
\description{
add logical columns to a dataset. Each column is a test to
perform against dataset.
}
\examples{
load_dir <- system.file(package = "antaDraft",
  "data_sample/load_sample_2017")

load_data <- anta_load(data_dir = load_dir )
load_data <- augment_validation(data = load_data)
head(load_data)

aggregated_db <- agg_data(load_data)
aggregated_db <- augment_validation(aggregated_db)
head(aggregated_db)

data_neg_val <- structure(list(country = c("LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
"LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
"LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
"LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
"LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
"LUXEMBOURG"), DateTime = structure(c(1502582400, 1502586000,
1502589600, 1502593200, 1502596800, 1502600400, 1502604000, 1502607600,
1502611200, 1502614800, 1502618400, 1502622000, 1502625600, 1502629200,
1502632800, 1502636400, 1502640000, 1502643600, 1502647200, 1502650800,
1502654400, 1502658000, 1502661600, 1502665200), class = c("POSIXct",
"POSIXt")), BZN = c(346.000000000003, 352.000000000003, 354.999999999998,
330.000000000002, 345.000000000003, 363.999999999998, 367, 374,
389.999999999997, 373, 351, 342, 339, 344, 367, 386, -36032.64,
398.999999999998, 414.000000000005, 412.000000000005, 381.999999999998,
351.999999999998, 336.000000000002, 332.999999999995), CTA = c(346,
352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
367, 386, 401, 399, 414, 412, 382, 352, 336, 333), CTY = c(346,
352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
367, 386, 401, 399, 414, 412, 382, 352, 336, 333)), .Names = c("country",
"DateTime", "BZN", "CTA", "CTY"), row.names = 179664:179687, class = c("data.frame",
"aggregated"))

aggregated_db <- augment_validation(data_neg_val)
aggregated_db

}
