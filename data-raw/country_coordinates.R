country_coordinates <- structure(list(
  country = c("UK", "NORTH_IRELAND", "IRELAND", "NETHERLANDS", "BELGIUM", "LUXEMBOURG", "SWITZERLAND", "FRANCE", "SPAIN", "PORTUGAL", "GERMANY", "AUSTRIA", "ITALY"),
  lat = c(51.616, 54.658, 53.017, 52.061, 50.901, 49.627, 46.983, 48.817, 40.933, 40.717, 51.115, 47.517, 43.096),
  lon = c(-1.096, -6.216, -8, 5.873, 4.484, 6.212, 8.25, 2.333, -1.3, -7.883, 9.286, 14.95, 12.513)),
  class = "data.frame", row.names = c(NA, -13L),
  .Names = c("country", "lat", "lon"))

devtools::use_data(country_coordinates, overwrite = TRUE)

rm(country_coordinates)
gc()
