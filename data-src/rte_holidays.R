library(rvest)
library(xml2)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)
library(readxl)
library(timetk)

unlink("data-src/holdidays_zip", recursive = TRUE, force = TRUE)

url <- 'http://clients.rte-france.com/lang/an/visiteurs/vie/vie_jours_feries.jsp'
all_ref <- read_html(url) %>%
  xml_find_all("//a[text()='File.zip']") %>%
  xml_attr("href")

dir.create("data-src/holdidays_zip", showWarnings = FALSE)
full_links <- paste0("http://clients.rte-france.com", all_ref)
dest_files <- file.path("data-src/holdidays_zip", basename(full_links))
download.file(full_links, dest_files)

dest_files <- file.path("data-src/holdidays_zip", basename(full_links))

map( dest_files, function(x) {
  dest <- dirname(x)
  utils::unzip(x, exdir = dest)
  } )


list.files("data-src/holdidays_zip", full.names = TRUE, pattern = "\\.xls$") %>%
  map_df(function(path){
    data <- read_excel(path, skip = 2)
    names(data)[1:3] <- c("Day", "Date", "Name")
    data <- data %>%
      drop_na(Name) %>%
      select(-Day, -Name)
    if( is.character(data$Date)){
      data <- data %>% mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30"))
    } else {
      data <- data %>% mutate(Date = as.Date(Date) )
    }

    data <- data %>%
      gather(country, flag, -Date) %>%
      filter(flag %in% "X") %>%
      select(Date, country)
  } ) -> list_data
# Allemagne	Autriche	Belgique	Danemark	Espagne	France	Italie	Luxembourg	Norvège	Pays Bas	Portugal	Royaume Uni	Suède	Suisse	République Tchèque
list_data <- list_data %>%
  mutate( country = case_when(
    country %in% "France" ~ "FRANCE",
    country %in% c("Germany", "Allemagne") ~ "GERMANY",
    country %in% c("Austria", "Autriche") ~ "AUSTRIA",
    country %in% c("Belgique", "Belgium") ~ "BELGIUM",
    country %in% c("Czech", "Czech Republic", "Rep. Czech", "République Tchèque") ~ "CZECH REPUBLIC",
    country %in% c("Danemark", "Denmark") ~ "DENMARK",
    country %in% c("Portugal") ~ "PORTUGAL",
    country %in% c("Luxembourg") ~ "LUXEMBOURG",
    country %in% c("Spain", "Espagne") ~ "SPAIN",
    country %in% c("Sweden", "Suède") ~ "SWEDEN",
    country %in% c("Norvège", "Norway") ~ "NORWAY",
    country %in% c("Italy", "Italie") ~ "ITALY",
    country %in% c("Royaume Uni", "The United Kingdom", "United Kingdom") ~ "UK",
    country %in% c("Netherlands", "The Netherlands", "Pays Bas") ~ "NETHERLANDS",
    country %in% c("Switzrland", "Switzerland" ,"Suisse") ~ "SWITZERLAND",
    TRUE ~ country
  )) %>%
  add_column(is_off = TRUE, likely_off = FALSE)

ponts_eventuels <- list_data %>%
  timetk::tk_augment_timeseries_signature() %>%
  select(Date, country, wday, wday.lbl) %>%
  mutate(Date = case_when(
    wday > 2 & wday < 7 ~ Date - 1,
    TRUE ~ as.Date(NA_real_, origin = "1970-01-01")
  )) %>%
  select(Date, country) %>%
  drop_na() %>%
  add_column(is_off = FALSE, likely_off = TRUE)

holidays <- bind_rows(list_data, ponts_eventuels) %>%
  group_by(Date, country) %>%
  summarise(is_off = any(is_off), likely_off = !any(is_off) && any(likely_off) ) %>%
  ungroup()
devtools::use_data(holidays, overwrite = TRUE)

unlink("data-src/holdidays_zip", recursive = TRUE, force = TRUE)
remove(list = setdiff(ls(), "holidays") )
gc()

