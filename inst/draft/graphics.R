require(RColorBrewer)
require(lubridate)



db %>%
  mutate(day_id = wday(DateTime), week_id = isoweek(DateTime),
         month_id = month(DateTime), year_id = isoyear(DateTime) ) %>%


query <- db_erros %>%
  filter(country %in% "FRANCE") %>%
  mutate(day_id = wday(DateTime), week_id = isoweek(DateTime),
         month_id = month(DateTime), year_id = year(DateTime) ) %>%
  group_by(year_id, week_id, day_id ) %>%
  tally() %>%
  complete(day_id=1:7, week_id = 1:53, year_id=2015:2017, fill = list(n=0) )


ggplot(query, aes(week_id, -day_id, fill = n)) +
  geom_tile(colour="wheat") +
  facet_wrap("year_id", ncol = 1) +
  scale_fill_gradient(low="transparent", high="red", limits = c(0, 24)) +
  coord_equal() +
  scale_y_continuous(breaks = -c(1:7), labels = c("Dimanche", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi") ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "yellow"), panel.grid.major.y = element_line(colour = "transparent"),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.minor.x = element_line(colour = "transparent")
        )


library(ggplot2)
library(ggcal)

mydate <- seq(as.Date("2017-02-01"), as.Date("2017-07-22"), by="1 day")
myfills <- rnorm(length(mydate))

print(ggcal(db_erros$, myfills))
