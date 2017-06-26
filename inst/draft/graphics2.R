library(ggplot2)

ggplot( sum_db_erros, aes( y = country,
                           yend = country,
                           x = start, xend = end,
                           colour = validator, group = validator
                           )) + geom_segment(position = position_jitterdodge()) +
  geom_point(data = filter(sum_db_erros, duration < 1 ), aes(y = country, x = start) )

ggplot( sum_db_erros, aes( y = validator,
                           yend = validator,
                           x = start, xend = end,
                           colour = validator )) +
  geom_segment( ) +
  geom_point(data = filter(sum_db_erros, duration < 1 ), aes(x = start, y = validator) ) +
  facet_wrap("country") + theme_light() + guides(colour = FALSE)
