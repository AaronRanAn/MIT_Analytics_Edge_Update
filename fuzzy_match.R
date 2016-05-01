library(reshape2)
library(ggplot2)
library(dplyr)

me <- melt(economics, id=c("date"))

me.2 <- me %>%
        group_by(variable) %>%
        mutate(color = (min(value) == value | max(value) == value))

ggplot(data=me.2, aes(x = date, y = value)) +
        geom_line() +
        geom_point(aes(color = color)) +
        facet_wrap(~variable, ncol=1, scales="free_y") +
        scale_color_manual(values = c(NA, "red"))

library(RecordLinkage, verbose = F)
soundex('P&G') == soundex('PG')

levenshtein.distance('Clinton', 'Clenton')
