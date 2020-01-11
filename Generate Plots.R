library(knitr)
library(rmarkdown)
library(scales)
library(xts)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(timeDate)
library(rlist)
library(ggforce)
library(ggrepel)
library(fable)
library(tsibble)
library(feasts)
library(urca)


load("DiverForecasts.RData")


listforplotting <- split.data.frame(combined, combined$BookSpecialty)

ggplotref <- function(DF) {
  plottitle <- head(DF$BookSpecialty,1)
  reflabelmonth <- month(DF$MonthEnding[length(DF$MonthEnding)-6], label = TRUE)
  reflabelyear <- year(DF$MonthEnding[length(DF$MonthEnding)-6])
  reflabelval <- DF$Forecast[length(DF$Forecast)-6]
  actlabelval <- DF$OvCapAvg[length(DF$OvCapAvg)-6]
  ggplot(data = DF, aes(x = MonthEnding))+
    geom_line(aes(y = Referrals, col = 'Referrals'), size = 1.1)+
    geom_ribbon(aes(ymin = `80% Lower`, ymax = `80% Upper`), col = "grey50", fill = "grey50", alpha = 0.3)+
    geom_line(aes(y = Forecast, col = 'Forecast'), size = 1.1)+
    geom_line(aes(y = OvCapAvg, col = 'OvCapAvg'), size = 1.1)+
    theme_minimal()+
    expand_limits(y=0)+
    labs(title = plottitle, x = "Month Ending",
         subtitle = paste0(reflabelmonth, " ",
                           reflabelyear,
                           "    -    ",
                           " Forecasted Referrals - ",
                           round(reflabelval,0),
                           "    -    ",
                           "Expected capacity - ",
                           round(actlabelval,0)),
         color = "Measure")+
    geom_point(data = DF[nrow(DF)-6,],
               aes(x = MonthEnding,
                   y = Forecast),
               fill = 'red',
               shape = 23,
               size = 5)+
    geom_point(data = DF[nrow(DF)-6,],
               aes(x = MonthEnding,
                   y = OvCapAvg),
               fill = 'blue',
               shape = 23,
               size = 5)+
    scale_color_manual(values = c("Referrals" = 'red', 
                                  "Forecast" = 'red',
                                  "OvCapAvg" = 'blue'))+
    theme_bw()+
    theme_update(legend.position = "top",
                 legend.text=element_text(size = 16),
                 text = element_text(family = "serif"),
                 plot.title = element_text(hjust = 0.5, face = "bold", size = 32),
                 plot.subtitle = element_text(hjust = 0.5,face = "bold", size = 25),
                 axis.title.x = element_text(face = "bold", size = 16),
                 axis.title.y = element_text(face = "bold", size = 16))
}

ggplots <- lapply(listforplotting, ggplotref)

monthend <- listforplotting$Anaesthetics %>%
  filter(is.na(Forecast)) %>%
  select(MonthEnding) %>%
  tail(1)

monthtext <- as.character(month(monthend$MonthEnding[1], label = TRUE, abbr = FALSE))
yeartext <- as.numeric(year(monthend$MonthEnding[1]))

save(listforplotting, ggplots, monthtext, yeartext, file = "ggplots.RData")


bulk_save <- function(x) {
  ggsave(filename = paste0("plots\\",x$data$BookSpecialty[1],".jpeg"),
         device = "jpeg", width = 16, height = 9, units = "in",
         dpi = 300, plot = x)
}

#outputting plots ----
lapply(ggplots, bulk_save)
