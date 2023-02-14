COMapData <- ColoradoCountyShapes %>%
  left_join(COPredictions,by="FIPS") %>%
  mutate(quantile_512 = 
           cut(abs(Resid_512),
               quantile(
                 abs(Resid_512),
                 na.rm=TRUE
                 )
               ),
         quantile_1218 = 
           cut(abs(Resid_1218),
               quantile(
                 abs(Resid_1218),
                 na.rm=TRUE
               )
           )
  )
         

library(RColorBrewer)
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

library(classInt)
breaks_qt <- classIntervals(c(min(COMapData$Resid_512) - .00001, COMapData$Resid_512), n = 7, style = "quantile")


#https://ggplot2.tidyverse.org/reference/ggsf.htm
ggplot(data = COMapData) +
  geom_sf(aes(fill = quantile_512))+
  scale_fill_brewer(palette = "OrRd",
                    guide = guide_legend(reverse=TRUE),
                    labels = c("0-25%",
                               "26-50%",
                               "51-75%",
                               "76-100%"
                               ),
                    na.translate = F) +
  theme_minimal()+
  theme(legend.title = element_blank(),
        
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title =  "Vaccine Rate, 5-11",
       subtitle = "Quantile of Residuals")

ggplot(data = COMapData) +
  geom_sf(aes(fill = quantile_1218))+
  scale_fill_brewer(palette = "OrRd",
                    guide = guide_legend(reverse=TRUE),
                    labels = c("0-25%",
                               "26-50%",
                               "51-75%",
                               "76-100%"
                    ),
                    na.translate = F) +
  theme_minimal()+
  theme(legend.title = element_blank(),
        
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title =  "Vaccine Rate, 12-17",
       subtitle = "Quantile of Residuals")
