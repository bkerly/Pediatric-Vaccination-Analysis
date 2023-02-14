
ggplot(Predictions,aes(x=Pct_512,y=Pred_512)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  xlab("Observed Vaccination Rate")+
  ylab("Predicted Vaccination Rate") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 5-11")

ggplot(Predictions,aes(x=Pct_1218,y=Pred_1218)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  xlab("Observed Vaccination Rate")+
  ylab("Predicted Vaccination Rate") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 12-17")



ggplot(data = COPredictions,aes(x=Pct_512,y=Pred_512)) +
  geom_abline(slope = 1,size = 1, linetype = "dotdash") +
  geom_point(size = 2) +
  #theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  xlab("Observed Vaccination Rate")+
  ylab("Predicted Vaccination Rate") +
  labs(title = "COLORADO: Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 5-11") +
  theme(legend.position = "none",
        legend.title = element_blank())


ggplot(COPredictions %>% filter(Colorado),aes(x=Pct_1218,y=Pred_1218)) +
  geom_abline(slope = 1,size = 1, linetype = "dotdash") +
  geom_point(size = 2) +
  #theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  xlab("Observed Vaccination Rate")+
  ylab("Predicted Vaccination Rate") +
  labs(title = "COLORADO: Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 12-17")+
  theme(legend.position = "none",
        legend.title = element_blank())
