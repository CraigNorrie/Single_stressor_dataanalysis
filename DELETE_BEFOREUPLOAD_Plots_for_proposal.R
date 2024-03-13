##Temp Oxygen consumption

# pH Oxygen consumption ---------------------------------------------------
pHresprates_summary$pCO2 <- factor(pHresprates_summary$pCO2, levels = c("CTRL", "250", "500", "750", "1000"))
pHcurve_nohs_proposal <- pHresprates_summary %>%
  #filter(Colour == "HS") %>%
  ggplot(aes(x = pCO2, y = meanrate, group = Colour, colour = Colour)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = meanrate - semrate, ymax = meanrate + semrate), width = 0.2) +
  theme_bw(base_size = 20) +
  xlab("pCO2") +
  ylab("Mean oxygen consumption (µmol h-1 g-1)") +
  scale_color_manual(values = c("#CD950C", "#00688B", "#8B0A50"),   # Custom colors for each group
                     breaks = c("Blue", "Green", "Red"),   # Names of your groups
                     labels = c("Diploid", "Mated Triploids", "Induced Triploids"))+   # Custom labels for each group
  labs(color = "Ploidy")+  # Change the legend title to "Ploidy"
  facet_wrap(~HS, nrow = 2) +
  theme(strip.text = element_blank())
pHcurve_nohs_proposal #THIS WAS EXPORTED AT A SIZE OF 10x12 Inches

pHcurve_proposal <- pHresprates_summary %>% filter(Colour == "Blue") %>% 
  ggplot(aes(x = pCO2, y = meanrate, group = HS, colour = HS))+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin = meanrate - semrate, ymax = meanrate + semrate), width = 0.2) +
  theme_bw(base_size = 20)+
  xlab("pCO2") +
  ylab("Mean oxygen consumption (µmol h-1 g-1)")+
  scale_color_manual(values = c("#CD950C", "#00688B"), 
                     breaks = c("HS", "NOHS"),   # Names of your groups
                     labels = c("Heat shocked", "Non-heat shocked"))+   # Custom labels for each group
  labs(color = "Heat treatment")  # Change the legend title to "Ploidy"
  
pHcurve_proposal
##DO Oxygen consumption
PhyscurvDO_proposal <- DO_resp_summarytable %>% filter(O2 != "9.57") %>%
  filter(Ploidy == "2M") %>% 
  ggplot(aes(x=O2, y= mean_rate, group = HS, colour = HS))+geom_line(size = 1)+#base graph
  geom_errorbar(aes(ymin=mean_rate-SEMrate, ymax=mean_rate+SEMrate), width = 0.2, size = 0.5)+ #error bars
  ylim(0,5.3)+
  scale_color_manual(values = c("#CD950C", "#00688B"),   # Custom colors for each group
                     breaks = c("HS", "NOHS"),   # Names of your groups
                     labels = c("Heat shocked", "Non-heat shocked"))+   # Custom labels for each group
  labs(color = "Heat treatment")+  # Change the legend title to "Ploidy"
  theme_bw(base_size = 20)+ylab("Mean oxygen consumption rate (µmol g-1 h-1 ± SEM)")+
  xlab("Oxygen concentration (mg l-1)")
PhyscurvDO_proposal
