##Figures and tables for publication
library(gridExtra)
library(openxlsx)
# Size --------------------------------------------------------------------
#temp
tempsize.plot <- sizedat_temp %>% 
  ggplot(aes(x = Ploidy, y = Mass, fill = Time)) +
  geom_violin(show.legend = FALSE) +
  xlab("") + ylab("") +
  theme_bw(base_size = 15) +
  theme(strip.background = element_blank(),  # Remove background fill from facet labels
        strip.text = element_text(color = "black")) +  # Set facet label color to black
  scale_fill_manual(values = c("red", "blue"), 
                    breaks = unique(sizedat_temp$Time), 
                    labels = c("Start", "End")) +
  scale_x_discrete(labels = c("", " ", "")) +
  facet_wrap(~ Treatment, 
             labeller = labeller(Treatment = c("7.5" = "7.5 °C", 
                                               "12.5" = "12.5 °C", 
                                               "17.5" = "17.5 °C", 
                                               "22.5" = "22.5 °C", 
                                               "27.5" = "27.5 °C")), 
             nrow = 1)

#DO
DOsize.plot <- sizedat_DO %>% 
  ggplot(aes(x = Ploidy, y = Mass, fill = Time)) +
  geom_violin(show.legend = FALSE) +
  theme_bw(base_size = 15) +
  scale_fill_manual(values = c("red", "blue"), 
                    breaks = unique(sizedat_temp$Time), 
                    labels = c("Start", "End")) +
  scale_x_discrete(labels = c("", "", "")) +
  ylab("Mass (g)") + xlab("") +
  facet_wrap(~ Treatment, 
             labeller = labeller(Treatment = c("20" = "1.91 mgL-1", 
                                               "40" = "3.83 mgL-1", 
                                               "60" = "5.74 mgL-1", 
                                               "80" = "7.66 mgL-1", 
                                               "100" = "delete me")), 
             nrow = 1) +
  theme(strip.background = element_blank(),  # Remove background fill from facet labels
        strip.text = element_text(color = "black"))  # Set facet label color to black
#Pco2
pHsize.plot <- pHsizedat %>% 
  ggplot(aes(x = Ploidy, y = Mass, fill = Time.point)) +
  geom_violin(show.legend = FALSE) +
  ylab("") +
  theme_bw(base_size = 15) +
  scale_fill_manual(values = c("red", "blue"), 
                    breaks = unique(sizedat_temp$Time), 
                    labels = c("Start", "End")) +
  scale_x_discrete(labels = c("Diploids", "Induced Triploids", "Mated Triploids")) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  facet_wrap(~ Treatment, nrow = 1) +
  theme(strip.background = element_blank(),  # Remove background fill from facet labels
        strip.text = element_text(color = "black"))  # Set facet label color to black

grid.arrange(tempsize.plot, DOsize.plot, pHsize.plot, ncol = 1)


**+*******pHsize.plot_legend <- pHsizedat %>% ggplot(aes(x=Ploidy, y = Mass, fill = Time.point))+geom_boxplot(show.legend = TRUE)+
  ylab("")+xlab("")+
  theme_bw(base_size = 15)+
  scale_fill_manual(values = c("red", "blue"), breaks = unique(sizedat_temp$Time), labels = c("Start", "End")) +
  scale_x_discrete(labels = c("Diploids", "Induced Triploids", "Mated Triploids")) +
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  facet_wrap(~Treatment, nrow = 5)

# respiration -------------------------------------------------------------
####Check labels are correct on all these graphs

#temp
tempresp.plot <- temprates_summary %>%
  mutate(HS = factor(HS, levels = c("NOHS", "HS"))) %>%
  ggplot(aes(x = Treatment, y = temp_mean_resprate, group = Ploidy, colour = Ploidy)) +
  geom_line(show.legend = FALSE) +
  xlab("Temperature (°C)") + ylab("") +
  geom_errorbar(aes(ymin = temp_mean_resprate - SEM, ymax = temp_mean_resprate + SEM, width = 0.2)) +
  theme_bw(base_size = 15) +
  scale_color_manual(values = c("#CD950C", "#00688B", "#8B0A50"),
                     breaks = c("2m", "3m", "3c")) +
  facet_wrap(~HS, nrow = 1, labeller = labeller(HS = c("NOHS" = "Non heat shocked", "HS" = "Heat shocked"))) +
  guides(color = FALSE) +
  theme(strip.text = element_blank()) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))

#DO
DOresp.plot <- DOrates_summary %>%
  filter(Treatment != "100") %>%
  mutate(HS = factor(HS, levels = c("NOHS", "HS"))) %>%
  ggplot(aes(x = Treatment, y = DO_mean_resprate, group = Ploidy, color = Ploidy)) +
  geom_line() +
  xlab("Dissolved oxygen concentration (mgL-1)") + 
  ylab("Mean oxygen consumption (μmol h−1 g−1 ± SEM)") +
  geom_errorbar(aes(ymin = DO_mean_resprate - SEM, ymax = DO_mean_resprate + SEM), width = 0.2) +
  theme_bw(base_size = 15) +
  scale_color_manual(values = c("#CD950C", "#00688B", "#8B0A50")) +
  facet_wrap(~HS, nrow = 1, labeller = labeller(HS = c("NOHS" = "Non heat shocked", "HS" = "Heat shocked"))) +
  guides(color = FALSE) +
  theme(strip.text = element_blank()) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x))

#pH
pHresp.plot <- pHrates_summary %>%
  mutate(HS = factor(HS, levels = c("NOHS", "HS"))) %>%
  ggplot(aes(x = Treatment, y = pH_mean_resprate, group = Colour, colour = Colour)) +
  geom_line(show.legend = FALSE) +
  xlab("relative (to be changed to absolute in next version) pCO2 level (µatm)") + ylab("") +
  geom_errorbar(aes(ymin = pH_mean_resprate - SEM, ymax = pH_mean_resprate + SEM, width = 0.2)) +
  theme_bw(base_size = 15) +
  scale_color_manual(values = c("#CD950C", "#00688B", "#8B0A50"),
                     breaks = c("Red", "Blue", "Green"),
                     labels = c("Induced Triploids", "Diploids", "Mated Triploids")) +
  labs(color = "Ploidy") +  # Change the legend title to "Ploidy"
  facet_wrap(~HS, nrow = 1) +
  guides(color = FALSE) +
  theme(strip.text = element_blank())

grid.arrange(tempresp.plot, DOresp.plot, pHresp.plot, nrow = 3)

# Mortality ---------------------------------------------------------------
#Temperature
survival_eventdat_temp$Temp <- factor(survival_eventdat_temp$Temp, levels = c("7.5", "12.5", "17.5", "22.5", "27.5"))#Set the order for the facets

pooled_site_survplot_temp_pooled <- survival_eventdat_temp %>% 
  filter(Time <= 28) %>% 
  ggsurvplot_facet(survfit(Surv(Time, event, type = "right") ~ Colour , data = .),
                   facet.by = "Temp",
                   data = .,
                   risk.table = FALSE, pval = FALSE, conf.int = TRUE, 
                   xlab = "",
                   ylab = "",
                   ylim = c(0.7, 1),
                   legend.strata = FALSE, 
                   ncol = 1)+
  theme_bw(base_size = 15) +
  theme(strip.background = element_blank(),  # Remove background fill from facet labels
        strip.text = element_text(color = "black")) +  # Set facet label color to black
  labs(legend = "ploidy") +
  guides(color = FALSE, fill = FALSE) +
  scale_color_manual(values = c("#00688B", "#CD950C", "#8B0A50")) +
  scale_fill_manual(values = c("#00688B", "#CD950C", "#8B0A50")) +
  facet_wrap(~Temp, ncol = 5, labeller = labeller(Temp = c("7.5" = "7.5°C", "12.5" = "12.5°C", "17.5" = "17.5°C", "22.5" = "22.5°C", "27.5" = "27.5°C")))

#DO
survival_eventdat_DO$DO <- factor(survival_eventdat_DO$DO, levels = c("20", "40", "60", "80", "100"))#Set#Order the facets

longpooled_site_survplot_DO_pooled <- survival_eventdat_DO %>% 
  filter(Time <= 28) %>% 
  ggsurvplot_facet(survfit(Surv(Time, event, type = "right") ~ Colour , data = .),
                   facet.by = "DO",
                   data = .,
                   risk.table = FALSE, pval = FALSE, conf.int = TRUE, 
                   xlab = "",
                   ylab = "Survival probability",
                   ylim = c(0.7, 1),
                   legend.strata = FALSE,
                   ncol = 1) +
  theme_bw(base_size = 15) +
  theme(strip.background = element_blank(),  # Remove background fill from facet labels
        strip.text = element_text(color = "black")) +  # Set facet label color to black
  guides(color = FALSE, fill = FALSE) +
  scale_color_manual(values = c("#00688B", "#CD950C", "#8B0A50")) +
  scale_fill_manual(values = c("#00688B", "#CD950C", "#8B0A50")) +
  facet_wrap(~DO, ncol = 5, labeller = labeller(DO = c("20" = "1.91 mgL-1", "40" = "3.83 mgL-1", "60" = "5.74 mgL-1", "80" = "7.66 mgL-1", "100" = "delete me")))

##pH
survival_eventdat_pH$Date1 <- survival_eventdat_pH$Date+3#The sampling events were off by 1 day

pooled_site_survplot_pH_pooled <- survival_eventdat_pH %>%
  ggsurvplot_facet(survfit(Surv(Date1, event, type = "right") ~ Colour , data = .),
                   facet.by = "CO2_level",
                   data = .,
                   risk.table = FALSE, pval = FALSE, conf.int = TRUE, 
                   xlab = "Time (days)",
                   ylab = "",
                   ylim = c(0.7, 1),
                   legend.strata = FALSE, 
                   ncol = 5)+
  theme_bw(base_size = 15) +
  theme(strip.background = element_blank(),  # Remove background fill from facet labels
        strip.text = element_text(color = "black")) +  # Set facet label color to black
  labs(legend = "ploidy") +
  scale_color_manual(values = c("#CD950C","#00688B", "#8B0A50")) +
  scale_fill_manual(values = c("#CD950C","#00688B", "#8B0A50")) +
  guides(color = FALSE, fill = FALSE)#+
  #facet_wrap(~CO2_level, labeller = labeller(CO2_level = c("20" = "1.91 mgL-1", "40" = "3.83 mgL-1", "60" = "5.74 mgL-1", "80" = "7.66 mgL-1", "100" = "delete me")))

grid.arrange(pooled_site_survplot_temp_pooled, longpooled_site_survplot_DO_pooled, pooled_site_survplot_pH_pooled, ncol = 1)

####################
#######Tables#######
####################
sizedat_pH <- pHsizedat
sizedat_pH <- sizedat_pH %>% mutate(Colour = gsub("Blue", "2M", Colour),
                                    Colour = gsub("Green", "3M", Colour),
                                    Colour = gsub("Red", "3C", Colour)) %>% 
  mutate(Colour = Ploidy)

pH_resprates1 <- pH_resprates %>% mutate(Colour = gsub("Blue", "2M", Colour),
                                       Colour = gsub("Green", "3M", Colour),
                                       Colour = gsub("Red", "3C", Colour)) %>% 
  mutate(Ploidy = Colour)
#Size
sizesummary_temp.table <- sizedat_temp %>% group_by(Treatment, Time, Ploidy) %>% 
  summarise(mean_mm = mean(Shell.Height), sem_mm = (sd(Shell.Height)/n()), mean_g = mean(Mass), sem_g = (sd(Mass)/n()))%>%
  mutate(mean_mm = sprintf("%.2f (± %.2f)", mean_mm, sem_mm),
         mean_g = sprintf("%.2f (± %.2f)", mean_g, sem_g)) %>%
  pivot_wider(names_from = Time, values_from = c(mean_mm, sem_mm, mean_g, sem_g)) %>% 
  select(-matches("sem_mm_.*"), -matches("sem_g_.*"))

sizesummary_DO.table <- sizedat_DO %>% group_by(Treatment, Time, Ploidy) %>% 
  summarise(mean_mm = mean(Shell.Height), sem_mm = (sd(Shell.Height)/n()), mean_g = mean(Mass), sem_g = (sd(Mass)/n()))%>%
  mutate(mean_mm = sprintf("%.2f (± %.2f)", mean_mm, sem_mm),
         mean_g = sprintf("%.2f (± %.2f)", mean_g, sem_g)) %>%
  pivot_wider(names_from = Time, values_from = c(mean_mm, sem_mm, mean_g, sem_g)) %>% 
  select(-matches("sem_mm_.*"), -matches("sem_g_.*"))

sizesummary_pH.table <- sizedat_pH %>% group_by(Treatment , Time.point, Ploidy) %>% 
  summarise(mean_mm = mean(Height), sem_mm = (sd(Height)/n()), mean_g = mean(Mass), sem_g = (sd(Mass)/n()))%>%
  mutate(mean_mm = sprintf("%.2f (± %.2f)", mean_mm, sem_mm),
         mean_g = sprintf("%.2f (± %.2f)", mean_g, sem_g)) %>%
  pivot_wider(names_from = Time.point, values_from = c(mean_mm, sem_mm, mean_g, sem_g)) %>% 
  select(-matches("sem_mm_.*"), -matches("sem_g_.*"))

sizetable <- bind_rows(sizesummary_temp.table, sizesummary_DO.table, sizesummary_pH.table) 

#Respiration
pH_resprates1 <- pH_resprates %>% mutate(Colour = gsub("Blue", "2M", Colour),
                                         Colour = gsub("Green", "3M", Colour),
                                         Colour = gsub("Red", "3C", Colour)) %>% 
  mutate(Ploidy = Colour)

temp_resprates1 <- temp_resprates %>% mutate(Ploidy = gsub("2m", "2M", Ploidy),
                                             Ploidy = gsub("3m", "3M", Ploidy),
                                             Ploidy = gsub("3c", "3C", Ploidy))

tempresp.table <- temp_resprates1 %>% 
  group_by(Treatment, HS, Ploidy) %>% 
  summarise(meano2 = mean(mass_corected_rate, na.rm = TRUE), sem_o2 = (sd(mass_corected_rate, na.rm = TRUE)/n())) %>% 
  mutate(meano2 = sprintf("%.2f (± %.2f)", meano2 , sem_o2))%>%
  pivot_wider(names_from = Ploidy, values_from = c(meano2, sem_o2)) %>% 
  select(meano2_2M, meano2_3C, meano2_3M)


doresp.table <- DO_resprates %>% group_by(Treatment, HS, Ploidy) %>% 
  summarise(meano2 = mean(mass_corected_rate, na.rm = TRUE), sem_o2 = (sd(mass_corected_rate, na.rm = TRUE)/n())) %>% 
  mutate(meano2 = sprintf("%.2f (± %.2f)", meano2 , sem_o2))%>%
  pivot_wider(names_from = Ploidy, values_from = c(meano2, sem_o2)) %>% 
  select(meano2_2M, meano2_3C, meano2_3M)

pHresp.table <- pH_resprates1 %>% group_by(Treatment, HS, Ploidy) %>% 
  summarise(meano2 = mean(mass_corected_rate, na.rm = TRUE), sem_o2 = (sd(mass_corected_rate, na.rm = TRUE)/n())) %>% 
  mutate(meano2 = sprintf("%.2f (± %.2f)", meano2 , sem_o2))%>%
  pivot_wider(names_from = Ploidy, values_from = c(meano2, sem_o2)) %>% 
  select(meano2_2M, meano2_3C, meano2_3M)

resptable <- bind_rows(tempresp.table, doresp.table, pHresp.table)

#Mortality 
#temp - 450 total group
tempsurv.plot <- survival_eventdat_temp %>% filter(Time <= 28) %>% 
  mutate(Ploidy = Colour) %>% 
  mutate(Treatment = Temp) %>% 
  group_by(Ploidy, Treatment) %>%
  summarise(sum=sum(event)) %>% 
  mutate(Ploidy = gsub("Blue","2M", Ploidy),
         Ploidy = gsub("Red","3M", Ploidy),
         Ploidy = gsub("Green","3C", Ploidy)) %>% 
  pivot_wider(names_from = Ploidy, values_from = sum)
  

#DO 450 total group
DOsurv.plot <- survival_eventdat_DO %>% filter(Time <= 28) %>% 
  mutate(Ploidy = Colour) %>%
  mutate(Treatment = DO) %>% 
  group_by(Ploidy, Treatment) %>%
  summarise(sum=sum(event)) %>% 
  mutate(Ploidy = gsub("Blue","2M", Ploidy),
         Ploidy = gsub("Red","3M", Ploidy),
         Ploidy = gsub("Green","3C", Ploidy)) %>% 
  pivot_wider(names_from = Ploidy, values_from = sum)

#pH 450 total group
pHsurv.plot <- survival_eventdat_pH %>% filter(Time <= 28) %>% 
  mutate(Ploidy = Colour) %>% 
  mutate(Treatment = CO2_level) %>% 
  group_by(Ploidy, Treatment) %>%
  summarise(sum=sum(event)) %>% 
  mutate(Ploidy = gsub("Blue","2M", Ploidy),
         Ploidy = gsub("Red","3M", Ploidy),
         Ploidy = gsub("Green","3C", Ploidy)) %>% 
  pivot_wider(names_from = Ploidy, values_from = sum)

morttable <- bind_rows(tempsurv.plot, DOsurv.plot, pHsurv.plot)

write.xlsx(sizetable, here("Output", "Tables for pub", "sizetable.xlsx"))
write.xlsx(resptable, here("Output", "Tables for pub", "resptable.xlsx"))
write.xlsx(morttable, here("Output", "Tables for pub", "morttable.xlsx"))

