#masters drought paper

#https://www.datanovia.com/en/blog/how-to-add-p-values-to-ggplot-facets/

library(ggplot2)
library(readr)
library(patchwork)
library(ggpubr)
library(rstatix)

LabHarvest <- read_csv("C:/Users/telfo/OneDrive/Desktop/Masters/Masters_harvest_2018.csv")

colnames(LabHarvest)
LabHarvest$Treatment <- factor(LabHarvest$Treatment, levels=c('4%','8%','16%'))
LabHarvest$Species<-factor(LabHarvest$Species, levels=c('VE','VS'))
LabHarvest$Treatment <- as.factor(LabHarvest$Treatment)
LabHarvest$Treatment_2 <- as.numeric(LabHarvest$Treatment_2)
LabHarvest$Harvest<-as.factor(LabHarvest$Harvest)
LabHarvest$Tag<-as.factor(LabHarvest$Tag)
LabHarvest$Nodule_mass_fraction <- as.numeric(LabHarvest$Nodule_mass_fraction)
LabHarvest$R_S <- as.numeric(LabHarvest$R_S)
LabHarvest$BG_DW <- as.numeric(LabHarvest$BG_DW)
LabHarvest$AG_DW <- as.numeric(LabHarvest$AG_DW)

supp.labs <- c("Vachellia erioloba", "Vachellia sieberiana")
names(supp.labs) <- c("VE", "VS")


H3<-subset(LabHarvest, Harvest=="3")
LabHarvest_VS<-subset(LabHarvest, Species=="VS")
LabHarvest_VE<-subset(LabHarvest, Species=="VE")

my_comparisons <- list( c("4%", "8%"), c("4%", "16%"), c("8%", "16%") )

###############################figure 3 boxplots

(P3_below <- ggplot(data = H3,
                   aes(x = Treatment, 
                       y = BG_DW,
                       fill= Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))  +
  guides(fill = "none")+  facet_grid(.~ Species, labeller = labeller(Species = supp.labs)) +
  labs(x = "Treatment", y = "Below biomass (g)")+
  theme(legend.position="none", strip.text = element_text(face = "italic")) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif"))


#####################################

(P3_above <- ggplot(data = H3,
                   aes(x = Treatment, 
                       y = AG_DW,
                       fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))  +
  facet_grid(.~ Species, labeller = labeller(Species = supp.labs)) +
  labs(x = "Treatment", y = "Above biomass (g)") +
  theme(legend.position="none", strip.text = element_text(face = "italic")) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif"))


#####################################

(P3_r_s <- ggplot(data = H3,
                 aes(fill = Treatment,
                   x = Treatment, 
                     y = R_S)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))  +
  guides(fill = "none")+  facet_grid(.~ Species, labeller = labeller(Species = supp.labs)) +
  labs(x = "Treatment", y = "Aboveground: belowground biomass")+
  theme(legend.position="none", strip.text = element_text(face = "italic")) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif"))

####################################### nodules Figure 4


stat.test.5 <- LabHarvest_VS %>%
  group_by(Treatment) %>%
  t_test(Nodules_W ~ Harvest) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test.5

(P4_nod_1 <- ggplot(data = LabHarvest_VS,
                   aes(x = Harvest, 
                       y = Nodules_W,
                       fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))   +
  labs(x = "Harvest", y = "Nodule biomass (g)") +
  facet_grid(.~ Treatment) +
  theme(legend.position="none"))

stat.test.5 <- stat.test.5 %>% add_xy_position(x = "Harvest")
P4_nod_1 + stat_pvalue_manual(stat.test.5, label = "p.adj.signif")


######################################

(P4_nod_2 <- ggplot(data = LabHarvest_VS,
                   aes(x = BG_DW, 
                       y = Nodules_W ,
                       color = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_minimal(base_size = 17) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Belowground biomass (g)", y = "Nodule biomass (g)") +
  theme(legend.position= c(0.2, 0.8)))

#######################################

stat.test <- LabHarvest_VS %>%
  group_by(Treatment) %>%
  t_test(Nodule_mass_fraction ~ Harvest) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test 


(P4_nod_3 <- ggplot(data = LabHarvest_VS,
                   aes(x = Harvest, 
                       y = Nodule_mass_fraction,
                       fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))   +
  labs(x = "Harvest", y = "Nodule mass fraction (g)") +
  facet_grid(.~ Treatment) +
  theme(legend.position="none"))

stat.test <- stat.test %>% add_xy_position(x = "Harvest")
P4_nod_3 + stat_pvalue_manual(stat.test, label = "p.adj.signif")

