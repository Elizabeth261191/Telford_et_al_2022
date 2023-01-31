#masters drought paper


library(ggplot2)
library(readr)
library(patchwork)
library(dplyr)

Height_data <- read_csv("C:/Users/telfo/OneDrive/Desktop/Masters/Height_data_2.csv")
Height_data$IndividualID <- as.factor(paste(as.character(Height_data$Tag),as.character(Height_data$Pot),sep="_")) 
Height_data$Treatment <- factor(Height_data$Treatment, levels=c('4%','8%','16%'))
Height_data$Species <- factor(Height_data$Species, levels=c('VE','VS'))
H3<-subset(Height_data, Harvest == "3")
VS<-subset(H3, Species=="VS")
VE<-subset(H3, Species=="VE")

# New facet label names for supp variable
supp.labs <- c("Vachellia erioloba", "Vachellia sieberiana")
names(supp.labs) <- c("VE", "VS")


fm1 <- lm(formula=Height~Treatment+Week, data=Height_data)
Height_data = cbind(Height_data, pred = predict(fm1))

predslm = predict(fm1, interval = "confidence")
head(predslm)

Height_data= cbind(Height_data, predslm)
head(Height_data)

colnames(Height_data)
colnames(Height_data)[colnames(Height_data) == 'fit'] <- 'height_fit'
colnames(Height_data)[colnames(Height_data) == 'lwr'] <- 'height_lwr'
colnames(Height_data)[colnames(Height_data) == 'upr'] <- 'height_upr'
colnames(Height_data)[colnames(Height_data) == 'pred'] <- 'height_pred'
colnames(Height_data)


(P2_height <- ggplot(data = Height_data,
                    aes(x = Week, 
                        y = Height,
                        color = Treatment, fill = Treatment)) +
  geom_point() +
  geom_line(mapping=aes(y=height_pred)) +
    geom_ribbon( aes(ymin = height_lwr, ymax = height_upr, fill = Treatment, color = NULL), alpha = .15) +
  theme_minimal(base_size = 17) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
  facet_grid(.~ Species, labeller = labeller(Species = supp.labs))+
  theme(legend.position = c(0.6, 0.8)) +
  labs(x = "Week", y = "Height (mm)"))

P2_height + scale_x_continuous(breaks = seq(1,15 , by = 2))

###height logged


fm2 <- lm(formula=log_Height~Treatment+Week+Species, data=Height_data)
Height_data = cbind(Height_data, pred = predict(fm2))

preds2m = predict(fm2, interval = "confidence")
head(preds2m)

Height_data= cbind(Height_data, preds2m)
head(Height_data)

colnames(Height_data)
colnames(Height_data)[colnames(Height_data) == 'fit'] <- 'log_height_fit'
colnames(Height_data)[colnames(Height_data) == 'lwr'] <- 'log_height_lwr'
colnames(Height_data)[colnames(Height_data) == 'upr'] <- 'log_height_upr'
colnames(Height_data)[colnames(Height_data) == 'pred'] <- 'log_height_pred'
colnames(Height_data)

Height_data$log_Height <- as.numeric(Height_data$log_Height)

(P2_height <- ggplot(data = Height_data,
                     aes(x = Week, 
                         y = log_Height,
                         color = Treatment, fill = Treatment)) +
    geom_point(size=2) +
    geom_line(mapping=aes(y=log_height_pred)) +
    geom_ribbon( aes(ymin = log_height_lwr, ymax = log_height_upr, fill = Treatment, color = NULL), alpha = .15) +
    theme_minimal(base_size = 17) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
    facet_grid(.~ Species, labeller = labeller(Species = supp.labs))+
    theme(legend.position = c(0.6, 0.8), strip.text = element_text(face = "italic")) +
    labs(x = "Week", y = "Log height (mm)"))

P2_height + scale_x_continuous(breaks = seq(1,15 , by = 2)) + scale_y_continuous(limits=c(0.8, 2.5))



#log absolute change

fm3 <- lm(formula=log_Absolute_change ~Treatment+Week+Species, data=Height_data)
Height_data = cbind(Height_data, pred = predict(fm3))

predslm3 = predict(fm3, interval = "confidence")
head(predslm3)

Height_data= cbind(Height_data, predslm3)
head(Height_data)

colnames(Height_data)
colnames(Height_data)[colnames(Height_data) == 'fit'] <- 'log_AC_fit'
colnames(Height_data)[colnames(Height_data) == 'lwr'] <- 'log_AC_lwr'
colnames(Height_data)[colnames(Height_data) == 'upr'] <- 'log_AC_upr'
colnames(Height_data)[colnames(Height_data) == 'pred'] <- 'log_AC_pred'
colnames(Height_data)


(P2_AGR <- ggplot(data = Height_data,
                     aes(x = Week, 
                         y = log_Absolute_change,
                         color = Treatment, fill = Treatment)) +
    geom_point(size=2) +
    geom_line(mapping=aes(y=log_AC_pred)) +
    geom_ribbon( aes(ymin = log_AC_lwr, ymax = log_AC_upr, fill = Treatment, color = NULL), alpha = .15) +
    theme_minimal(base_size = 17) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
    facet_grid(.~ Species, labeller = labeller(Species = supp.labs))+
    theme(legend.position = "none", strip.text = element_text(face = "italic")) +
    labs(x = "Week", y = "Log absolute growth (mm)"))



P2_AGR + scale_x_continuous(breaks = seq(1,15 , by = 2))

###absolute growth

fm4 <- lm(formula=Absolute_change ~Treatment+Week, data=Height_data)
Height_data = cbind(Height_data, pred = predict(fm4))

predslm4 = predict(fm4, interval = "confidence")
head(predslm4)

Height_data= cbind(Height_data, predslm4)
head(Height_data)

colnames(Height_data)
colnames(Height_data)[colnames(Height_data) == 'fit'] <- 'AC_fit'
colnames(Height_data)[colnames(Height_data) == 'lwr'] <- 'AC_lwr'
colnames(Height_data)[colnames(Height_data) == 'upr'] <- 'AC_upr'
colnames(Height_data)[colnames(Height_data) == 'pred'] <- 'AC_pred'
colnames(Height_data)



(P2_AGR <- ggplot(data = Height_data,
                  aes(x = Week, 
                      y = Absolute_change,
                      color = Treatment, fill = Treatment)) +
    geom_point() +
    geom_line(mapping=aes(y=AC_pred)) +
    geom_ribbon( aes(ymin = AC_lwr, ymax = AC_upr, fill = Treatment, color = NULL), alpha = .15) +
    theme_minimal(base_size = 17) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_fill_manual(values = c("darkorange","purple","cyan4")) + 
    facet_grid(.~ Species, labeller = labeller(Species = supp.labs))+
    theme(legend.position = "none", strip.text = element_text(face = "italic")) +
    labs(x = "Week", y = "Absolute growth (mm)"))



P2_AGR + scale_x_continuous(breaks = seq(1,15 , by = 2))



