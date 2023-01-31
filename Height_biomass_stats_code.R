#load necessary packages

library(lme4)
library(car)

Height_data <- read_csv("C:/Users/telfo/OneDrive/Desktop/Masters/Height_data_2.csv")
Height_data$IndividualID <- as.factor(paste(as.character(Height_data$Tag),as.character(Height_data$Pot),sep="_")) 
Height_data$Treatment <- factor(Height_data$Treatment, levels=c('4%','8%','16%'))
Height_data$Species <- factor(Height_data$Species, levels=c('VE','VS'))
H3<-subset(Height_data, Harvest == "3")
VS<-subset(H3, Species=="VS")
VE<-subset(H3, Species=="VE")



#one issue to sort is getting a unique identifier for each individual
Height_data$IndividualID <- as.factor(paste(as.character(Height_data$Tag),as.character(Height_data$Pot),sep="_")) 
summary(Height_data)

#only using H3 to see if there is a significant relationship
Height.data_H3 <- subset(Height_data,Harvest=="3")
tmp <- subset(Height_data,Harvest!="3")
sort(Height.data_H3$IndividualID[which(Height.data_H3$Pot%in%tmp$IndividualID)])


#simple linear model if growth varies by species
lmem0 <- lmer(Height~1 + (1|IndividualID),data=Height.data_H3)
lmem1 <- lmer(Height ~ Species + (1|IndividualID), data=Height.data_H3)
summary(lmem1)
anova(lmem0,lmem1)

lmem4 <- lmer(Height ~ Species + Treatment + Week + Treatment:Week + Species:Treatment + Species:Week + (1|IndividualID),data=Height.data_H3)
lmem5 <- lmer(Height ~ Species*Treatment*Week + (1|IndividualID),data=Height.data_H3)
anova(lmem4,lmem5)

prior2 <- list(R = list(V = 1, nu = 0.002),
               G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                        G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                        G3 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))


#################################### biomass

LabHarvest <- read_csv("C:/Users/telfo/OneDrive/Desktop/Masters/Masters_harvest_2018.csv")
#LabHarvest <- read_csv("C:/Users/telfo/OneDrive/Desktop/Masters/LabHarvestCSV.csv")
LabHarvest<-na.omit(LabHarvest)
LabHarvest$Treatment <- factor(LabHarvest$Treatment, levels=c('4%','8%','16%'))
LabHarvest$Species<-factor(LabHarvest$Species, levels=c('VS','VE'))
LabHarvest$Harvest<-as.factor(LabHarvest$Harvest)
LabHarvest$Tag<-as.factor(LabHarvest$Tag)
LabHarvest$Nodule_mass_fraction <- as.numeric(LabHarvest$Nodule_mass_fraction)
LabHarvest$Nodules_W <- as.numeric(LabHarvest$Nodules_W)
LabHarvest$Treatment<-as.factor(LabHarvest$Treatment)
LabHarvest_VS<-subset(LabHarvest, Species=="VS")
LabHarvest_VE<-subset(LabHarvest, Species=="VE")
H3 <- subset(LabHarvest, Harvest=="3")

########################## biomass (only using harvest 3 data)

hist(H3$BG_DW)
#left skewed
log_BG<-log(H3$BG_DW)
hist(log_BG)
BG.aov <- aov(log_BG ~ Treatment + Species + Treatment:Species, data = H3)
summary(BG.aov)
coef(BG.aov) 
confint(BG.aov)
TukeyHSD(BG.aov)
model.tables(BG.aov, "means")
plot(BG.aov, 1)
#3 outliers
leveneTest(BG_DW~ Treatment*Species, data = H3)

hist(H3$AG_DW)
#looks fine to me
AG.aov <- aov(AG_DW ~ Treatment + Species + Treatment:Species, data = H3)
summary(AG.aov)
coef(AG.aov) 
confint(AG.aov)
TukeyHSD(AG.aov)
model.tables(AG.aov, "means")
plot(AG.aov, 1)
#3 outliers
leveneTest(AG_DW~ Treatment*Species, data = H3)

hist(H3$R_S)
#left skewed
R_S_log <- log(H3$R_S)
hist(R_S_log)
RS.aov <- aov(R_S_log ~ Treatment + Species + Treatment:Species, data = H3)
summary(RS.aov)
coef(AG.aov) 
confint(AG.aov)
TukeyHSD(AG.aov)
model.tables(AG.aov, "means")
plot(AG.aov, 1)
#3 outliers
leveneTest(AG_DW~ Treatment*Species, data = H3)


####################### nodules (only using V. sieberiana data)

hist(LabHarvest_VS$Nodules_W)
#left skewed
log_N<-log(LabHarvest_VS$Nodules_W+1 )
hist(log_N)
log_N<-na.omit(log_N)
Nw.aov <- aov(log_N~ Treatment + Harvest + Treatment:Harvest , data = LabHarvest_VS)
summary(Nw.aov)
coef(Nw.aov) 
confint(Nw.aov)
TukeyHSD(Nw.aov)
model.tables(Nw.aov, "means")
plot(Nw.aov, 1)
#3 outliers
leveneTest(Nodules_W~ Treatment*Harvest, data = LabHarvest_VS)

LabHarvest$Nodule_mass_fraction <- as.numeric(LabHarvest$Nodule_mass_fraction)
hist(LabHarvest_VS$Nodule_mass_fraction)
#left skewed
log_Nmf<-log(LabHarvest_VS$Nodule_mass_fraction +1 )
hist(log_Nmf)
Nmf.aov <- aov(log_Nmf ~ Treatment + Harvest + Treatment:Harvest, data = LabHarvest_VS)
summary(Nmf.aov)
coef(Nmf.aov) 
confint(Nmf.aov)
TukeyHSD(Nmf)
model.tables(Nmf.aov, "means")
plot(Nw.aov, 1)
#3 outliers
leveneTest(Nodule_mass_fraction~ Treatment*Harvest, data = LabHarvest_VS)


