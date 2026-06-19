Raw_data <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Raw data.xlsx", sheet = "Analysis",
  col_types = c(
    # Col  1: Date
    "date",
    # Col  2: Site
    "text",
    # Col  3: Month1
    "text",
    # Col  4: Plot
    "text",
    # Col  5: Treatment
    "text",
    # Col  6: Cable No
    "numeric",
    # Col  7: Species
    "text",
    # Col  8: Position
    "text",
    # Col  9: Individu22
    "text",
    # Col 10: Individu
    "text",
    # Col 11: Time (predawn)
    "text",
    # Col 12: DV
    "numeric",
    # Col 13: Predawn
    "numeric",
    # Col 14: PredawnWP
    "numeric",
    # Col 15: Time (midday)
    "text",
    # Col 16: DVmd
    "numeric",
    # Col 17: DT
    "numeric",
    # Col 18: Midday
    "numeric",
    # Col 19: MiddayWP
    "numeric",
    # Col 20: DWP
    "numeric",
    # Col 21: DBH_Aug
    "numeric",
    # Col 22: Area_Aug
    "numeric",
    # Col 23: SA
    "numeric",
    # Col 24: K
    "numeric",
    # Col 25: u101
    "numeric",
    # Col 26: u10
    "numeric",
    # Col 27: (blank / empty column)
    "skip",
    # Col 28: F10
    "numeric",
    # Col 29: F
    "numeric",
    # Col 30: k
    "numeric",
    # Col 31: Spflow
    "numeric",
    # Col 32: Normalizedk
    "numeric",
    # Col 33: Tcan
    "numeric",
    # Col 34: DBH
    "numeric",
    # Col 35: Height
    "numeric",
    # Col 36: u
    "numeric",
    # Col 37: SWC
    "numeric",
    # Col 38: Succ
    "text",
    # Col 39: Origin
    "text",
    # Col 40: Leaf
    "text",
    # Col 41: RH
    "numeric",
    # Col 42: VPD
    "numeric",
    # Col 43: PPFD
    "numeric",
    # Col 44: RGR
    "numeric",
    # Col 45: RHeight
    "numeric"
  ))
library(ggpubr)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(gridExtra)
library(scales)
library(purrr)
library(readxl)
cleanup=theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(color = "black"))

Control<-subset(Raw_data, Treatment=="Control")
Makera1<-subset(Raw_data, Site=="Makera")
## Remove croton
Makera <- Makera1 %>% filter(Species != "Cme")
## Figure 1
Month<-factor(Makera$Month1,c("Early Jun","Mid Jun","Early Jul","Mid Jul","Early Aug","Mid Aug","Early Sep"))
data1=summarySE(Makera,na.rm=TRUE, measurevar="PredawnWP", groupvars=c("Treatment","Month", "Species"))
data11=summarySE(Makera,na.rm=TRUE, measurevar="PredawnWP", groupvars=c("Month","Treatment"))

g1<-ggplot(data1, aes(x = Month, y = PredawnWP, group = Month, color = Species)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = PredawnWP- se, ymax = PredawnWP + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank())+
  geom_line(aes(group = Species))+facet_wrap(~Treatment, labeller = labeller(Treatment = c( Control = "Control", Irrigated = "Irrigated"))) +labs( x = "Months", y = "Predawn DT") +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+labs(y=expression(italic(ψ)["pd"]*" (MPa)"))+labs( x = "Months")+theme(
    strip.text = element_text(size = 12, face = "bold"), axis.text = element_text(size = 12),  axis.title.y = element_text(size = 12), axis.title.x = element_blank(), axis.text.x = element_blank() )+
  geom_line(data = data11, aes(x = Month, y = PredawnWP, group = Treatment), color = "black", linetype = "dashed")+geom_errorbar(data = data11, aes(x = Month, ymin = PredawnWP - se, ymax = PredawnWP + se), width = 0.2, color = "black") +  geom_point(data = data11, aes(x = Month, y = PredawnWP), shape = 8, size = 3, color = "black")

g1
data2=summarySE(Makera,na.rm=TRUE, measurevar="MiddayWP", groupvars=c("Species","Month","Treatment"))
data21=summarySE(Makera,na.rm=TRUE, measurevar="MiddayWP", groupvars=c("Month","Treatment"))

g2<-ggplot(data2, aes(x = Month, y = MiddayWP, group = Month, color = Species)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = MiddayWP - se, ymax = MiddayWP + se), width = 0.1) +geom_point(size = 2) +theme(strip.text = element_blank()) +
  geom_line(aes(group = Species))+labs(y=expression(italic(ψ)["md"]*" (MPa)")) +labs( x = "Months") +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+facet_wrap (~Treatment)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+theme(legend.position="none")+geom_line(data = data21, aes(x = Month, y = MiddayWP, group = Treatment), color = "black", linetype = "dashed")+geom_errorbar(data = data21, aes(x = Month, ymin = MiddayWP - se, ymax = MiddayWP + se), width = 0.2, color = "black") +  geom_point(data = data21, aes(x = Month, y = MiddayWP), shape = 8, size = 3, color = "black")
g2

data3=summarySE(Makera,na.rm=TRUE, measurevar="DWP", groupvars=c("Species","Month","Treatment"))
data31=summarySE(Makera,na.rm=TRUE, measurevar="DWP", groupvars=c("Month","Treatment"))

g3<-ggplot(data3, aes(x = Month, y = DWP, group = Month, color = Species)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = DWP - se, ymax = DWP + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank())+geom_line(aes(group = Species)) +labs( x = "Months", y= expression(Delta*italic(ψ)~"(MPa)")) +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+facet_wrap (~Treatment)+theme(legend.position="none")+
  geom_line(data = data31, aes(x = Month, y = DWP, group = Treatment), color = "black", linetype = "dashed")+geom_errorbar(data = data31, aes(x = Month, ymin = DWP - se, ymax = DWP + se), width = 0.2, color = "black") +  geom_point(data = data31, aes(x = Month, y = DWP), shape = 8, size = 3, color = "black")
g3
Makera2 <- Makera %>% filter(Species != "Mla")
Month<-factor(Makera2$Month1,c("Early Jun","Mid Jun","Early Jul","Mid Jul","Early Aug","Mid Aug","Early Sep"))

data5=summarySE(Makera2,na.rm=TRUE, measurevar="Spflow", groupvars=c("Species","Month","Treatment"))
data51=summarySE(Makera2,na.rm=TRUE, measurevar="Spflow", groupvars=c("Month","Treatment"))
g5<-ggplot(data5, aes(x = Month, y = Spflow, group = Month, color = Species)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = Spflow- se, ymax = Spflow + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank())+ theme(legend.position="none")+
  geom_line(aes(group = Species))+facet_wrap(~Treatment) +labs( x = "Months", y = "Predawn DT") +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+ylab(bquote(italic(F)~'('*kg~h^-1*')'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_line(data = data51, aes(x = Month, y = Spflow, group = Treatment), color = "black", linetype = "dashed")+geom_errorbar(data = data51, aes(x = Month, ymin = Spflow - se, ymax = Spflow + se), width = 0.2, color = "black") +  geom_point(data = data51, aes(x = Month, y = Spflow), shape = 8, size = 3, color = "black")
g5
## Predawn DV
Month <- factor(Makera$Month1, c("Early Jun","Mid Jun","Early Jul","Mid Jul","Early Aug","Mid Aug","Early Sep"))
dataDV  <- summarySE(Makera, na.rm = TRUE, measurevar = "DV", groupvars = c("Treatment", "Month", "Species"))
dataDV1 <- summarySE(Makera, na.rm = TRUE, measurevar = "DV", groupvars = c("Month", "Treatment"))
gg<-ggplot(dataDV, aes(x = Month, y = DV, group = Month, color = Species)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = DV- se, ymax = DV + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank())+
geom_line(aes(group = Species))+facet_wrap(~Treatment, labeller = labeller(Treatment = c( Control = "Control", Irrigated = "Irrigated"))) +labs( x = "Months", y = "Predawn ΔV (mV)") +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+labs( x = "Months" )+theme(
 strip.text = element_text(size = 12, face = "bold"), axis.text = element_text(size = 12),  axis.title.y = element_text(size = 12))+
 geom_line(data = dataDV1, aes(x = Month, y = DV, group = Treatment), color = "black", linetype = "dashed")+geom_errorbar(data = dataDV1, aes(x = Month, ymin = DV - se, ymax = DV + se), width = 0.2, color = "black") +  geom_point(data = dataDV1, aes(x = Month, y = DV), shape = 8, size = 3, color = "black")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg
ggplot2::ggsave('PredawnDV.png', gg,width = 10.14, height = 5.75, dpi = 400)
library(cowplot)
legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")
plots <- plot_grid(g1, g2, g3,g5, ncol = 1, align = 'v')
plots <- plot_grid(g1, g2, g3, g5, ncol = 1, align = 'v', rel_heights = c(2.5, 2, 2, 4))
plots
Figure <- plot_grid( plots,ncol = 2, rel_widths = c(3, 1.5))
Figure
ggplot2::ggsave('Waterfff.png', plots,width = 10.14, height = 5.75, dpi = 1000)
ggplot2::ggsave('Wateruses.png', g1,width = 10.14, height = 5.75, dpi = 1000)
### Leaf habit
Month<-factor(Makera$Month1,c("Early Jun","Mid Jun","Early Jul","Mid Jul","Early Aug","Mid Aug","Early Sep"))
data1=summarySE(Makera,na.rm=TRUE, measurevar="PredawnWP", groupvars=c("Treatment","Month", "Leaf"))
data11=summarySE(Makera,na.rm=TRUE, measurevar="PredawnWP", groupvars=c("Month","Treatment"))

g1<-ggplot(data1, aes(x = Month, y = PredawnWP, group = Month, color = Leaf))  +geom_errorbar(aes(ymin = PredawnWP- se, ymax = PredawnWP + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank())+
  geom_line(aes(group = Leaf))+facet_wrap(~Treatment, labeller = labeller(Treatment = c( Control = "Control", Irrigated = "Irrigated"))) +labs( x = "Months", y = "Predawn DT")+ scale_color_manual(values = c("E" = "#117733", "SD" = "#E69F00"))  +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+labs(y=expression(italic(ψ)["pd"]*" (MPa)"))+labs( x = "Months")+theme(
    strip.text = element_text(size = 12, face = "bold"), axis.text = element_text(size = 12),  axis.title.y = element_text(size = 12), axis.title.x = element_blank(), axis.text.x = element_blank() )

g1
data2=summarySE(Makera,na.rm=TRUE, measurevar="MiddayWP", groupvars=c("Leaf","Month","Treatment"))
data21=summarySE(Makera,na.rm=TRUE, measurevar="MiddayWP", groupvars=c("Month","Treatment"))

g2<-ggplot(data2, aes(x = Month, y = MiddayWP, group = Month, color = Leaf)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = MiddayWP - se, ymax = MiddayWP + se), width = 0.1) +geom_point(size = 2) +theme(strip.text = element_blank()) +
  geom_line(aes(group = Leaf))+scale_color_manual(values = c("E" = "#117733", "SD" = "#E69F00")) +labs(y=expression(italic(ψ)["md"]*" (MPa)")) +labs( x = "Months") +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+facet_wrap (~Treatment)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+theme(legend.position="none")
g2

data3=summarySE(Makera,na.rm=TRUE, measurevar="DWP", groupvars=c("Leaf","Month","Treatment"))
data31=summarySE(Makera,na.rm=TRUE, measurevar="DWP", groupvars=c("Month","Treatment"))

g3<-ggplot(data3, aes(x = Month, y = DWP, group = Month, color = Leaf)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = DWP - se, ymax = DWP + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank())+geom_line(aes(group = Leaf))+scale_color_manual(values = c("E" = "#117733", "SD" = "#E69F00"))  +labs( x = "Months", y= expression(Delta*italic(ψ)~"(MPa)")) +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+facet_wrap (~Treatment)+theme(legend.position="none")
g3
Makera2 <- Makera %>% filter(Species != "Mla")
Month<-factor(Makera2$Month1,c("Early Jun","Mid Jun","Early Jul","Mid Jul","Early Aug","Mid Aug","Early Sep"))

data5=summarySE(Makera2,na.rm=TRUE, measurevar="Spflow", groupvars=c("Leaf","Month","Treatment"))
data51=summarySE(Makera2,na.rm=TRUE, measurevar="Spflow", groupvars=c("Month","Treatment"))
g5<-ggplot(data5, aes(x = Month, y = Spflow, group = Month, color = Leaf)) +scale_shape_manual(values = c(15, 17, 18, 19, 4)) +geom_errorbar(aes(ymin = Spflow- se, ymax = Spflow + se), width = 0.1) +geom_point(size = 2) + theme(strip.text = element_blank())+scale_color_manual(values = c("E" = "#117733", "SD" = "#E69F00"))+geom_line(aes(group = Leaf)) +facet_wrap(~Treatment) +labs( x = "Months", y = "Predawn DT") +cleanup+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+ylab(bquote(italic(F)~'('*kg~h^-1*')'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.position="none")
g5
library(cowplot)
legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")
plots <- plot_grid(g1, g2, g3,g5, ncol = 1, align = 'v')
plots <- plot_grid(g1, g2, g3, g5, ncol = 1, align = 'v', rel_heights = c(2.5, 2, 2, 4))
plots
Figure <- plot_grid( plots,ncol = 2, rel_widths = c(3, 1.5))
Figure
ggplot2::ggsave('Leaf11.png', plots,width = 10.14, height = 5.75, dpi = 1000)
ggplot2::ggsave('Leaf111.png', plots,width = 10.14, height = 5.75, dpi = 1000)


## Figure S2
## Daily change
Daily <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Daily.xlsx", 
                    sheet = "Rubona")
Daily<-subset(Daily, DAYS=="Day1")
data1=summarySE(Daily,na.rm=TRUE, measurevar="Value", groupvars=c("Species", "Time"))
data1$Species <- factor(data1$Species , levels=c("Fth", "Sgu", "Bbr","Pfu","Pfa","Dto"))
Fig1<-ggplot(data1, aes(x = Time, y = Value ))+geom_line(aes(color = Species))+geom_point(aes(color = Species))  +cleanup+scale_x_datetime(name="Time of the day", labels = date_format("%H", tz = "Europe/London"), date_breaks = "2 hour")
Fig1
Fig1<-Fig1+labs(y = "ΔV (mV)")
ggplot2::ggsave('Daily sapflux.png', Fig1,width = 8.14, height = 3.65, dpi = 1000)
## Add temperature
data_u10 <- summarySE(Daily, na.rm = TRUE, measurevar = "u10", groupvars = c("Species", "Time"))
data_temp <- summarySE(Daily, na.rm = TRUE, measurevar = "Tcan", groupvars = c("Species", "Time"))
data_merged <- merge(data_u10, data_temp, by = c("Species", "Time"))
Fig1 <- ggplot(data_merged, aes(x = Time)) + geom_line(aes(y = u10, color = Species), size = 1) +
  geom_line(aes(y = Tcan, color = Species), linetype = "dashed", size = 1) +
  ylab(bquote('Total sapflux density ('*10^-6~m~s^-1*')')) +
  scale_y_continuous(  name = bquote('Total sapflux density ('*10^-6~m~s^-1*')'),
                       sec.axis = sec_axis(~ ., name = "Temperature (°C)")  )+  scale_x_datetime(name = "Time of the day",      labels = date_format("%H", tz = "Europe/London"), date_breaks = "2 hour") +theme_minimal() + theme(axis.text = element_text(size = 12), axis.title.y = element_text(size = 12),  axis.title.x = element_text(size = 12),  legend.position = "right") +cleanup
Fig1
## Figure S1
### Taller trees are more tolerant to drought???
## Making a graph from two datasets 
Changes1 <- read_excel("C:/Gothenburg/Data/Hydraulic/Analysis/Changes.xlsx", 
                       sheet = "Change")
Changes <- Changes1 %>% filter(Species != "Cme")

data2 = summarySE(Changes1, na.rm = TRUE, measurevar = "Height", groupvars = c("Species", "Treatment"))
data1 = summarySE(Changes1, na.rm = TRUE, measurevar = "PMD", groupvars = c("Species", "Treatment"))
merged_data <- merge(data1, data2, by = c("Species", "Treatment"))
GG1 <- ggscatter(merged_data, x = 'Height', y = 'PMD', color = "darkgreen", conf.int = TRUE, add = "none",  palette = "jco") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),      show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01,label.y=0) +
  scale_color_discrete(name = 'Species') + labs(y = "Changes in ψpd (MPa)", x = "Tree height (cm)") +
  theme(legend.position = "right", axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) + facet_wrap(~Treatment)
GG1
GG1 <- GG1 + geom_smooth(data = subset(merged_data, Treatment == "Control"),  aes(x = Height, y = PMD), method = "lm", color = "black", se = TRUE)
GG1
ggplot2::ggsave('ChangeMPA.png', GG1,width = 8.14, height = 3.65, dpi = 1000)

## Figure 3
Combined_Data <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Combined_Data.xlsx", sheet = "Sheet2")
Combined_Data$Month <- factor(Combined_Data$Month, levels = c("Early Jun", "Mid Jun", "Early Jul", "Mid Jul", "Early Aug", "Mid Aug", "Early Sep"))
g<-ggscatter(Combined_Data, x = 'Height', y = 'PredawnWP', color = 'Month', add = "reg.line", conf.int = FALSE,palette = "jco",alpha=0.3,
)+stat_cor(aes(color = Month, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = TRUE, p.accuracy = 0.001, r.accuracy = 0.01, label.x=250) +scale_color_discrete( name = 'Month')+labs(y=expression(italic(ψ)["pd"]*" (MPa)"))+labs(x = "Tree height (cm)")+facet_wrap(~Treatment)
g
### different colors and regression lines for only significant relationships
p_values <- Combined_Data %>% group_by(Month, Treatment) %>% group_modify(~ {
  if (nrow(.x) > 1) {
    model <- lm(PredawnWP ~ Height, data = .x)
    p_value <- summary(model)$coefficients[2, 4]
  } else {
    p_value <- NA
  }
  tibble(p_value = p_value)
}) %>%
  ungroup()
filtered_p_values <- p_values %>% filter(p_value < 0.05)
# Create the base plot
g <- ggscatter(Combined_Data, x = 'Height', y = 'PredawnWP', color = 'Month', conf.int = FALSE, palette = "jco", alpha=0.3) +
  stat_cor(aes(color = Month, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.x = 250) +
  scale_color_discrete(name = 'Month') + facet_wrap(~Treatment)+theme(legend.position = "right")+
  labs(y=expression(italic(ψ)["pd"]*" (MPa)")) + theme(axis.text = element_text(size = 12), axis.title.y = element_text(size = 12))+
  labs(x = "Tree height (cm)")
# Add regression lines only for the significant p-values, with the condition of having either treatment but not both
control_months <- filtered_p_values %>% filter(Treatment == "Control") %>% pull(Month)
irrigated_months <- filtered_p_values %>% filter(Treatment == "Irrigated") %>% pull(Month)

# Plot the regression lines for the Control treatment where p-value < 1
for(month in control_months) {
  g <- g + geom_smooth(data = filter(Combined_Data, Treatment == "Control", Month == month),
                       aes(x = Height, y = PredawnWP, color = Month), method = "lm",linetype = "dotted",alpha=0.6, se = FALSE)
}

# Plot the regression lines for the Irrigated treatment where p-value < 1
for(month in irrigated_months) {
  g <- g + geom_smooth(data = filter(Combined_Data, Treatment == "Irrigated", Month == month),
                       aes(x = Height, y = PredawnWP, color = Month), method = "lm",linetype = "dotted", alpha=0.6, se = FALSE)
}
g
ggplot2::ggsave('size.png', g,width = 8.14, height = 3.65, dpi = 1000)

#blurred line
for (month in control_months) {
  g <- g + geom_smooth(data = filter(Combined_Data, Treatment == "Control", Month == month),
                       aes(x = Height, y = PredawnWP, color = Month), method = "lm", alpha = 0.15, size = 0.5, linetype = "dotted", se = FALSE)
}

for (month in irrigated_months) {
  g <- g + geom_smooth(data = filter(Combined_Data, Treatment == "Irrigated", Month == month),
                       aes(x = Height, y = PredawnWP, color = Month), method = "lm", alpha = 0.15, size = 0.5, linetype = "dotted", se = FALSE)
}
g
## Figure 2

dataP=summarySE(Makera,na.rm=TRUE, measurevar="PredawnWP", groupvars=c("Date"))
dataM=summarySE(Makera,na.rm=TRUE, measurevar="MiddayWP", groupvars=c("Date"))
dataU=summarySE(Makera,na.rm=TRUE, measurevar="Spflow", groupvars=c("Date"))
datak=summarySE(Makera,na.rm=TRUE, measurevar="u", groupvars=c("Date"))
dataPPFD=summarySE(Makera,na.rm=TRUE, measurevar="PPFD", groupvars=c("Date"))
dataWP=summarySE(Makera,na.rm=TRUE, measurevar="DWP", groupvars=c("Date"))

dataSWC=summarySE(Makera,na.rm=TRUE, measurevar="SWC", groupvars=c("Date"))
dataVPD=summarySE(Makera,na.rm=TRUE, measurevar="VPD", groupvars=c("Date"))
data_merged <- merge(dataWP, dataSWC , by = c( "Date"))
data_merged1 <- merge(dataWP, dataSWC , by = c( "Date"))

data_merged1 <- merge(dataP, dataSWC , by = c( "Date"))
data_merged2 <- merge(dataM, dataSWC , by = c( "Date"))
data_merged3 <- merge(dataU, dataSWC , by = c( "Date"))
data_merged4 <- merge(dataU, dataVPD , by = c( "Date"))
data_merged5 <- merge(dataP, dataVPD , by = c( "Date"))
data_merged6 <- merge(dataM, dataVPD , by = c( "Date"))
data_merged7 <- merge(dataWP, dataVPD , by = c( "Date"))
data_merged8 <- merge(dataWP, dataSWC , by = c( "Date"))
data_merged9 <- merge(datak, dataSWC , by = c( "Date"))

## rESIDUALS
DataDP<- merge(data_merged7, data_merged8 , by = c( "Date"))
model <- lm(DWP.x~ SWC, data = DataDP)
DataDP$residuals <- residuals(model)
ggscatter( DataDP, x = 'VPD', y = 'residuals', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right")  +  # Y-axis error bars
  labs(y="ψpd (MPa)")+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
DataMP<- merge(data_merged2, data_merged6 , by = c( "Date"))
model <- lm(MiddayWP.x~ SWC, data = DataMP)
DataMP$residuals <- residuals(model)
ggscatter( DataMP, x = 'VPD', y = 'residuals', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right")  +  # Y-axis error bars
  labs(y="ψpd (MPa)")+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
DataWP<- merge(data_merged1, data_merged5 , by = c( "Date"))
model <- lm(PredawnWP.x~ SWC, data = DataWP)
DataWP$residuals <- residuals(model)
ggscatter( DataWP, x = 'VPD', y = 'residuals', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right")  +  # Y-axis error bars
  labs(y="ψpd (MPa)")+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
SWC1<-ggscatter( data_merged1, x = 'SWC', y = 'PredawnWP', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01,label.y = -0.2)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = PredawnWP - se.x, ymax = PredawnWP+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = SWC - se.y, xmax = SWC + se.y), height = 0.1)+labs(y=expression(italic(ψ)["pd"]*" (MPa)"))+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
SWC1
SWC2<-ggscatter( data_merged2, x = 'SWC', y = 'MiddayWP', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = MiddayWP - se.x, ymax = MiddayWP+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = SWC - se.y, xmax = SWC + se.y), height = 0.1)+labs(y=expression(italic(ψ)["md"]*" (MPa)"))+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
SWC2
SWC3<-ggscatter( data_merged3, x = 'SWC', y = 'Spflow', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = Spflow - se.x, ymax = Spflow+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = SWC - se.y, xmax = SWC + se.y), height = 0.1)+labs(y=expression(italic(ψ)["md"]*" (MPa)"))+ylab(bquote(italic(F)~'('*kg~h^-1*')'))+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
SWC3
SWC4<-ggscatter( data_merged8, x = 'SWC', y = 'DWP', conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = DWP - se.x, ymax = DWP+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = SWC - se.y, xmax = SWC + se.y), height = 0.1)+labs(  y = "Δψ  (MPa)")+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
SWC4
SWC5<-ggscatter( data_merged9, x = 'SWC', y = 'u', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Soil water content ('* g~g^-1*')'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = u- se.x, ymax = u+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = SWC - se.y, xmax = SWC + se.y), height = 0.1)+ylab(bquote('Js ('*~10^-6~m~s^-1*')'))+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
SWC5
ggplot2::ggsave('SWCU.png', SWC5,width = 9.14, height = 3.65, dpi = 1000)

plots <- plot_grid(SWC1, SWC2, SWC4,SWC3, nrow = 1, align = 'H')
plots
ggplot2::ggsave('SWC.png', plots,width = 14.14, height = 3.75, dpi = 1000)

VPD1<-ggscatter( data_merged5, x = 'VPD', y = 'PredawnWP', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Vapor pressure deficit (KPa)'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = PredawnWP - se.x, ymax = PredawnWP+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = VPD - se.y, xmax = VPD + se.y), height = 0.1)+labs(y="ψpd (MPa)")+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
VPD2<-ggscatter( data_merged6, x = 'VPD', y = 'MiddayWP', add = "reg.line", conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Vapor pressure deficit (KPa)'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = MiddayWP - se.x, ymax = MiddayWP+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = VPD - se.y, xmax = VPD + se.y), height = 0.1)+labs(y="ψmd (MPa)")+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
VPD3<-ggscatter( data_merged4, x = 'VPD', y = 'Spflow', conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Vapor pressure deficit (KPa)'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = Spflow - se.x, ymax = Spflow+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = VPD - se.y, xmax = VPD+ se.y), height = 0.1)+ylab(bquote('F ('*kg~h^-1*')'))+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
VPD3
VPD4<-ggscatter( data_merged7, x = 'VPD', y = 'DWP', conf.int = FALSE,palette = "jco") + stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.01)+xlab(bquote('Vapor pressure deficit (KPa)'))  +theme(legend.position = "right") +geom_errorbar(aes(ymin = DWP - se.x, ymax = DWP+ se.x), width = 0.1) +  # Y-axis error bars
  geom_errorbarh(aes(xmin = VPD - se.y, xmax = VPD+ se.y), height = 0.1)+labs(  y = "Δψ  (MPa)")+theme(axis.text = element_text(size = 14))+theme(axis.title.y = element_text(size = 14))+theme(axis.title.x = element_text(size = 14))
VPD4
plots2 <- plot_grid(VPD1, VPD2, VPD4, VPD3, nrow = 1, align = 'H')
plots2
ggplot2::ggsave('VPD11.png', plots2,width = 14.14, height = 3.65, dpi = 1000)

## Figure 4 & 5
### Max and min values
umax <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(u == max(u, na.rm = TRUE)) %>% select(Individu22, Treatment, max_u = u) %>%  # Rename column for clarity
  ungroup()
umin <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(u == min(u, na.rm = TRUE)) %>% select(Individu22, Treatment, min_u = u) %>%  # Rename column for clarity
  ungroup()
combined_u <- full_join(umax, umin, by = c("Individu22", "Treatment"))
Fmax <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(Spflow == max(Spflow, na.rm = TRUE)) %>% select(Individu22, Treatment, max_Spflow = Spflow) %>%  # Rename column for clarity
  ungroup()
Fmin <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(Spflow == min(Spflow, na.rm = TRUE)) %>% select(Individu22, Treatment, min_Spflow = Spflow) %>%  # Rename column for clarity
  ungroup()
combined_F <- full_join(Fmax, Fmin, by = c("Individu22", "Treatment"))

pmax <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(PredawnWP == max(PredawnWP, na.rm = TRUE)) %>% select(Individu22, Treatment, max_PredawnWP = PredawnWP) %>%  # Rename column for clarity
  ungroup()
pmin <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(PredawnWP == min(PredawnWP, na.rm = TRUE)) %>% select(Individu22, Treatment, min_PredawnWP = PredawnWP) %>%  # Rename column for clarity
  ungroup()
combined_P <- full_join(pmax, pmin, by = c("Individu22", "Treatment"))
mmax <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(MiddayWP == max(MiddayWP, na.rm = TRUE)) %>% select(Individu22, Treatment, max_MiddayWP = MiddayWP) %>%  # Rename column for clarity
  ungroup()
mmin <- Makera1 %>% group_by(Individu22, Treatment) %>% filter(MiddayWP == min(MiddayWP, na.rm = TRUE)) %>% select(Individu22, Treatment, min_MiddayWP = MiddayWP) %>%  # Rename column for clarity
  ungroup()
combined_M <- full_join(mmax, mmin, by = c("Individu22", "Treatment"))
combined_all2 <- combined_P %>%
  full_join(combined_F, by = c("Individu22", "Treatment")) %>%
  full_join(combined_u, by = c("Individu22", "Treatment")) %>%
  full_join(combined_M, by = c("Individu22", "Treatment"))
write_xlsx(combined_all2,"C:\\Gothenburg\\Data\\Hydraulic\\Publication\\Decrease2.xlsx")
Decrease <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Decrease.xlsx", 
                       sheet = "Sheet2")
change_F<-ggscatter(Decrease, x = 'Height', y = 'Percent_U',  add = "reg.line", palette = "jco",conf.int = TRUE,   color = "darkgreen",add.params = list(color = "black"))+stat_cor(aes( label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.x=250) +labs(y = expression("Change in"~italic(F)~"(%)"))+labs(x = "Tree height (cm)")+facet_wrap(~Treatment)+theme(legend.position = "right")+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+theme(axis.title.x = element_text(size = 12))
change_F
data2=summarySE(Decrease,na.rm=TRUE, measurevar="Percent_U", groupvars=c("Species","Treatment"))
data3=summarySE(Decrease,na.rm=TRUE, measurevar="Height", groupvars=c("Species","Treatment"))
data_decrease <- merge(data2, data3 , by = c(  "Species","Treatment"))
write_xlsx(data_decrease,"C:\\Gothenburg\\Data\\Hydraulic\\Publication\\MEAN_DECREASE.xlsx")

change_F<-ggscatter(data_decrease, x = 'Height', y = 'Percent_U',  add = "reg.line", palette = "jco",conf.int = TRUE,   color = "darkgreen",add.params = list(color = "black"))+stat_cor(aes( label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.x=250) +labs(y = expression("Change in"~italic(F)~"(%)"))+labs(x = "Tree height (cm)")+facet_wrap(~Treatment)+theme(legend.position = "right")+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+theme(axis.title.x = element_text(size = 12))
change_F

Decrease21 <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Decrease21.xlsx", sheet = "Sheet1")
Decrease22 <- Decrease21  # same source sheet — update if a separate file/sheet is intended

data2=summarySE(Decrease21,na.rm=TRUE, measurevar="Percent_U", groupvars=c("Species","Treatment"))
data3=summarySE(Decrease21,na.rm=TRUE, measurevar="RHeight", groupvars=c("Species","Treatment"))
data_decrease <- merge(data2, data3 , by = c(  "Species","Treatment"))

changeF<-ggscatter(Decrease22, x = 'Percent_U', y = 'RHeight',  add = "reg.line", palette = "jco",conf.int = TRUE,   add.params = list(color = "black"))+stat_cor(aes( label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01) +labs( x = "Change in the total sapflow (%)")+labs(y = "RGR Height (%)")+facet_wrap(~Treatment)+theme(legend.position = "right")+theme(axis.text = element_text(size = 12))+theme(axis.title.y = element_text(size = 12))+theme(axis.title.x = element_text(size = 12))
changeF
ggplot2::ggsave('Change12.png', changeF,width = 8.14, height = 3.65, dpi = 1000)

## FIGURE 5
data2=summarySE(Makera,na.rm=TRUE, measurevar="Spflow", groupvars=c("Individu", "Species","Treatment"))
data1=summarySE(Makera,na.rm=TRUE, measurevar="RHeight", groupvars=c("Individu", "Species", "Treatment"))
data3=summarySE(Makera,na.rm=TRUE, measurevar="Spflow", groupvars=c("Individu", "Species", "Treatment"))
data_FH <- merge(data2, data1 , by = c( "Individu", "Species","Treatment"))
data_UH <- merge(data3, data1 , by = c( "Individu", "Species"))
my_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
               "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", 
               "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")
g1<-ggscatter(data_FH, x = 'Spflow', y = 'RHeight',color = 'Species',  add = "reg.line", 
              conf.int = FALSE, palette = my_colors) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.x = 0, label.y = 30) +
  theme(legend.position = "right") +labs(y = "RGR height (%/Y)") +theme(axis.text = element_text(size = 12), axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12)) +xlab(bquote('F ('*kg~m^-2~h^-1*')'))
g1
g1 <- ggscatter( data_FH, x = "Spflow",y = "RHeight", add = "reg.line", conf.int = TRUE,color = "darkgreen",add.params = list(color = "black")) +stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
 show.legend = FALSE,  p.accuracy = 0.001, r.accuracy = 0.001,  
 label.x = 0, label.y = 33) +facet_wrap(~Treatment) +labs( y = "RGR height (%/Y)", x = bquote(italic(F)~"("*kg~h^-1*")")) +theme( legend.position = "right",strip.text = element_text(size = 12, face = "bold"), strip.background = element_rect(fill = "grey90", color = NA),axis.text = element_text(size = 12), axis.title.y = element_text(size = 12),axis.title.x = element_blank(),  axis.text.x = element_blank())                                                                       
g1
g1 = ggscatter(data_FH,x = "Spflow", y = "RHeight", add = "reg.line", conf.int = TRUE,
  add.params = list(color = "black")) +stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    show.legend = FALSE,
    p.accuracy = 0.001,
    r.accuracy = 0.001,
    label.x = 0,label.y = 30) +facet_wrap(~Treatment) +labs( y = "RGR height (%/Y)", x = bquote(italic(F) ~ "(" * kg ~ h^-1 * ")")
  ) + theme(legend.position = "right", strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey85", color = NA), axis.text = element_text(size = 12),axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12)
  )
g1
changeF<-ggscatter(Decrease22, x = 'Percent_U', y = 'RHeight',  add = "reg.line", palette = "jco",conf.int = TRUE,   add.params = list(color = "black"))+stat_cor(aes( label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=30, show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01) +labs(x = bquote("% Change in " * italic(F)))+labs(y = "RGR Height (%/Year)")+facet_wrap(~Treatment)+theme(legend.position = "right")+ theme(
  legend.position = "right",axis.text = element_text(size = 12),axis.title.y = element_text(size = 12),axis.title.x = element_text(size = 12),strip.text = element_blank())  # Removes facet titles
changeF
plots <- plot_grid(g1, changeF, nrow = 2, align = 'V',  rel_heights = c(1.25, 1))
plots
ggplot2::ggsave("Rheight.png", plots,width = 6.14, height = 5.15, dpi = 1000)
## CHANGES IN TOTAL SAPFLOW WITH RGR
data2=summarySE(Decrease2,na.rm=TRUE, measurevar="Percent_U", groupvars=c("Species","Treatment"))
data3=summarySE(Decrease2,na.rm=TRUE, measurevar="RHeight", groupvars=c("Species","Treatment"))
data_decrease <- merge(data2, data3 , by = c(  "Species","Treatment"))

changeF<-ggscatter(Decrease2, x = 'Percent_U', y = 'RHeight',  add = "reg.line", palette = "jco",conf.int = TRUE,   color = "darkgreen",add.params = list(color = "black"))+stat_cor(aes( label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=35, show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01) +labs(x = expression("Change in"~italic(F)~"(%)"))+labs(y = "RGR Height (%)")+facet_wrap(~Treatment)+theme(legend.position = "right")+ theme(
  legend.position = "right",axis.text = element_text(size = 12),axis.title.y = element_text(size = 12),axis.title.x = element_text(size = 12),strip.text = element_blank())  # Removes facet titles
changeF
plots <- plot_grid(g1, changeF, nrow = 2, align = 'V',  rel_heights = c(1.30, 1.1))
plots
ggplot2::ggsave('Fig55.png', plots,width = 8.14, height = 3.95, dpi = 1000)

## Fig. 7 (single-panel version) -- J. Uddling comment: "As treatment did not
## affect the relationships, consider showing data from both control and
## irrigation in the same panel (with different symbol shapes/colours), with
## one overall regression (and p and r2 value). It's not necessary, but it is
## justified by the lack of treatment effect according to the statistical
## test." Manuscript text: Spflow x Treatment interaction p = 0.53 (panel a),
## Percent_U x Treatment interaction p = 0.98 (panel b) -- neither significant.
g1_pooled <- ggscatter(data_FH, x = "Spflow", y = "RHeight", color = "Treatment", shape = "Treatment",
                       add = "none", palette = c("Control" = "#D55E00", "Irrigated" = "#0072B2")) +
  geom_smooth(aes(x = Spflow, y = RHeight), inherit.aes = FALSE, method = "lm", se = TRUE, color = "black") +
  stat_cor(aes(x = Spflow, y = RHeight, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE, show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.001, label.x = 0, label.y = 33) +
  labs(y = "RGR height (%/Y)", x = bquote(italic(F)~"("*kg~h^-1*")"), color = "Treatment", shape = "Treatment") +
  theme(legend.position = "right", axis.text = element_text(size = 12), axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))
g1_pooled

changeF_pooled <- ggscatter(Decrease2, x = 'Percent_U', y = 'RHeight', color = "Treatment", shape = "Treatment",
                            add = "none", palette = c("Control" = "#D55E00", "Irrigated" = "#0072B2")) +
  geom_smooth(aes(x = Percent_U, y = RHeight), inherit.aes = FALSE, method = "lm", se = TRUE, color = "black") +
  stat_cor(aes(x = Percent_U, y = RHeight, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE, label.y = 35, show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01) +
  labs(x = expression("Change in"~italic(F)~"(%)"), y = "RGR Height (%)", color = "Treatment", shape = "Treatment") +
  theme(legend.position = "right", axis.text = element_text(size = 12), axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))
changeF_pooled

plots_pooled <- plot_grid(g1_pooled, changeF_pooled, nrow = 2, align = 'V', rel_heights = c(1.30, 1.1))
plots_pooled
ggplot2::ggsave('Fig55_pooled.png', plots_pooled, width = 8.14, height = 3.95, dpi = 1000)
## Graph on environmental data
library(readxl)
environment_daily <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/environment.xlsx", 
                                sheet = "Sheet1", col_types = c("date", 
                                                                "date", "numeric", "numeric", "numeric"))
# Add a Date column for grouping
environment_daily <- environment_daily %>%
  mutate(Date = as.Date(Date1))
environment <- environment_daily %>%
  group_by(Date) %>%
  summarise(
    mean_VPD = mean(VPD180cm, na.rm = TRUE),
    mean_RH = mean(RH180cm, na.rm = TRUE) * 100
  )
library(writexl)
write_xlsx(environment_daily, "C:/Gothenburg/Data/Hydraulic/Publication/environment_daily.xlsx")
max_vpd_data <- max(environment$mean_VPD, na.rm = TRUE)
Fig1<-ggplot(environment, aes(x = Date)) +
  geom_line(aes(y = mean_VPD), color = "steelblue", size = 1) +
  geom_point(aes(y = mean_VPD), color = "steelblue", size = 2) +
  geom_line(aes(y = (mean_RH / 100) * max_vpd_data), color = "darkred", size = 1, linetype = "dashed") +
  geom_point(aes(y = (mean_RH / 100) * max_vpd_data), color = "darkred", size = 2) +
  scale_y_continuous( name = "VPD at 180cm (kPa)",limits = c(0, max_vpd_data), sec.axis = sec_axis( trans = ~ . / max_vpd_data * 100,
      name = "Relative Humidity at 180cm (%)") ) + labs(
       x = "Month") +
  theme_minimal() + cleanup +
  theme(
    axis.title.y = element_text(color = "steelblue", face = "bold"),
    axis.title.y.right = element_text(color = "darkred", face = "bold"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks = element_line(color = "black", size = 0.5)
  )
Fig1
### SWC
Data_environment <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Data_environment.xlsx", sheet = "Sheet3")
Data_environment$Date <- as.Date(Data_environment$Date)
ggplot(Data_environment, aes(x = date_only)) +
  geom_line(aes(y = Control), color = "steelblue", size = 1) +
  geom_point(aes(y = Control), color = "steelblue", size = 1) +
  geom_line(aes(y = Irrigated), color = "darkred", size = 1) +
  geom_point(aes(y = (Irrigated), color = "darkred", size = 1)) +
               labs(title = "swc",
                    x = "Month") +
               theme_minimal() + cleanup
### MEAN VALUES 
# VPD
vpd_summary <- summarySE(environment_daily, measurevar = "VPD180cm", groupvars = c("Date"), na.rm = TRUE)

# PPFD
ppfd_summary <- summarySE(environment_daily, measurevar = "PPFD", groupvars = c("Date"), na.rm = TRUE)

# RH
rh_summary <- summarySE(environment_daily, measurevar = "RH180cm", groupvars = c("Date"), na.rm = TRUE)
## combine them 
vpd_summary  <- vpd_summary  %>% select(Date, mean_VPD  = VPD180cm)
ppfd_summary <- ppfd_summary %>% select(Date, mean_PPFD = PPFD)
rh_summary   <- rh_summary   %>% select(Date, mean_RH   = RH180cm)
library(purrr)
# Join by date
all_summary <- reduce(list(vpd_summary, ppfd_summary, rh_summary), full_join, by = "Date")

## Graphs
Fig1<-ggplot(all_summary, aes(x = Date, y = mean_VPD)) + geom_line(color = "darkred", size = 1) +  geom_point(color = "darkred")  +  labs(   x = "Month",
    y = "VPD at 180cm (kPa)") + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
Fig1
Fig2<-ggplot(all_summary, aes(x = Date, y = mean_RH)) + geom_line(color = "darkred", size = 1) +  geom_point(color = "darkred")  +  labs(   x = "Month",
 y = "Relative Humidity at 180cm (%)") + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
Fig2
Fig3<-ggplot(all_summary, aes(x = Date, y = mean_PPFD)) +geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue")  +labs(  x = "Month", y = "Mean PPFD (µmol m⁻² s⁻¹)") + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
Fig3

#### three graphs in one panel

# Ensure Date format
png("my_plot.png", width = 9.14, height = 5.95, units = "in", res = 300)
# Set margins and text size (cex = 1.2 ≈ 12 pt)
par(mar = c(5, 4, 0.5, 8) + 0.5, cex.axis = 1.2, cex.lab = 1.2, cex = 1.2)
all_summary$Date <- as.Date(all_summary$Date)

# --- Plot 1: VPD
plot(all_summary$Date, all_summary$mean_VPD,
     type = "l", col = "black", lwd = 1.5,
     ylim = c(0, 2.5), xlab = "Date", ylab = "VPD (kPa)",
     xaxt = "n")

axis.Date(1, at = seq(min(all_summary$Date), max(all_summary$Date), by = "1 month"), format = "%Y/%m/%d")

# --- Plot 2: PPFD on right axis
par(new = TRUE)
plot(all_summary$Date, all_summary$mean_PPFD,
     type = "l", col = "skyblue", lwd = 1.5,
     axes = FALSE, xlab = "", ylab = "", ylim = c(200, 900))
axis(side = 4, col.axis = "skyblue", las = 1)
mtext("PPFD (µmol m⁻² s⁻¹)", side = 4, line = 2.5, cex = 1.2, col = "skyblue")

# --- Plot 3: RH — on *rescaled* axis, with offset
# Rescale RH to match the visible range (0 to 500) for plotting purposes
scaled_RH <- scales::rescale(all_summary$mean_RH, to = c(0, 500))

par(new = TRUE)
plot(all_summary$Date, scaled_RH,
     type = "l", col = "#009E73", lwd = 1.5,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 500))

# Add RH axis on right side with offset (line = 5)
axis(side = 4, at = scales::rescale(seq(20, 100, 20), to = c(0, 500)),
     labels = seq(20, 100, 20), line = 5, col.axis = "#009E73", col = "#009E73", las = 1)
mtext("RH (%)", side = 4.5, line = 7, cex = 1.2, col = "#009E73")

# --- Add legend
legend("topleft",
       legend = c("VPD", "PPFD", "RH"),
       col = c("black", "skyblue", "#009E73"),
       lwd = 2, bty = "n")


dev.off()  # close PNG device

### SWC

# Colors
cols <- c("Irrigated" = "blue", "Control" = "red")

# Set margins
par(mar = c(5, 5, 2, 2), cex.axis = 1.2, cex.lab = 1.4)

# Y limits
ylim_swc <- range(Data_environment$all2, na.rm = TRUE)

# Empty plot
plot(range(Data_environment$Date), ylim_swc,
     type = "n",
     xlab = "Date",
     ylab = "Mean SWC (%)",
     xaxt = "n")

# Monthly x-axis
axis.Date(1,
          at = seq(min(Data_environment$Date),
                   max(Data_environment$Date),
                   by = "1 month"),
          format = "%d/%m/%Y",
          las = 1)

# Draw lines
for(t in unique(Data_environment$Treatment)){
  d <- subset(Data_environment, Treatment == t)
  lines(d$Date, d$all2, col = cols[t], lwd = 2.5)
}

# Legend
legend("topright",
       legend = c("Irrigated", "Control"),
       col = cols,
       lwd = 2.5,
       bty = "n")


## Decrease in sapflow, predawn, midday.... in different PFT
Decrease2 <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Decrease21.xlsx", sheet = "Sheet6")
ggplot(Decrease2, aes(x = data, y = Value, fill = PFT)) +geom_boxplot() +labs( x = "Plant Functional Type (PFT)", y = "Percent Midday Decrease",
    title = "Percent Midday by PFT Group" ) + scale_fill_manual(values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00")) +
  theme_minimal() +theme(text = element_text(size = 12), axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),axis.text = element_text(color = "black"),  legend.position = "none"
  )+cleanup
##Figure 3
Combined_Data <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Combined_Data.xlsx", sheet = "Sheet2")
Combined_Data$Month <- factor(Combined_Data$Month, levels = c("Early Jun", "Mid Jun", "Early Jul", "Mid Jul", "Early Aug", "Mid Aug", "Early Sep"))

# Specify the combinations you want labels for
selected_labels <- tibble::tibble(
  Month = c("Mid Jun", "Mid Jul", "Mid Aug", "Early Sep"),
  Treatment = c("Irrigated", "Control", "Control", "Control")
)
# Height x PredawnWP regression p-value per Month x Treatment campaign --
# drives which campaigns get a regression line drawn in Fig. 4 (manuscript
# numbering; p < 0.05 only). `sig_data` was referenced below without ever
# being defined -- added here.
p_values_fig4 <- Combined_Data %>% group_by(Month, Treatment) %>% group_modify(~ {
  if (nrow(.x) > 1) {
    model <- lm(PredawnWP ~ Height, data = .x)
    p_value <- summary(model)$coefficients[2, 4]
  } else {
    p_value <- NA
  }
  tibble(p_value = p_value)
}) %>%
  ungroup()
filtered_p_values_fig4 <- p_values_fig4 %>% filter(p_value < 0.05)
sig_data <- Combined_Data %>% semi_join(filtered_p_values_fig4, by = c("Month", "Treatment"))
sig_data_filtered <- sig_data %>%
  semi_join(selected_labels, by = c("Month", "Treatment"))
g <- ggplot(Combined_Data, aes(x = Height, y = PredawnWP, color = Month)) +
  geom_point(alpha = 0.3) + facet_wrap(~Treatment) +
  scale_color_discrete(name = 'Month') +
  labs(x = "Tree height (cm)",y = expression(italic(ψ)["pd"]*" (MPa)")
  ) + theme_minimal(base_size = 12) +cleanup + scale_x_continuous(breaks = seq(200, 800, by = 200)) +
  scale_y_continuous(breaks = seq(-3, 0, by = 0.5))+
  theme(legend.position = "right") + geom_smooth( data = sig_data_filtered, aes(x = Height, y = PredawnWP, color = Month), method = "lm", se = FALSE, linetype = "dotted", alpha = 0.6,
    inherit.aes = FALSE) +ggpmisc::stat_poly_eq( data = sig_data_filtered, aes(x = Height, y = PredawnWP, label =  paste( ..rr.label.., ..p.value.label.., sep = "~~~"), color = Month),
    formula = y ~ x,  parse = TRUE, label.x = 250, inherit.aes = FALSE
  )
g <- ggplot(Combined_Data, aes(x = Height, y = PredawnWP, color = Month)) +
  geom_point(alpha = 0.3) +facet_wrap(~Treatment) +scale_color_discrete(name = 'Month') +
  labs( x = "Tree height (cm)",y = expression(italic(ψ)["pd"]*" (MPa)")) + theme_minimal(base_size = 12) +
  cleanup + scale_x_continuous(breaks = seq(200, 800, by = 200)) +
  scale_y_continuous(breaks = seq(-3, 0, by = 0.5)) +
  theme(legend.position = "right",legend.title = element_text(size = 12),legend.text = element_text(size = 12), axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),strip.text = element_text(size = 12),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm") ) +geom_smooth( data = sig_data_filtered,aes(x = Height, y = PredawnWP, color = Month),
    method = "lm", se = FALSE, linetype = "solid", alpha = 0.6,
    inherit.aes = FALSE) +ggpmisc::stat_poly_eq(data = sig_data_filtered,
    aes(  x = Height, y = PredawnWP,label = paste(..rr.label.., ..p.value.label.., sep = "~~~"),
      color = Month ), formula = y ~ x,  parse = TRUE,label.x = 600,label.y = c(0.1, 0.1, 0.2, 0.3), inherit.aes = FALSE)
g
ggplot2::ggsave('PredawnH.png', g,width = 8.14, height = 3.65, dpi = 1000)

## Fig. 4 sensitivity check -- J. Uddling comment: "Just for confidence: does
## this pattern hold if excluding Fth where we are a bit unsure about the
## accuracy of WP measurements?" (latex exudation may bias Scholander
## pressure-chamber readings for this species). Re-runs the same per-campaign
## regressions with Fth dropped and compares which Month x Treatment
## campaigns stay significant.
Combined_Data_noFth <- Combined_Data %>% filter(Species != "Fth")
p_values_fig4_noFth <- Combined_Data_noFth %>% group_by(Month, Treatment) %>% group_modify(~ {
  if (nrow(.x) > 1) {
    model <- lm(PredawnWP ~ Height, data = .x)
    p_value <- summary(model)$coefficients[2, 4]
  } else {
    p_value <- NA
  }
  tibble(p_value = p_value)
}) %>%
  ungroup()
filtered_p_values_fig4_noFth <- p_values_fig4_noFth %>% filter(p_value < 0.05)

# Side-by-side comparison of which campaigns are significant with vs without Fth
p_values_fig4 %>%
  rename(p_with_Fth = p_value) %>%
  full_join(p_values_fig4_noFth %>% rename(p_without_Fth = p_value), by = c("Month", "Treatment")) %>%
  mutate(sig_with_Fth = p_with_Fth < 0.05, sig_without_Fth = p_without_Fth < 0.05)

sig_data_noFth <- Combined_Data_noFth %>% semi_join(filtered_p_values_fig4_noFth, by = c("Month", "Treatment"))
g_noFth <- ggplot(Combined_Data_noFth, aes(x = Height, y = PredawnWP, color = Month)) +
  geom_point(alpha = 0.3) + facet_wrap(~Treatment) +
  scale_color_discrete(name = 'Month') +
  labs(x = "Tree height (cm)", y = expression(italic(ψ)["pd"]*" (MPa)")) +
  theme_minimal(base_size = 12) + cleanup + scale_x_continuous(breaks = seq(200, 800, by = 200)) +
  scale_y_continuous(breaks = seq(-3, 0, by = 0.5)) +
  theme(legend.position = "right") +
  geom_smooth(data = sig_data_noFth, aes(x = Height, y = PredawnWP, color = Month), method = "lm", se = FALSE, linetype = "solid", alpha = 0.6, inherit.aes = FALSE)
g_noFth
ggplot2::ggsave('PredawnH_noFth.png', g_noFth, width = 8.14, height = 3.65, dpi = 1000)

### changes with tree height
Decrease2 <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Decrease21.xlsx", sheet = "Sheet1")
GG1 <- ggscatter(Decrease2, x = 'Height', y = 'Percent_WP', color = "PFT", conf.int = TRUE, add = "none",  palette = "jco")  +scale_color_manual(values = c("E" = "#117733","SD" = "#E69F00"))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),      show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01,label.y=-40) +
  scale_color_discrete(name = 'Species') + labs(y = "Changes in ψpd (%)", x = "Tree height (cm)") +
  theme(legend.position = "right", axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) + facet_wrap(~Treatment)
GG1 <- ggscatter(Decrease2, x = 'Height', y = 'Percent_WP', color = "PFT", conf.int = FALSE, add = "none", palette = "jco") +  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.y = -50) +scale_color_manual(values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00"), name = "PFT") +labs(y = expression("Changes in " * italic(ψ)[pd] * " (%)")) + theme(legend.position = "none", 
 axis.text = element_text(size = 12), axis.title.y = element_text(size = 12)) + labs(x = "Tree height (cm)")+facet_wrap(~Treatment)
GG1
GG1 <- GG1 + geom_smooth(data = subset(Decrease2, Treatment == "Control"),  aes(x = Height, y = Percent_WP), method = "lm", color = "black", se = FALSE)
GG1
ggplot2::ggsave('ChangePH.png', GG1,width = 8.14, height = 3.65, dpi = 1000)

##Changes in the sapflow

GG2 <- ggscatter(Decrease, x = 'Height', y = 'Percent_F', color = "PFT", conf.int = TRUE, add = "none",  palette = "jco")  +scale_color_manual(values = c("E" = "#117733","SD" = "#E69F00"))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),      show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01,label.y=-40) +
  scale_color_discrete(name = 'Species') + labs(y = "Changes in the sap flow (%)", x = "Tree height (cm)") +
  theme(legend.position = "right", axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) + facet_wrap(~Treatment)
GG2
GG2 <- ggscatter(Decrease, x = 'Height', y = 'Percent_U', color = "PFT", conf.int = FALSE, add = "none", palette = "jco") +  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.y = -50) +scale_color_manual(values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00"), name = "PFT") +labs(y = expression("Changes in the sapflow (%)")) + theme(legend.position = "none", 
    axis.text = element_text(size = 12), axis.title.y = element_text(size = 12)) + labs(x = "Tree height (cm)")+facet_wrap(~Treatment)+geom_smooth(data = Decrease,  aes(x = Height, y = Percent_U), method = "lm", color = "black", se = FALSE)
GG2

GG2 <- ggscatter(Decrease, x = 'Height', y = 'Percent_U', 
                 color = "PFT", conf.int = FALSE, add = "none", palette = "jco") +  
  # Correlation stats
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),  
           show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.y = -50) +
  # Custom colors
  scale_color_manual(values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00"), 
                     name = "PFT") +
  # Dashed regression lines by PFT
  geom_smooth(aes(linetype = PFT, color = PFT), method = "lm", se = FALSE) +
  # Solid overall regression line
  geom_smooth(data = Decrease, aes(x = Height, y = Percent_U), 
              method = "lm", se = FALSE, color = "black", linetype = "solid") +
  labs(y = expression("Changes in the sapflow (%)"), x = "Tree height (cm)") +
  theme(legend.position = "right",   axis.text = element_text(size = 12),   axis.title.y = element_text(size = 12)) + 
  facet_wrap(~Treatment)
GG2

GG2 <- ggscatter(Decrease, x = 'Height', y = 'Percent_U', color = "PFT", conf.int = FALSE, add = "none", palette = "jco"
) +   stat_cor (data = subset(Decrease, PFT == "Evergreen"),
aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
method = "pearson", 
show.legend = FALSE, 
p.accuracy = 0.001, 
r.accuracy = 0.01, 
label.y = -50, color="#117733"
) + stat_cor(data = subset(Decrease, PFT == "Semi-deciduous"),
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  method = "pearson", 
  p.accuracy = 0.001, 
  r.accuracy = 0.01, 
  label.y = -53,color="#E69F00",
  show.legend = FALSE
)+ scale_color_manual(
    values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00"), 
    name = "PFT"
  ) +geom_smooth(aes(linetype = PFT, color = PFT), method = "lm",  se = FALSE
  ) +labs(y = expression("Changes in the sapflow (%)"),  x = "Tree height (cm)") + theme(legend.position = "right", axis.text = element_text(size = 12),    axis.title.y = element_text(size = 12)
  ) +  facet_wrap(~Treatment)
GG2
ggplot2::ggsave('ChangeF.png', GG2,width = 8.14, height = 3.65, dpi = 1000)

## Fig. 6 (single-panel version) -- J. Uddling comment: "I think p values and
## r2 values are for data across water treatments, right? Since these did not
## differ, according to the text. Then I suggest that you show just one plot,
## with data from the two different treatments having different
## symbols/colours, and one regression line (and p and r2 values) across all
## data." Justified because the manuscript text states the Height x
## sap-flow-change relationship "did not differ between water treatments or
## leaf habits, as no significant interactions were detected (all p > 0.35)".
GG2_pooled <- ggscatter(Decrease, x = 'Height', y = 'Percent_U', color = "Treatment", shape = "Treatment",
                        add = "none", palette = c("Control" = "#D55E00", "Irrigated" = "#0072B2")) +
  geom_smooth(aes(x = Height, y = Percent_U), inherit.aes = FALSE, method = "lm", se = TRUE, color = "black") +
  stat_cor(aes(x = Height, y = Percent_U, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE, show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01, label.x = 250, label.y = -50) +
  labs(y = expression("Changes in the sapflow (%)"), x = "Tree height (cm)", color = "Treatment", shape = "Treatment") +
  theme(legend.position = "right", axis.text = element_text(size = 12), axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))
GG2_pooled
ggplot2::ggsave('ChangeF_pooled.png', GG2_pooled, width = 8.14, height = 3.65, dpi = 1000)
###
dataHeight=summarySE(Decrease2,na.rm=TRUE, measurevar="Height", groupvars=c("Species","Treatment","PFT"))
dataU=summarySE(Decrease2,na.rm=TRUE, measurevar="Percent_U", groupvars=c("Species","Treatment","PFT"))
data_merged <- merge(dataHeight, dataU, by = c("Species", "Treatment","PFT"))
Height <- ggscatter(data_merged, x = 'Height', y = 'Percent_U',color = "PFT",  add = "reg.line",conf.int = FALSE,
                    palette = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00")) +stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01) +geom_errorbar(aes(ymin = Percent_U - se.y, ymax = Percent_U + se.y), width = 0.1) +
  geom_errorbarh(aes(xmin = Height - se.x, xmax = Height + se.x), height = 0.1) +
  facet_wrap(~ Treatment) + xlab("Tree Height (cm)") + ylab(expression(Delta~italic(U)~"[% change in sap flux]")) +
  theme_minimal(base_size = 14) + theme(legend.position = "right", axis.text = element_text(size = 14), axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
Height
## statistical tests RGR AND SAPFLOW
## NOTE: `Growth` is never created in this script -- every lmer() call below
## (through line ~890) reads from it but nothing assigns it. These are the
## models behind the Fig. 7 / Results-section stats (e.g. F1,48.4=11.27,
## p=0.0015 for height RGR ~ max sap flow), so they cannot be reproduced or
## checked until the individual-tree-level dataset with RHeight, Sapflow,
## Rdiameter, RHeight1, Percent_U1 and PFT columns (one row per tree) is
## loaded into `Growth` here, e.g.:
##   Growth <- read_excel("<path to the per-individual growth/sapflow file>", sheet = "...")
## Left as-is rather than guessing a source file, since silently pointing
## this at the wrong table would change the manuscript's reported statistics
## without anyone noticing.
m1 <- lmer(RHeight ~ Sapflow + (1 | Species), data = Growth)
summary(m1)
# This model considers 1️⃣ Response variable

#RHeight: Relative growth rate in height (your dependent variable).

#2️⃣ Fixed effect

#Spflow: Sap flow rate is treated as a fixed effect — you are testing whether changes in sap flow are associated with changes in RHeight.
#The coefficient for Spflow will tell you the average slope of the relationship across all species.

#3️⃣ Random effect
#(1 | Species): This means you are including random intercepts for Species.
#It allows each species to have its own baseline RHeight value when Spflow = 0.
#The slope of Spflow is assumed to be the same for all species in this model.
#The random intercept accounts for the fact that observations from the same species are not independent and may have consistently higher or lower RHeight than others.

m2 <- lmer(RHeight ~ Sapflow + (Sapflow | Species), data = Growth)
summary(m2)
#2️⃣ Fixed effect

# Spflow: Sap flow rate is the fixed effect — the fixed‐effect coefficient gives the average slope of the Spflow–RHeight relationship across all species.

#3️⃣ Random effects
#(Spflow | Species) specifies that both intercepts and slopes can vary among species.
# Random intercepts: Each species can have a different baseline RHeight when Spflow = 0.
# Random slopes: Each species can have a different strength (and potentially direction) of the relationship between Spflow and RHeight.
# The correlation reported in the output (Corr = -0.32) tells you how the intercepts and slopes covary among species — here, species with higher baseline RHeight tend to have slightly lower slopes for Spflow, on average.

anova(m1, m2)

#Biological meaning compared to m1
#m1 assumes all species share the same slope for Spflow — i.e., sap flow has the same proportional effect on height growth for every species.
#m2 allows each species to have its own slope — i.e., some species may show a strong positive association, others a weak or even negative one.
#Your likelihood ratio test showed m2 fits significantly better (p = 0.002), meaning there is real evidence that species differ in their Spflow–RHeight relationship.

m1 <- lmer(Rdiameter ~ Sapflow + (1 | Species), data = Growth)
summary(m1)
m2 <- lmer(Rdiameter ~ Sapflow + (Sapflow | Species), data = Growth)
summary(m2)

m1 <- lmer(RHeight1 ~ Percent_U1 + (1 | Species), data = Growth)
summary(m1)
m2 <- lmer(RHeight1 ~ Percent_U1 + (Percent_U | Species), data = Growth)
summary(m2)

m1 <- lmer(RHeight ~ Sapflow + (1 | Species), data = Growth)
summary(m1)
m2 <- lmer(RHeight ~ Sapflow  + (Sapflow | Species), data = Growth)
summary(m2)
## PFT

m1 <- lmer(RHeight1 ~ Percent_U1 * PFT + (1 | Species), data = Growth)
summary(m1)
m2 <- lmer(RHeight1 ~ Percent_U1 * PFT + (Percent_U1 | Species), data = Growth)
summary(m2)

m1 <- lmer(RHeight ~ Sapflow* PFT+ (1 | Species), data = Growth)
summary(m1)
m2 <- lmer(RHeight ~ Sapflow*PFT  + (Sapflow | Species), data = Growth)
summary(m2)
## Change in predawn with tree height 
Decrease2 <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Decrease2.xlsx",  sheet = "Sheet1")
data2 = summarySE(Decrease21, na.rm = TRUE, measurevar = "Height", groupvars = c("Species", "Treatment","PFT"))
data1 = summarySE(Decrease21, na.rm = TRUE, measurevar = "Percent_WP", groupvars = c("Species", "Treatment","PFT"))
merged_data <- merge(data1, data2, by = c("Species", "Treatment", "PFT"))
GG1 <- ggscatter(merged_data, x = 'Height', y = 'Percent_WP',color = "PFT", conf.int = FALSE, add = "none",  palette = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00")) +stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
show.legend = FALSE, p.accuracy = 0.001, r.accuracy = 0.01,
label.y = -50) + geom_smooth( data = subset(merged_data, Treatment == "Control"), method = "lm", se = FALSE, color = "black", size = 1)  +scale_color_manual( name = 'Species',values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00"))  +labs( y = expression("Changes in " * italic(psi)[pd] ~ "(MPa)"),  x = "Tree height (cm)" ) +theme( legend.position = "none", axis.text = element_text(size = 12),axis.title.y = element_text(size = 12)) +facet_wrap(~Treatment)
GG1
ggplot2::ggsave('ChangeMPA.png', GG1,width = 8.14, height = 3.65, dpi = 1000)

## Extra figures
#### Staistical tests
#Predawn water potential 
library(dplyr)
library(car)

# 1) Prepare data (Makera only)
df <- Raw_data %>% 
  filter(Site == "Makera") %>% filter(Species != "Mla")%>% filter(Species != "Cme")%>%
  filter(!is.na(PredawnWP),
         !is.na(Species),
         !is.na(Month1),
         !is.na(Treatment))

# 2) Unique months and treatments
months <- sort(unique(df$Month1))
treatments <- sort(unique(df$Treatment))

# 3) Empty results table
assumptions <- data.frame(
  Treatment = character(),
  Month1 = character(),
  Shapiro_p = numeric(),
  Levene_p = numeric(),
  n = integer(),
  stringsAsFactors = FALSE
)

# 4) Loop over Treatment × Month
for(t in treatments){
  for(m in months){
    
    d <- df %>% filter(Treatment == t, Month1 == m)
    
    if(nrow(d) < 6) next
    if(length(unique(d$Species)) < 2) next
    
    mod <- lm(PredawnWP ~ Species, data = d)
    sh_p <- shapiro.test(residuals(mod))$p.value
    
    lev <- car::leveneTest(PredawnWP ~ Species, data = d)
    lev_p <- lev$`Pr(>F)`[1]
    
    assumptions <- rbind(
      assumptions,
      data.frame(
        Treatment = t,
        Month1 = m,
        Shapiro_p = sh_p,
        Levene_p = lev_p,
        n = nrow(d)
      )
    )
  }
}

# 5) View results
assumptions
### Model results 

library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)

df <- Raw_data%>%
  filter(Site == "Makera") %>%
  filter(!is.na(PredawnWP),
         !is.na(MiddayWP),
         !is.na(Species),
         !is.na(Month1),
         !is.na(Treatment),
         !is.na(Individu22))
predawn <- lmer(
  PredawnWP ~ Species * Month1 * Treatment + (1 | Individu22),
  data = df)
anova(predawn)

Midday <- lmer(
  MiddayWP ~ Species * Month1 * Treatment + (1 | Individu22),
  data = df)
anova(Midday)
df <- Raw_data%>%
  filter(Site == "Makera") %>%
  filter(!is.na(DWP),
         !is.na(Species),
         !is.na(Month1),
         !is.na(Treatment),
         !is.na(Individu22))
DWP <- lmer(
  DWP ~ Species * Month1 * Treatment + (1 | Individu22),
  data = df)
anova(DWP)

df <- Raw_data%>%  
  filter(Site == "Makera") %>%
  filter(!is.na(Spflow),
         !is.na(Species),
         !is.na(Month1),
         !is.na(Treatment),
         !is.na(Individu22))
Spflow<- lmer(
  Spflow ~ Species * Month1 * Treatment + (1 | Individu22),
  data = df)
anova(Spflow)

### Leaf habit
library(lme4)
library(lmerTest)

model <- lmer(Spflow ~ Leaf * Treatment * Month1 + (1|Individu), data = Makera)
anova(model)
model1 <- lmer(DWP ~ Leaf * Treatment * Month1 + (1|Individu), data = Makera)
anova(model1)
model2 <- lmer(MiddayWP ~ Leaf * Treatment * Month1 + (1|Individu), data = Makera)
anova(model2)
model3 <- lmer(PredawnWP ~ Leaf * Treatment * Month1 + (1|Individu), data = Makera)
anova(model3)

## change
model <- lm(Percent_U ~ Height * PFT, data = Decrease)
anova(model)
change_F <- ggplot(Decrease, aes(x = Height, y = Percent_U)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
  facet_wrap(~Treatment) +
  scale_color_manual(values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00")) +
  labs(y = "% change in tree sapflow",x = "Tree height (cm)", color = "Leaf habit") +
  theme(legend.position = "right",axis.text = element_text(size = 12),axis.title = element_text(size = 12))
model <- lm(Percent_U ~ Height, data = Decrease)

r2 <- summary(model)$r.squared
p  <- summary(model)$coefficients[2,4]
Total<-change_F +cleanup+annotate("text", x = 200, y = -50, label = paste0("R² = ", sprintf("%.2f", r2),
 ", p = ", ifelse(p < 0.001, "< 0.001", signif(p, 2))), hjust = 0,  size = 4) 
Total
ggplot2::ggsave('ChngeH1.png', Total,width = 8.14, height = 3.65, dpi = 1000)

## Percent WP
model <- lm(Percent_WP ~ Height * PFT, data = Decrease21)
anova(model)

## Formal test of the other species-group effects on the Height x change-in-
## psi_pd relationship (Fig. 5), to support the manuscript/comment claim:
## "We show a single pooled regression across all species, as there was no
## significant species group (origin elevation range, successional strategy)
## effects on psi_pd responses." PFT (leaf habit) is tested above; Origin and
## Succ are pulled in from Raw_data by Species (any_of() guards against
## Decrease21 already carrying its own copies of these columns).
species_groups <- Raw_data %>% distinct(Species, Succ, Origin)
Decrease21_groups <- Decrease21 %>%
  select(-any_of(c("Succ", "Origin"))) %>%
  left_join(species_groups, by = "Species")

model_origin <- lm(Percent_WP ~ Height * Origin, data = Decrease21_groups)
anova(model_origin)
model_succ <- lm(Percent_WP ~ Height * Succ, data = Decrease21_groups)
anova(model_succ)
change_F <- ggplot(Decrease, aes(x = Height, y = Percent_U, color = PFT)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
  facet_wrap(~Treatment) +
  scale_color_manual(values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00")) +
  labs(y = "% change in tree sapflow",x = "Tree height (cm)", color = "Leaf habit") +
  theme(legend.position = "right",axis.text = element_text(size = 12),axis.title = element_text(size = 12))
change_F
model <- lm(Percent_U ~ Height, data = Decrease)

GG1 <- ggscatter(merged_data, x = 'Height', y = 'Percent_WP',color = "PFT", conf.int = FALSE, add = "none",  palette = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00")) +stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
 show.legend = TRUE, p.accuracy = 0.001, r.accuracy = 0.01, label.y = -50) +theme(legend.position = "right",axis.text = element_text(size = 12),axis.title = element_text(size = 12))+
  geom_smooth( data = subset(merged_data, Treatment == "Control"), method = "lm", se = TRUE, color = "black", size = 1)  +scale_color_manual( name = 'Species',values = c("Evergreen" = "#117733", "Semi-deciduous" = "#E69F00"))+ theme(
    strip.text = element_text(size = 12, face = "bold"), axis.text = element_text(size = 12),  axis.title.y = element_text(size = 12), axis.title.x = element_blank(), axis.text.x = element_blank() )+  labs( y = expression("% change in " * italic(ψ)[pd]),  x = "Tree height (cm)" ) +facet_wrap(~Treatment)
GG1
# Remove PFT
GG1 <- ggscatter(merged_data, x = 'Height', y = 'Percent_WP', conf.int = FALSE, add = "none") +stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
 show.legend = TRUE, p.accuracy = 0.001, r.accuracy = 0.01, label.y = -50) +theme(legend.position = "right",axis.text = element_text(size = 12),axis.title = element_text(size = 12))+geom_smooth( data = subset(merged_data, Treatment == "Control"), method = "lm", se = TRUE, color = "black", size = 1)  +scale_color_manual( name = 'Species')+ theme(strip.text = element_text(size = 12, face = "bold"), axis.text = element_text(size = 12),  axis.title.y = element_text(size = 12))+  labs( y = expression("% change in " * italic(ψ)[pd]),  x = "Tree height (cm)" ) +facet_wrap(~Treatment)
GG1
ggplot2::ggsave('ChngeP1.png', GG1,width = 8.14, height = 3.65, dpi = 1000)
##RGR vs sapflow and change in sapflow
Decrease2 <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/Decrease2.xlsx",
  sheet = "Sheet1", col_types = c(
    # Col  1: Individu22
    "text",
    # Col  2: Treatment
    "text",
    # Col  3: max_PredawnWP
    "numeric",
    # Col  4: min_PredawnWP
    "numeric",
    # Col  5: max_Spflow
    "numeric",
    # Col  6: min_Spflow
    "numeric",
    # Col  7: max_u
    "numeric",
    # Col  8: min_u
    "numeric",
    # Col  9: max_MiddayWP
    "numeric",
    # Col 10: min_MiddayWP
    "numeric"
  ))
model<-lmer(Percent_U ~ RHeight * Treatment  + (1 | Species), data=Decrease2)
anova(model)
model1 <- lmer(Percent_U ~ RHeight + Treatment + (RHeight | Species), data = Decrease2)
anova(model1)
anova(model, model1)
model<-lmer(Percent_U ~ RHeight * Treatment+PFT+  + (1 | Species), data=Decrease2)
model<-lmer(Percent_U ~ RHeight *PFT+ Treatment+PFT+  + (1 | Species), data=Decrease2)
model_a <- lmer(RHeight ~ max_Spflow * Treatment + (1 | Species), data = Decrease2)
model_a_slope <- lmer(RHeight ~ max_Spflow + Treatment + (max_Spflow | Species), data = Decrease2)
model_a <- lmer(RGR ~ max_Spflow * Treatment + (1 | Species), data = Decrease22)
model_a_slope <- lmer(RGR ~ max_Spflow + Treatment + (max_Spflow | Species), data = Decrease22)
anova(model_a)
anova(model_a_slope)

## SWC graph
environment1 <- read_excel("C:/Gothenburg/Data/Hydraulic/Publication/environment1.xlsx", sheet = "SWC", col_types = c("date", "numeric", "text"))
SWC1<-ggplot(swc, aes(x = Date, y = SWC, color = Treatment)) + geom_line(linewidth = 0.6) +scale_color_manual(values = c("Irrigated" = "blue","Control" = "red")) +  labs(x = "Date",  y = "SWC (%)",color = "") +theme_classic(base_size = 12) +theme(legend.position = c(0.95, 0.95),legend.justification = c("right", "top"),legend.text = element_text(size = 12), axis.title = element_text(size = 12),  axis.text = element_text(size = 12), panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)) 
SWC1
ggplot2::ggsave('SWCAA.png', SWC1,width = 5.14, height = 3.65, dpi = 400)
