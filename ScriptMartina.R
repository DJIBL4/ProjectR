data <- read.csv("C:/Users/alber/OneDrive/Documenti/Uni/PrimoSemestre/BiometriaStat/Progetti per esame/Wildfire US/data.csv")

View(data)
library(ggplot2)
library(tidyverse)
library(palmerpenguins)
library(GGally)
library(vegan)
library(scales)
library(cowplot)
library(ggpubr)

select_data <- select(data, c(1,15,21,25,26,31,32,36,39))
colnames(select_data)[c(1,2,3,4,5,6,7,8,9)] <- c("ID","Nome_Incendio","Anno","Class_Cause","Cause_Generali","Terreno_bruciato_acres","Class_Entità", "Stato", "Contea") #ho tenuto anche i nomi perchè mi facevano ridere:)
colnames(select_data) #verifico il cambio nome
view(select_data)

###CAMBIO ACRI IN KM2 E VERIFICO E (ORDINO IN MODO CRESCENTE ANNI) -> non utile###
class(select_data$Terreno_bruciato_acres)
new_data <- select_data %>%
  arrange(Anno) %>%
  mutate(terreno_bruciato_km2 = Terreno_bruciato_acres/247)
new_data$terreno_bruciato_km2
view(new_data)

###CALCOLO MODA DI STATI CON PIU INCENDI###
moda <- table(new_data$Stato)
moda[moda == max(moda)] #noto che la California ha la moda più alta

###CREO NUOVO DATAFRAME CON SOLO CALIFORNIA###
california <- new_data %>%
  filter(Stato == 'CA') %>%
  select(Stato,
         Contea,
         Anno,
         Class_Cause,
         Cause_Generali,
         terreno_bruciato_km2)
california

###DIVIDO PER HUMAN AND NATURAL CAUSE DELLA CALIFORNIA###

data_natural <- california %>%
  filter(Class_Cause == 'Natural')
data_natural
data_human <- california %>%
  filter(Class_Cause == 'Human')
data_human
sort(unique(data_natural$Cause_Generali)) #1 causa
sort(unique(data_human$Cause_Generali)) #12 cause 

###CAPISCO QUAL è LA CAUSA SPECIFICA MAGGIORE IN HUMAN CAUSE###

cause_human <- data_human %>%
  group_by(Cause_Generali) %>%
  summarise(n = n())
which(cause_human == max(cause_human$n)) #Missing data/not specified/undetermined
sort(cause_human$n)
cause_human #45660 equipment and vehicle use (1°)


###OSSERVO TREND NEGLI ANNI TRAMITE GRAFICO###
###PRENDO IN CONSIDERAZIONE SOLO I DATI DEGLI INCENDI NATURALI

california_anni_nat <- data_natural %>%
  group_by(Anno) %>%
  summarise(numero_incendi = n())
view(california_anni_nat)
sort(california_anni_nat$numero_incendi) #1992 anno con più incendi
california_anni_nat
summary(california)

ggplot(data = california_anni_nat) +
  geom_point(aes(x = Anno, y = numero_incendi))


####
#california_anni_hum <- data_human %>%
#  group_by(Anno) %>%
#  summarise(numero_incendi = n())
#view(california_anni_hum)
#sort(california_anni_hum$numero_incendi) #1992 anno con più incendi
#california_anni_hum
####

###

Data_Arson_incendiarism <- select_data %>%                               
  filter(Class_Cause == "Human", Cause_Generali == "Arson/incendiarism") 

Arson_Incendiarism <- Data_Arson_incendiarism %>%
  ggplot() +
  geom_density_2d_filled(aes (x = Anno , y = log10(terreno_bruciato_km2)), contour_var = "count", alpha = 0.8) +
  geom_smooth(aes (x = Anno , y = log10(terreno_bruciato_km2)), method = "gam", formula = y ~ s(x, bs = "cs") ) +
  labs(
    title = "Entità Incendio km2 x Anno in USA per cause dovute all'uomo",
    x = "Anno",
    y = "log10 Entità Incendio in km2"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = c(1992:2020)) +
  scale_y_continuous(breaks = seq(from = 0 , to = 6 , by = 0.5), limits = c(0,6))

Data_Natural <- select_data %>%                               
  filter(Class_Cause == "Natural") %>%
  select(Anno,Class_Cause, terreno_bruciato_km2)

Arson_Incendiarism




####################
########################################## CALIFORNIA ##################################################

Data_CA_Human_Arson <- california %>%                              
  filter( Class_Cause == "Human", Cause_Generali == "Arson/incendiarism") %>%
  select(Anno, Class_Cause, Cause_Generali, terreno_bruciato_km2)

CA <- Data_CA_Human_Arson %>% 
  ggplot() +
  geom_density_2d_filled(aes (x= Anno , y = log10(terreno_bruciato_km2)), contour_var = "count", alpha = 0.8) +
  geom_smooth(aes (x= Anno , y = log10(terreno_bruciato_km2)), method = "gam", formula = y ~ s(x, bs = "cs") ) +
  labs(
    title = " Entità Incendio km2 x Anno in CALIFORNIA per cause dovute all'uomo",
    x = "Anno",
    y = " log10 Entità Incendio in km2"
  ) +
  
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = c(1992:2020)) +
  scale_y_continuous(breaks = seq(from = 0 , to = 5 , by = 0.5), limits = c(0,5)) 
ggarrange(CA,Arson_Incendiarism)
CA

######################

temperature_1880_2022 <- read.csv("C:/Users/Martina/Desktop/Biometria e statistica/R/Progetto biometria/Global_annual_mean_temp.csv")
temperature_1992_2020 <- subset(temperature_1880_2022, Year %in% 1992:2020) %>%
  select(Year,
         No_Smoothing)
temperature_1992_2020



#H1 -> c'è una correlazione positiva tra il numero di incendi e l'aumento delle temperature
#H0 -> non c'è una correlazione positiva tra il numero di incendi e l'aumento delle temperature

cor.test(temperature_1992_2020$No_Smoothing,
         california_anni_nat$numero_incendi,
         alternative = "greater")
