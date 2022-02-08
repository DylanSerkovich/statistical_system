#Instalar paquete readxl
#install.packages("readxl")

#Instalar paquetes de Temas.
#install.packages("ggthemes")

#instalar paquete para seleccionar columna en caso que no este instalado
#install.packages("dplyr")

#Instalar libreria para trabajar con series de tiempo
#install.packages("fpp2")

#Instalar libreria para manipular fechas
#install.packages("lubridate")

#Instalar libreria para manipular datos
#install.packages("tidyr")

#Instalar libreria para hacer graficos dinamicos
#install.packages("highcharter")

#Instalar libreria para cambiar variables
#install.packages("car")

#Instalar libreria para cambiar variables
##install.packages("kableExtra")

#Instalar paquete para exportar a excel
install.packages("rio")
#Cargar Paquetes
library(readxl)
library(ggthemes)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(fpp2)
library(lubridate)
library(tidyr)
library(car)


#Defninir directorio
setwd("C:/Users/USER/Documents/RProjects/statistical_system")

#Importar los datos del archivo de excel
Datos <- read_excel(paste0(getwd(), "/data.xlsx"))

#Datos Imaginarios
DatosL <- read_excel(paste0(getwd(), "/dataL.xlsx"))


nrow(Datos)

#Prototipo de la parte 2 del reporte, pronostico de ventas
Ventas_diarias <- select(DatosL, Importe, fecha)

colnames(Ventas_diarias)[1]<- "ventas"

ventas_mensuales <- Ventas_diarias %>%
  mutate(month = format(fecha, "%m"), year =format(fecha, "%Y")) %>%
  group_by(month, year) %>%
  summarise(ventas = sum(ventas))

ventas_mensuales <- ventas_mensuales[with(ventas_mensuales, order(ventas_mensuales$year)),]

Y<- ts(ventas_mensuales[,3], start = c(2019,1), frequency = 12) #, end = c(2021,11)
Y
#DatosM<-as.data.frame(Y)
#DatosM
#######Necesitamos más datos
autoplot(Y)+
  ggtitle("Grafica de ventas en el Tiempo")+
  ylab("Expresado en soles")

##Ver estacionalidad de la cerie
descom=decompose(Y)
autoplot(descom)
acf(Y)
pacf(Y)

##Revisar diferencia de serie

DY <- diff(Y)

##Observar las diferencias
autoplot(DY)+
  ggtitle("Grafica de ventas en el Tiempo")+
  ylab("Expresado en soles")

##Elaborar el modelo Arima
modelo_arima <- auto.arima(Y,d=1, D=1, stepwise = TRUE, approximation = TRUE, trace = FALSE)

print(modelo_arima)

##Realizamos una evaluacion de los residuos del modelo
checkresiduals(modelo_arima)

##calculamos el forescast de ventas para los porximos 6 meses
fcst <- forecast(modelo_arima, h=6, level = c(95))
autoplot(fcst)+
  ggtitle("Grafica de ventas en el Tiempo")+
  ylab("Expresado en soles")

print(summary(fcst))

pronostico <- as.data.frame(fcst)

##Exportar a Excel
library(rio)
export(pronostico,"Pronostico_rio.xlsx")
#
#Selecciónar columnas que se van a utilizar
datos2 <- select(Datos, Descripción, Cantidad, fecha)

datos3 <- Datos
Datos
datos3$fecha <-as.Date(datos3$fecha)
datos3$Importe<-as.double(datos3$Importe)
datos3

#Posible Grafico para Cantidad vs importe de los productos
library(highcharter)
data( diamonds,Datos, package = "ggplot2")

hchart(datos3, "scatter", hcaes(x = Cantidad, y = Importe, group = Descripción, )) %>%
  hc_title(text = "Importe de ventas segun la cantidad y tipo de prenda") %>%
  hc_tooltip(crosshairs = TRUE, pointFormat = "Cantidad: {point.x} <br> Importe: {point.y} ") %>%
  hc_add_theme(hc_theme_darkunica())
  
#

#Posible Grafico para Cantidad vs importe de los productos
library(highcharter)
#data( datos3, package = "ggplot2")

hchart(datos3, "scatter", hcaes(x = fecha, y = Cantidad, group = Descripción, size = Importe, alpha = 0.8)) %>%
  hc_title(text = "cantidad de prendas por fecha") %>%
  hc_tooltip(crosshairs = TRUE, pointFormat = "fecha: {point.x:%d/%m/%Y} <br> cantidad: {point.y} <br> Importe: {point.size}") %>% 
  hc_add_theme(hc_theme_darkunica())

#

hchart(object = datos3, type = "column", hcaes(x=Descripción, y=Cantidad), color='red') %>% 
  hc_xAxis(title = list(text = "Prendas")) 


  

#Crear El Grafico
rm(pf)
pf<-ggplot(data = datos2, mapping = aes(x = fecha, y = Cantidad, color = Descripción, size = Cantidad, alpha=0.8))+
  geom_point() + geom_smooth(se=FALSE)+
  labs(title = "Cantidad de productos por Mes",
       subtitle = "Excel de Ventas",
       x = "Mes(fecha)",
       y = "Unidades",
       color = list(text='Prenda/Pieza'),
       size = "Cantidad") +
  guides(size = FALSE, alpha=FALSE) +
  theme_fivethirtyeight()+
  theme(legend.title = element_text(color="black", size=12, face="bold"),
        plot.subtitle = element_text(color="grey", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"),
        plot.title = element_text(color="black", size=18, face="bold")) +
  scale_colour_brewer(palette = "Paired")
pf

