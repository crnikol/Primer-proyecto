library(hrbrthemes)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(grid)

datos_clima <- read.csv("liberia_datos_climaticos.csv",
                        sep = "," ,
                        na.strings = "" ,
                        dec = "," ) 

datos_clima <- na.omit(datos_clima)

datos_clima <-
  datos_clima %>%
  rename(humedad = "HumedadRelativa....",
         vel_del_viento = "VelocidadViento..m.s.",
         lluvia = "Lluvia..mm.",
         irradiacion = "Irradiacion..W.m2.",
         evaporacion = "EvapoTranspiracion..mm.",
         tem_celsius = "Temperatura..Celsius.",
         fecha = "Date",
  )

datos_clima <- 
  datos_clima %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))

# Grafico de histogramas
hum <- ggplot(datos_clima, aes(x = humedad)) + geom_histogram(color = "red") + ylab("%")

vel <- ggplot(datos_clima, aes(x = vel_del_viento)) + geom_histogram(color = "blue") + ylab("m/s")

lluv <- ggplot(datos_clima, aes(x = lluvia)) + geom_histogram(color = "yellow") + ylab("mm")

irra <- ggplot(datos_clima, aes(x = irradiacion)) + geom_histogram(color = "pink") + ylab("W/m2")

eva <- ggplot(datos_clima, aes(x = evaporacion)) + geom_histogram(color = "brown") + ylab("mm")

temp <- ggplot(datos_clima, aes(x = tem_celsius)) + geom_histogram(color = "gray") + ylab("celsius")

grid.arrange(hum, vel, lluv, irra, eva, temp)

# Grafico de linea
promedio_delclima <-
  datos_clima %>%
  select(fecha, humedad, vel_del_viento, lluvia, irradiacion, evaporacion, 
         tem_celsius) %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))%>%
  group_by (fecha = format(fecha,"%m"))%>%
  summarise(tem_celsius = mean(tem_celsius), humedad = mean(humedad), vel_del_viento = mean(vel_del_viento), irradiacion = mean(irradiacion), lluvia = sum(lluvia), evaporacion = sum(evaporacion))

hume <- ggplot(promedio_delclima, aes(x = humedad, y = fecha, group = 1)) + geom_line(color = "red") + ylab("fecha") + xlab("humedad(%)")

velo <- ggplot(promedio_delclima, aes(x = vel_del_viento, y = fecha, group = 1)) + geom_line(color = "blue") + ylab("fecha") + xlab("vel_del_viento(m/s)")

lluvi <- ggplot(promedio_delclima, aes(x = lluvia, y = fecha, group = 1)) + geom_line(color = "yellow") + ylab("fecha") + xlab("lluvia(mm)")

irradi <- ggplot(promedio_delclima, aes(x = irradiacion, y = fecha, group = 1)) + geom_line(color = "pink") + ylab("fecha") + xlab("irradiacion(W/m2)")

evapo <- ggplot(promedio_delclima, aes(x = evaporacion, y = fecha, group = 1)) + geom_line(color = "brown") + ylab("fecha") + xlab("evaporacion (mm)")

tempe <- ggplot(promedio_delclima, aes(x = tem_celsius, y = fecha, group = 1)) + geom_line(color = "gray") + ylab("fecha") + xlab("temperatura(celsius)")

grid.arrange(hume, velo, lluvi, irradi, evapo, tempe, nrow = 3, ncol = 2)

# Grafico x-y para relacionar las variables entre si

hu <- ggplot(datos_clima, aes(x = humedad, y = fecha, group = 1)) + geom_point(color = "red") + ylab("fecha") + xlab("humedad(%)")

ve <- ggplot(datos_clima, aes(x = vel_del_viento, y = fecha, group = 1)) + geom_point(color = "blue") + ylab("fecha") + xlab("vel_del_viento(m/s)")

llu <- ggplot(datos_clima, aes(x = lluvia, y = fecha, group = 1)) + geom_point(color = "yellow") + ylab("fecha") + xlab("lluvia(mm)")

ir <- ggplot(datos_clima, aes(x = irradiacion, y = fecha, group = 1)) + geom_point(color = "pink") + ylab("fecha") + xlab("irradiacion(W/m2)")

ev <- ggplot(datos_clima, aes(x = evaporacion, y = fecha, group = 1)) + geom_point(color = "brown") + ylab("fecha") + xlab("evaporacion (mm)")

tem <- ggplot(datos_clima, aes(x = tem_celsius, y = fecha, group = 1)) + geom_point(color = "gray") + ylab("fecha") + xlab("temperatura(celsius)")

grid.arrange(hu, ve, llu, ir, ev, tem, nrow = 2, ncol = 3)


