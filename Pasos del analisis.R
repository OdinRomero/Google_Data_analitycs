# Se pasa primero los archivos y se unen.



#=======================
##Paso 1 colectar datos
#=======================
library(tidyverse) 
library(lubridate) 
library(ggplot2)
library(kableExtra)

#for para leer los últimos archivos
for (i in 1:12){
  if (i<10){
    assign(paste0("tab",i),read.csv(paste0(20220,i,"-divvy-tripdata.csv")))
  } else{
    assign(paste0("tab",i),read.csv(paste0(2021,i,"-divvy-tripdata.csv")))
  }
}


#========================
##Paso 2 manejor de datos
#========================

#for para comprobar que los nombres de todas las tablas sea el mismo
namesDF<-names(tab1)
result<-vector()

for (i in 1:11){
  ho<-names(get(paste0("tab",i)))
  result[i]<-setequal(ho, namesDF)
}
result

#Se eliminan variables innecesarias de la memoria
rm(ho,result,namesDF)

#Procedemos a combinar las tablas

DF<-rbind(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11,tab12)
head(DF)
str(DF)#Se descubre que en los ids de las estaciones hay problemas por que no son del mismo formato de forma
#No se encotnraron más detalles mas que los de las horas y su formato,y lo de los ids


#============================================================
##PASO 3 Limpieza y preparación para el análisis de los datos
#============================================================

#Conociendo más a fondo el nuevo data frame
colnames(DF) 
dim(DF)# Se ve que son 5828235 filas con 13 variables
summary(DF[,9]);summary(DF[,10]);summary(DF[,11]);summary(DF[,12])#Se detecta la presencia de NA's en las lat y long finales
unique(DF$member_casual)#no hay problemas con las categorías puestas 
#Se procede a trabajar con los datos de hora y fecha
#"started_at"[3],"ended_at"[4]
class(DF$"started_at")
DF[,3]<-ymd_hms(DF$"started_at")
DF[,4]<-ymd_hms(DF$"ended_at")
#Se verifica que se hayan formateado correctamente
class(DF$"started_at")
class(DF$"ended_at")
#Se crean las columnas de día, mes  y año.
#Se procede a crear una nueva variable llamada "ride_length" duración de los viajes
#year(x),month(x, label, abbr),day(x)

DF<-DF%>%mutate(Year= year(DF$"ended_at"))%>%
  mutate(Month=month(DF$"ended_at",label = TRUE))%>%
  mutate(Day=day(DF$"ended_at"))%>%
  mutate(ride_length= seconds(DF$"ended_at")-seconds(DF$"started_at"))
DF$day_of_week <- format(as.Date(DF$"started_at"), "%A")
  

class(DF$ride_length)
#Para poder hacer operaciones aritmeticas con los fechas-tiempos SIEMPRE HAY QUE CONVERTIRLOS
#PRIMERO EN SEGUNDOS Y YA EN SEGUNDOS SE REALIZAN LAS OPERACIONES.
#Se procede a retirar los datos negativos de ride_length
DF$ride_length <- as.numeric(period_to_seconds(DF$ride_length))
DF2 <- DF[-DF$ride_length<0,]
#se corrobora que todo esté bien
str(DF);str(DF2)




#============================
# Paso 4 análisis descriptivo
#============================
pr<-c()
pr[1]<-mean(DF2$ride_length)
pr[2]<-median(DF2$ride_length)
pr[3]<-max(DF2$ride_length)
pr[4]<-min(DF2$ride_length)
ride<-data.frame("medidas"=c("Media","Mediana","Máximo","Mínimo") , "viaje"= pr)

knitr::kable(ride, caption = 'Medidas de tendencia central y el mínimo y máximo de la duración del viaje')



aggregate(DF2$ride_length ~ DF2$member_casual, FUN = mean)
aggregate(DF2$ride_length ~ DF2$member_casual, FUN = median)
aggregate(DF2$ride_length ~ DF2$member_casual, FUN = max)
aggregate(DF2$ride_length ~ DF2$member_casual, FUN = min)

aggregate(DF2$ride_length ~ DF2$member_casual + DF2$day_of_week,
          FUN = mean)
#Se ordenan los días de la semana para poder hacer el resumen de manera correcta

class(DF2$day_of_week)
DF2$day_of_week <-as.factor(DF2$day_of_week)%>%
  ordered( levels=c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado"))
aggregate(DF2$ride_length ~ DF2$member_casual + DF2$day_of_week,
          FUN = mean)

#Se crea una variable que es el número de pasajeros y se proceden a realizar dos visualizaciones 
#de gráficos de barra, uno del número de viajes y el otro el promedio de viajes
DF2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

DF2 %>%mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(DF2$ride_length ~ DF2$member_casual +
                      DF2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

