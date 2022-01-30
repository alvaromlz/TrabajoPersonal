#TRABAJO PERSONAL ÁLVARO MARTÍN LOZANO

#Analizando dataset nycflights13::flights

install.packages("nycflights13")

#Crear las 5 datasets con los datos del paquete de "nycflights13"
library(tidyverse)
library(lubridate)
library(nycflights13)
aerolineas <- nycflights13::airlines
aeropuertos <- nycflights13::airports
vuelos <- nycflights13::flights
aviones <- nycflights13::planes
clima <- nycflights13::weather

#Ejercicio 1
vuelos_retrasados <- filter(vuelos, arr_delay>60)
vuelos_retrasados
dim(vuelos_retrasados)

#Ejercicio 2
vuelos_sf <- filter(vuelos, dest == "SFO" | dest == "OAK")
vuelos_sf
dim(vuelos_sf)
dim(vuelos_sf[vuelos_sf$dest=="SFO",])
dim(vuelos_sf[vuelos_sf$dest=="OAK",])

#Ejercicio 3
vuelos_american <- filter(vuelos, carrier == "UA" | carrier == "AA")
vuelos_american
dim(vuelos_american)
dim(vuelos_american[vuelos_american$carrier=="UA",])
dim(vuelos_american[vuelos_american$carrier=="AA",])

#Ejercicio 4
vuelos_primavera <- filter(vuelos, month == 4 | month == 5 | month == 6)
vuelos_primavera
dim(vuelos_primavera)
dim(vuelos_primavera[vuelos_primavera$month==4,])
dim(vuelos_primavera[vuelos_primavera$month==5,])
dim(vuelos_primavera[vuelos_primavera$month==6,])

#Ejercicio 5
vuelos_retra2 <- filter(vuelos, arr_delay>60 & dep_delay<60)
vuelos_retra2
dim(vuelos_retra2)

#Ejercicio 6
vuelos_retra3 <- filter(vuelos, dep_delay>60 & arr_delay<30)
vuelos_retra3
dim(vuelos_retra3)

#Ejercicio 7
vuelos_nocturnos <- filter(vuelos, hour<=7)
vuelos_nocturnos
dim(vuelos_nocturnos)

#Ejercicio 8
vuelos_desconocidos <- filter(vuelos, is.na(dep_time))
vuelos_desconocidos
dim(vuelos_desconocidos)

#Ejercicio 9
apply(is.na(vuelos),2,sum)

#Ejercicio 10
arrange(vuelos, desc(dep_delay))
arrange(vuelos, dep_delay)

#Ejercicio 11
vuelos$velocidad <- vuelos$distance/(vuelos$air_time/60) #millas/hora
arrange(vuelos, desc(velocidad))

#Ejercicio 12
arrange(vuelos, desc(distance))

#Ejercicio 13
arrange(vuelos, distance)

#Ejercicio 14
#Con los operadores %/% y %%
vuelos$dep_time_min <- ((vuelos$dep_time %/% 100)*60) + (vuelos$dep_time %% 100)
vuelos$sched_dep_time_min <- ((vuelos$sched_dep_time %/% 100)*60) + (vuelos$sched_dep_time %% 100)
#La función "mutate" sirve para crear nuevas variables.
mutate(vuelos,
          dep_time,
          hour_dep = dep_time %/% 100,
          minute_dep = dep_time %% 100,
          dep_time_min = ((vuelos$dep_time %/% 100)*60) + (vuelos$dep_time %% 100),
          sched_dep_time,
          hour_shced_dep = sched_dep_time %/% 100,
          minute_sched_dep = sched_dep_time %% 100,
          sched_dep_time_min = ((vuelos$sched_dep_time %/% 100)*60) + (vuelos$sched_dep_time %% 100))


#Ejercicio 15
vuelos$dep_delay_min <- vuelos$dep_time_min - vuelos$sched_dep_time_min
vuelos$dif_dep_delay <- vuelos$dep_delay - vuelos$dep_delay_min
table(dif_dep_delay)


#Ejercicio 16
vuelos_cancel_dia <-  vuelos %>%
  mutate(vuelos_cancel = (is.na(dep_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarise(cancelados = sum(vuelos_cancel), totales = n(),)
vuelos_cancel_dia
#Gráfica para representar la relación entre el número de vuelos cancelados y el total por día.
ggplot(data=vuelos_cancel_dia) +
  geom_point(mapping=aes(x=totales, y=cancelados), color="red")


#Ejercicio 17
cancel_retraso_dia <- vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(ratio_cancelados = mean(cancelados),
            media_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ungroup()
cancel_retraso_dia
#Gráfica
ggplot(data=cancel_retraso_dia) +
  geom_line(aes(x=media_dep_delay, y=ratio_cancelados, col=ratio_cancelados))


#Ejercicio 18
cancel_retraso_aerop <- vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(ratio_cancelados = mean(cancelados),
            media_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ungroup()
cancel_retraso_aerop
#Gráfica
ggplot(data=cancel_retraso_aerop) +
  geom_line(aes(x=media_dep_delay, y=ratio_cancelados, col=ratio_cancelados))


#Ejercicio 19
by_carrier <- group_by(vuelos, carrier)
retrasos_carrier <- summarise(by_carrier,
                      dep_delay = mean(dep_delay, na.rm = TRUE),
                      arr_delay = mean(arr_delay, na.rm = TRUE))
retrasos_carrier
#Retrasos a la salida
arrange(retrasos_carrier, desc(dep_delay))
#Retrasos a la llegada
arrange(retrasos_carrier, desc(arr_delay))


#Ejercicio 20
by_hour <- group_by(vuelos, hour)
retrasos_hora <- summarise(by_hour,
                           dep_delay = mean(dep_delay, na.rm = TRUE))
arrange(retrasos_hora, dep_delay)


#Ejercicio 21
library(lubridate)
make_dtime <- function(year, month, day, time)
{
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
vuelos_dt <- vuelos %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_dtime(year, month, day, dep_time),
    arr_time = make_dtime(year, month, day, arr_time),
    sched_dep_time = make_dtime(year, month, day, sched_dep_time),
    sched_arr_time = make_dtime(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
vuelos_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)
#Gráfica
vuelos_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_dep_delay)) + 
  geom_bar(stat = "identity", fill = "dark blue")


#Ejercicio 22
#Convertir arr_delay en minutos totales
vuelos$arr_delay_min <- ((vuelos$arr_delay %/% 100)*60) + (vuelos$arr_delay %% 100)
#Calcular minutos totales por aeropuerto de destino
by_destino <- group_by(vuelos, dest)
retraso_destino <- summarise(by_destino,
                             arr_delay = sum(arr_delay_min, na.rm = TRUE))
retraso_destino


#Ejercicio 23
vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop) 


#Ejercicio 24
vuelos_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()

sessionInfo()

