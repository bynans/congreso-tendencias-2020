#Procesamiento de base de TWITTER


#Cargo librerías
library(tidyverse)
library(lubridate)
library(zoo)
library(hrbrthemes)
library(viridis)
library(data.table)
library(scales)


#Creo la lista de archivos de mi directorio
file_list <- list.files(path="base_tw/", full.names = T)

#DF blank
dataset <- data.frame()


for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], col_types = cols(.default = "c")) 
  temp_data <- temp_data %>% select(date,username, tweet)
  dataset <- rbindlist(list(dataset, temp_data), use.names = T) 
}

vector_ordenado <- c("eternacadencia", "fceargentina", "uranoargentina", "megustaleerarg", "paidosargentina", "planetalibrosar","sigloxxiarg","noveduc","ivreality","oceanoarg", "atlantidatw", "sanpabloar","editorialunlp","edeudeba", "lamarcaeditora","lagataylaluna", "cespedeslibros", "cuspidelibrosok", "hernandezlibre", "lalibrelibreria","libreriabetania","waldhuter")

#Parámetros para el procesado
fecha_inicio <- "2019-01-01"
fecha_fin <- "2020-10-31"
intervalo_excluido <- seq(as.Date("2019-11-01"), as.Date("2019-12-31"), by="days")
vector_anio_mes <- c("2019-01", "2019-02", "2019-03", "2019-04", "2019-05", "2019-06", "2019-07","2019-08", "2019-09", "2019-10", "2019-11", "2019-12", "2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06", "2020-07","2020-08", "2020-09", "2020-10")


#Preparo el dataset para el análisis  
dataset_modificado <- dataset %>% 
  mutate(fecha_publicacion=as.Date(date)) %>% 
  filter(fecha_publicacion >= fecha_inicio, fecha_publicacion <= fecha_fin) %>% 
  rename(usuario=username) %>% 
  filter(usuario %in% vector_ordenado) %>% 
  select(fecha_publicacion, usuario, tweet)

#Tabla agregada con total de posteos por editorial por fecha

dataset_fecha <- dataset_modificado %>% 
  group_by(usuario, fecha_publicacion) %>% 
  tally()

#Tabla pivoteada con listado de usuarios y columnas mes-año con total de posteos

tabla_mes_anio <- dataset_fecha %>% 
  mutate(anio_mes = format(fecha_publicacion, "%Y-%m")) %>% 
  group_by(usuario, anio_mes) %>% 
  tally() %>% 
  group_by(usuario) %>% 
  complete(anio_mes = vector_anio_mes) %>% 
  ungroup() %>% 
  mutate(n = replace_na(n, 0),
         anio = substr(anio_mes, 1, 4),
         mes = substr(anio_mes, 6, 7)) %>% 
  arrange(anio_mes)


tabla_mes_anio_pivot <- tabla_mes_anio %>% 
  select(usuario, anio_mes, n) %>% 
  pivot_wider(names_from = anio_mes,
              values_from = n)

ggplot(tabla_mes_anio, aes(anio_mes, usuario, fill= n)) + 
  geom_tile() +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

#Calculo metricas año/año. Esta manera de hacerlo requiere que cada usuario tenga una fila por cada mes de los años analizados

tabla_mes_anio_yoy <- tabla_mes_anio %>% 
  group_by(usuario) %>% 
  mutate(YoY = (n - lag(n, 12)) / lag(n, 12)) %>% 
  ungroup()
  
pivot_yoy <- tabla_mes_anio_yoy %>% 
  select(usuario, anio_mes, YoY) %>% 
  filter(str_detect(anio_mes, regex("2020"))) %>% 
  pivot_wider(names_from = anio_mes,
              values_from = YoY)


tabla_mes_anio_yoy %>% 
  filter(usuario %in% vector_ordenado) %>% 
  mutate(usuario=as.factor(usuario),
         usuario = fct_relevel(usuario, vector_ordenado)) %>% 
  select(usuario, anio_mes, YoY) %>% 
  filter(str_detect(anio_mes, regex("2020"))) %>% 
  mutate(n = as.character(YoY)) %>% 
  mutate(valor = case_when(n == "NaN" ~ "nulo",
                           n == "Inf" ~ "positivo",
                           str_detect(n, regex("-")) ~"negativo",
                           TRUE ~ "positivo")) %>% 
  ggplot(aes(anio_mes, usuario, fill=factor(valor))) + 
  geom_tile() + 
  geom_text(aes(label = round(YoY, 2))) +
  scale_fill_manual(values = c('red','white', "lightgreen")) +
  ggtitle("Comparación de publicaciones mensuales Año/Año") +
  xlab("Mes del 2020")



#Series de tiempo con editoriales Año/Año

#Función para las visualizaciones
serie_yoy <- function(x) {
  tabla_mes_anio %>% 
  filter(usuario == x) %>% 
  ggplot() +
    geom_line(aes(mes, n, group = anio, colour=anio), size=2) +
    theme_bw()  +
    theme(axis.text.x=element_text(angle = 45, vjust = 0.5),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          plot.title = element_text(size = 26))+
    ggtitle(paste("Posteos Año/Año del usuario", x))  +
    ylab("Total de publicaciones") +
    xlab("Mes del año") + 
    labs(color = "Año") 

}

#Genero vector con nombres (únicos) de usuarios
librerias_editoriales <- vector_ordenado

plots_yoy <- map(librerias_editoriales, serie_yoy)
plots_yoy


#### Análisis de estrategias vinculadas a cuarentena ----

#Regex expresiones vinculadas a promociones
regex_domicilio <- regex("domicilio")
regex_online <- regex("online")
regex_envio <- regex("envio|envío")
regex_descuento <- regex("descuento")
regex_ebook <- regex("[^face]ebook|ebooks|e-book|libro digital|\bebook")
regex_encasa <- regex("encasa")

#Marcado de posteos relativos a promociones (1 col x promo, 1 o 0, clasificación binaria)

dataset_marcado <- dataset_modificado %>% 
  mutate(a_domicilio =ifelse(str_detect(tweet, regex(regex_domicilio, ignore_case = TRUE)), 1, 0),
         online = ifelse(str_detect(tweet, regex(regex_online, ignore_case = TRUE)), 1, 0),
         envio=ifelse(str_detect(tweet, regex(regex_envio, ignore_case = TRUE)), 1, 0),
         descuento=ifelse(str_detect(tweet, regex(regex_descuento, ignore_case = TRUE)), 1, 0),
         ebooks=ifelse(str_detect(tweet, regex(regex_ebook, ignore_case = TRUE)), 1, 0),
         en_casa=ifelse(str_detect(tweet, regex(regex_encasa, ignore_case = TRUE)), 1, 0) )

#Filtro noviembre y diciembre 2019
#Genero columna año  
dataset_marcado_anual <- dataset_marcado %>% 
  filter(!fecha_publicacion %in% intervalo_excluido) %>% 
  mutate(año = year(fecha_publicacion)) %>% 
  group_by(usuario, año) %>% 
  summarise(a_domicilio = sum(a_domicilio),
            online = sum(online),
            envio = sum(envio),
            descuento = sum(descuento),
            ebooks = sum(ebooks),
            en_casa = sum(en_casa))

#Prevalencia: % de editoriales/librerias que utilizaron las regex 2019-2020  

props <- function(x, y) {
  
  dataset_marcado_anual %>% 
    filter(año == x, !!sym(y) > "0") %>% 
    pull(usuario) %>% 
    n_distinct()
  
}


prop_domicilio_2019 <- props("2019", "a_domicilio") / n_distinct(librerias_editoriales) * 100
prop_domicilio_2020 <- props("2020", "a_domicilio") / n_distinct(librerias_editoriales) * 100
prop_online_2019 <- props("2019", "online") / n_distinct(librerias_editoriales) * 100
prop_online_2020 <- props("2020", "online") / n_distinct(librerias_editoriales) * 100
prop_envio_2019 <-  props("2019", "envio") / n_distinct(librerias_editoriales) * 100 
prop_envio_2020 <-  props("2020", "envio") / n_distinct(librerias_editoriales) * 100 
prop_descuento_2019 <- props("2019", "descuento") / n_distinct(librerias_editoriales) * 100 
prop_descuento_2020 <- props("2020", "descuento") / n_distinct(librerias_editoriales) * 100 
prop_ebooks_2019 <- props("2019", "ebooks") / n_distinct(librerias_editoriales) * 100 
prop_ebooks_2020 <- props("2020", "ebooks") / n_distinct(librerias_editoriales) * 100   

#Visualizaciones de prevalencia
dataset_marcado_anual_grafico <- dataset_marcado_anual %>% 
  filter(año == "2019") %>%  
  mutate(a_domicilio = a_domicilio*-1,
         online = online*-1,
         envio = envio*-1,
         descuento = descuento*-1,
         ebooks = ebooks*-1) %>% 
  bind_rows(dataset_marcado_anual %>% filter(año=="2020"))

dataset_marcado_anual_grafico %>% 
  pivot_longer(!c(usuario, año), names_to="expresion", values_to="total") %>% 
  filter(expresion == "a_domicilio") %>% 
ggplot(aes(x = usuario, y = total, fill = as.factor(año))) + 
  coord_flip() + 
  geom_bar(stat = "identity", position = "identity", width = 0.525) +
  ggtitle("Comparación Año/Año Enero-Octubre de la expresión 'domicilio'") +
  ylab("Número de publicaciones")  +
  theme_bw()

dataset_marcado_anual_grafico %>% 
  pivot_longer(!c(usuario, año), names_to="expresion", values_to="total") %>% 
  filter(expresion == "online") %>% 
  ggplot(aes(x = usuario, y = total, fill = as.factor(año))) + 
  coord_flip() + 
  geom_bar(stat = "identity", position = "identity", width = 0.525) +
  ggtitle("Comparación Año/Año Enero-Octubre de la expresión 'online'") +
  ylab("Número de publicaciones")  +
  theme_bw()

dataset_marcado_anual_grafico %>% 
  pivot_longer(!c(usuario, año), names_to="expresion", values_to="total") %>% 
  filter(expresion == "envio") %>% 
  ggplot(aes(x = usuario, y = total, fill = as.factor(año))) + 
  coord_flip() + 
  geom_bar(stat = "identity", position = "identity", width = 0.525) +
  ggtitle("Comparación Año/Año Enero-Octubre de la expresión 'envio'") +
  ylab("Número de publicaciones")  +
  theme_bw()

dataset_marcado_anual_grafico %>% 
  pivot_longer(!c(usuario, año), names_to="expresion", values_to="total") %>% 
  filter(expresion == "ebooks") %>% 
  ggplot(aes(x = usuario, y = total, fill = as.factor(año))) + 
  coord_flip() + 
  geom_bar(stat = "identity", position = "identity", width = 0.525) +
  ggtitle("Comparación Año/Año Enero-Octubre de expresiones relativas a ebooks") +
  ylab("Número de publicaciones")  +
  theme_bw()
