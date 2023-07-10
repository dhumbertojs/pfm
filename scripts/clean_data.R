library(clipr)
library(tidyr)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(lubridate)

data <- read.csv("./data/raw_data.csv") %>% 
  mutate(
    organo_de_gobierno = case_when(
      str_detect(dependencia, "Alcal") ~ "Alcaldías",
      str_detect(dependencia, "Dele") ~ "Alcaldías",
      str_detect(dependencia, "Agua") ~ "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
      str_detect(dependencia, "Ambiental y") ~ "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
      str_detect(dependencia, "Rosario") ~ "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
      str_detect(dependencia, "Medio A") ~ "Administración Pública Central",
      str_detect(dependencia, "Obras") ~ "Administración Pública Central",
      str_detect(dependencia, "Protección Civil") ~ "Administración Pública Central",
      str_detect(dependencia, "Planeación") ~ "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
      str_detect(dependencia, "Instancia Ejecutora del Sistema Integral de Derechos Humanos") ~ "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
      str_detect(dependencia, "Jefatura") ~ "Administración Pública Central",
      str_detect(dependencia, "Secretaría de Desarrollo Urbano y Vivienda") ~ "Administración Pública Central",
      str_detect(dependencia, "Fiscalía General de Justicia de la CDMX") ~ "Órganos Autónomos",
      str_detect(organo_de_gobierno, "Desconcen") ~ "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
      str_detect(organo_de_gobierno, "Interés") ~ "Personas físicas o morales que ejercen recursos públicos o realizan actos de autoridad",
      T ~ organo_de_gobierno
    ),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Administración Pública Centralizada", 
                                         "Administración Pública Central"),
    organo_de_gobierno = str_remove_all(organo_de_gobierno, "\\(baja 10-06-2019\\)"),
    organo_de_gobierno = str_trim(organo_de_gobierno, "both"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Instituto para la Atención de los Adultos Mayores en el Distrito Federal \\(17/02/2010\\)", 
                                         "Organismos desconcentrados, descentralizados, paraestatales y auxiliares"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Desconcentrados, Descentralizados, Paraestatales y Auxiliares", 
                                         "Organismos desconcentrados, descentralizados, paraestatales y auxiliares"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Descentralizados", 
                                         "Organismos desconcentrados, descentralizados, paraestatales y auxiliares"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Fondos y fideicomisos públicos", 
                                         "Fideicomisos y fondos públicos"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Organismos Autónomos", 
                                         "Órganos Autónomos"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Órgano Autónomo", 
                                         "Órganos Autónomos"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Órgano Autónomo", 
                                         "Órganos Autónomos"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Tribunales Administrativos", 
                                         "Órganos Autónomos"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Órgano político administrativos", 
                                         "Organismos desconcentrados, descentralizados, paraestatales y auxiliares"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "\\bÓrgano\\b", 
                                         "Poder"),
    organo_de_gobierno = str_remove_all(organo_de_gobierno, "en el Distrito Federal"),
    organo_de_gobierno = str_trim(organo_de_gobierno, "both"),
    organo_de_gobierno = str_replace_all(organo_de_gobierno, "Entidades Paramunicipales", 
                                         "Organismos desconcentrados, descentralizados, paraestatales y auxiliares"),
    
    organo_de_gobierno = ifelse(dependencia == "Órgano Regulador de Transporte",  
                                "Organismos desconcentrados, descentralizados, paraestatales y auxiliares", organo_de_gobierno),
    organo_de_gobierno = ifelse(organo_de_gobierno == "Persona física o moral que realiza actos de interés Público", 
                                "Personas físicas o morales que ejercen recursos públicos o realizan actos de autoridad",
                                organo_de_gobierno),
    organo_de_gobierno = ifelse(organo_de_gobierno == "Personas Morales", 
                                "Personas físicas o morales que ejercen recursos públicos o realizan actos de autoridad",
                                organo_de_gobierno),
    organo_de_gobierno = ifelse(organo_de_gobierno == "Poder Ejecutivo", 
                                "Administración Pública Central",
                                organo_de_gobierno),
    
    dependencia = str_replace_all(dependencia, "Policia Auxiliar",
                                  "Policía Auxiliar"),
    dependencia = str_remove_all(dependencia, "[:punct:]"),
    dependencia = str_remove_all(dependencia, "FONDECO"),
    dependencia = str_remove_all(dependencia, "PGJDF"),
    dependencia = str_remove_all(dependencia, "P "),
    dependencia = str_remove_all(dependencia, "RTP"),
    dependencia = str_remove_all(dependencia, "SUTIEMS"),
    dependencia = str_remove_all(dependencia, "SUTUACM"),
    dependencia = str_remove_all(dependencia, "SUTGCDMX"),
    dependencia = str_remove_all(dependencia, "SITIEMS"),
    dependencia = str_remove_all(dependencia, "FIDERE III"),
    dependencia = str_squish(dependencia),
    dependencia = str_trim(dependencia, "both"),
    dependencia = str_replace_all(dependencia, "Fiscalía Generar de Justicia de la CDMX", "Fiscalía General de Justicia de la CDMX")
  )

renglones_original <- nrow(data)

fin <- data %>%
  mutate(
    fecha_de_ingreso = as_datetime(fecha_de_ingreso),
    fecha_ingreso = as_datetime(fecha_ingreso), 
    fecha_limite_de_respuesta = as_date(fecha_limite_de_respuesta),
    fecha_respuesta = as_datetime(fecha_respuesta)
  ) %>% 
  unite(fecha, fecha_de_ingreso, fecha_ingreso, na.rm=T) %>% 
  rename(fecha_de_ingreso = fecha) %>% 
  filter(fecha_de_ingreso != "") %>% 
  unite(otros_datos, otros_datos, otro_datos, na.rm=T) %>%
  mutate(
    fecha_de_ingreso = as_datetime(fecha_de_ingreso),
    
    dependencia = str_remove_all(dependencia, "Alcaldía "),
    dependencia = str_remove_all(dependencia, "Delegación "),
    dependencia = str_remove_all(dependencia, "[.]$"),
    dependencia = str_remove_all(dependencia, "de la Ciudad de México"),
    dependencia = str_remove_all(dependencia, "del Distrito Federal"),
    dependencia = str_trim(dependencia, "both"),
    dependencia = str_replace_all(dependencia, "Gustavo A Madero", "Gustavo A. Madero" ),
    
    estatus = str_to_sentence(estatus),
    estatus = ifelse(estatus == "Proceso", "En proceso", estatus),
    
    medio_entrada = ifelse(medio_entrada == "Electronica", "Electrónica", medio_entrada),
    pais = str_to_sentence(pais), 
    pais = str_replace_all(pais, c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u")),
    estado = str_remove_all(estado, "de Zaragoza"),
    estado = str_remove_all(estado, "de Ocampo"),
  ) %>% 
  mutate(
    dependencia = str_trim(dependencia, "both"), 
    folio = paste0("'", folio),
  ) %>% 
  filter(!str_detect(tipo_solicitud, regex("datos", ignore_case = T ))) %>% 
  remove_empty("cols") %>% 
  remove_constant() %>% 
  arrange(desc(fecha_de_ingreso)) %>% 
  group_by(as.character(folio)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(`as.character(folio)`, tipo_solicitud))

names(fin)

fin <- fin %>% 
  mutate(
  respuesta = str_remove_all(respuesta, "A. "),
  respuesta = str_remove_all(respuesta, "B. "),
  respuesta = str_remove_all(respuesta, "C. "),
  respuesta = str_remove_all(respuesta, "D. "),
  respuesta = str_remove_all(respuesta, "E. "),
  respuesta = str_remove_all(respuesta, "F. "),
  respuesta = str_remove_all(respuesta, "G. "),
  respuesta = str_remove_all(respuesta, "H. "),
  respuesta = str_remove_all(respuesta, "J. "),
  respuesta = str_remove_all(respuesta, "K. "),
  respuesta = str_remove_all(respuesta, "\\bJ\\b"),
  respuesta = str_remove_all(respuesta, "\\bIP\\b"),
  respuesta = ifelse(respuesta == "Aviso de caducidad y opción a interponer Recurso de Revisión",
                     "Aviso de caducidad y opción de interponer un Recurso de Revisión", respuesta),
  respuesta = ifelse(respuesta == "Aviso de caducidad y opción a interponer RR",
                     "Aviso de caducidad y opción de interponer un Recurso de Revisión", respuesta),
  respuesta = ifelse(respuesta == "ReciboDePago", "Recibo De Pago", respuesta),
  respuesta = str_trim(respuesta, "both"),
  respuesta = str_to_sentence(respuesta),
  respuesta = str_squish(respuesta),
  respuesta = str_replace_all(respuesta, "\\bua\\b", "unidad administrativa")
) %>% 
  filter(!is.na(descripcion_solicitud)) %>% 
  mutate(
    descripcion_solicitud = str_remove_all(descripcion_solicitud, "\r"),
    descripcion_solicitud = str_remove_all(descripcion_solicitud, "\t"),
    descripcion_solicitud = str_remove_all(descripcion_solicitud, "\n"),
    descripcion_solicitud = str_remove_all(descripcion_solicitud, "-"),

    otros_datos = str_remove_all(otros_datos, "\r"),
    otros_datos = str_remove_all(otros_datos, "\t"),
    otros_datos = str_remove_all(otros_datos, "\n"),
    otros_datos = str_remove_all(otros_datos, "-"),
    otros_datos = ifelse(is.na(otros_datos), "", otros_datos)
  )

fin <- fin %>% 
  mutate(
    descripcion_solicitud = str_squish(descripcion_solicitud)
  ) %>% 
  filter(!nchar(descripcion_solicitud)<=15)

renglones_limpia <- nrow(fin)

renglones_original
renglones_limpia
names(fin)

write.csv(fin, "./data/clean_data.csv", row.names = F)

# Análisis Exploratorio de Datos ------------------------------------------

# Obtener información resumida del conjunto de datos
summary(fin) %>%
  write_clip()

a <- fin %>%
  group_by(dependencia) %>%
  count()

b <- fin %>%
  group_by(organo_de_gobierno) %>%
  count()

c <- fin %>%
  group_by(estatus) %>%
  count()

d <- fin %>%
  group_by(medio_entrada) %>%
  count()

e <- fin %>%
  group_by(pais) %>%
  count()

f <- fin %>%
  group_by(estado) %>%
  count()

g <- fin %>%
  group_by(respuesta) %>%
  count()

ggplot(fin, aes(x = as_date(fecha_de_ingreso))) +
  geom_histogram() +
  theme_bw() +
  labs(x="", y="", title = "Histograma")
ggsave("histograma.png")

# ggplot(fin, aes(x = organo_de_gobierno)) +
#   geom_bar() +
#   coord_flip()

# Ejemplo 2: Gráfico de dispersión
# fin %>%
#   ggplot(aes(x = organo_de_gobierno, y = folio), alpha = 0.2) +
#   geom_jitter()

