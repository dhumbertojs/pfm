library(tm)
library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(syuzhet)
library(forcats)
library(stringr)
library(pdftools)
library(tidytext)

# Datos -------------------------------------------------------------------

data <- read.csv("./data/clean_data.csv")   

solicitud <- data %>% 
  select(descripcion_solicitud) 

texto <- solicitud %>% 
  unnest_tokens(Palabra, descripcion_solicitud) %>% 
  filter(!Palabra %in% stopwords("es")) %>% 
  filter(nchar(Palabra)>2) %>% 
  filter(!Palabra %in% c(
    "solicito", "información", "ciudad", "méxico", "distrito", "federal", 
    "así", "nombre", "requiero", "cdmx", "transparencia"
  )) %>% 
  mutate(
    Palabra = str_remove_all(Palabra, "[:punct:]")
  ) 
df <- data %>% 
  sample_n(10000)
rm(data)

pre <- texto %>% 
  count(Palabra, sort = T) %>% 
  mutate(
    Palabra = str_replace_all(Palabra, c("á" = "a", "é" = "e",
                                         "ó" = "o", "ú" = "u"))
  ) 
rm(texto)

top_palabra <- pre %>% 
  group_by(Palabra) %>% 
  summarise(n = sum(n, na.rm=T)) %>% 
  filter(n>=100000) %>%
  mutate(Palabra = reorder(Palabra, n)) 
  
ggplot(top_palabra, aes(x = n, y = Palabra)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(labels = scales::label_comma()) +
  theme_bw() +
  labs(x="", y="", title = "Palabras recurrentes",
       subtitle = "Con más de 100,000 repeticiones")


# Sentimiento -------------------------------------------------------------
#8:44 ~ 
pre_nrc <- get_nrc_sentiment(char_v = df$descripcion_solicitud, language = "spanish")
pre_sentimientos <- bind_cols(df, pre_nrc)

x <- pre_sentimientos %>% 
  rowwise() %>%
  mutate(
    sent = sum(c_across(15:22)),
    pos = sum(c_across(23:24))
  ) %>% 
  group_by(organo_de_gobierno) %>% 
  summarise(
    anger = sum(anger, na.rm = T),
    anticipation = sum(anticipation, na.rm = T),
    disgust = sum(disgust, na.rm = T),
    fear = sum(fear, na.rm = T),
    joy = sum(joy, na.rm = T),
    sadness = sum(sadness, na.rm = T),
    surprise = sum(surprise, na.rm = T),
    trust = sum(trust, na.rm = T),
    negative = sum(negative, na.rm = T),
    positive = sum(positive, na.rm = T),
    sent = sum(sent, na.rm = T),
    pos = sum(pos, na.rm = T),
    
    anger = anger/sent, 
    anticipation = anticipation/sent, 
    disgust = disgust/sent, 
    fear = fear/sent, 
    joy = joy/sent, 
    sadness = sadness/sent, 
    surprise = surprise/sent, 
    trust = trust/sent, 
    
    negative = negative/pos, 
    positive = positive/pos
    ) %>% 
  ungroup() %>% 
  mutate(
    organo_de_gobierno = str_replace_all(organo_de_gobierno, 
                                         "Organismos desconcentrados, descentralizados, paraestatales y auxiliares",
                                         "Organismos desconcentrados")
  ) 
  

x %>% 
  select(1:9) %>% 
  pivot_longer(
    cols = 2:9
  )  %>% 
  ggplot(aes(x = organo_de_gobierno, y = value, 
             fill = name
             )) + 
  geom_bar(stat = "identity", position = "dodge" ) +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = "", y = "", 
       title = "Emociones promedio por ámbito", 
       fill = "emociones") +
  coord_flip()
ggsave("./plots/emociones_ambito.png")

x %>% 
  select(1, 10, 11) %>% 
  pivot_longer(
    cols = 2:3
  ) %>% 
  ggplot(aes(x = organo_de_gobierno, y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "dodge" ) +
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = "", y = "", 
       title = "Emociones promedio por ámbito", 
       fill = "Negativa y positiva") +
  coord_flip()
ggsave("./plots/pos-neg_ambito.png")





y <- pre_sentimientos %>% 
  rowwise() %>%
  mutate(
    pos = sum(c_across(23:24)),
    fecha = lubridate::as_date(fecha_de_ingreso)
  ) %>% 
  group_by(fecha) %>% 
  summarise(
    pos = sum(pos, na.rm = T),
    
    negative = sum(negative, na.rm = T),
    positive = sum(positive, na.rm = T),
    
    negative = negative/pos, 
    positive = positive/pos
  ) %>% 
  ungroup()

y %>% 
  select(1, 3:4) %>% 
  pivot_longer(
    cols = 2:3
  ) %>% 
  ggplot(aes(x = fecha, y = value, fill = name)) + 
  geom_area() +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.9),
    legend.position = "bottom"
  ) +
  labs(x = "", y = "", 
       title = "Emociones promedio por día", 
       fill = "") 
ggsave("./plots/pos-neg_dia.png")
