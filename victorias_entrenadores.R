
library(tidyverse)
library(datapasta)
library(worldfootballR)

simeone <- tibble::tribble(
             ~Partidos,   ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                    371L, 228L, 88L, 55L, "612:259",    772L,                 208
             )

simeone <- simeone %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Simeone")


cervera <- data.frame(
          Partidos = c(45L),
                   G = c(12L),
                   E = c(14L),
                   P = c(19L),
               Goles = c("43:69"),
              Puntos = c(50L),
  Puntos.por.partido = c(111)
           )

cervera <- cervera %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Cervera")

paco_lopez <- tibble::tribble(
                ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                       132L, 42L, 37L, 53L, "180:204",    163L,                 123
                )

paco_lopez <- paco_lopez %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "P. López")


jagoba_arrasate <- tibble::tribble(
                     ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                            131L, 44L, 40L, 47L, "165:183",    172L,                 131
                     )

jagoba_arrasate <- jagoba_arrasate %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "J. Arrasate")

imanol <- tibble::tribble(
            ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                   113L, 51L, 28L, 34L, "167:125",    181L,                 160
            )

imanol <- imanol %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Imanol")

lopetegui <- tibble::tribble(
               ~Partidos,  ~G,  ~E,  ~P,   ~Goles, ~Puntos, ~Puntos.por.partido,
                       92L, 51L, 22L, 19L, "131:83",    175L,                 190
               )

lopetegui <- lopetegui %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Lopetegui")

emery <- tibble::tribble(
           ~Partidos,   ~G,  ~E,   ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                  367L, 170L, 92L, 105L, "583:447",    602L,                 164
           )

emery <- emery %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Emery")


pellegrini <- tibble::tribble(
                ~Partidos,   ~G,  ~E,   ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                       377L, 187L, 88L, 102L, "601:455",    649L,                 172
                )


pellegrini <- pellegrini %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Pellegrini")


vicente_moreno <- data.frame(
    stringsAsFactors = FALSE,
          Partidos = c(45L),
                   G = c(10L),
                   E = c(9L),
                   P = c(26L),
               Goles = c("44:72"),
              Puntos = c(39L),
  Puntos.por.partido = c("0,87")
                  )

vicente_moreno <- vicente_moreno %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "V. Moreno")

luis_garcia <- tibble::tribble(
                 ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                        154L, 46L, 40L, 68L, "159:224",    178L,                 116
                 )

luis_garcia <- luis_garcia %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Luis García")
  
  
iraola <- tibble::tribble(
            ~Partidos, ~G, ~E, ~P, ~Goles, ~Puntos, ~Puntos.por.partido,
                     7L, 4L, 1L, 2L, "13:7",     13L,                 186
            )

iraola <- iraola %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Iraola")

koeman <- tibble::tribble(
            ~Partidos,  ~G,  ~E,  ~P,   ~Goles, ~Puntos, ~Puntos.por.partido,
                    67L, 32L, 16L, 19L, "117:78",    112L,                 167
            )

koeman <- koeman %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Koeman")
  

coudet <- data.frame(
          Partidos = c(36L),
                   G = c(15L),
                   E = c(8L),
                   P = c(13L),
               Goles = c("56:52"),
              Puntos = c(53L),
  Puntos.por.partido = c(147)
            )  

coudet <- coudet %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Coudet")

marcelino <- tibble::tribble(
               ~Partidos,   ~G,   ~E,   ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                      346L, 142L, 100L, 104L, "460:389",    526L,                 152
               )

marcelino <- marcelino %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Marcelino")
  

escriba <- tibble::tribble(
               ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                      200L, 60L, 52L, 88L, "197:275",    232L,                 116
               )

escriba <- escriba %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Escribá")

calleja <- tibble::tribble(
             ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                    117L, 49L, 25L, 43L, "171:153",    172L,                 147
             )

calleja <- calleja %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Calleja")


michel <- tibble::tribble(
             ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                    154L, 53L, 28L, 73L, "195:218",    187L,                 121
             )

michel <- michel %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Míchel")

robert_moreno <- tibble::tribble(
                   ~Partidos, ~G, ~E, ~P, ~Goles, ~Puntos, ~Puntos.por.partido,
                            7L, 0L, 3L, 4L, "5:12",      3L,              "0,43"
                   )

robert_moreno <- robert_moreno %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "R. Moreno")
  
  
ancelotti <- tibble::tribble(
                ~Partidos,  ~G,  ~E,  ~P,   ~Goles, ~Puntos, ~Puntos.por.partido,
                        83L, 62L, 10L, 11L, "243:84",    196L,                 236
                )

ancelotti <- ancelotti %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Ancelotti")

bordalas <- tibble::tribble(
              ~Partidos,  ~G,  ~E,  ~P,    ~Goles, ~Puntos, ~Puntos.por.partido,
                     159L, 56L, 49L, 54L, "173:156",    217L,                 136
              )

bordalas <- bordalas %>% 
  select(G,E,P, Partidos) %>% 
  gather("resultado", "value", -Partidos) %>% 
  mutate(resultado = as.factor(resultado),
         value = as.numeric(value),
         Partidos = as.numeric(Partidos)) %>% 
  group_by(resultado, value) %>% 
  group_by(resultado) %>%
  mutate(Victorias = value/sum(Partidos)*100) %>% 
  mutate(Entrenador = "Bordalás")
  

  
 

  
  




df <- bind_rows(simeone, cervera, paco_lopez, jagoba_arrasate, imanol, lopetegui, emery, pellegrini,
                vicente_moreno, luis_garcia, iraola, koeman, coudet, marcelino, escriba, calleja,
                michel, robert_moreno, ancelotti, bordalas)
  
  

# plot -----------------

library(gt)
library(gtExtras)

df <- df %>% 
  filter(resultado %in% c("G")) %>% 
  ungroup() %>% 
  select(-resultado) %>% 
  arrange(-Victorias) %>% 
  relocate(where(is.numeric), .after = last_col()) %>%
  gt() %>% 
  gt_plt_bullet(column = Partidos, target = value,
                colors = c("skyblue", "red")) %>% 
  gt_theme_nytimes() %>% 
  fmt_symbol_first(column = Victorias, suffix = "%", decimals = 1, scale_by = 1) %>% 
  tab_header(title = "Entrenadores de LaLiga 21-22, % de victorias en la máxima categoría del fútbol español",
             subtitle = "Datos a 30-09-2021") %>% 
  tab_source_note(md("Source: Transfermarkt | @dataR_amateur")) %>% 
  tab_options(table.background.color = "gray95") %>% 
  cols_width(Entrenador ~ px(300), 3 ~ px(30))


gtExtras::gtsave_extra(df, "Entrenadores.png", vwidth = 450, vheight = 430)


  

