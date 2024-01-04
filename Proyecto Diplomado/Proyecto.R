library(tidyverse)
library(janitor)
library(rio)
library(skimr)

library(tibble)
library(tidyr)


# Limpieza y consolidación de las 3 bases de datos ------------------------


ECV2021 <- rio::import("Calidad de vida 2021/BBDD - Calidad de Vida 2021 - Fundación Piensa.dta")
ECV2019 <- rio::import("Calidad de Vida 2019/BBDD Calidad de vida ECV 2019.dta")
ECV2018 <- rio::import("Calidad de vida 2018/BBDD Calidad de vida 2018.dta")

glimpse(ECV2021)
glimpse(ECV2019)
glimpse(ECV2018)


nombres_columnas <- data.frame(col2018, col2019, col2021)

write.table(nombres_columnas, file = "nombres_columnas_nueva.txt", sep = ",", 
            quote = FALSE, row.names = F)

ECV2018 <- ECV2018 %>% add_column(anio = 2018) %>% clean_names() %>% 
  select(anio, gsex, comuna, 
         b01_t1:b10_t19, 
         b13:b16_t16, 
         c01_t1:c08_2,
         d01:d02_t5, ponderador_2018) %>% 
  select(anio, gsex,comuna, b01_t1:b04_t6, b04_t7:b04_t10,
                                                 b04_t11:b04_t12, 
         everything()) %>% 
  mutate(b05_o11 = case_when(b05_o1 == 1 | b05_o2 == 1 | b05_o3 == 1 | b05_o4 == 1 ~ 1,
                             b05_o1 == 99 | b05_o2 == 99 | b05_o3 == 99 | b05_o4 == 99 ~ 99,
                            TRUE ~ 2),
         b05_o22 = case_when(b05_o1 == 2 | b05_o2 == 2 | b05_o3 == 2 | b05_o4 == 2 ~ 1,
                             b05_o1 == 99 | b05_o2 == 99 | b05_o3 == 99 | b05_o4 == 99 ~ 99,
                            TRUE ~ 2),
         b05_o33 = case_when(b05_o1 == 3 | b05_o2 == 3 | b05_o3 == 3 | b05_o4 == 3 ~ 1,
                             b05_o1 == 99 | b05_o2 == 99 | b05_o3 == 99 | b05_o4 == 99 ~ 99,
                            TRUE ~ 2)) %>% select(anio, gsex, comuna, b01_t1:b04_t12,
                                                  b05_o11:b05_o33,
                                                  everything()) %>% select(-(b05_o1:b05_o4)) %>% 
mutate(b08_11 = case_when(b08_o1 == 1 | b08_o2 == 1 | b08_o3 == 1 | 
                            b08_o4 == 1 | b08_o5 == 1 | b08_o6 == 1 ~ 1,
                          TRUE ~ NA_real_),
       b08_21 = case_when(b08_o1 == 2 | b08_o2 == 2 | b08_o3 == 2 | 
                           b08_o4 == 2 | b08_o5 == 2 | b08_o6 == 2 ~ 2,
                         TRUE ~ NA_real_),
       b08_31 = case_when(b08_o1 == 3 | b08_o2 == 3 | b08_o3 == 3 | 
                           b08_o4 == 3 | b08_o5 == 3 | b08_o6 == 3 ~ 3,
                         TRUE ~ NA_real_),
       b08_41 = case_when(b08_o1 == 4 | b08_o2 == 4 | b08_o3 == 4 | 
                           b08_o4 == 4 | b08_o5 == 4 | b08_o6 == 4 ~ 4,
                         TRUE ~ NA_real_),
       b08_51 = case_when(b08_o1 == 5 | b08_o2 == 5 | b08_o3 == 5 | 
                           b08_o4 == 5 | b08_o5 == 5 | b08_o6 == 5 ~ 5,
                         TRUE ~ NA_real_),
       b08_99 = case_when(b08_o1 == 99 | b08_o2 == 99 | b08_o3 == 99 | 
                            b08_o4 == 99 | b08_o5 == 99 | b08_o6 == 99 ~ 99,
                          TRUE ~ NA_real_)) 
ECV2018 <- ECV2018 %>% select(anio, gsex, comuna, 
                   b01_t1:b07, 
                   b08_11:b08_99, everything()) %>% select(-(b08_o1:b08_o6))

ECV2018 <- ECV2018 %>% mutate(b15_0 = case_when(b15_o1 == 16 ~ 0,
                                     TRUE ~ NA_real_),
                   b15_1 = case_when(b15_o1 == 1 ~ 1,
                                     TRUE ~ NA_real_),
                   b15_2 = case_when(b15_o1 == 2 | b15_o2 == 2 | b15_o3 == 2 |
                                     b15_o4 == 2 | b15_o5 == 2 | b15_o6 == 2 |
                                     b15_o7 == 2 | b15_o8 == 2 | b15_o9 == 2 |
                                     b15_o10 == 2 | b15_o11 == 2 | b15_o12 == 2 |
                                       b15_o13 == 2 | b15_o14 == 2 | b15_o15 == 2 |
                                       b15_o16 == 2 ~2,
                                     TRUE ~ NA_real_),
                   b15_3 = case_when(b15_o1 == 3 | b15_o2 == 3 | b15_o3 == 3 |
                                       b15_o4 == 3 | b15_o5 == 3 | b15_o6 == 3 |
                                       b15_o7 == 3 | b15_o8 == 3 | b15_o9 == 3 |
                                       b15_o10 == 3 | b15_o11 == 3 | b15_o12 == 3 |
                                       b15_o13 == 3 | b15_o14 == 3 | b15_o15 == 3 |
                                       b15_o16 == 3 ~ 3,
                                     TRUE ~ NA_real_),
                   b15_4 = case_when(b15_o1 == 4 | b15_o2 == 4 | b15_o3 == 4 |
                                       b15_o4 == 4 | b15_o5 == 4 | b15_o6 == 4 |
                                       b15_o7 == 4 | b15_o8 == 4 | b15_o9 == 4 |
                                       b15_o10 == 4 | b15_o11 == 4 | b15_o12 == 4 |
                                       b15_o13 == 4 | b15_o14 == 4 | b15_o15 == 4 |
                                       b15_o16 == 4 ~4,
                                     TRUE ~ NA_real_),
                   b15_5 = case_when(b15_o1 == 5 | b15_o2 == 5 | b15_o3 == 5 |
                                      b15_o4 == 5 | b15_o5 == 5 | b15_o6 == 5 |
                                      b15_o7 == 5 | b15_o8 == 5 | b15_o9 == 5 |
                                      b15_o10 == 5 | b15_o11 == 5 | b15_o12 == 5 |
                                      b15_o13 == 5 | b15_o14 == 5 | b15_o15 == 5 |
                                      b15_o16 == 5 ~ 5,
                                    TRUE ~ NA_real_),
                   b15_6 = case_when(b15_o1 == 6 | b15_o2 == 6 | b15_o3 == 6 |
                                       b15_o4 == 6 | b15_o5 == 6 | b15_o6 == 6 |
                                       b15_o7 == 6 | b15_o8 == 6 | b15_o9 == 6 |
                                       b15_o10 == 6 | b15_o11 == 6 | b15_o12 == 6 |
                                       b15_o13 == 6 | b15_o14 == 6 | b15_o15 == 6 |
                                       b15_o16 == 6 ~6,
                                     TRUE ~ NA_real_),
                   b15_7 = case_when(b15_o1 == 7 | b15_o2 == 7 | b15_o3 == 7 |
                                       b15_o4 == 7 | b15_o5 == 7 | b15_o6 == 7 |
                                       b15_o7 == 7 | b15_o8 == 7 | b15_o9 == 7 |
                                       b15_o10 == 7 | b15_o11 == 7 | b15_o12 == 7 |
                                       b15_o13 == 7 |b15_o14 == 7 | b15_o15 == 7 |
                                       b15_o16 == 7 ~7,
                                     TRUE ~ NA_real_),
                   b15_8 = case_when(b15_o1 == 8 | b15_o2 == 8 | b15_o3 == 8 |
                                       b15_o4 == 8 | b15_o5 == 8 | b15_o6 == 8 |
                                       b15_o7 == 8 | b15_o8 == 8 | b15_o9 == 8 |
                                       b15_o10 == 8 | b15_o11 == 8 | b15_o12 == 8 |
                                       b15_o13 == 8 | b15_o14 == 8 | b15_o15 == 8 |
                                       b15_o16 == 8 ~8,
                                     TRUE ~ NA_real_),
                   b15_9 = case_when(b15_o1 == 9 | b15_o2 == 9 | b15_o3 == 9 |
                                       b15_o4 == 9 | b15_o5 == 9 | b15_o6 == 9 |
                                       b15_o7 == 9 | b15_o8 == 9 | b15_o9 == 9 |
                                       b15_o10 == 9 | b15_o11 == 9 | b15_o12 == 9 |
                                       b15_o13 == 9 | b15_o14 == 9 | b15_o15 == 9 |
                                       b15_o16 == 9 ~9,
                                     TRUE ~ NA_real_),
                   b15_10 = case_when(b15_o1 == 10 | b15_o2 == 10 | b15_o3 == 10 |
                                        b15_o4 == 10 | b15_o5 == 10 | b15_o6 == 10 |
                                        b15_o7 == 10 | b15_o8 == 10 | b15_o9 == 10 |
                                        b15_o10 == 10 | b15_o11 == 10 | b15_o12 == 10 |
                                        b15_o13 == 10 | b15_o14 == 10 | b15_o15 == 10 |
                                        b15_o16 == 10 ~10,
                                      TRUE ~ NA_real_),
                   b15_11 = case_when(b15_o1 == 11 | b15_o2 == 11 | b15_o3 == 11 |
                                        b15_o4 == 11 | b15_o5 == 11 | b15_o6 == 11 |
                                        b15_o7 == 11 | b15_o8 == 11 | b15_o9 == 11 |
                                        b15_o10 == 11 | b15_o11 == 11 | b15_o12 == 11 |
                                        b15_o13 == 11 | b15_o14 == 11 | b15_o15 == 11 |
                                        b15_o16 == 11 ~11,
                                      TRUE ~ NA_real_),
                   b15_12 = case_when(b15_o1 == 12 | b15_o2 == 12 | b15_o3 == 12 |
                                        b15_o4 == 12 | b15_o5 == 12 | b15_o6 == 12 |
                                        b15_o7 == 12 | b15_o8 == 12 | b15_o9 == 12 |
                                        b15_o10 == 12 | b15_o11 == 12 | b15_o12 == 12 |
                                        b15_o13 == 12 | b15_o14 == 12 | b15_o15 == 12 |
                                        b15_o16 == 12 ~12,
                                      TRUE ~ NA_real_),
                   b15_13 = case_when(b15_o1 == 13 | b15_o2 == 13 | b15_o3 == 13 |
                                        b15_o4 == 13 | b15_o5 == 13 | b15_o6 == 13 |
                                        b15_o7 == 13 | b15_o8 == 13 | b15_o9 == 13 |
                                        b15_o10 == 13 | b15_o11 == 13 | b15_o12 == 13 |
                                        b15_o13 == 13 | b15_o14 == 13 | b15_o15 == 13 |
                                        b15_o16 == 13 ~13,
                                      TRUE ~ NA_real_),
                   b15_14 = case_when(b15_o1 == 14 | b15_o2 == 14 | b15_o3 == 14 |
                                        b15_o4 == 14 | b15_o5 == 14 | b15_o6 == 14 |
                                        b15_o7 == 14 | b15_o8 == 14 | b15_o9 == 14 |
                                        b15_o10 == 14 | b15_o11 == 14 | b15_o12 == 14 |
                                        b15_o13 == 14 | b15_o14 == 14 | b15_o15 == 14 |
                                        b15_o16 == 14 ~14,
                                      TRUE ~ NA_real_),
                   b15_15 = case_when(b15_o1 == 15 | b15_o2 == 15 | b15_o3 == 15 |
                                        b15_o4 == 15 | b15_o5 == 15 | b15_o6 == 15 |
                                        b15_o7 == 15 | b15_o8 == 15 | b15_o9 == 15 |
                                        b15_o10 == 15 | b15_o11 == 15 | b15_o12 == 15 |
                                        b15_o13 == 15 | b15_o14 == 15 | b15_o15 == 15 |
                                        b15_o16 == 15 ~15,
                                      TRUE ~ NA_real_)) %>% 
  select(anio, gsex, comuna,b01_t1:b10_t19, b13:b14_t8, 
         b15_0:b15_15, everything()) %>% select(-(b15_o1:b15_o16)) 


ECV2018 <- ECV2018 %>% mutate(c4_1 = case_when(c04_o1 == 1 | c04_o2 == 1 |
                                                 c04_o3 == 1 | c04_o4 == 1 | 
                                                 c04_o5 == 1 | c04_o6 == 1 |
                                                 c04_o7 == 1 | c04_o8 == 1 |
                                                 c04_o9 == 1 | c04_o10 == 1 ~ 1,
                                               TRUE ~ NA_real_),
                              c4_2 = case_when(c04_o1 == 2 | c04_o2 == 2 |
                                                 c04_o3 == 2 | c04_o4 == 2 | 
                                                 c04_o5 == 2 | c04_o6 == 2 |
                                                 c04_o7 == 2 | c04_o8 == 2 |
                                                 c04_o9 == 2 | c04_o10 == 2 ~ 2,
                                               TRUE ~ NA_real_),
                              c4_3 = case_when(c04_o1 == 3 | c04_o2 == 3 |
                                                 c04_o3 == 3 | c04_o4 == 3 | 
                                                 c04_o5 == 3 | c04_o6 == 3 |
                                                 c04_o7 == 3 | c04_o8 == 3 |
                                                 c04_o9 == 3 | c04_o10 == 3 ~ 3,
                                               TRUE ~ NA_real_),
                              c4_4 = case_when(c04_o1 == 4 | c04_o2 == 4 |
                                                 c04_o3 == 4 | c04_o4 == 4 | 
                                                 c04_o5 == 4 | c04_o6 == 4 |
                                                 c04_o7 == 4 | c04_o8 == 4 |
                                                 c04_o9 == 4 | c04_o10 == 4 ~ 4,
                                               TRUE ~ NA_real_),
                              c4_5 = case_when(c04_o1 == 5 | c04_o2 == 5 |
                                                 c04_o3 == 5 | c04_o4 == 5 | 
                                                 c04_o5 == 5 | c04_o6 == 5 |
                                                 c04_o7 == 5 | c04_o8 == 5 |
                                                 c04_o9 == 5 | c04_o10 == 5 ~ 5,
                                               TRUE ~ NA_real_),
                              c4_6 = case_when(c04_o1 == 6 | c04_o2 == 6 |
                                                 c04_o3 == 6 | c04_o4 == 6 | 
                                                 c04_o5 == 6 | c04_o6 == 6 |
                                                 c04_o7 == 6 | c04_o8 == 6 |
                                                 c04_o9 == 6 | c04_o10 == 6 ~ 6,
                                               TRUE ~ NA_real_),
                              c4_7 = case_when(c04_o1 == 7 | c04_o2 == 7 |
                                                 c04_o3 == 7 | c04_o4 == 7 | 
                                                 c04_o5 == 7 | c04_o6 == 7 |
                                                 c04_o7 == 7 | c04_o8 == 7 |
                                                 c04_o9 == 7 | c04_o10 == 7 ~ 7,
                                               TRUE ~ NA_real_),
                              c4_8 = case_when(c04_o1 == 8 | c04_o2 == 8 |
                                                 c04_o3 == 8 | c04_o4 == 8 | 
                                                 c04_o5 == 8 | c04_o6 == 8 |
                                                 c04_o7 == 8 | c04_o8 == 8 |
                                                 c04_o9 == 8 | c04_o10 == 8 ~ 8,
                                               TRUE ~ NA_real_),
                              c4_10 = case_when(c04_o1 == 9 | c04_o2 == 9 |
                                                 c04_o3 == 9 | c04_o4 == 9 | 
                                                 c04_o5 == 9 | c04_o6 == 9 |
                                                 c04_o7 == 9 | c04_o8 == 9 |
                                                 c04_o9 == 9 | c04_o10 == 9 ~ 10,
                                               TRUE ~ NA_real_),
                              c4_99 = case_when(c04_o1 == 99 | c04_o2 == 99 |
                                                  c04_o3 == 99 | c04_o4 == 99 | 
                                                  c04_o5 == 99 | c04_o6 == 99 |
                                                  c04_o7 == 99 | c04_o8 == 99 |
                                                  c04_o9 == 99 | c04_o10 == 99 ~ 99,
                                                TRUE ~ NA_real_)) %>% 
  select(anio, gsex,comuna, b01_t1:c03_t3, c4_1:c4_99, everything()) %>% 
  select(-(c04_o1:c04_o10))

ECV2018[,130:133][ECV2018[, 130:133] == 0] <- NA




ECV2019 <- ECV2019 %>% add_column(anio = 2019) %>% 
  select(anio, GSEX , COMUNA ,B1_1:B11_16,
         B14:B17_16,
         C1_1:C9_2,
         D1:D2_5, 
         PONDERAD, Latitude, Longitud) %>% select(-c(C5_1:C5_99)) %>% 
  mutate(C4_2 = ifelse(is.na(C4_2), 0, C4_2),
         C4_9 = ifelse(is.na(C4_9), 0, C4_9)) %>% 
  mutate(C4_2 = case_when(c(C4_2 == 0 & C4_9 == 9) ~ 2,
                          c(C4_2 == 2 & C4_9 == 9) ~ 2,
                          c(C4_2 == 2 & C4_9 == 0) ~ 2,
                          c(C4_2 == 0 & C4_9 == 0) ~ 0)) %>% 
  select(-C4_9) %>% mutate(C4_2 = replace(C4_2, C4_2 == 0, NA)) %>% 
  clean_names() %>% 
  mutate(b9_99 = case_when(b9_99 == 9 ~ 99,
                           TRUE ~ NA_real_),
         b16_11 = case_when(b16_11 == 1 ~ 11,
                            TRUE ~ NA_real_),
         b16_12 = case_when(b16_12 == 1 ~ 12,
                            TRUE ~ NA_real_),
         b16_13 = case_when(b16_13 == 1 ~ 13,
                            TRUE ~ NA_real_),
         b16_14 = case_when(b16_14 == 1 ~ 14,
                            TRUE ~ NA_real_),
         b16_15 = case_when(b16_15 == 1 ~ 15,
                            TRUE ~ NA_real_)) %>% 
  select(-c6_4)
ECV2019 <-  ECV2019 %>% rename(latitud = latitude)
ECV2021 <- ECV2021 %>% add_column(anio = 2021) %>% 
  select(anio, GSEX ,COMUNA , B1_1:B11_16,
         B14:B17_16,
         C1_1:C9_2,
         D1:D3_4, 
         PONDERAD, Latitud, Longitud) %>% select(-c(C5_1:C5_99)) %>% 
  mutate(C4_2 = ifelse(is.na(C4_2), 0, C4_2),
         C4_9 = ifelse(is.na(C4_9), 0, C4_9)) %>% 
  mutate(C4_2 = case_when(c(C4_2 == 0 & C4_9 == 9) ~ 2,
                          c(C4_2 == 2 & C4_9 == 9) ~ 2,
                          c(C4_2 == 2 & C4_9 == 0) ~ 2,
                          c(C4_2 == 0 & C4_9 == 0) ~ 0)) %>% 
  select(-C4_9) %>% mutate(C4_2 = replace(C4_2, C4_2 == 0, NA)) %>% clean_names() %>% 
  select(-c6_4) %>% select(-c(d3_1:d3_4))



tabla_2 <- ECV2019 %>% add_column(anio = 2019) %>% 
  select(anio, COMUNA ,B1_1:B11_16,
         B14:B17_16,
         C1_1:C9_2,
         D1:D2_5, 
         PONDERAD) %>% select(-c(C5_1:C5_99)) %>% 
  mutate(C4_2 = ifelse(is.na(C4_2), 0, C4_2),
         C4_9 = ifelse(is.na(C4_9), 0, C4_9)) %>% 
  mutate(C4_2_nueva = case_when(c(C4_2 == 0 & C4_9 == 9) ~ 2,
                          c(C4_2 == 2 & C4_9 == 9) ~ 2,
                          c(C4_2 == 2 & C4_9 == 0) ~ 2,
                          c(C4_2 == 0 & C4_9 == 0) ~ 0)) %>% select(C4_2, C4_9, C4_2_nueva)
tabla_2 <- tabla_2 %>% add_column(numero = row.names(tabla_2)) %>% 
  select(numero, C4_2, C4_9, C4_2_nueva)
write.table(tabla_2, file = "tabla2.txt", sep = ",", 
            quote = FALSE, row.names = F)

# Tabla 3 

tabla_3 <- ECV20182 %>% select(B08_O1:B08_O6) %>% arrange(-desc(B08_O3)) %>% head(1)

# Tabla 4

tabla_4 <- ECV2018 %>% select(b9_1:b9_99) %>% arrange(-desc(b9_5)) %>% head(2)

write.table(tabla_4, file = "tabla4.txt", sep = ",",
            quote = F, row.names = F)
# Después de toda esta limpieza papu se pueden unir las bases de datos

ECV_18_21 <- rbind(ECV2018, ECV2019, ECV2021)

save(ECV_18_21, file = "base_consolidada_18_21.RData")


NA2018 <- ECV2018 %>% map_dbl(~sum(is.na(.)))
NA2019 <- ECV2019 %>% map_dbl(~sum(is.na(.)))
NA2021 <- ECV2021 %>% map_dbl(~sum(is.na(.)))

data.frame(NA2018,NA2019,NA2021)

NA2018


# Estadisticas descriptivas -----------------------------------------------

# Salud y educacion: grafico y tablas 

ECV_18_21 <- ECV_18_21 %>% filter(comuna == 3 | comuna == 6 | comuna == 7 | comuna == 8 | 
                       comuna == 9 | comuna == 10)

ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                                     gsex == 3 ~ "C3",
                                                     gsex == 4 ~ "D",
                                                     gsex == 5 ~ "E",
                                                     gsex == 99 ~ "NS")) %>% 
  mutate(b2 = case_when(b2 == 1 ~ "Atendido",
                        b2 == 2 | b2 == 99 ~ "No se ha atendido")) %>% 
  mutate(b3 = case_when(b3 == 1 ~ "Sistema publico",
                        b3 == 2 ~ "Sistema Privado",
                        b3 == 3 ~ "Ambas")) %>% 
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(promedio = rowMeans(.[,11:21], na.rm = T))  %>% 
  summarise(total = n(),
            promedio = mean(promedio, na.rm = T)) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 1))) 



ggplot(data = tabla_año, aes(x = as.character(anio),
                             y = promedio,
                             fill = b3)) +
  geom_bar(stat = "identity", width = 0.5,
           position=position_dodge(width = 0.9)) +
  coord_cartesian(ylim = c(1, 7)) +
  geom_text(aes(label= promedio), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x = "años",
       y = "Promedio valorización salud",
       title = "Gráfico 1:Evolución del promedio de la nota a los distintos aspectos 
       de los servicios de salud por año",
       caption = "Fuente:elaborado a partir de las Encuestas de calidad de vida 2018, 2019 y 2021"
       ) +
  scale_fill_discrete(labels = c("Ambos sistemas",
                                 "Sistema Privado",
                                 "Sistema Público",
                                 "No se ha atendido")) + 
  theme_light()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="lightgrey") ) 
  
  
  


write.table(tabla_5, file = "tabla5.txt", sep = ",", 
            quote = FALSE, row.names = F)

ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>% 
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(promedio = rowMeans(.[,41:48], na.rm = T)) %>% 
  summarise(total = n(),
            promedio = mean(promedio, na.rm = T)) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 1))) 

tabla_ed_com <- ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>% filter(gsex != 'NS') %>% 
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(calidad_ed = rowMeans(.[c("b10_2", "b10_4", "b10_6")], na.rm = T)) %>% 
  select(anio,gsex, calidad_ed, b10_8) %>% group_by(anio, gsex) %>% 
  summarise(promedio_ed_med = mean(calidad_ed, na.rm = T),
            promedio_ed_sup = mean(b10_8, na.rm = T)) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 1)))  
library(reshape2) 

data_melt = melt(tabla_ed_com, id.vars = c('anio', 'gsex'))

ggplot(data = data_melt, aes(x = gsex, y = value,
                             group = variable, fill = variable)) +
  geom_bar(stat = "identity", width = 0.5,
           position=position_dodge(width = 0.9)) +
  geom_text(aes(label= value),position=position_dodge(width=0.9), vjust=-0.25) +
  facet_grid(. ~ anio) + coord_cartesian(ylim = c(1, 7)) +
  labs(x = 'Estrato Socioeconómico',
       y = 'Promedio de nota educación',
       title = 'Nota educación media y superior',
       subtitle = 'Por año y GSE',
       caption = 'Encuestas de calidad de vida 2018, 2019 y 2021') +
  scale_fill_manual(labels = c('Educación Media', 'Educación Superior'),
                    values = c('sky blue', '#008080')) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))
  
  
  



write.table(tabla_ed_com, file = "tablaedcom.txt", sep = ",", 
            quote = FALSE, row.names = F)
# Delitos

tabla_delitos <- ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                                       gsex == 3 ~ "C3",
                                                       gsex == 4 ~ "D",
                                                       gsex == 5 ~ "E",
                                                       gsex == 99 ~ "NS")) %>% 
  mutate_at(88, ~replace_na(.,1)) %>% 
  filter(b16_14 == 1) %>% mutate_at(89, ~replace_na(.,1)) %>% 
  filter(b16_15 == 1) %>% 
  mutate_at(74, ~replace_na(.,1))  %>% mutate(b16_0 = case_when(b16_0 == 1 ~ "Sí",
                                                                b16_0 == 0 ~ "No")) %>% 
  group_by(anio, gsex, b16_0) %>% summarise(n = n()) %>% 
  mutate(freq = round(n / sum(n), digits = 2)) %>% filter(b16_0 == "Sí")  
 
ggplot(data = tabla_delitos, aes(x = as.character(gsex),
             y = freq,
             fill = b16_0)) +geom_bar(stat = "identity", width = 0.5,
                                     position=position_dodge(width = 0.9)
                                     ) +
     ylim(c(0,1)) +
geom_text(aes(label= freq*100), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x = "Año",
       y = "porcentaje",
       title = "Gráfico 3: proporción de personas que han sufrido
       un delito frente a los que no",
       caption = "Encuestas de calidad de vida 2018, 2019 y 2021") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="lightgrey") ) +
  facet_wrap(~anio)

srvyr::row
  

tabla_peligrosidad <- ECV_18_21%>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>% 
  mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                    b17_10 == 4 | b17_11 == 4 |
                                    b17_10 == 3 | b17_11 == 3 |
                                    b17_7 == 4 ~ "zona roja",
                                  b17_9 == 4 | b17_9 == 3 |
                                    b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                  | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                  TRUE ~ "zona verde")) %>% 
  group_by(anio, peligrosidad) %>% 
  summarise(n = n()) %>% mutate(freq = round(n / sum(n), digits = 2))

  
ggplot(data = tabla_peligrosidad, aes(x = as.character(anio),
                                 y = freq,
                                 fill = peligrosidad)) +geom_bar(stat = "identity", width = 0.5,
                                                          position=position_dodge(width = 0.9)
                                 ) +
  ylim(c(0,1)) +
  geom_text(aes(label= freq*100), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x = "Año",
       y = "porcentaje",
       title = "Percepción de la cantidad de delitos
       por sus habitantes",
       caption = "Encuestas de calidad de vida 2018, 2019 y 2021") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="lightgrey") ) +
  scale_fill_manual(values = c("gold1", "firebrick2", "forestgreen"))

tabla_trans <- ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>% 
  mutate(calidad_trans = rowMeans(.[,134:141], na.rm = T)) %>% group_by(anio, gsex) %>% 
  summarise(promedio = mean(calidad_trans)) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 1)))
write.table(tabla_trans, file = "tablatrans.txt", sep = ",", 
            quote = FALSE, row.names = F)
  
tabla <- ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>% 
  mutate(d1 = case_when(d1 == 1 ~ "Muy infeliz",
                        d1 == 2 ~ "Poco infeliz",
                        d1 == 3 ~ "Bastante",
                        d1 == 4 ~ "Mucho",
                        d1 == 5 ~ "Muy feliz",
                        d1 == 98 | d1 == 99 ~ "No sabe/No responde")) %>% 
  group_by(anio, d1) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n/sum(n), digits = 3)) 
  
melt_felicidad = melt(tabla, id.vars = c('anio', 'gsex'))

write.table(tabla, file = "tablaF.txt", sep = ",",
            quote = F, row.names = F)

ggplot(data = tabla, aes(x = as.character(anio),
                         y = freq,
                         fill = factor(d1, levels = c("Muy infeliz",
                                                      "Poco infeliz",
                                                      "Bastante",
                                                      "Mucho",
                                                      "Muy feliz",
                                                      "No sabe/No responde"))))+ geom_bar(stat = "identity", width = 0.5,
                                               position=position_dodge(width = 0.9)
                         )+
  ylim(c(0,1)) +
  geom_text(aes(label= freq*100), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x = "Año",
       y = "porcentaje",
       title = "Percepción de felicidad individual por año",
       caption = "Encuestas de calidad de vida 2018, 2019 y 2021") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="lightgrey") ) 

tabla_gse_anio <- ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>% 
  group_by(anio, gsex) %>% 
  summarise(total = n()) %>% 
  mutate(freq = round(total/sum(total)*100, digits = 2))

write.table(tabla_gse_anio, file = "tabla_Gse_anio.txt", sep = ",",
            quote = F, row.names = F)


# Tabla 2021 --------------------------------------------------------------
tabla_d3_4 <- ECV2021 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                    gsex == 3 ~ "C3",
                                    gsex == 4 ~ "D",
                                    gsex == 5 ~ "E",
                                    gsex == 99 ~ "NS")) %>% 
  mutate(d3_1 = case_when(d3_1 == 5 | d3_1 == 4 ~ "Nuevos espacios de diálogo",
                          d3_1 == 3 ~ "Neutro",
                          d3_1 == 1 | d3_1 == 2 ~ "No hay nuevos espacios",
                          TRUE ~ "No aplica"),
         d3_2 = case_when(d3_2 == 5 | d3_2 == 4 ~ "Más discusiones",
                          d3_2 == 3 ~ "Neutro",
                          d3_2 == 1 | d3_2 == 2 ~ "Menos discusiones",
                          TRUE ~ "No aplica"),
         d3_3 = case_when(d3_3 == 5 | d3_3 == 4 ~ "Más acercamiento familiar",
                          d3_3 == 3 ~ "Neutro",
                          d3_3 == 1 | d3_3 == 2 ~ "No se han acercado",
                          TRUE ~ "No aplica"),
         d3_4 = case_when(d3_4 == 5 | d3_4 == 4 ~ "Incertidumbre",
                          d3_4 == 3 ~ "Neutro",
                          d3_4 == 1 | d3_4 == 2 ~ "No hay incertidumbre",
                          TRUE ~ "No aplica")) %>% 
  group_by(d3_4) %>% 
  summarise(total = n()) %>% 
  mutate(freq = round(total/sum(total)*100, digits = 2)) 


  write.table(tabla_d3_4, file = "tabla_d3_gse.txt", sep = ",",
            quote = F, row.names = F)

  