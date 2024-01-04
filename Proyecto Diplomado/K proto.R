require(dplyr)
require(ggplot2)
require(caret)
library(cluster) 
library(factoextra)
require(clustMixType)
library(gridExtra)
ECV_18_21 = ECV_18_21 %>% filter(comuna == 6 | comuna == 7 | comuna == 8 |
                                   comuna == 9)

prueba = ECV_18_21 %>% mutate(b5 = case_when(b5_1 == 1 |
                                               b5_2 == 1 |
                                               b5_3 == 1 ~ 1,
                                             TRUE ~ 2)) %>% 
  mutate(b8 = case_when(b8 > 0 ~ 1,
                        TRUE ~ 0))%>% select(c(anio, gsex, comuna, b5, b8, b2,d1 , b1_1:b1_4, b4_1:b4_12, b10_1:b10_8, 
                       b11_1:b15_8, b17_1:c2_4, c7_1:c7_8,
                                d1:d2_5)) 
prueba = prueba %>% mutate(b2 = as.factor(b2),
                           b5 = as.factor(b5),
                           b8 = as.factor(b8),
                           d1 = as.factor(d1)) 
prueba$b14[prueba$b14 == 9] <- 99

glimpse(prueba)
?mutate

preprocess_data <- preProcess(prueba[7:97], method = c('center','scale'))
prueba_scaled <- predict(preprocess_data, prueba) 
prueba_scaled <- na.omit(prueba_scaled)

tw <- c()
for(k in 1:15){
  model <-  kproto(x = prueba_scaled[,4:97], k = k, verbose = FALSE)
  tw[k] <- model$tot.withinss
}

ggplot(data = data.frame(k = c(1:15), TW = tw), aes(x = k, y = TW)) +
  geom_line(color = 'blue') + 
  geom_point(color = 'blue') + 
  theme_bw() +
  xlab('k') +
  ylab('Total Within SS')
fviz_nbclust(x = prueba_scaled[,4:97], kproto, method = 'silhouette')
?fviz_nbclust
  k_prototype_6 = kproto(x = prueba_scaled[,4:97], k = 6)

prueba$label = k_prototype$cluster


prueba %>%  mutate(calidad_trans = rowMeans(.[,84:91], na.rm = T)) %>% 
  group_by(label) %>% 
  summarise(promedio = mean(calidad_trans))

prueba %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                   gsex == 3 ~ "C3",
                                   gsex == 4 ~ "D",
                                   gsex == 5 ~ "E",
                                   gsex == 99 ~ "NS")) %>% 
  group_by(anio, label) %>% 
  summarise(n = n()) 

install.packages('ggmap')
library(ggmap)
ECV2021 = ECV2021 %>% filter(comuna == 6 | comuna == 7 | comuna == 8 |
                               comuna == 9) %>% mutate(longitud = as.numeric(longitud),
                                                       latitud = as.numeric(latitud))


mapa = get_map(get_googlemap(center = c(mean(ECV2021$latitud), mean(ECV2021$longitud)),
                 zoom = 10))
ggmap::register_google(key = 'AIzaSyBU8bLxVef91kRVibtroypCAGzzTZHGL9I',
                       write = T)


Chile = geocode('viña del mar')

mapa_general = ggmap(get_map(Chile, maptype = 'roadmap', zoom = 11)) #NO EJEcutaR
ggmap(mapa_general)
mapa_vi_vap = ggmap(get_map(Chile, maptype = 'roadmap', zoom = 12))
mapa_vi_vap
?geocode

# Anio 2021 ---------------------------------------------------------------
ECV2021$b14[ECV2021$b14 == 9] <- 99

ECV2021 = ECV2021 %>% filter(comuna == 6 | comuna == 7 | comuna == 8 |
                               comuna == 9)

ECV2021 = ECV2021 %>% mutate(b5 = case_when(b5_1 == 1 |
                                              b5_2 == 1 |
                                              b5_3 == 1 ~ 1,
                                            TRUE ~ 2)) %>% 
  mutate(b8 = case_when(b8 > 0 ~ 1,
                        TRUE ~ 0))%>% select(c(anio, gsex, comuna,latitud, longitud,b15_1:b15_8,b17_1:b17_16,
                                               d1 ,b5, b8, b2,
                                               b15_1:b15_8,b17_1:b17_16,
                                               b1_1:b1_4, b4_1:b4_12, b10_1:b10_8, 
                                               b11_1:b14, c1_1:c2_4, c7_1:c7_8,
                                               d2_1:d2_5)) 
 ECV2021 = ECV2021 %>% mutate(b2 = as.factor(b2),
                              b5 = as.factor(b5),
                              b8 = as.factor(b8),
                              d1 = as.factor(d1)) 

preproceso_2021 <- preProcess(ECV2021[34:99], method = c('center','scale'))
ECV2021_scaled <- predict(preproceso_2021, ECV2021) 
ECV2021_scaled <- na.omit(ECV2021_scaled)

tw <- c()
for(k in 1:15){
  model <-  kproto(x = ECV2021_scaled[,30:99], k = k, verbose = FALSE)
  tw[k] <- model$tot.withinss
}

ggplot(data = data.frame(k = c(1:15), TW = tw), aes(x = k, y = TW)) +
  geom_line(color = 'blue') + 
  geom_point(color = 'blue') + 
  theme_bw() +
  xlab('k') +
  ylab('Total Within SS')

k_prototype_2021 = kproto(x = ECV2021_scaled[,30:99], k  = 4)

ECV2021$etiqueta = k_prototype_2021$cluster

ECV2021 = ECV2021 %>% mutate(etiqueta = as.factor(etiqueta)) %>% 
  mutate(longitud = as.numeric(longitud),
         latitud = as.numeric(latitud))


saludmental_general_2021 = mapa_general+ geom_point(data = ECV2021,
                         aes(x = longitud, y = latitud,
                             color = d1),
                         size = .5)



mapa_general+ geom_point(data = ECV2021,
                         aes(x = longitud, y = latitud,
                             color = etiqueta),
                         size = 1)

ECV2021 %>% mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(across(where(is.numeric), ~na_if(., 98))) %>% 
  mutate(promedio = rowMeans(.[,10:99], na.rm = T))  %>% group_by(etiqueta) %>% 
  summarise(total = n(),
            promedio = mean(promedio, na.rm = T)) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 1))) %>% print(n = 28)


ECV2021 %>% mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                              b17_10 == 4 | b17_11 == 4 |
                                              b17_10 == 3 | b17_11 == 3 |
                                              b17_7 == 4 ~ "zona roja",
                                            b17_9 == 4 | b17_9 == 3 |
                                              b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                            | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                            TRUE ~ "zona verde")) %>% group_by(etiqueta, peligrosidad) %>% 
  summarise(n = n())
#Grupo 1 Posee mas en la zona roja. Ningún grupo posee harto verde y poco rojo,
#a diferencia del 2019]

mapa_clusters_21 = mapa_general+ geom_point(data = ECV2021,
                                            aes(x = longitud, y = latitud,
                                                color = etiqueta),
                                            size = 1) +
  labs(x = 'Longitud',
       y = 'Latitud',
       title = 'Ubicación espacial por cluster',
       subtitle = 'Año 2021',
       caption = "ECV 2021",
       color = "Cluster") +
  theme_classic()
  
  
mapa_delitos_21 = mapa_general + geom_point(data = ECV2021 %>% mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                                                               b17_10 == 4 | b17_11 == 4 |
                                                                               b17_10 == 3 | b17_11 == 3 |
                                                                               b17_7 == 4 ~ "zona roja",
                                                                             b17_9 == 4 | b17_9 == 3 |
                                                                               b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                                                             | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                                                             TRUE ~ "zona verde")) %>% filter(peligrosidad == 'zona roja'),
                          aes(x = longitud, y = latitud,
                              color = etiqueta),
                          size = 1) +
  labs(x = 'Longitud',
       y = 'Latitud',
       title = 'Personas que han observado con alta frecuencia 
       DMCS por cluster',
       subtitle = 'Año 2021',
       caption = "ECV 2021",
       color = "Cluster") +
  theme_classic()

ECV2021 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                    gsex == 3 ~ "C3",
                                    gsex == 4 ~ "D",
                                    gsex == 5 ~ "E",
                                    gsex == 99 ~ "NS")) %>% filter(etiqueta == 2 | etiqueta == 3) %>% group_by(etiqueta, gsex) %>% 
  summarise(n = n())

ECV2021 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                    gsex == 3 ~ "C3",
                                    gsex == 4 ~ "D",
                                    gsex == 5 ~ "E",
                                    gsex == 99 ~ "NS")) %>% filter(etiqueta == 1 | etiqueta == 4) %>% group_by(etiqueta, gsex) %>% 
  summarise(n = n())
tabla_etiqueta_2021 = ECV2021 %>% mutate(etiqueta = as.numeric(etiqueta)) %>% 
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(across(where(is.numeric), ~na_if(., 98))) %>% select_if(is.numeric) %>% 
  group_by(etiqueta) %>% 
  summarise_all(mean, na.rm = T) 
# 2019 solo --------------------------------------------------------------

ECV2019$b14[ECV2019$b14 == 9] <- 99

ECV2019 = ECV2019 %>% filter(comuna == 6 | comuna == 7 | comuna == 8 |
                               comuna == 9)
ECV2019 = ECV2019  %>% mutate(b5 = case_when(b5_1 == 1 |
                                                  b5_2 == 1 |
                                                  b5_3 == 1 ~ 1,
                                                TRUE ~ 2)) %>% 
  mutate(b8 = case_when(b8 > 0 ~ 1,
                        TRUE ~ 0))%>% select(c(anio, gsex, comuna,latitud, longitud,b15_1:b15_8,b17_1:b17_16,
                                               d1 ,b5, b8, b2,
                                               b15_1:b15_8,b17_1:b17_16,
                                               b1_1:b1_4, b4_1:b4_12, b10_1:b10_8, 
                                               b11_1:b14, c1_1:c2_4, c7_1:c7_8,
                                               d2_1:d2_5)) 

ECV2019 = ECV2019 %>%mutate(b2 = as.factor(b2),
                                 b5 = as.factor(b5),
                                 b8 = as.factor(b8),
                                 d1 = as.factor(d1)) 

preproceso_2019 <- preProcess(ECV2019[34:99], method = c('center','scale'))
ECV2019_scaled <- predict(preproceso_2019, ECV2019) 
ECV2019_scaled <- na.omit(ECV2019_scaled)



tw <- c()
for(k in 1:15){
  model <-  kproto(x = ECV2019_scaled[,30:99], k = k, verbose = FALSE)
  tw[k] <- model$tot.withinss
}

ggplot(data = data.frame(k = c(1:15), TW = tw), aes(x = k, y = TW)) +
  geom_line(color = 'blue') + 
  geom_point(color = 'blue') + 
  theme_bw() +
  xlab('k') +
  ylab('Total Within SS')

kproto_2019 = kproto(ECV2019_scaled[,30:99], k = 4)

ECV2019$etiqueta = kproto_2019$cluster

ECV2019 = ECV2019 %>% mutate(etiqueta = as.factor(etiqueta))

mapa_general + geom_point(data = ECV2019,
                         aes(x = longitud, y = latitud,
                             color = etiqueta),
                         size = 1)
ECV2019 %>% group_by(etiqueta) %>% 
  summarise(n = n())

ECV2021 %>% group_by(etiqueta) %>% 
  summarise(n = n())

tabla_etiqueta = ECV2019 %>% mutate(etiqueta = as.numeric(etiqueta)) %>% 
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(across(where(is.numeric), ~na_if(., 98))) %>% select_if(is.numeric) %>% 
  group_by(etiqueta) %>% 
  summarise_all(mean, na.rm = T) 

ECV2019 %>% mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                              b17_10 == 4 | b17_11 == 4 |
                                              b17_10 == 3 | b17_11 == 3 |
                                              b17_7 == 4 ~ "zona roja",
                                            b17_9 == 4 | b17_9 == 3 |
                                              b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                            | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                            TRUE ~ "zona verde")) %>% 
  group_by(etiqueta, peligrosidad) %>% summarise(n = n())


ECV2019 %>% mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                              b17_10 == 4 | b17_11 == 4 |
                                              b17_10 == 3 | b17_11 == 3 |
                                              b17_7 == 4 ~ "zona roja",
                                            b17_9 == 4 | b17_9 == 3 |
                                              b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                            | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                            TRUE ~ "zona verde")) %>% group_by(peligrosidad) %>% 
  summarise(n = n())

mapa_delitos_19 = mapa_general + geom_point(data = ECV2019 %>% mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                                                               b17_10 == 4 | b17_11 == 4 |
                                                                               b17_10 == 3 | b17_11 == 3 |
                                                                               b17_7 == 4 ~ "zona roja",
                                                                             b17_9 == 4 | b17_9 == 3 |
                                                                               b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                                                             | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                                                             TRUE ~ "zona verde")) %>% filter(peligrosidad == 'zona roja'),
                          aes(x = longitud, y = latitud,
                              color = etiqueta),
                          size = 1) +
  labs(x = 'Longitud',
       y = 'Latitud',
       title = 'Personas que han observado con alta frecuencia 
       DMCS por cluster',
       subtitle = 'Año 2019',
       caption = "ECV 2019",
       color = "Cluster") +
  theme_classic()

mapa_clusters_19 = mapa_general+ geom_point(data = ECV2019,
                         aes(x = longitud, y = latitud,
                             color = etiqueta),
                         size = 1) +
  labs(x = 'Longitud',
       y = 'Latitud',
       title = 'Ubicación espacial por cluster',
       subtitle = 'Año 2019',
       caption = "ECV 2019",
       color = "Cluster") +
  theme_classic()

ECV2019 %>% filter(etiqueta == 1) %>% 
  group_by(d1) %>% 
  summarise(n = n())

gridExtra::grid.arrange(mapa_delitos_19, mapa_delitos_21, nrow = 1)

gridExtra::grid.arrange(mapa_clusters_19, mapa_clusters_21, nrow = 1)
ECV2019 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                    gsex == 3 ~ "C3",
                                    gsex == 4 ~ "D",
                                    gsex == 5 ~ "E",
                                    gsex == 99 ~ "NS")) %>% filter(etiqueta == 2 | etiqueta == 3) %>% group_by(etiqueta, gsex) %>% 
  summarise(n = n())

ECV2019 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                    gsex == 3 ~ "C3",
                                    gsex == 4 ~ "D",
                                    gsex == 5 ~ "E",
                                    gsex == 99 ~ "NS")) %>% filter(etiqueta == 1 | etiqueta == 4) %>% group_by(etiqueta, gsex) %>% 
  summarise(n = n())

ECV2021 %>% mutate(comuna = case_when(comuna == 6 ~ "Concon",
                                      comuna == 7 ~ 'Valparaiso',
                                      comuna == 8 ~ 'Viña del Mar',
                                      comuna == 9 ~ 'Quilpué')) %>% group_by(etiqueta, comuna) %>% 
  summarise(n = n())

tabla_promedio_general_2019 = tabla_etiqueta %>%  mutate(Servicios_Basicos = rowMeans(.[c("b1_1", "b1_2", "b1_3", 'b1_4')]),
                           Salud = rowMeans(.[,34:46]),
                           Educacion_dis = rowMeans(.[c('b10_1', 'b10_3', 'b10_5',
                                                        'b10_7')]),
                           Educacion_cal = rowMeans(.[c('b10_2', 'b10_4', 'b10_6',
                                                        'b10_8')]),
                           Seguridad = rowMeans(.[55:57]),
                           Plan_cuadrante = b14,
                           ciudad = rowMeans(.[72:79]),
                           ciudad_2 = rowMeans(.[80:83]),
                           calidad_transporte = rowMeans(.[c('c7_1', 'c7_4', 'c7_8')]),
                           precio_transporte = rowMeans(.[c('c7_3', 'c7_7')]),
                           frecuencia_transporte = rowMeans(.[c('c7_2', 'c7_5', 'c7_6')]),
                           Satisfaccion_vida = rowMeans(.[92:96])) %>% 
  select(etiqueta, Servicios_Basicos,Salud, Educacion_dis, Educacion_cal,
         Seguridad, Plan_cuadrante, ciudad, ciudad_2, calidad_transporte,
         precio_transporte, frecuencia_transporte, Satisfaccion_vida) %>% 
  mutate_all(~round(.,1)) 

tabla_promedio_general_2019 

tabla_promedio_general_2021 = tabla_etiqueta_2021%>%  mutate(Servicios_Basicos = rowMeans(.[c("b1_1", "b1_2", "b1_3", 'b1_4')]),
                                          Salud = rowMeans(.[,34:46], na.rm = T),
                                          Educacion_dis = rowMeans(.[c('b10_1', 'b10_3', 'b10_5',
                                                                       'b10_7')]),
                                          Educacion_cal = rowMeans(.[c('b10_2', 'b10_4', 'b10_6',
                                                                       'b10_8')]),
                                          Seguridad = rowMeans(.[55:57]),
                                          Plan_cuadrante = b14,
                                          ciudad = rowMeans(.[72:79]),
                                          ciudad_2 = rowMeans(.[80:83]),
                                          calidad_transporte = rowMeans(.[c('c7_1', 'c7_4', 'c7_8')]),
                                          precio_transporte = rowMeans(.[c('c7_3', 'c7_7')]),
                                          frecuencia_transporte = rowMeans(.[c('c7_2', 'c7_5', 'c7_6')]),
                                          Satisfaccion_vida = rowMeans(.[92:96])) %>% 
  select(etiqueta, Servicios_Basicos,Salud, Educacion_dis, Educacion_cal,
         Seguridad, Plan_cuadrante, ciudad, ciudad_2, calidad_transporte,
         precio_transporte, frecuencia_transporte, Satisfaccion_vida) %>% 
  mutate_all(~round(.,1))


ECV2019%>% mutate(peligrosidad = case_when(b17_16 == 4 | b17_16 == 3 |
                                              b17_10 == 4 | b17_11 == 4 |
                                              b17_10 == 3 | b17_11 == 3 |
                                              b17_7 == 4 ~ "zona roja",
                                            b17_9 == 4 | b17_9 == 3 |
                                              b17_12 == 4 | b17_12 == 3| b17_7 == 3
                                            | b17_3 == 4 | b17_3 == 3 |b17_6 == 4 ~ " zona amarilla",
                                            TRUE ~ "zona verde")) %>% group_by(comuna, peligrosidad) %>% 
  summarise(n = n()) %>% mutate(freq = round(n / sum(n), digits = 2)*100) %>% 
  filter(peligrosidad == 'zona roja')

ECV2021 %>% group_by(etiqueta, b2) %>% summarise(n = n()) %>% mutate(freq = round(n / sum(n), digits = 2)*100) %>% 
  filter(b2 == '1')


