# Proyecto R 2
install.packages("naniar")
skimr::skim(ECV_18_21)


writexl::write_xlsx(ECV_18_21, path = 'ECV_18_21_filtrada.xlsx', col_names = T)

ECV_18_21 = ECV_18_21 %>% filter(comuna == 6 | comuna == 7 | comuna == 8 |
                                   comuna == 9)

encuesta = rio::import("BBDD Calidad de vida 2018.dta")

glimpse(encuesta)

ECV_18_21 %>% mutate(b8 = ifelse(b8 == 0, "No", "si")) %>% 
  group_by(anio, b8) %>% 
  summarise(n = n())

ECV_18_21 %>% mutate(b8 = ifelse(b8 == 0, "2", "1")) %>% 
  mutate(estudiante = case_when(b5_1 == 1 | b5_2 == 1 | 
                                              b5_3 == 1 ~ "Si",
                                            b5_1 == 2 | b5_2 == 2 | 
                                              b5_3 == 2  ~ "No",
                                TRUE ~ "99")) %>% 
  group_by(anio, estudiante) %>% 
  summarise(n = n())


tabla_an = ECV_18_21 %>% group_by(anio) %>% 
  summarise(n = n())

write.table(tabla_ed_disponibilidad, file = "tabla_edudisp.txt", sep = ",",
            quote = F, row.names = F)

tabla_promedio_servicios = ECV_18_21 %>%
  mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                                                gsex == 3 ~ "C3",
                                                                gsex == 4 ~ "D",
                                                                gsex == 5 ~ "E",
                                                                gsex == 99 ~ "NS")) %>% 
  rowwise() %>% 
  mutate(promedio = mean(c(b1_1:b1_4))) %>% 
  group_by(anio, gsex) %>% 
  summarise(mean = mean(promedio))

tabla_salud = ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                          gsex == 3 ~ "C3",
                                          gsex == 4 ~ "D",
                                          gsex == 5 ~ "E",
                                          gsex == 99 ~ "NS")) %>% 
  
  rowwise() %>% filter(across(c(b4_1:b4_12), function(x) !x %in% 99)) %>% 
  mutate(promedio = mean(c(b4_1:b4_12))) %>% group_by(anio, gsex) %>% 
  summarise(mean = round(mean(promedio), 2))
#Elimina todos los valores 99 este metodo, hay que buscar uno que permita 
#calcular sin eliminar todos los 99 

tabla_salu2 = ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                      gsex == 3 ~ "C3",
                                      gsex == 4 ~ "D",
                                      gsex == 5 ~ "E",
                                      gsex == 99 ~ "NS")) %>%
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  mutate(promedio = rowMeans(.[,11:21], na.rm = T))  %>% group_by(anio, gsex) %>% 
  summarise(total = n(),
            promedio = mean(promedio, na.rm = T)) %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 1))) 

  tabla_ed_disponibilidad <- ECV_18_21 %>% mutate(gsex = case_when(gsex == 1 | gsex == 2 ~ "ABC1-C2",
                                                        gsex == 3 ~ "C3",
                                                        gsex == 4 ~ "D",
                                                        gsex == 5 ~ "E",
                                                        gsex == 99 ~ "NS")) %>% 
    mutate(across(where(is.numeric), ~na_if(., 99))) %>%
    mutate(calidad_ed = rowMeans(.[c("b10_1", "b10_3", "b10_5")], na.rm = T)) %>% 
    select(anio,gsex, calidad_ed, b10_7) %>% group_by(anio, gsex) %>% 
    summarise(promedio_ed_med = mean(calidad_ed, na.rm = T),
              promedio_ed_sup = mean(b10_7, na.rm = T)) %>% 
    mutate(across(where(is.numeric), ~ round(., digits = 1))) 

# agregando lingutd y latitud a la base 19 y 21----------------------------------------------------------

#Unimos las dos bases con latitud y longitud (y las guardamos)

base_longitud = rbind(ECV2019, ECV2021)

save(base_longitud, file = "longitud21_19.RData")
  
base_longitud_filtrada = base_longitud %>%  filter(comuna == 6 | comuna == 7 | comuna == 8 |
                                                     comuna == 9)
  
      
ECV_19_21 = ECV_18_21 %>% filter(anio == 2021 | anio == 2019)



# Eliminacion de NA -------------------------------------------------------

prueba = ECV_18_21 %>% select(c(b1_1:b2, b4_1:b4_12, b10_1:b10_8, b11_1:b15_8, b17_1:c2_4, c7_1:c7_8,
                       d1:d2_5)) %>% filter(b2 != 99) 
glimpse(prueba)

require(caret)
library(cluster) 
library(factoextra)
require(clustMixType)

tw <- c()
for(k in 1:15){
  model <-  kmeans(x = prueba, centers = k)
  tw[k] <- model$tot.withinss
}

ggplot(data = data.frame(k = c(1:15), TW = tw), aes(x = k, y = TW)) +
  geom_line(color = 'blue') + 
  geom_point(color = 'blue') + 
  theme_bw() +
  xlab('k') +
  ylab('Total Within SS')
fviz_nbclust(prueba, kmeans, method = 'silhouette')

cluster = kmeans(x = prueba, centers = 3)

prueba$label = cluster$cluster

pr