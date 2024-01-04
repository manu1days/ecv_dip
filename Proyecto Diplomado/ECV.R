ecv2022 = rio::import('Calidad de Vida 2022/BBDD ECV 2022.dta')


ecv2022 %>% filter(COMUNA == 1) %>% mutate_if(is.numeric, as.factor) %>% 
  mutate(B10_7 = case_when(B10_7 == 6 | B10_7 == 7 ~ 'Bien',
                           B10_7 == 99 ~ 'NS',
                           TRUE ~ 'Regular'),
         B10_8 = case_when(B10_8 == 6 | B10_8 == 7 ~ 'Bien',
                           B10_8 == 99 ~ 'NS',
                           TRUE ~ 'Regular')) %>% 
  select(COMUNA, B10_7, B10_8) %>% filter(B10_8 != 'NS') %>% 
  group_by(B10_8) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(n / sum(n), digits = 2))

ecv2022 %>% mutate_if(is.numeric, as.factor) %>% 
  mutate(evalua = case_when(B11_6 == 98 | B11_6 == 99 ~ 'No',
                                      TRUE ~ 'Si')) %>% 
  group_by(COMUNA, evalua) %>% summarise(n = n()) %>% 
  mutate(freq = round(n / sum(n), digits = 2)*100) 

ecv2022 %>% filter(COMUNA == 6) %>% mutate_if(is.numeric, as.factor) 
  
ECV2021 %>% mutate_if(is.numeric, as.factor) %>% 
  mutate(evalua = case_when(B11_6 == 98 | B11_6 == 99 ~ 'No',
                            TRUE ~ 'Si')) %>% 
  group_by(COMUNA, evalua) %>% summarise(n = n()) %>% 
  mutate(freq = round(n / sum(n), digits = 2)*100) 
