#### Análise do banco de dados COVID-19 - Imed Group


library(tidyverse)
dados_hsc <- readxl::read_excel("sao_camilo_combinado.xlsx")
dados_outros <- readxl::read_excel("outros_hospitais.xlsx")

covid <- bind_rows(dados_hsc, dados_outros, .id = NULL)
glimpse(dados_hsc)
glimpse(dados_outros)
View(covid)
covid %>%
  select(hospital, id_banco_1, id_banco_2) %>% view()
dim(covid)
