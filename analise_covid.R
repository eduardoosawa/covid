#### Análise do banco de dados COVID-19 - Imed Group

# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rstatix)
library(janitor)
library(survival)
library(ggpubr)
library(survminer)
library(freqtables)

#library(summarytools)
#print(dfSummary(covid_publico), method = "browser")


# Montagem da base --------------------------------------------------------

## Carregar bases de dados São Camilo e outros hospitais

dados_hsc <- readxl::read_excel("sao_camilo_combinado.xlsx")
dados_outros <- readxl::read_excel("outros_hospitais.xlsx")

## Juntar bases

covid_unificado <- bind_rows(dados_hsc, dados_outros)


# Limpeza de dados --------------------------------------------------------

covid_clean <- covid_unificado %>%
  filter(!is.na(hospital), laudo_pcr == "positivo") %>%
  mutate(
    comorbidades = str_remove(
      comorbidades, pattern = "[;+]$"),
    sintomas = str_remove(
      sintomas, pattern = "[;+]$"),
    nao_invasiva = str_remove(
      nao_invasiva, pattern = "[;+]$")
  ) %>%
  mutate(
    cpap = ifelse(str_detect(nao_invasiva, "cpap"), "sim", "nao"),
    bipap = ifelse(str_detect(nao_invasiva, "bipap"), "sim", "nao"),
    caf = ifelse(str_detect(nao_invasiva, "caf"), "sim", "nao"),
  ) %>%
  mutate(
    cpap = replace_na(cpap, "nao"),
    bipap = replace_na(bipap, "nao"),
    caf = replace_na(caf, "nao")
  )

# Categorizando variáveis -------------------------------------------------


covid_publico <- covid_clean %>%
  filter(hospital != "grajau") %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira"), labels = c("São Camilo", "Salvalus", "Itapevi", "Pedreira")),
    desfecho_uti = case_when(
      desfecho_uti == "alta_domicilio" ~ "alta",
      desfecho_uti == "alta_quarto" ~ "alta",
      desfecho_uti == "obito_quarto" ~ "alta",
      desfecho_uti == "obito_uti" ~ "obito"
    ),
    publico = case_when (
      hospital == "grajau" ~ "publico",
      hospital == "hscamilo" ~ "privado",
      hospital == "itapevi" ~ "publico",
      hospital == "pedreira" ~ "publico",
      hospital == "salvalus" ~ "privado"
    ),
    sup_vent = case_when(
      str_detect(ventilacao, "invasiva") & str_detect(nao_invasiva, "cpap|bipap|caf") ~ "niv_to_mv",
      str_detect(ventilacao, "invasiva") & !str_detect(nao_invasiva, "cpap|bipap|caf") ~ "mv_only",
      str_detect(ventilacao, "invasiva") & is.na(nao_invasiva) ~ "mv_only",
      is.na(ventilacao) & is.na(nao_invasiva) ~ "room_air",
      !str_detect(ventilacao, "invasiva") & str_detect(nao_invasiva, "cpap|bipap|caf") ~ "niv_only",
      TRUE ~ "other"),
    idade_cat = case_when(
      idade < 40 ~ "<40",
      idade < 50 ~ "40-49",
      idade < 60 ~ "50-59",
      idade < 70 ~ "60-69",
      idade < 80 ~ "70-79",
      TRUE ~ ">=80"),
    idade_cat = as.factor(idade_cat),
    idade_cat = fct_relevel(idade_cat, "<40", "40-49", "50-59", "60-69", "70-79", ">=80"),
    imc = peso/(altura*altura),
    imc_cat = case_when(
      imc < 25 ~ "<25",
      imc < 30 ~ "25-29",
      imc < 35 ~ "30-34",
      TRUE ~ ">=35"
    ),
    periodo = case_when(
      data_admissao_uti < ymd("2020-04-26") ~ "p1",
      data_admissao_uti < ymd("2020-05-24") ~ "p2",
      data_admissao_uti < ymd("2020-06-27") ~ "p3",
      TRUE ~ "p4"
    ),
    periodo = as.factor(periodo),
    periodo = fct_relevel(periodo, c("p1", "p2", "p3", "p4")),
    vmi = case_when(
      is.na(data_inicio_vmi) ~ "nao",
      TRUE ~ "sim"
    ),
    data_inicio_vmi = ymd(data_inicio_vmi),
    data_desfecho_vm = ymd(data_desfecho_vm),
    tempo_vmi = as.numeric(data_inicio_vmi %--% data_desfecho_vm, units = "days"),
    data_admissao_hospital_origem = ymd(data_admissao_hospital_origem),
    data_admissao_hospital_imed = ymd(data_admissao_hospital_imed),
    data_admissao_hospitalar = case_when(
      is.na(data_admissao_hospital_origem) ~ data_admissao_hospital_imed,
      TRUE ~ data_admissao_hospital_origem
    ),
    data_sintomas = ymd(data_sintomas),
    data_sintomas = case_when(
      data_sintomas < data_admissao_hospitalar ~ data_sintomas,
      TRUE ~ NA_real_),
    vmi = ifelse(
      !is.na(data_inicio_vmi), "sim", "nao"
    ),
    across(.cols = c(data_inicio_vni, data_inicio_prona, data_re_iot, data_inicio_vasopressor,
                     data_inicio_tsr, data_final_tsr, data_inicio_hcq, data_final_hcq,
                     data_admissao_uti, data_alta_uti, data_alta_hospitalar, data_desfecho_re_iot),
           as.Date, format = "%Y/%m/%d"),
    traqueo = case_when(
      desfecho_vm == "traqueostomia" ~ "sim",
      desfecho_re_iot == "traqueostomia" ~ "sim",
      TRUE ~ "nao"
    ),
    data_final_vmi = case_when(
      desfecho_vm == "obito" & is.na(data_desfecho_vm) ~ data_alta_uti,
      desfecho_vm == "obito" ~ data_desfecho_vm,
      desfecho_vm == "traqueostomia" ~ data_desfecho_vm,
      desfecho_vm == "extubacao" & re_iot == "nao" ~ data_desfecho_vm,
      desfecho_re_iot == "traqueostomia" & is.na(data_desfecho_re_iot) ~ data_desfecho_vm,
      !is.na(data_inicio_vmi) & is.na(data_desfecho_vm) ~ data_alta_uti,
      !is.na(data_inicio_vmi) & is.na(desfecho_vm) ~ data_desfecho_vm,
      !is.na(data_re_iot) ~ data_desfecho_re_iot
    )
  )

covid_publico %>%
  filter(desfecho_uti == "obito") %>%
  select(desfecho_alta_hospitalar, desfecho_uti) %>%
  view()


# Checagem lógica ---------------------------------------------------------

covid_publico %>%
  filter(data_admissao_uti < data_admissao_hospitalar) %>%
  select(id_banco_1, id_banco_2, hospital, iniciais, where(is.Date)) %>% view()

covid_publico %>%
  filter(data_inicio_vmi < data_admissao_hospitalar) %>%
  select(id_banco_1, id_banco_2, iniciais, where(is.Date)) %>% view()

covid_publico %>%
  filter(data_sintomas > data_admissao_hospitalar) %>%
  select(data_sintomas, data_admissao_hospitalar)

covid_publico %>%
  select(sup_vent, ventilacao, nao_invasiva) %>%
  view()

covid_publico %>%
  filter(sup_vent == "niv_only") %>%
  select(nao_invasiva) %>% view()


glimpse(covid_publico)

## Período conforme quartis

covid_publico %>%
  group_by(periodo, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n
    ) %>%
  ggplot(aes(periodo, pct_obito, fill = hospital)) +
  geom_col(stat = "identity", position = position_dodge()) +
  labs(
    x = "Período",
    y = "Mortalidade",
    fill = "Hospital"
  ) +
  theme_minimal()

covid_publico %>%
  filter(publico == "publico") %>%
  tabyl(periodo, desfecho_uti) %>%
  kruskal.test(desfecho_uti ~ periodo)

## VMI

covid_publico %>%
  mutate(vmi = case_when(
    is.na(data_inicio_vmi) ~ "nao",
    TRUE ~ "sim"
  )) %>%
  group_by(periodo, publico) %>%
  summarise(
    n = n(),
    n_vmi = sum(vmi == "sim"),
    pct_vmi = n_vmi/n
  ) %>%
  ggplot(aes(periodo, pct_vmi, fill = publico)) +
  geom_col(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("grey", "black"), labels = c("Private", "Public")) +
  labs(
    x = "Period",
    y = "Requirement of mechanical ventilation (%)",
    fill = "Hospital category"
  ) +
  theme_minimal()

covid_publico %>%
  group_by(periodo, publico) %>%
  ggplot() +
  geom_boxplot(aes(periodo, saps3_mundial, fill = publico)) +
  ylim(0, 1) +
  scale_fill_manual(values = c("grey", "white"), labels = c("Private", "Public")) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "SAPS 3 predicted mortality (%)",
    fill = "Hospital category"
  )

covid_publico %>%
  ggplot() +
  geom_boxplot(aes(hospital, saps3_mundial))

covid_publico %>%
  group_by(periodo, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n,
    pct_predito_mundial = median(saps3_mundial, na.rm = TRUE),
    pct_predito_latina = median(saps3_latina, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    smr_mundial = pct_obito/pct_predito_mundial,
    smr_latina = pct_obito/pct_predito_latina
  ) %>%
  ggplot(aes(periodo, smr_latina, colour = hospital)) +
  geom_point() +
  geom_line(aes(group = hospital)) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "SMR Latin America",
    colour = "Hospital"
  )

covid_publico %>%
  group_by(periodo, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n,
    pct_predito_mundial = median(saps3_mundial, na.rm = TRUE),
    pct_predito_latina = median(saps3_latina, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    smr_mundial = pct_obito/pct_predito_mundial,
    smr_latina = pct_obito/pct_predito_latina
  ) %>%
  ggplot(aes(periodo, smr_mundial, colour = hospital)) +
  geom_point() +
  geom_line(aes(group = hospital)) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "SMR (compared to SAPS 3 Global)",
    colour = "Hospital"
  )

covid_publico %>%
  group_by(periodo, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n,
    pct_predito_mundial = median(saps3_mundial, na.rm = TRUE),
    pct_predito_latina = median(saps3_latina, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c("pct_predito_mundial", "pct_obito"), names_to = "pct_mort", values_to = "pct") %>%
  ggplot() +
  geom_col(aes(periodo, pct, fill = pct_mort), position = position_dodge()) +
  facet_wrap(~hospital) +
  scale_fill_manual(values = c("grey", "black"), labels = c("Actual", "Predicted")) +
  labs (
    x = "Period",
    y = "Mortality (%)",
    fill = "Mortality"
  ) +
  theme_bw()

## SIMED

covid_publico %>%
  group_by(periodo, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n,
    pct_predito_mundial = median(saps3_mundial, na.rm = TRUE),
    pct_predito_latina = median(saps3_latina, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c("pct_predito_latina", "pct_obito"), names_to = "pct_mort", values_to = "pct") %>%
  ggplot() +
  geom_col(aes(periodo, pct, fill = pct_mort), position = position_dodge()) +
  facet_wrap(~hospital) +
  scale_fill_manual(values = c("#a6bddb", "#2b8cbe"), labels = c("Observada", "Predita (SAPS3)")) +
  labs (
    x = "Período",
    y = "Mortalidade",
    fill = "Mortalidade"
  ) +
  theme_bw()


covid_publico %>%
  group_by(periodo, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n,
    pct_predito_mundial = median(saps3_mundial, na.rm = TRUE),
    pct_predito_latina = median(saps3_latina, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c("pct_predito_latina", "pct_obito"), names_to = "pct_mort", values_to = "pct") %>%
  ggplot() +
  geom_point(aes(periodo, pct, colour = hospital))


covid_publico %>%
  filter(publico == "publico") %>%
  kruskal_test(saps3_mundial ~ periodo)

covid_publico %>%
  filter(tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio")
  ) %>%
  group_by(publico, sintomas_cat) %>%
  vmi_pct() %>%
  ggplot(aes(publico, pct_vmi, fill = sintomas_cat)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("grey", "black"))


# Funções -----------------------------------------------------------------

count_pct <- function(x, column) {
  x %>%
    summarise(n = n()) %>%
    mutate(freq = n/sum(n))
}

count_pct_comorb <- function(tbl, column) {
  tbl %>%
    summarise(
      n = n(),
      n_comorb = sum({{column}} == 1),
      pct_comorb = n_comorb/n
    )
}

count_pct_sintoma <- function(tbl, column) {
  tbl %>%
    summarise(
      n = n(),
      n_sintoma = sum({{column}} == 1),
      pct_sintoma = n_sintoma/n
    )
}

count_pct_suporte <- function(tbl, column) {
  tbl %>%
    summarise(
      n = n(),
      n_suporte = sum({{column}} == "sim"),
      pct_suporte = n_suporte/n
    )
}

obito_pct <- function(tbl) {
  tbl %>%
    summarise(
      n = n(),
      n_obito = sum(desfecho_uti == "obito"),
      pct_obito = n_obito/n
    )
}

vmi_pct <- function(tbl) {
  tbl %>%
    summarise(
      n = n(),
      n_vmi = sum(vmi == "sim"),
      pct_vmi = n_vmi/n
    )
}

# Características basais --------------------------------------------------

covid_publico %>%
  count(publico) %>%
  janitor::adorn_totals()

covid_publico %>%
  group_by(hospital) %>%
  vmi_pct()

covid_publico %>%
  group_by(hospital) %>%
  ggplot() +
  geom_boxplot(aes(hospital, saps3_mundial))

## Gênero

covid_publico %>%
  group_by(hospital) %>%
  count_pct()

covid_publico %>%
  group_by(genero) %>%
  count_pct()

covid_publico %>%
  tabyl (genero, hospital) %>%
  chisq.test()

## Idade

covid_publico %>%
  group_by(hospital) %>%
  summarise(
    mediana_idade = median(idade, na.rm = TRUE),
    iqr_idade = quantile(idade, prob = c(0.25, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  kruskal_test(idade ~ hospital)

covid_publico %>%
  group_by(publico, idade_cat, desfecho_uti) %>%
  count() %>%
  mutate(publico = factor(
    publico, levels = c("privado", "publico"),
    labels = c("Private", "Public")
    )) %>%
  ggplot() +
  geom_col(aes(idade_cat, n, fill = desfecho_uti)) +
  facet_wrap(~publico) +
  scale_fill_manual(values = c("grey", "black"),
                    labels = c("Discharged", "Death")) +
  theme_minimal() +
  labs(
    x = "Age category",
    y = "Count",
    fill = "ICU outcome"
  )

## Tempo de sintomas

covid_publico %>%
  ggplot() +
  geom_histogram(aes(x = tempo_sintomas), binwidth = 1.0)

covid_publico %>%
  select(tempo_sintomas) %>%
  ggplot() +
  geom_histogram(aes(x = tempo_sintomas), bindwidth = 1.0)

covid_publico %>%
  filter(tempo_sintomas>0, tempo_sintomas < 22) %>%
  group_by(hospital) %>%
  summarise(
    iqr = quantile(tempo_sintomas, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  filter(tempo_sintomas>0, tempo_sintomas < 22) %>%
  kruskal_test(tempo_sintomas ~ hospital)


## Comorbidades e sintomas


# Top comorbidades

covid_publico %>%
  separate_rows(comorbidades, sep = ";") %>%
  group_by(comorbidades) %>%
  count(comorbidades, sort = TRUE) %>%
  top_n(15) %>%
  view()

# Contando cada comorbidade (substituir nome pela comorbidade dentro de group_by)

covid_publico %>%
  separate_rows(comorbidades, sep = ";") %>%
  mutate(value = 1) %>%
  spread(comorbidades, value, fill = 0) %>%
  group_by(hospital) %>%
  count_pct_comorb(column = arritmia)


covid_publico %>%
  separate_rows(comorbidades, sep = ";") %>%
  mutate(value = 1) %>%
  spread(comorbidades, value, fill = 0) %>%
  tabyl(arritmia, hospital) %>%
  chisq.test()

## Incluindo DPOC no grupo das pneumopatias

covid_publico %>%
  separate_rows(comorbidades, sep = ";") %>%
  mutate(value = 1) %>%
  spread(comorbidades, value, fill = 0) %>%
  mutate (dpoc = "pneumopatia") %>%
  group_by(hospital, pneumopatia) %>%
  count_pct()

covid_publico %>%
  separate_rows(comorbidades, sep = ";") %>%
  mutate(value = 1) %>%
  spread(comorbidades, value, fill = 0) %>%
  mutate (dpoc = "pneumopatia") %>%
  group_by(publico, pneumopatia) %>%
  tabyl(pneumopatia, publico) %>%
  chisq.test()

## Sintomas

covid_publico %>%
  separate_rows(sintomas, sep = ";") %>%
  group_by(sintomas) %>%
  count(sintomas, sort = TRUE) %>%
  top_n(15) %>%
  view()

covid_publico %>%
  separate_rows(sintomas, sep = ";") %>%
  mutate(value = 1) %>%
  spread(sintomas, value, fill = 0) %>%
  group_by(hospital) %>%
  count_pct_sintoma(column = coriza)


covid_publico %>%
  separate_rows(sintomas, sep = ";") %>%
  mutate(value = 1) %>%
  spread(sintomas, value, fill = 0) %>%
  tabyl(publico, coriza) %>%
  chisq.test()

## Sinais vitais

covid_publico %>%
  ggplot() +
  geom_histogram(aes(x = saps3), bindwidth = 1.0)

shapiro.test(covid_publico$spo2)

covid_publico %>%
  summarise(
    quantile(saps3_latina, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  group_by(hospital) %>%
  summarise(
    quantile(saps3_latina, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  group_by(hospital) %>%
  summarise(
    quantile(spo2, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  kruskal_test(spo2 ~ hospital)

covid_publico %>%
  kruskal_test(saps3_mundial ~ hospital)

covid_publico %>%
  wilcox_test(tax ~ hospital)

# Número de quadrantes

covid_publico$n_quadrantes <- as.factor(covid_publico$n_quadrantes)

covid_publico %>%
  group_by(n_quadrantes) %>%
  count_pct()

covid_publico %>%
  filter(!is.na(n_quadrantes)) %>%
  group_by(hospital, n_quadrantes) %>%
  summarise(
    n = n()
  ) %>%
  mutate(pct = n/sum(n))

covid_publico %>%
  kruskal.test(n_quadrantes ~ hospital)

covid_publico %>%
  group_by(hospital) %>%
  summarise(
    sintomas = quantile(tempo_sintomas, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )


# Suporte orgânico na UTI -------------------------------------------------

# Ventilação mecânica

covid_publico %>%
  mutate(
    vmi = ifelse(
      is.na(data_inicio_vmi), "nao", "sim")) %>%
  group_by(publico) %>%
  count_pct_suporte(column = vmi)


covid_publico %>%
  mutate(
    vmi = ifelse(
      is.na(data_inicio_vmi), 0, 1)) %>%
  tabyl(vmi, publico) %>%
  chisq_test()

## Tempo de VM

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  mutate(
    data_inicio_vmi = ymd(data_inicio_vmi),
    data_desfecho_vm = ymd(data_desfecho_vm),
    tempo_vmi = as.numeric(data_inicio_vmi %--% data_desfecho_vm, units = "days")
  ) %>%
  group_by(publico) %>%
  ggplot() +
  geom_boxplot(aes(tempo_vmi, fill = publico)) +
  coord_flip() +
  scale_fill_manual(values = c("grey", "white"), labels = c("Private", "Public")) +
  theme_minimal() +
  labs(
    x = "Duration of mechanical ventilation (days)",
    fill = "Hospital category"
  )

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  mutate(
    data_inicio_vmi = ymd(data_inicio_vmi),
    data_desfecho_vm = ymd(data_desfecho_vm),
    tempo_vmi = as.numeric(data_inicio_vmi %--% data_desfecho_vm, units = "days")
  ) %>%
  ggplot() +
  geom_boxplot(aes(tempo_vmi, fill = hospital)) +
  coord_flip() +
  scale_fill_grey(labels = c("Grajaú", "São Camilo", "Itapevi", "Pedreira", "Salvalus")) +
  theme_minimal() +
  labs(
    x = "Duration of mechanical ventilation (days)",
    fill = "Hospital"
  )

# Outros dispositivos

covid_publico %>%
  group_by(sup_vent) %>%
  count_pct()

covid_publico %>%
  count(sup_vent)

covid_publico %>%
  group_by(sup_vent) %>%
  count_pct()

covid_publico %>%
  mutate(value = 1) %>%
  spread(sup_vent, value, fill = 0) %>%
  tabyl(room_air, hospital) %>%
  chisq.test()

# Prona

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  mutate(prona = replace_na(prona,"nao")) %>%
  group_by(hospital) %>%
  count_pct_suporte(column = prona)

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  mutate(prona = replace_na(prona,"nao")) %>%
  tabyl(publico, prona) %>%
  chisq.test()

# Bloqueador neuromuscular

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  mutate(bnm = replace_na(bnm, "nao")) %>%
  group_by(hospital) %>%
  count_pct_suporte(column = bnm)

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  mutate(bnm = replace_na(bnm, "nao")) %>%
  tabyl(hospital, bnm) %>%
  chisq.test()

# Vasopressor

covid_publico %>%
  group_by(hospital) %>%
  count_pct_suporte(column = vasopressor)

covid_publico %>%
  tabyl(hospital, vasopressor) %>%
  chisq.test()

# TSR

covid_publico %>%
  group_by(hospital) %>%
  count_pct_suporte(column = tsr)


covid_publico %>%
  tabyl(hospital, tsr) %>%
  chisq.test()

# CRRT

covid_publico %>%
  mutate(value = "sim") %>%
  spread(metodo_tsr, value, fill = "nao") %>%
  group_by(hospital) %>%
  count_pct_suporte(column = continua)


covid_publico %>%
  mutate(value = 1) %>%
  spread(metodo_tsr, value, fill = 0) %>%
  tabyl(hospital, continua) %>%
  fisher.test()

# SLED

covid_publico %>%
  mutate(value = "sim") %>%
  spread(metodo_tsr, value, fill = "nao") %>%
  group_by(hospital) %>%
  count_pct_suporte(column = sled)

covid_publico %>%
  mutate(value = 1) %>%
  spread(metodo_tsr, value, fill = 0) %>%
  tabyl(hospital, sled) %>%
  fisher.test()

# HD

covid_publico %>%
  mutate(value = "sim") %>%
  spread(metodo_tsr, value, fill = "nao") %>%
  group_by(hospital) %>%
  count_pct_suporte(column = classica)

covid_publico %>%
  mutate(value = 1) %>%
  spread(metodo_tsr, value, fill = 0) %>%
  tabyl(hospital, classica) %>%
  chisq.test()

# ECMO

covid_publico %>%
  mutate(ecmo = replace_na(ecmo, "nao")) %>%
  group_by(hospital) %>%
  count_pct_suporte(column = ecmo)

covid_publico %>%
  mutate(ecmo = replace_na(ecmo, "nao")) %>%
  group_by(hospital, ecmo) %>%
  count_pct()

covid_publico %>%
  mutate(ecmo = replace_na(ecmo, "nao")) %>%
  tabyl(hospital, ecmo) %>%
  fisher.test()

# Traqueostomia

covid_publico %>%
  mutate(traqueostomia = case_when(
    desfecho_vm == "traqueostomia" ~ "sim",
    desfecho_re_iot == "traqueostomia" ~ "sim",
    TRUE ~ "nao")) %>%
  group_by(hospital) %>%
  count_pct_suporte(column = traqueostomia)

covid_publico %>%
  mutate(traqueostomia = case_when(
    desfecho_vm == "traqueostomia" ~ "sim",
    desfecho_re_iot == "traqueostomia" ~ "sim",
    TRUE ~ "nao")) %>%
  tabyl(hospital, traqueostomia) %>%
  chisq.test()


# Gráficos ----------------------------------------------------------------

summarize_covid <- function(tbl) {
  tbl %>%
    summarize(n = n(),
              n_obito = sum(desfecho_uti == "obito"),
              pct_obito = mean(desfecho_uti == "obito"))
  }


covid_publico %>%
  group_by(idade_cat, publico) %>%
  summarize(n = n(),
           n_obito = sum(desfecho_uti == "obito"),
           razao_obito = mean(desfecho_uti == "obito"),
           pct_obito = (razao_obito * 100)) %>%
  ggplot(aes(pct_obito, idade_cat)) +
  geom_col(aes(fill = publico), position= "dodge") +
  coord_flip() +
  labs (y = "Age group", x = "Mortality (%)", fill = "Category") +
  scale_fill_grey(labels = c("Private", "Public")) +
  theme_minimal()

covid_publico %>%
  ggplot(aes(x = tempo_uti)) +
  geom_boxplot(aes(fill = publico), outlier.shape = NA) +
  coord_flip() +
  scale_fill_manual(values = c("white", "grey"), labels = c("Private", "Public")) +
  xlim(0, 100) +
  labs (
    x = "Length of stay in ICU",
    fill = "Category"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

covid_publico %>%
  filter(hospital != "grajau") %>%
  group_by(publico) %>%
  summarise(
    iqr = quantile(tempo_hospital, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  filter(hospital != "grajau") %>%
  wilcox_test(tempo_hospital ~ publico)

covid_publico %>%
  filter(hospital != "grajau") %>%
  kruskal_test(tempo_hospital ~ hospital)


covid_publico %>%
  ggplot(aes(x = tempo_uti)) +
  geom_boxplot(aes(fill = hospital)) +
  coord_flip() +
  scale_fill_grey(labels = c("Grajaú", "São Camilo", "Itapevi", "Pedreira", "Salvalus")) +
  xlim(0, 50) +
  labs (
    x = "Length of stay in ICU",
    fill = "Category"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

covid_publico %>%
  filter(!is.na(tempo_hospital)) %>%
  ggplot(aes(x = tempo_hospital)) +
  geom_boxplot(aes(fill = publico)) +
  coord_flip() +
  scale_fill_manual(values = c("white", "grey"), labels = c("Private", "Public")) +
  xlim(0, 100) +
  labs (
    x = "Hospital length of stay",
    fill = "Category"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

covid_publico %>%
  filter(!is.na(tempo_hospital)) %>%
  ggplot(aes(x = tempo_hospital)) +
  geom_boxplot(aes(fill = hospital)) +
  coord_flip() +
  scale_fill_grey(labels = c("Grajaú", "São Camilo", "Itapevi", "Pedreira", "Salvalus")) +
  xlim(0, 50) +
  labs (
    x = "Hospital length of stay",
    fill = "Category"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

covid_publico %>%
  filter(!is.na(data_inicio_vmi)) %>%
  group_by(idade_cat, publico) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    obito_pct = n_obito/sum(n),
    obito_100 = 100*obito_pct
  ) %>%
  ggplot(aes(idade_cat, obito_100)) +
  geom_col(position = "dodge", aes(fill = publico)) +
  scale_fill_manual(values = c("black", "grey"), labels = c("Private", "Public")) +
  theme_bw() +
  labs(x = "Age group",
       y = "In-hospital mortality (%)",
       fill = "Hospital category",
       title = "In-hospital mortality by age group among mechanically ventilated patients")

covid_publico %>%
  filter(!is.na(data_inicio_vmi), hospital != "grajau") %>%
  group_by(idade_cat, hospital) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    obito_pct = n_obito/sum(n),
    obito_100 = 100*obito_pct
  ) %>%
  ggplot(aes(idade_cat, obito_100)) +
  geom_col(position = "dodge", aes(fill = hospital)) +
  theme_bw() +
  scale_fill_grey(labels = c("São Camilo", "Itapevi", "Pedreira", "Salvalus")) +
  labs(x = "Age group",
       y = "In-hospital mortality (%)",
       fill = "Hospital",
       title = "In-hospital mortality by age group among mechanically ventilated patients")


glimpse(covid_publico)

covid_publico %>%
  filter(!is.na(data_inicio_vmi), !is.na(imc)) %>%
  drop_na(imc_cat) %>%
  group_by(imc_cat, publico) %>%
  summarise(
    n = n(),
    n_prona = sum(prona == "sim"),
    pct_prona = (n_prona/sum(n))*100) %>%
  ggplot(aes(imc_cat, pct_prona)) +
  geom_col(aes(fill = publico), position = "dodge") +
  labs(
    x = "BMI category",
    y = "% of patients who underwent prone positioning",
    title = "Relationship betwwen BMI and prone positioning among intubated patients"
  ) +
  theme_minimal()

covid_publico %>%
  filter(!is.na(imc)) %>%
  group_by(imc_cat) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = (n_obito/sum(n))*100) %>%
  ggplot(aes(imc_cat, pct_obito)) +
  geom_col() +
  labs(
    x = "BMI category",
    y = "Mortality (%)",
    title = "Relationship betwwen BMI and mortality"
  ) +
  theme_minimal()


covid_publico %>%
  filter(tempo_sintomas >= 0, tempo_sintomas <=21, !is.na(tempo_sintomas)) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8~ "precoce",
    TRUE ~ "tardio"
  )) %>%
  group_by(sintomas_cat, publico) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = 100*(n_obito/sum(n))
  ) %>%
  ggplot(aes(sintomas_cat, pct_obito)) +
  geom_col(aes(fill = publico), position = "dodge") +
  scale_fill_manual(values = c("Black", "Grey"), labels = c("Private", "Public")) +
  theme_minimal() +
  labs (
    x = "Duration from symptom onset to ICU admission",
    y = "Mortality (%)",
    fill = "Hospital category"
  ) +
  scale_x_discrete(labels = c("<= 8 days", ">8 days"))

covid_publico %>%
  filter(tempo_sintomas > 0, tempo_sintomas < 22) %>%
  ggplot() +
  geom_boxplot(aes(x = tempo_sintomas, fill = hospital)) +
  coord_flip() +
  labs(
    x = "Hospital",
    y = "Duration from symptom onset to ICU admission",
    fill = "Hospital"
  ) +
  theme_minimal() +
  scale_fill_grey(labels = c("Grajaú", "São Camilo", "Itapevi", "Pedreira", "Salvalus"))

covid_publico %>%
  filter(tempo_sintomas >= 0, tempo_sintomas <=21, !is.na(tempo_sintomas)) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio"
  )) %>%
  group_by(sintomas_cat, publico) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = 100*(n_obito/sum(n))
  ) %>%
  ggplot(aes(publico, n_obito)) +
  geom_col(aes(fill = sintomas_cat)) +
  theme_minimal()

covid_publico %>%
  filter(tempo_sintomas >= 0, tempo_sintomas <=21, !is.na(tempo_sintomas)) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio"
  )) %>%

covid_publico %>%
  filter(tempo_sintomas >= 0, tempo_sintomas <=21, !is.na(tempo_sintomas)) %>%
  ggplot() +
  geom_histogram(aes(x = tempo_sintomas), fill = "grey", colour = "black", binwidth = 1.0)

  tabyl(sintomas_cat, desfecho_uti) %>%
  chisq.test()

covid_publico %>%
  count(hospital)

# Sobrevida ---------------------------------------------------------------

sobrevida <- covid_publico %>%
  mutate(
    data_admissao_uti = ymd(data_admissao_uti),
    data_alta_uti = ymd(data_alta_uti),
    data_28 = case_when(
      tempo_uti > 28 ~ data_admissao_uti + days(28),
      TRUE ~ data_alta_uti
    ),
    tempo_fup = as.numeric(data_admissao_uti %--% data_28/ddays(1)),
    status = case_when(
      tempo_uti > 28 ~ 0,
      desfecho_uti == "obito" & tempo_uti <= 28 ~ 1,
      TRUE ~ 0)
    )

Surv(sobrevida$tempo_fup, sobrevida$status)

plot(survfit(Surv(tempo_fup, status) ~ 1, data = sobrevida))

ggsurvplot(
  fit = survfit(Surv(tempo_fup, status) ~ publico, data = sobrevida),
  conf.int = TRUE,
  ggtheme = theme_bw(), censor = FALSE, pval = TRUE,
  pval.method = TRUE, pval.size = 3,
  legend.title = "Hospital",
  legend.labs = c("Privado",
                  "Público"),
  xlab = "Time (days)",
  surv.scale = "percent")

ggsurvplot(
  fit = survfit(Surv(tempo_fup, status) ~ hospital, data = sobrevida),
  conf.int = TRUE,
  ggtheme = theme_bw(), censor = FALSE, pval = TRUE,
  pval.method = TRUE, pval.size = 3,
  legend.title = "Hospital",
  legend.labs = c("São Camilo", "Itapevi", "Pedreira", "Salvalus"),
  xlab = "Time (days)",
  surv.scale = "percent"
)

## Tempo para VM

glimpse(covid_publico)

sobrevida_vmi <- covid_publico %>%
  mutate(
    data_admissao_uti = ymd(data_admissao_uti),
    data_inicio_vmi = ymd(data_inicio_vmi),
    d28 = data_admissao_uti + days(28),
    data_28_vmi = case_when(
      data_inicio_vmi < data_admissao_uti ~ data_admissao_uti,
      !is.na(data_inicio_vmi) & (data_inicio_vmi < d28) ~ data_inicio_vmi,
      TRUE ~ data_admissao_uti + days(28)
    ),
    tempo_fup_vmi = as.numeric(data_admissao_uti %--% data_28_vmi/ddays(1)),
    status_vmi = case_when(
      !is.na(data_inicio_vmi) & tempo_fup_vmi <= 28 ~ 1,
      !is.na(data_inicio_vmi) & tempo_fup_vmi > 28 ~ 0,
      is.na(data_inicio_vmi) & tempo_fup_vmi > 28 ~ 0,
      tempo_fup_vmi > 28 ~ 0,
      TRUE ~ 0)
    )

plot(survfit(Surv(tempo_fup_vmi, status_vmi) ~ 1, data = sobrevida_vmi))

ggsurvplot(
  fit = survfit(Surv(tempo_fup_vmi, status_vmi) ~ publico, data = sobrevida_vmi),
  conf.int = TRUE,
  ggtheme = theme_minimal(), censor = FALSE, pval = TRUE,
  pval.method = TRUE, pval.size = 3,
  legend.title = "Hospital category",
  legend.labs = c("Private",
                  "Públic"),
  xlab = "Time (days)",
  ylab = "Probability of mechanical ventilation-free time",
  title = "Probability of mechanical ventilation-free time",
  surv.scale = "percent",
  )

ggsurvplot(
  fit = survfit(Surv(tempo_fup_vmi, status_vmi) ~ hospital, data = sobrevida_vmi),
  conf.int = FALSE,
  ggtheme = theme_minimal(), censor = FALSE, pval = TRUE,
  pval.method = TRUE, pval.size = 3,
  legend.title = "Hospital",
  legend.labs = c("São Camilo", "Itapevi", "Pedreira", "Salvalus"),
  xlab = "Time (days)",
  ylab = "Probability of mechanical ventilation-free time",
  title = "Probability of mechanical ventilation-free time",
  surv.scale = "percent",
)


##SIMED

ggsurvplot(
  fit = survfit(Surv(tempo_fup_vmi, status_vmi) ~ hospital, data = sobrevida_vmi),
  conf.int = FALSE,
  ggtheme = theme_minimal(), censor = FALSE, pval = TRUE,
  pval.method = TRUE, pval.size = 3,
  legend.title = "Hospital",
  legend.labs = c("São Camilo", "Salvalus", "Itapevi", "Pedreira"),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de tempo-livre de VM",
  surv.scale = "percent",
)

covid_publico %>%
  filter(!is.na(n_quadrantes)) %>%
  mutate(
    n_quadrantes = as.factor(n_quadrantes),
    n_quadrantes = fct_relevel(n_quadrantes, "0", "1", "2", "3", "4")
  ) %>%
  group_by(n_quadrantes) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct2 = 100*(n_obito/sum(n))
  )

covid_publico %>%
  select(tax) %>%
  count(tax > 37)

glimpse(covid_publico)


# qSOFA -------------------------------------------------------------------

qsofa <- covid_publico %>%
  filter(!(tempo_uti <=2 & desfecho_uti == "alta")) %>%
  mutate(
    qsofa_neuro = case_when(
      neuro_adm ==  "sedado_iot" ~ 0,
      neuro_adm == "alerta" ~ 0,
      TRUE ~ 1
    ),
    qsofa_resp = case_when(
      is.na(fr) ~ 0,
      fr >= 22 ~ 1,
      TRUE ~ 0
    ),
    qsofa_pas = case_when(
      is.na(pas) ~ 0,
      pas <= 100 ~ 1,
      TRUE ~ 0
    ),
    qsofa_total = case_when(
      (qsofa_neuro + qsofa_resp + qsofa_pas) >= 2 ~ "positivo",
      TRUE ~ "negativo"
    )
  )

qsofa %>%
  count(hospital, qsofa_total)



# Mortality

qsofa %>%
  group_by(publico, qsofa_total) %>%
  summarise(
    n = n(),
    n_obito = sum(desfecho_uti == "obito"),
    pct_obito = n_obito/n()
  ) %>%
  ggplot() +
  geom_col(aes(publico, pct_obito, fill = qsofa_total), position = "dodge") +
  labs(
    x = "Hospital category",
    y = "Mortality (%)",
    fill = "qSOFA at ICU admission"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Private", "Public")) +
  scale_fill_grey(labels= c("Negative", "Positive"))

qsofa %>%
  filter(publico == "privado") %>%
  tabyl(qsofa_total, desfecho_uti) %>%
  chisq.test()

# Mechanical ventilation

qsofa %>%
  group_by(publico, qsofa_total) %>%
  summarise(
    n = n(),
    n_vmi = sum(!is.na(data_inicio_vmi)),
    pct_vmi = n_vmi/n()
  ) %>%
  ggplot() +
  geom_col(aes(publico, pct_vmi, fill = qsofa_total), position = "dodge") +
  labs(
    x = "Hospital category",
    y = "Requirement of mechanical ventilation (%)",
    fill = "qSOFA at ICU admission"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Private", "Public")) +
  scale_fill_grey(labels= c("Negative", "Positive"))

qsofa %>%
  filter(publico == "publico") %>%
  mutate(vmi = ifelse(
    !is.na(data_inicio_vmi), "sim", "nao"
  )) %>%
  tabyl(vmi, qsofa_total) %>%
  chisq.test()

# Use of vasopressor

qsofa %>%
  group_by(publico, qsofa_total) %>%
  summarise(
    n = n(),
    n_vasopressor = sum(vasopressor == "sim"),
    pct_vasopressor = n_vasopressor/n()
  ) %>%
  ggplot() +
  geom_col(aes(publico, pct_vasopressor, fill = qsofa_total), position = "dodge") +
  labs(
    x = "Hospital category",
    y = "Requirement of vasopressor use (%)",
    fill = "qSOFA at ICU admission"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Private", "Public")) +
  scale_fill_grey(labels= c("Negative", "Positive"))

# TSR

qsofa %>%
  group_by(publico, qsofa_total) %>%
  summarise(
    n = n(),
    n_tsr = sum(tsr == "sim"),
    pct_tsr = n_tsr/n()
  ) %>%
  ggplot() +
  geom_col(aes(publico, pct_tsr, fill = qsofa_total), position = "dodge") +
  labs(
    x = "Hospital category",
    y = "Requirement of renal replacement therapy (%)",
    fill = "qSOFA at ICU admission"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Private", "Public")) +
  scale_fill_grey(labels= c("Negative", "Positive"))

qsofa %>%
  filter(publico == "privado") %>%
  tabyl(qsofa_total, tsr) %>%
  chisq.test()

qsofa %>%
  filter(publico == "publico") %>%
  tabyl(qsofa_total, desfecho_uti) %>%
  chisq.test()


# Gráficos adicionais -----------------------------------------------------

covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas < 22, tempo_sintomas > 0) %>%
  ggplot() +
  geom_boxplot(aes(publico, tempo_sintomas))

covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas < 22, tempo_sintomas > 0) %>%
  group_by(publico) %>%
  summarise(
    iqr = quantile(tempo_sintomas, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas < 22, tempo_sintomas > 0) %>%
  wilcox_test(tempo_sintomas ~ publico)

covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas < 22, tempo_sintomas > 0) %>%
  kruskal_test(tempo_sintomas ~ hospital)

covid_publico %>%
  tabyl(desfecho_uti, publico) %>%
  chisq.test()

covid_publico %>%
  filter(tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio")
  ) %>%
  group_by(publico, sintomas_cat) %>%
  summarize(
    iqr_saps3 = quantile(saps3_mundial, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  filter(tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio")
  ) %>%
  ggplot() +
  geom_boxplot(aes(hospital, saps3_mundial, fill = sintomas_cat))

covid_publico %>%
  group_by(publico) %>%
  summarise(
    mediana = median(saps3_mundial, na.rm = TRUE)
  )
  wilcox_test(saps3_mundial ~ publico)


covid_publico %>%
  filter(hospital != "grajau", publico == "publico", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  wilcox_test(tempo_sintomas ~ hospital)

covid_publico %>%
  filter(hospital != "grajau", publico == "publico", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  ggplot() +
  geom_boxplot(aes(hospital, tempo_sintomas))

covid_publico %>%
  filter(hospital != "grajau", publico == "publico", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  group_by(hospital) %>%
  summarise(
    iqr = quantile(tempo_sintomas, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )


covid_publico %>%
  filter(publico == "publico", hospital != "grajau") %>%
  wilcox_test(saps3_mundial ~ hospital)

  group_by(hospital) %>%
  summarise(
    mediana = median(saps3_mundial, na.rm = TRUE)
  )

covid_publico %>%
  filter(hospital == "salvalus", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio")
  ) %>%
  group_by(sintomas_cat) %>%
  summarise(
    iqr = quantile(saps3_mundial, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  filter(hospital == "salvalus", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio")
  ) %>%
  wilcox_test(saps3_mundial ~ sintomas_cat)

  covid_publico %>%
    filter(hospital != "grajau", tempo_sintomas > 0, tempo_sintomas < 22) %>%
    mutate(
      hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
      sintomas_cat = case_when(
        tempo_sintomas <= 8 ~ "precoce",
        TRUE ~ "tardio")
    ) %>%
    group_by(hospital, sintomas_cat) %>%
    obito_pct() %>%
    ggplot() +
    geom_col(aes(hospital, pct_obito, fill = sintomas_cat), position = "dodge")


  covid_publico %>%
    filter(hospital == "pedreira", tempo_sintomas > 0, tempo_sintomas < 22) %>%
    mutate(
      sintomas_cat = case_when(
        tempo_sintomas <= 8 ~ "precoce",
        TRUE ~ "tardio")
    ) %>%
    tabyl(desfecho_uti, sintomas_cat) %>%
    chisq.test()

covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
    sintomas_cat = case_when(
      tempo_sintomas <= 8 ~ "precoce",
      TRUE ~ "tardio")
  ) %>%
  group_by(hospital, sintomas_cat) %>%
  vmi_pct() %>%
  ggplot() +
  geom_col(aes(hospital, pct_vmi, fill = sintomas_cat), position = "dodge") +
  scale_fill_discrete() +
  theme_minimal()

covid_publico %>%
  filter(hospital == "pedreira", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
    sintomas_cat = case_when(
      tempo_sintomas <= 8 ~ "precoce",
      TRUE ~ "tardio"),
    vmi = case_when(
      is.na(data_inicio_vmi) ~ "nao",
      TRUE ~ "sim"
    )
  ) %>%
  tabyl(vmi, sintomas_cat) %>%
  chisq.test()


covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
    sintomas_cat = case_when(
      tempo_sintomas <= 8 ~ "precoce",
      TRUE ~ "tardio")
  ) %>%
  group_by(hospital, sintomas_cat) %>%
  count_pct_suporte(column = tsr) %>%
  ggplot() +
  geom_col(aes(hospital, pct_suporte, fill = sintomas_cat), position = "dodge") +
  scale_fill_discrete() +
  theme_minimal() +
  labs (y = "RRT")

covid_publico %>%
  filter(hospital != "grajau", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
    sintomas_cat = case_when(
      tempo_sintomas <= 8 ~ "precoce",
      TRUE ~ "tardio")
  ) %>%
  group_by(hospital, sintomas_cat) %>%
  count_pct_suporte(column = vasopressor) %>%
  ggplot() +
  geom_col(aes(hospital, pct_suporte, fill = sintomas_cat), position = "dodge") +
  scale_fill_discrete() +
  theme_minimal() +
  labs (y = "Vasopressor")

covid_publico %>%
  filter(hospital =="pedreira", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
    sintomas_cat = case_when(
      tempo_sintomas <= 8 ~ "precoce",
      TRUE ~ "tardio")
  ) %>%
  tabyl(vasopressor, sintomas_cat) %>%
  chisq.test()


covid_publico %>%
  filter(hospital =="pedreira", tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira")),
    sintomas_cat = case_when(
      tempo_sintomas <= 8 ~ "precoce",
      TRUE ~ "tardio")
  ) %>%
  tabyl(tsr, sintomas_cat) %>%
  chisq.test()


# Ventilação mecânica -----------------------------------------------------

covid_publico %>%
  filter(hospital != "grajau", !is.na(prona)) %>%
  mutate(
    vmi_cat = ifelse(is.na(data_inicio_vmi), "nao", "sim"),
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira"))
  ) %>%
  group_by(hospital, prona) %>%
  count(vmi_cat) %>%
  filter(vmi_cat == "sim") %>%
  ggplot(aes(hospital, n, fill = prona)) +
  geom_col() +
  labs(
    x = "Hospital",
    y = "Number of patients",
    fill = "Prone positioning"
  ) +
  scale_fill_manual(values = c("grey", "black"), labels = c("No", "Yes")) +
  theme_minimal()

covid_publico %>%
  filter(hospital != "grajau", !is.na(bnm)) %>%
  mutate(
    vmi_cat = ifelse(is.na(data_inicio_vmi), "nao", "sim"),
    hospital = factor(hospital, levels = c("hscamilo", "salvalus", "itapevi", "pedreira"))
  ) %>%
  group_by(hospital, bnm) %>%
  count(vmi_cat) %>%
  filter(vmi_cat == "sim") %>%
  ggplot(aes(hospital, n, fill = bnm)) +
  geom_col() +
  labs(
    x = "Hospital",
    y = "Number of patients",
    fill = "Neuromuscular blockade use"
  ) +
  scale_fill_manual(values = c("grey", "black"), labels = c("No", "Yes")) +
  theme_minimal()

covid_publico %>%
  mutate(
    sintomas_cat = case_when(
    tempo_sintomas <= 8 ~ "precoce",
    TRUE ~ "tardio")
    ) %>%
  ggplot(aes(sintomas_cat, tempo_vmi)) +
  geom_boxplot() +
  labs(
    x = "Duration from symptom onset to ICU admission",
    y = "Duration of mechanical ventilation (days)"
  ) +
  scale_x_discrete(labels = c("<= 8 days", "> 8 days"))

covid_publico %>%
  filter(tempo_sintomas > 0, tempo_sintomas < 22) %>%
  mutate(
    data_inicio_vmi = ymd(data_inicio_vmi),
    data_sintomas = ymd(data_sintomas),
    tempo_sintomas_vmi = as.numeric(data_sintomas %--% data_inicio_vmi, units = "days")
  ) %>%
  ggplot(aes(tempo_sintomas_vmi, tempo_vmi)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "Diferença entre início de sintomas e VM",
    y = "Tempo de VM (dias)"
  )

covid_publico %>%
  filter(!is.na(data_inicio_vmi), hospital != "grajau") %>%
  count(data_inicio_vmi, hospital) %>%
  ggplot(aes(data_inicio_vmi)) +
  geom_histogram(binwidth = 15, color = "black", fill = "grey") +
  facet_wrap(~hospital) +
  labs(
    x = "Data",
    y = "Número de intubações por quinzena"
  ) +
  theme_bw()

covid_publico %>%
  filter(!is.na(data_inicio_vmi), hospital != "grajau") %>%
  mutate(
    data_inicio_vmi = ymd(data_inicio_vmi),
    mes_iot = month(data_inicio_vmi)
  ) %>%
  ggplot(aes(mes_iot)) +
  geom_bar(color = "black", fill = "grey") +
  facet_wrap(~hospital) +
  labs(
    x = "Data",
    y = "Número de intubações por mês"
  ) +
  theme_bw()

covid_publico %>%
  filter(hospital != "grajau") %>%
  mutate(
    data_admissao_uti = ymd(data_admissao_uti),
    mes_uti = month(data_admissao_uti),
    data_inicio_vmi = ymd(data_inicio_vmi),
    mes_iot = month(data_inicio_vmi),
    vmi_cat = ifelse(is.na(data_inicio_vmi), "nao", "sim")
  ) %>%
  group_by(hospital, mes_uti) %>%
  summarize(
    n = n(),
    n_vmi = sum(vmi_cat == "sim"),
    pct_vmi = n_vmi/n
  ) %>%
  ggplot(aes(mes_uti, pct_vmi)) +
  geom_area(fill = "grey") +
  facet_wrap(~hospital) +
  labs(
    x = "Mês de admissão na UTI",
    y = "% dos pacientes em VM"
  ) +
  theme_bw()

covid_publico %>%
  ggplot(aes(hospital, saps3_mundial)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Hospital",
    y = "SAPS 3 predicted mortality"
  ) +
  theme_minimal()

covid_publico %>%
  filter(!is.na(data_admissao_uti)) %>%
  ggplot(aes(hospital, tempo_hospital)) +
  geom_boxplot(fill = "grey") +
  theme_minimal() +
  labs(
    x = "Hospital",
    y = "Tempo de internação hospitalar",
    title = "Tempo de internação hospitalar dos pacientes admitidos na UTI"
  )

covid_publico %>%
  filter(!is.na(data_admissao_uti)) %>%
  group_by(hospital) %>%
  mutate(
    pct_uti = tempo_uti/tempo_hospital
  ) %>%
  ggplot(aes(hospital, pct_uti)) +
  geom_boxplot(fill = "grey") +
  theme_minimal() +
  labs(
    x = "Hospital",
    y = "% of duration of first ICU stay over total hospital stay"
  )

covid_publico %>%
  group_by(hospital) %>%
  count(!is.na(data_inicio_vmi)) %>%
  mutate(n/sum(n))

covid_publico %>%
  kruskal_test(tempo_uti ~ hospital)

colnames(covid_publico)

# Tempo de sintomas -------------------------------------------------------

## Tempo entre início de sintomas e internação hospitalar

covid_publico %>%
  mutate(
    tempo_sintomas_hospital = as.numeric(data_sintomas %--% data_admissao_hospitalar, units = "days")
  ) %>%
  filter(tempo_sintomas_hospital >= 0) %>%
  group_by(hospital) %>%
  summarise(
    quartis = quantile(tempo_sintomas_hospital, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  mutate(
    tempo_sintomas_hospital = as.numeric(data_sintomas %--% data_admissao_hospitalar, units = "days")
  ) %>%
  filter(tempo_sintomas_hospital >= 0) %>%
  kruskal_test(tempo_sintomas_hospital ~ hospital)

covid_publico %>%
  mutate(
    tempo_sintomas_hospital = as.numeric(data_sintomas %--% data_admissao_hospitalar, units = "days")
  ) %>%
  filter(tempo_sintomas_hospital >= 0) %>%
  ggplot(aes(hospital, tempo_sintomas_hospital)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Hospital",
    y = "Diferença entre início de sintomas e internação hospitalar"
  )

## Tempo entre início de sintomas e internação na UTI

covid_publico %>%
  mutate(
    tempo_sintomas_uti = as.numeric(data_sintomas %--% data_admissao_uti, units = "days")
  ) %>%
  filter(tempo_sintomas_uti >= 0) %>%
  group_by(hospital) %>%
  summarise(
    quartis = quantile(tempo_sintomas_uti, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  mutate(
    tempo_sintomas_uti = as.numeric(data_sintomas %--% data_admissao_uti, units = "days")
  ) %>%
  filter(tempo_sintomas_uti >= 0) %>%
  kruskal_test(tempo_sintomas_uti ~ hospital)

covid_publico %>%
  mutate(
    tempo_sintomas_uti = as.numeric(data_sintomas %--% data_admissao_uti, units = "days")
  ) %>%
  filter(tempo_sintomas_uti >= 0) %>%
  ggplot(aes(hospital, tempo_sintomas_uti)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Hospital",
    y = "Diferença entre início de sintomas e internação na UTI"
  )

## Diferença entre tempos de admissão hospitalar e UTI

covid_publico %>%
  mutate(
    tempo_hosp_uti = as.numeric(data_admissao_hospitalar %--% data_admissao_uti, units = "days")
  ) %>%
  group_by(hospital) %>%
  summarise(
    quartis = quantile(tempo_hosp_uti, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )

covid_publico %>%
  mutate(
    tempo_hosp_uti = as.numeric(data_admissao_hospitalar %--% data_admissao_uti, units = "days")
  ) %>%
  kruskal_test(tempo_hosp_uti ~ hospital)

covid_publico %>%
  mutate(
    tempo_hosp_uti = as.numeric(data_admissao_hospitalar %--% data_admissao_uti, units = "days")
  ) %>%
  filter(tempo_hosp_uti >=0) %>%
  ggplot(aes(hospital, tempo_hosp_uti)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Hospital",
    y = "Diferença entre tempo de admissão hospitalar e admissão na UTI"
  ) +
  ylim(0,5) +
  theme_minimal()

