### Limpeza de Banco de Dados - COVID multicêntrico


# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rstatix)
library(janitor)

# Importação das bases de dados -------------------------------------------

dados_hsc <- readxl::read_excel("sao_camilo_combinado.xlsx")
dados_outros <- readxl::read_excel("outros_hospitais.xlsx")
covid_unificado <- bind_rows(dados_hsc, dados_outros)


# Início da limpeza -------------------------------------------------------

## Filtro dos PCR positivos, ajuste de variáveis separadas com ponto e vírgula,
## Criação das variáveis de VNI

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
    caf = replace_na(caf, "nao"),
    data_inicio_vni =
      case_when(
        cpap == "sim" ~ data_inicio_vni,
        bipap == "sim" ~ data_inicio_vni,
        caf == "sim" ~ data_inicio_vni,
        TRUE ~ NA_real_)
  )

# Segunda etapa -----------------------------------------------------------

## Retirada do Hospital Grajaú, definir desfecho primário, separar em público e
## privado, separação por períodos, crianção da variável traqueo,
## definir data final de VMI

covid_publico <- covid_clean %>%
  filter(hospital != "grajau") %>%
  mutate(
    desfecho_uti = case_when(
      desfecho_uti == "alta_domicilio" ~ "alta",
      desfecho_uti == "alta_quarto" ~ "alta",
      desfecho_uti == "obito_quarto" ~ "alta",
      desfecho_uti == "obito_uti" ~ "obito"
    ),
    desfecho_hospital = case_when(
      desfecho_uti == "alta" & is.na(desfecho_alta_hospitalar) ~ "alta",
      desfecho_alta_hospitalar == "domicilio" ~ "alta",
      desfecho_uti == "obito" & is.na(desfecho_alta_hospitalar) ~ "obito",
      TRUE ~ "obito"
    ),
    publico = case_when (
      hospital == "grajau" ~ "publico",
      hospital == "hscamilo" ~ "privado",
      hospital == "itapevi" ~ "publico",
      hospital == "pedreira" ~ "publico",
      hospital == "salvalus" ~ "privado"
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
    vmi = ifelse(
      !is.na(data_inicio_vmi), "sim", "nao"
    ),
    across(.cols = starts_with("data"),
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

# Terceira etapa ----------------------------------------------------------

## Seleção de variáveis relevantes para análise estatística

covid <- covid_publico %>%
  select(
    -id_banco_1, -iniciais, -atendimento, -hospital_transferencia, -tempo_uti, -tempo_hospital,
    -data_admissao_hospital_origem, -data_admissao_hospital_imed, -desfecho_alta_hospitalar,
    -contato_covid, -sofa, -c(28:36), -neuro_adm, -n_readm_uti, -laudo_pcr, -tempo_sintomas,
    -cno2_lpm, -venturi_o2, -pf, -spo2fio2, -tc, -rx, -infiltrado, -consolidacao,
    -desfecho_vm, -data_desfecho_vm, data_re_iot, -desfecho_re_iot,
    -data_desfecho_re_iot, -data_re_iot,
    -vidro_fosco, -infiltrado_intersticial, -iot_uti, -ventilacao, -nao_invasiva, -glasgow,
    -c(66:70), -rcp, -alta_uti_dialise, -sara, -c(91:142), -c(144:189)
  )

write_rds(covid, file = "covid.rds")
