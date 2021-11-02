usethis::use_git()
setwd("../shiny/")
library(shiny)

library(tidyverse)
library(scales)

covid <- readRDS("data/covid.rds")

ui <- navbarPage(
  title = "Gráficos COVID",
  navbarMenu(
    title = "Boxplots",
    tabPanel(
      title = "Sinais vitais",
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "sinais_vitais",
            label = "Sinais vitais na admissão da UTI",
            choices = names(covid[,15:21])
          )
        ),
        column(
          width = 9,
          plotOutput("grafico_sinais_vitais")
        )
    )
    ),
    tabPanel(
      title = "SAPS 3",
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "saps",
            label = "SAPS 3",
            choices = c("saps3_mundial", "saps3_latina")
          )
        ),
        column(
          width = 9,
          plotOutput("boxplot_saps")
        )
      )
    )
  ),
  tabPanel(
    title = "SMR",
    fluidRow(
      column(
        width = 3,
        selectizeInput(
          inputId = "hospital",
          label = "Nome do hospital",
          multiple = TRUE,
          choices = unique(covid$hospital)
        )
      ),
      column(
        width = 9,
        plotOutput(outputId = "linha")
      )
    )
  ),
  tabPanel(
    title = "Suporte de UTI",
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = "suporte",
          label = "Suporte de UTI",
          choices = c("vni", "caf", "vmi","prona", "bnm",
                      "ecmo", "vasopressor", "tsr", "traqueo", "obito")
        )
      ),
      column(
        width = 9,
        plotOutput("suporte_grafico")
      )
    )
  )
)

server <- function(input, output, session) {

  output$grafico_sinais_vitais <- renderPlot({
    covid %>%
      select(hospital, y = input$sinais_vitais) %>%
      ggplot(aes(hospital, y)) +
      geom_boxplot() +
      theme_minimal()
  })

  output$boxplot_saps <- renderPlot({
    covid %>%
      select(hospital, y = input$saps) %>%
      ggplot(aes(hospital, y)) +
      geom_boxplot() +
      theme_minimal()
  })

  output$linha <- renderPlot({
    covid %>%
      group_by(hospital, periodo) %>%
      summarise(
        mort_hosp = mean(desfecho_uti == "obito") * 100,
        saps3_mundial = median(saps3_mundial, na.rm = TRUE) * 100,
        smr = mort_hosp/saps3_mundial
      ) %>%
      filter(hospital %in% input$hospital) %>%
      ggplot(aes(periodo, smr, colour = hospital)) +
      geom_line(aes(group = hospital)) +
      theme_minimal() +
      labs(
        x = "Período de observação",
        y = "SMR"
      )
  })

  output$suporte_grafico <- renderPlot({
    covid %>%
      mutate(
        vni = case_when(
          cpap == "sim" ~ "sim",
          bipap == "sim" ~ "sim",
          TRUE ~ "nao"
        ),
        obito = ifelse(desfecho_uti == "obito", "sim", "nao")
      ) %>%
      pivot_longer(
        cols = c("vni", "caf", "vmi","prona", "bnm", "ecmo",
                 "vasopressor", "tsr", "traqueo", "obito"),
        names_to = "suporte_uti",
        values_to = "ocorrencia"
      ) %>%
      group_by(hospital, suporte_uti) %>%
      summarise(
        pct_suporte = mean(ocorrencia == "sim", na.rm = TRUE)
      ) %>%
      filter(suporte_uti %in% input$suporte) %>%
      ggplot(aes(hospital, pct_suporte)) +
      geom_col(position = "dodge", fill = "#4288ae") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = "Hospital",
           y = "Porcentagem")
  })


}

shinyApp(ui, server)
