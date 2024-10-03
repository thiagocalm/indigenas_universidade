#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2024-10-03
#' @description Estimativas para pop indigena no Brasil
#' @update-description -
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, censobr)

# Selecionando variaveis de interesse -------------------------------------

censobr::data_dictionary(year = 2000, dataset = "population")
censobr::data_dictionary(year = 2010, dataset = "population")
censobr::data_dictionary(year = 2022, dataset = "population")

# Importacao dos dados de interesse ---------------------------------------

year = c(2000, 2010, 2022)

for(i in seq_along(year)){
  df_pop <- read_population(
    year = year,
    columns = c("V0010","V0606","V1006","V6036","sexo","educ1","educ2"),
    as_data_frame = TRUE,
    showProgress = TRUE,
    cache = FALSE
  )

  # Manipulação dos dados

  df_pop <- df_pop %>%
    as_tibble() %>%
    rename(
      peso = V0010,
      raca = V0606,
      sexo = sexo,
      situacao_dom = V1006,
      idade = V6036
    ) %>%
    mutate(
      PopFem = case_when(sexo == feminino ~ 1, TRUE ~ 0),
      PPI = case_when(raca %in% c(2,4,indigena) ~ 1, TRUE ~ 0),
      PI = case_when(raca %in% c(indigena) ~ 1, TRUE ~ 0),
      PPI_fem = case_when(raca %in% c(2,4,indigena) & sexo == feminino ~ 1, TRUE ~ 0),
      PI_fem = case_when(raca %in% c(indigena) & sexo == feminino ~ 1, TRUE ~ 0),
      univ = case_when(educ == universidade ~ 1, TRUE ~ 0),
      univ_fem = case_when(educ == universidade & sexo == feminino ~ 1, TRUE ~ 0),
      PPI_univ = case_when(raca %in% c(2,4,indigena) & educ == universidade ~ 1, TRUE ~ 0),
      PI_univ = case_when(raca %in% c(indigena) & educ == universidade ~ 1, TRUE ~ 0)
      PPI_univ_fem = case_when(raca %in% c(2,4,indigena) & educ == universidade & sexo == feminino ~ 1, TRUE ~ 0),
      PI_univ_fem = case_when(raca %in% c(indigena) & educ == universidade & sexo == feminino ~ 1, TRUE ~ 0)
    ) %>%
    mutate_all(., ~as.numeric(.x))

  # Criacao de tabela

  t <- df_pop %>%
    summarise(
      ano = year[i],
      Total = sum(peso, na.rm = TRUE),
      Feminino = sum(peso[PopFem == 1], na.rm = TRUE),
      PPI = sum(peso[PPI == 1], na.rm = TRUE),
      PI = sum(peso[PI == 1], na.rm = TRUE),
      PPI_fem = sum(peso[PPI_fem == 1], na.rm = TRUE),
      PI_fem = sum(peso[PI_fem == 1], na.rm = TRUE),
      univ = sum(peso[univ == 1], na.rm = TRUE),
      PPI_univ = sum(peso[PPI_univ == 1], na.rm = TRUE),
      PI_univ = sum(peso[PI_univ == 1], na.rm = TRUE)
      univ_fem = sum(peso[univ_fem == 1], na.rm = TRUE),
      PPI_univ_fem = sum(peso[PPI_univ_fem == 1], na.rm = TRUE),
      PI_univ_fem = sum(peso[PI_univ_fem == 1], na.rm = TRUE)
    ) %>%
    pivot_longer(
      Total:PI_univ_fem,
      names_to = "subgrupos",
      values_to = "N"
    ) %>%
    pivot_wider(
      names_from = ano,
      values_from = N,
      names_prefix = "ano_"
    )

  if(i == 1){
    tabela <- t
  } else{
    tabela <- t %>%
      bind_cols(t %>% select(-subgrupos))
  }

  rm(df_pop)
  invisible(gc())
  print(paste0("Finalizamos a construção da tabela para : ", year[i],"..."))
}

# Criando tabulacoes relativas

# Criando grafico(s)

# Exportacao
