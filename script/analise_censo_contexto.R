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

# Importacao dos dados de interesse ---------------------------------------

year = c(2000, 2010, 2022)

for(i in seq_along(year)){

  if(year[i] == 2000){
    column = c("P001","V0408","V1006","V4752","V0401","V0432","V0434")
  } else{
    column = c("V0010","V0606","V1006","V6036","V0601","V6400")
  }
  if(year[i] != 2022){
    df_pop <- read_population(
      year = year[i],
      columns = column,
      as_data_frame = TRUE,
      showProgress = TRUE,
      cache = FALSE
    )
  }

  # Manipulação dos dados

  if(year[i] == 2000){
    df_pop <- df_pop %>%
      as_tibble() %>%
      rename(
        peso = P001,
        raca = V0408,
        sexo = V0401,
        situacao_dom = V1006,
        idade = V4752,
        educ = V0432,
        educ_conclusao = V0434
      ) %>%
      mutate(
        PopFem = case_when(sexo == 2 ~ 1, TRUE ~ 0),
        PPI = case_when(raca %in% c(2,4,5) ~ 1, TRUE ~ 0),
        PI = case_when(raca %in% c(5) ~ 1, TRUE ~ 0),
        PPI_fem = case_when(raca %in% c(2,4,5) & sexo == 2 ~ 1, TRUE ~ 0),
        PI_fem = case_when(raca %in% c(5) & sexo == 2 ~ 1, TRUE ~ 0),
        univ = case_when(educ == 4 ~ 1, TRUE ~ 0),
        univ_fem = case_when(educ == 4 & sexo == 2 ~ 1, TRUE ~ 0),
        PPI_univ = case_when(raca %in% c(2,4,5) & educ == 7 & educ_conclusao == 1 ~ 1, TRUE ~ 0),
        PI_univ = case_when(raca %in% c(5) & educ == 7 & educ_conclusao == 1 ~ 1, TRUE ~ 0),
        PPI_univ_fem = case_when(raca %in% c(2,4,5) & educ == 7 & educ_conclusao == 1 & sexo == 2 ~ 1, TRUE ~ 0),
        PI_univ_fem = case_when(raca %in% c(5) & educ == 7 & educ_conclusao == 1 & sexo == 2 ~ 1, TRUE ~ 0)
      ) %>%
      mutate_all(., ~as.numeric(.x))
  }
  if(year[i] == 2010){
    df_pop <- df_pop %>%
      as_tibble() %>%
      rename(
        peso = V0010,
        raca = V0606,
        sexo = V0601,
        situacao_dom = V1006,
        idade = V6036,
        educ = V6400
      ) %>%
      mutate(
        PopFem = case_when(sexo == 2 ~ 1, TRUE ~ 0),
        PPI = case_when(raca %in% c(2,4,5) ~ 1, TRUE ~ 0),
        PI = case_when(raca %in% c(5) ~ 1, TRUE ~ 0),
        PPI_fem = case_when(raca %in% c(2,4,5) & sexo == 2 ~ 1, TRUE ~ 0),
        PI_fem = case_when(raca %in% c(5) & sexo == 2 ~ 1, TRUE ~ 0),
        univ = case_when(educ == 4 ~ 1, TRUE ~ 0),
        univ_fem = case_when(educ == 4 & sexo == 2 ~ 1, TRUE ~ 0),
        PPI_univ = case_when(raca %in% c(2,4,5) & educ == 4 ~ 1, TRUE ~ 0),
        PI_univ = case_when(raca %in% c(5) & educ == 4 ~ 1, TRUE ~ 0),
        PPI_univ_fem = case_when(raca %in% c(2,4,5) & educ == 4 & sexo == 2 ~ 1, TRUE ~ 0),
        PI_univ_fem = case_when(raca %in% c(5) & educ == 4 & sexo == 2 ~ 1, TRUE ~ 0)
      ) %>%
      mutate_all(., ~as.numeric(.x))
  }

  # Criacao de tabela
  if(year[i] != 2022){
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
        PI_univ = sum(peso[PI_univ == 1], na.rm = TRUE),
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
  }

  if(year[i] == 2022){
    # Incorporacao de 2022 - dados divulgados ate 2024.10.06

    t <- read_csv2("./input/dados_censo_2022.csv") %>%
      mutate(
        Total = case_when(Cor_raca == "Total" & sexo == "Total" ~ 1, TRUE ~ 0),
        PopFem = case_when(Cor_raca == "Total" & sexo == "Mulheres" ~ 1, TRUE ~ 0),
        PPI = case_when(Cor_raca != "Total" & sexo == "Total" ~ 1, TRUE ~ 0),
        PI = case_when(Cor_raca == "Indígena" & sexo == "Total" ~ 1, TRUE ~ 0),
        PPI_fem = case_when(Cor_raca != "Total" & sexo == "Mulheres" ~ 1, TRUE ~ 0),
        PI_fem = case_when(Cor_raca == "Indígena" & sexo == "Mulheres" ~ 1, TRUE ~ 0)
      ) %>%
      summarise(
        ano = year[i],
        Total = sum(N[Total == 1], na.rm = TRUE),
        Feminino = sum(N[PopFem == 1], na.rm = TRUE),
        PPI = sum(N[PPI == 1], na.rm = TRUE),
        PI = sum(N[PI == 1], na.rm = TRUE),
        PPI_fem = sum(N[PPI_fem == 1], na.rm = TRUE),
        PI_fem = sum(N[PI_fem == 1], na.rm = TRUE),
        univ = NA_integer_,
        PPI_univ = NA_integer_,
        PI_univ = NA_integer_,
        univ_fem = NA_integer_,
        PPI_univ_fem = NA_integer_,
        PI_univ_fem = NA_integer_
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
  }

  if(i == 1){
    tabela <- t
  } else{
    tabela <- tabela %>%
      bind_cols(t %>% select(-subgrupos))
  }

  rm(df_pop)
  invisible(gc())
  print(paste0("Finalizamos a construção da tabela para : ", year[i],"..."))
}

# Criando tabulacoes relativas

tabela <- tabela %>%
  mutate(
    tx_10_00 = 100*(log(ano_2010/ano_2000)/(2010-2000)),
    tx_22_10 = 100*(log(ano_2022/ano_2010)/(2022-2010)),
  )

# Criando grafico(s)

tabela_grafico <- tabela %>%
  filter(!grepl("univ",subgrupos)) %>%
  select(subgrupos, tx_10_00,tx_22_10) %>%
  mutate(
    subgrupos = factor(
      subgrupos,
      levels = c(subgrupos),
      labels = c("Total","Pop. Feminina","PPI","Pop. Indígena","PPI Feminina","Pop. Indígena Feminina")
    )
  )

# tabela_grafico %>%
#   ggplot() +
#   geom_segment(aes(x=subgrupos, xend=subgrupos, y=var_10_00, yend=var_22_10), color="grey") +
#   geom_point(aes(x=subgrupos, y=var_10_00), color=rgb(0.2,0.7,0.1,0.5), size=3) +
#   geom_point(aes(x=subgrupos, y=var_22_10), color=rgb(0.7,0.2,0.1,0.5), size=3) +
#   coord_flip() +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#   ) +
#   labs(
#     x = "Variação relativa entre anos censitários (%)",
#     y = "Subgrupos populacionais",
#     title = "Variação relativa da população entre anos censitários por subgrupos por cor ou raça e sexo - Brasil, 2000-2022."
#   )

right_label <- paste(tabela_grafico$subgrupos)

tabela_grafico$class <- ifelse((tabela_grafico$tx_22_10 - tabela_grafico$tx_10_00) < 0, "red","green")

tabela_grafico %>%
  ggplot() +
  geom_segment(aes(x=1, xend=2, y=tx_10_00, yend=tx_22_10, col=class), size=.75, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"),
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(
    y = "Taxa de crescimento média da população entre anos\ncensitários anualizada (%)",
    title = "Taxa de crescimento média da população entre anos censitários anualizada por subgrupos por cor ou raça e sexo - Brasil, 2000-2022."
  ) +
  scale_y_continuous(breaks = seq(0,5,.5)) +
  coord_cartesian(xlim = c(.9, 2.3), ylim = c(0,5)) +
  geom_text(label=right_label, y=tabela_grafico$tx_22_10, x=rep(2, NROW(tabela_grafico)), hjust=-0.1, size=3.5) +
  geom_text(label="2010-2000", x=1, y=1.1*(max(tabela_grafico$tx_22_10,tabela_grafico$tx_10_00)), hjust=1.2, size=5) +
  geom_text(label="2022-2010", x=2, y=1.1*(max(tabela_grafico$tx_22_10,tabela_grafico$tx_10_00)), hjust=-0.1, size=5) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(1,2,1,2), "cm")
  )


# Exportacao

tabela_exportacao <- tabela %>%
  mutate(
    subgrupos = factor(
      subgrupos,
      levels = c(subgrupos),
      labels = c("Total","Pop. Feminina","PPI","Pop. Indígena","PPI Feminina","Pop. Indígena Feminina","Pop. Total com Universitário",
                 "PPI com Universitário", "Pop. Indígena com Universitário","Pop. Feminina com Universitário","PPI Feminina com Universitário",
                 "Pop. Feminina Indígena com Universitário")
    )
  )

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb = wb,sheetName = "tabela_geral")
openxlsx::writeData(
  wb = wb,
  sheet = "tabela_geral",
  x = tabela_exportacao,
  colNames = TRUE,
  rowNames = FALSE,
  keepNA = FALSE
)

openxlsx::saveWorkbook(
  wb,
  paste0('./output/','[Ultima atualizacao em ',today(),'] Tabela - Descritivas gerais.xlsx'),
  overwrite = TRUE
)
