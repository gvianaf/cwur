
library(tidyverse)
library(readxl)
library(janitor)

options(OutDec = ",")

# ---- lê o arquivo

arquivo <- "dados_2014_2020.xlsx"

dados <- arquivo %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(read_excel, path = arquivo, .id = "SheetName") %>% 
  clean_names()   # arruma os nomes

# ---- verifica cada variável

glimpse(dados)
# map(dados, ~count(data.frame(x = .x), x))   # não dá muito certo

dados %>% count(sheet_name)   # transformar em número
dados %>% count(world_rank)   # já é número
dados %>% count(institution == "University of Brasília")
dados %>% count(location == "Brazil")
dados %>% count(national_rank)   # já é número
dados %>% count(quality_of_education, sort = TRUE)   # transformar em número
dados %>% count(alumni_employment, sort = TRUE)      # transformar em número
dados %>% count(quality_of_faculty, sort = TRUE)     # transformar em número
dados %>% count(research_output, sort = TRUE)        # transformar em número
dados %>% count(quality_publications, sort = TRUE)   # transformar em número
dados %>% count(publications)
dados %>% count(influence, sort = TRUE)   # transformar em número
dados %>% count(citations, sort = TRUE)   # transformar em número
dados %>% count(broad_impact)
dados %>% count(patents, sort = TRUE)   # transformar em número
dados %>% count(score, sort = TRUE)     # transformar em número
dados %>% count(research_performance, sort = TRUE)     # transformar em número

# ---- arruma as variáveis

# o problema de todas as notas é ter símbolos misturados (+, -, etc.)
# fazer função para aplicar em todas as variáveis que estão como chr

tira_simbolos <- function(variavel){as.double(str_remove(variavel, "\\D"))}

dados_limpos <- dados %>% 
  # tirei o score da transformação, pois deu erro
  modify_at(c(6:12, 14:17), tira_simbolos) %>% 
  mutate(score = as.double(score))

# arruma o ano, preciso que seja numérica

dados_limpos <- dados_limpos %>% 
  mutate(ano = case_when(
    
    sheet_name == "2014" ~ 2014,
    sheet_name == "2015" ~ 2015,
    sheet_name == "2016" ~ 2016,
    sheet_name == "2017" ~ 2017,
    sheet_name == "2018-2019" ~ 2018,
    sheet_name == "2019-2020" ~ 2019,
    sheet_name == "2020-2021" ~ 2020
    
  )) %>% 
  select(-sheet_name)

glimpse(dados_limpos)

dados_limpos %>% map(., summary)

# parece tudo certo
# salva o arquivo limpo

rio::export(dados_limpos, "CWUR_SH.xlsx")

# ---- quero inicialmente só o Brasil
dados_limpos_br <- dados_limpos %>% 
  filter(location == "Brazil")

# vou precisar das siglas das universidades brasileiras

siglas <- dados_limpos_br %>% 
  distinct(institution)

# são muitas... vou exportar e preencher manualmente

rio::export(siglas, "siglas_br.xlsx")

# siglas preenchidas

siglas <- rio::import("siglas_br_preenchido.xlsx")
dados_limpos_br <- dados_limpos_br %>% left_join(siglas)
dados_limpos_br %>% count(is.na(sigla))

# --- informações adicionais
# já tem variável de classificação nacional
# cria a variável de classificação federal
dados_limpos_br <- dados_limpos_br %>% 
  left_join(dados_limpos_br %>% 
              mutate(federal = ifelse(str_detect(institution, "Federal") | institution == "University of Brasília", 
                                      1, 
                                      0)) %>% 
              filter(federal == 1) %>% 
              group_by(ano) %>% 
              mutate(federal_rank = row_number()) %>% 
              select(ano, institution, federal_rank))

# salva o arquivo BR

rio::export(dados_limpos_br, "CWUR_SH_BR.xlsx")
