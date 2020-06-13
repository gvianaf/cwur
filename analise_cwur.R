
library(tidyverse)
library(readxl)
library(janitor)
library(ggbump)

options(OutDec = ",")
# --- lê o arquivo
arquivo <- "dados_2014_2020.xlsx"

dados <- arquivo %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(read_excel, path = arquivo, .id = "SheetName") %>% 
  clean_names()   # arruma os nomes

# --- verifica cada variável
# map(dados, ~count(data.frame(x = .x), x))   # não dá muito certo

dados %>% count(sheet_name)   # transformar em factor
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

# --- arruma as variáveis
dados_clean <- dados %>% 
  mutate(sheet_name = factor(sheet_name,
                             levels = c("2014",
                                        "2015",
                                        "2016",
                                        "2017",
                                        "2018-2019",
                                        "2019-2020",
                                        "2020-2021")))
dados_clean %>% count(sheet_name)

### vou separar para ficar mais legível
dados_clean <- dados_clean %>% 
  mutate(quality_of_education = as.double(str_remove(quality_of_education, "\\D")))
dados_clean %>% count(quality_of_education, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(alumni_employment = as.double(str_remove(alumni_employment, "\\D")))
dados_clean %>% count(alumni_employment, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(quality_of_faculty = as.double(str_remove(quality_of_faculty, "\\D")))
dados_clean %>% count(quality_of_faculty, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(influence = as.double(str_remove(influence, "\\D")))
dados_clean %>% count(influence, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(citations = as.double(str_remove(citations, "\\D")))
dados_clean %>% count(citations, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(patents = as.double(str_remove(patents, "\\D")))
dados_clean %>% count(patents, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(research_output = as.double(str_remove(research_output, "\\D")))
dados_clean %>% count(research_output, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(research_performance = as.double(str_remove(research_performance, "\\D")))
dados_clean %>% count(research_performance, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(quality_publications = as.double(str_remove(quality_publications, "\\D")))
dados_clean %>% count(quality_publications, sort = TRUE)

dados_clean <- dados_clean %>% 
  mutate(score = as.double(score))
dados_clean %>% count(score, sort = TRUE)
summary(dados_clean$score)

dados_clean <- dados_clean %>% 
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

# salva o arquivo limpo
rio::export(dados_clean, "CWUR_SH.xlsx")

# --- quero inicialmente ver só o Brasil
cwur_br <- dados_clean %>% 
  filter(location == "Brazil")

# --- informações adicionais
# já tem variável de classificação nacional
# cria a variável de classificação federal
cwur_br <- cwur_br %>% 
  left_join(cwur_br %>% 
              mutate(federal = ifelse(str_detect(institution, "Federal") | institution == "University of Brasília", 1, 0)) %>% 
              filter(federal == 1) %>% 
              group_by(ano) %>% 
              mutate(federal_rank = row_number()) %>% 
              select(ano, institution, federal_rank))

# --- gráficos de acompanhamento https://github.com/davidsjoberg/ggbump
# arruma a base para o gráfico
# quero saber quem são as mais presentes nesses resultados
br_n <- cwur_br %>% 
  count(institution, sort = TRUE) %>% 
  filter(n >= 7)

# tem 15 que apareceram em todas as versões. vamos utilizá-las
cwur_br_g <- cwur_br %>% 
  filter(institution %in% br_n$institution) %>% 
  select(institution, ano, national_rank)
  
cwur_br_g %>% 
  ggplot(aes(x = ano, y = national_rank, color = institution)) +
  geom_bump() +
  theme(legend.position = "none") +
  scale_y_continuous(trans = "reverse")

# tem muita informação... tentar o ranking federal
cwur_br_g <- cwur_br %>% 
  filter(institution %in% br_n$institution) %>% 
  select(institution, ano, federal_rank) %>% 
  drop_na()

cwur_br_g %>% 
  ggplot(aes(x = ano, y = federal_rank, color = institution)) +
  geom_bump() +
  theme(legend.position = "none") +
  scale_y_continuous(trans = "reverse")

# esse dá pra manter, vamos formatar
# preciso das siglas antes
siglas <- cwur_br_g %>% 
  distinct(institution) %>% 
  mutate(sigla = case_when(
    
    institution == "Federal University of Rio de Janeiro" ~ "UFRJ",
    institution == "Federal University of Rio Grande do Sul" ~ "UFRGS",
    institution == "Federal University of Minas Gerais" ~ "UFMG",
    institution == "Federal University of São Paulo" ~ "UNIFESP",
    institution == "Federal University of Santa Catarina" ~ "UFSC",
    institution == "Federal University of Paraná" ~ "UFPR",
    institution == "Federal University of Pernambuco" ~ "UFPE",
    institution == "Federal University of São Carlos" ~ "UFSCar",
    institution == "Fluminense Federal University" ~ "UFF",
    institution == "Federal University of Santa Maria" ~ "UFSM",
    institution == "Federal University of Bahia" ~ "UFBA",
    institution == "University of Brasília" ~ "UnB"
    
  ))

cwur_br_g <- cwur_br_g %>% 
  left_join(siglas) %>% 
  select(-institution)

# pantone classic blue #0F4C81
# pantone silver-plated #BCB29E
# pantone opal gray #A49E9E
cores <- c("UFRJ"    = "#A49E9E",
           "UFRGS"   = "#A49E9E",
           "UFMG"    = "#A49E9E",
           "UNIFESP" = "#A49E9E",
           "UFSC"    = "#A49E9E",
           "UFPR"    = "#A49E9E",
           "UFPE"    = "#A49E9E",
           "UFSCar"  = "#A49E9E",
           "UFF"     = "#A49E9E",
           "UFSM"    = "#A49E9E",
           "UFBA"    = "#A49E9E",
           "UnB"     = "#0F4C81")

library(showtext)
font_add("charter", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Regular.otf")
font_add("charter-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Bold.otf")
showtext_auto()

theme_set(theme_classic(base_family = "charter"))
theme_update(legend.position = "none",
             axis.line.y = element_blank(),
             axis.line.x = element_blank())

graf <- cwur_br_g %>% 
  ggplot(aes(x = ano, y = federal_rank, color = sigla)) +
  geom_point(size = 4) +
  geom_bump(size = 2, smooth = 8) +
  geom_text(data = cwur_br_g %>% filter(ano == min(ano)), family = "charter",
            aes(x = ano - .1, label = sigla), size = 5, hjust = 1) +
  geom_text(data = cwur_br_g %>% filter(ano == max(ano)), family = "charter",
            aes(x = ano + .1, label = sigla), size = 5, hjust = 0) +
  geom_text(data = cwur_br_g %>% filter(ano == max(ano)), family = "charter",
            aes(x = ano + 1.1, label = glue::glue("{federal_rank}ª"), size = 5, hjust = 0)) +
  scale_y_reverse(breaks = c(seq(1, 18))) +
  scale_x_continuous(limits = c(2013.3, 2021.2),
                     breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  scale_color_manual(values = cores, guide = F) +
  labs(title = "Evolução das Universidades Federais no Ranking CWUR",
       subtitle = "A UnB destaca-se entre as melhores IES Federais, mantendo a 7ª posição nos últimos três anos",
       x = "Ano de divulgação do ranking",
       y = "") +
  # cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(axis.title.x = element_text(hjust = 0.78),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(family = "charter-bold")) +
  annotate(geom = "curve", x = 2017, xend = 2018, y = 15, yend = 7.3,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2017, y = 15, hjust = "right", family = "charter",
           label = "A partir de 2018, a metodologia do CWUR\nfoi revista e aprimorada")


graf
ggsave("cwur-federais.pdf", width = 8, height = 6, device = cairo_pdf)
pdftools::pdf_convert("cwur-federais.pdf", format = "png", dpi = 350)

showtext_auto(FALSE)

# --- tabelas
# tabela de série histórica da UnB
tab_unb <- cwur_br %>% 
  filter(institution == "University of Brasília") %>% 
  select("Ano do ranking" = ano, 
         "Posição mundial" = world_rank, 
         "Posição nacional" = national_rank, 
         "Posição federal" = federal_rank, 
         "Pontuação" = score) %>% 
  arrange(desc(`Ano do ranking`))

tab_unb

# tabela do último resultado
tab_geral <- cwur_br %>% 
  # left_join(siglas) %>% 
  filter(ano == max(ano)) %>% 
  select(Universidade = institution, 
         "Posição mundial" = world_rank, 
         "Posição nacional" = national_rank, 
         "Posição federal" = federal_rank, 
         "Pontuação" = score) %>% 
  head(20)

tab_geral

