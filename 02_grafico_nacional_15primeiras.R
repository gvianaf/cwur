
library(tidyverse)
library(ggbump)
library(showtext)
library(ggtext)

options(OutDec = ",")

# importa os dados

cwur_br <- rio::import("CWUR_SH_BR.xlsx")

# --- gráficos de acompanhamento https://github.com/davidsjoberg/ggbump
# arruma a base para o gráfico
# pega as universidades que tiveram resultado em 2020

br_2020 <- cwur_br %>% 
  filter(ano == 2020) %>% 
  distinct(sigla)

# são muitas, não vai caber no gráfico...
# pegar as 15 primeiras

br_2020 <- cwur_br %>% 
  filter(ano == 2020) %>% 
  arrange(desc(score)) %>% 
  slice(1:15) %>% 
  distinct(sigla)

cwur_br %>% 
  filter(sigla %in% br_2020$sigla) %>% 
  ggplot(aes(x = ano, y = national_rank, color = institution)) +
  geom_bump() +
  theme(legend.position = "none") +
  scale_y_continuous(trans = "reverse")

# salvar num elemento novo

cwur_br_15 <- cwur_br %>% filter(sigla %in% br_2020$sigla)

# pantone classic blue #0F4C81
# pantone opal gray #A49E9E

# carrega as fontes para serem usadas no gráfico

font_add("charter", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Regular.otf")
font_add("charter-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Bold.otf")
font_add("fira", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Regular.ttf")
showtext_auto()

theme_set(theme_classic(base_family = "charter"))
theme_update(legend.position = "none",
             axis.line.y = element_blank(),
             axis.line.x = element_blank())

graf <- cwur_br_15 %>% 
  ggplot(aes(x = ano, y = national_rank, fill = sigla)) +
  geom_point(size = 4, aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E"))) +
  geom_bump(size = 2, smooth = 8, aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E"))) +
  scale_color_identity() +
  scale_y_reverse(breaks = c(seq(1, 18))) +
  scale_x_continuous(limits = c(2013.3, 2021.1),
                     breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  geom_text(data = cwur_br_15 %>% filter(ano == min(ano)), family = "fira",
            aes(x = ano - .1, label = sigla), size = 5, hjust = 1) +
  geom_text(data = cwur_br_15 %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + .1, label = sigla), size = 5, hjust = 0) +
  geom_text(data = cwur_br_15 %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + 1.1, label = glue::glue("{national_rank}ª"), size = 5, hjust = 0)) +
  labs(title = "Evolução das Universidades Brasileiras no Ranking CWUR — 2014 a 2020",
       subtitle = "A <span style='color:#0F4C81'>Universidade de Brasília</span> manteve-se dentre as 15 melhores no Brasil<br>Instituições como Fiocruz e CBPF fizeram sua primeira aparição em 2020",
       x = "Ano de divulgação do ranking",
       y = "",
       caption = "Fonte: cwur.org\nElaboração: DAI/DPO/UnB") +
  # cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(axis.title.x = element_text(hjust = 0.79),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(family = "charter-bold"),
        plot.subtitle = element_markdown(lineheight = 1.2),
        plot.caption = element_text(margin = margin(10,0,0,0)))

graf
ggsave("cwur-nacional-15primeiras.pdf", width = 8, height = 6, device = cairo_pdf)
pdftools::pdf_convert("cwur-nacional-15primeiras.pdf", format = "png", dpi = 350)

showtext_auto(FALSE)

