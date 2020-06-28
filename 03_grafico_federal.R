
library(tidyverse)
library(ggbump)
library(showtext)
library(ggtext)
library(patchwork)

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
# pegar as federais apenas

br_2020 <- cwur_br %>% 
  filter(ano == 2020,
         str_detect(institution, "Federal") | sigla == "UnB") %>% 
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

cwur_br_federal <- cwur_br %>% filter(sigla %in% br_2020$sigla)

# pantone classic blue #0F4C81
# pantone opal gray #A49E9E

# carrega as fontes para serem usadas no gráfico

font_add("charter", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Regular.otf")
font_add("charter-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Bold.otf")
font_add("fira", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Regular.ttf")
showtext_auto()

# theme_set(theme_classic(base_family = "charter"))
# theme_update(legend.position = "none",
#              axis.line.y = element_blank(),
#              axis.line.x = element_blank())

graf <- cwur_br_federal %>% 
  ggplot(aes(x = ano, y = federal_rank, fill = sigla)) +
  geom_point(size = 4, aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E"))) +
  geom_bump(size = 2, smooth = 8, aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E"))) +
  scale_color_identity() +
  scale_y_reverse(breaks = c(seq(1, 18))) +
  scale_x_continuous(limits = c(2013.2, 2021.2),
                     breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  geom_text(data = cwur_br_federal %>% filter(ano == min(ano)), family = "fira",
            aes(x = ano - .1, label = sigla), size = 5, hjust = 1) +
  geom_text(data = cwur_br_federal %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + .1, label = sigla), size = 5, hjust = 0) +
  geom_text(data = cwur_br_federal %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + 1.2, label = glue::glue("{federal_rank}ª"), size = 5, hjust = 0)) +
  # quero botar o score ao lado da colocação
  # geom_text(data = cwur_br_federal %>% filter(ano == max(ano)), family = "fira", color = "#A58D7F",
  #           aes(x = ano + 1.6, label = glue::glue("({format(score, nsmall = 1)})"), size = 5, hjust = 0)) +
  labs(title = "Evolução das Universidades Federais no Ranking CWUR",
       subtitle = "A <span style='color:#0F4C81'>Universidade de Brasília</span> destaca-se entre as melhores IES Federais,<br>mantendo a 7ª posição nos últimos três anos",
       x = "Ano de divulgação do ranking",
       y = "") +
  theme_classic(base_family = "charter") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_text(hjust = 0.78),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(family = "charter-bold"),
        plot.subtitle = element_markdown(lineheight = 1.2)) +
  annotate(geom = "curve", x = 2017, xend = 2018, y = 16, yend = 7.3, curvature = 0.3,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2017, y = 16, hjust = 1, family = "fira",
           label = "A partir de 2018, a metodologia do CWUR\nfoi revista e aprimorada")

graf
ggsave("cwur-federal.pdf", width = 8, height = 6, device = cairo_pdf)
pdftools::pdf_convert("cwur-federal.pdf", format = "png", dpi = 350)

# ---- gráfico da evolução do score

# theme_set(theme_light(base_family = "charter"))
# theme_update(legend.position = "none",
#              axis.line.y = element_blank(),
#              axis.line.x = element_blank(),
#              panel.grid.minor = element_blank())

graf_score <- cwur_br_federal %>% 
  ggplot(aes(x = ano, y = score, fill = sigla)) +
  geom_line(aes(color = ifelse(sigla == "UnB", "#0F4C81", alpha("#A49E9E", 0.3)))) +
  # geom_point(aes(color = ifelse(sigla == "UnB", "#0F4C81", "#A49E9E")), size = 0.75) +
  scale_color_identity() +
  scale_x_continuous(breaks = c(seq(2014,2020))) +
  scale_y_continuous(breaks = c(seq(40,80, 10))) +
  expand_limits(y = c(40,80)) +
  labs(title = "Evolução da nota final das Universidades Federais no Ranking CWUR",
       subtitle = "Após a revisão metodológica que tomou efeito a partir de 2018, ocorreu uma mudança de nível nas notas,<br>inclusive para a <span style='color:#0F4C81'>Universidade de Brasília</span>. No entanto, as diferenças entre as IFES são mínimas.",
       x = "Ano de divulgação do ranking",
       y = "Nota final") +
  theme_light(base_family = "charter") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_markdown(family = "charter", lineheight = 1.2),
        plot.title = element_markdown(family = "charter-bold"),
        axis.title.x = element_text(hjust = 1, size = 8),
        axis.title.y = element_text(hjust = 1, size = 8),
        axis.text.y = element_text(size = 7),
        panel.grid.major.x = element_blank())

graf_score
ggsave("cwur-score-federal.pdf", width = 8, height = 3, device = cairo_pdf)
pdftools::pdf_convert("cwur-score-federal.pdf", format = "png", dpi = 350)

# junta os gráficos

p <- graf_score / graf + plot_layout(heights = c(1,3))
ggsave("cwur-federal-conjunto.pdf", width = 8, height = 9, device = cairo_pdf)
pdftools::pdf_convert("cwur-federal-conjunto.pdf", format = "png", dpi = 350)

showtext_auto(FALSE)

