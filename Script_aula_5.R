# Pacotes ----

library(tidyverse)

library(readxl)

library(report)

library(performance)

# Dados ----

## Importando ----

dados_1 <- readxl::read_xlsx("Atividade_1.xlsx",
                             sheet = 3)

dados_1

## Tratando ----

### gerando dados aleatórios ----

set.seed(123); runif(10, min = 5, max = 11) %>% round(1) -> com3

com3

### Criando uma nova variável com os dados aleatórios ----

dados_1_trat <- dados_1 %>%
  dplyr::select(6:7) %>%
  dplyr::rename(`Plantação` = `3`, `Restauração` = `...7`) %>%
  dplyr::mutate(`Floresta Nativa` = com3)

dados_1_trat

## Checando ----

dados_1_trat %>%
  dplyr::glimpse()

dados_1_trat %>%
  report::report()

# Modelando ----

## Tratando os dados ----

dados_1_trat_long <- dados_1_trat %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Tratamento",
                      values_to = "Diversidade")

dados_1_trat_long

## Criando o modelo ----

modelo_anova <- aov(Diversidade ~ Tratamento, data = dados_1_trat_long)

## Pressupostos ----

### Normalidade ----

normalidade <- modelo_anova %>%
  stats::residuals() %>%
  stats::shapiro.test()

normalidade

### Homogeneidade de variâncias ----

homogeneidade <- stats::bartlett.test(Diversidade ~ Tratamento, data = dados_1_trat_long)

homogeneidade

### Gráficos de avaliações ----

modelo_anova %>%
  performance::check_model()

## Avaliando o modelo ----

modelo_anova %>%
  anova()

modelo_anova %>%
  report::report()

## F-Crítico ----

f_critico <- stats::qf(p = 0.05, df1 = 2, df2 = 27, lower.tail = FALSE)

f_critico

## Avaliação post-hoc ----

teste_dunn <- FSA::dunnTest(Diversidade ~ Tratamento, data = dados_1_trat_long)

teste_dunn

# Gráfico ----

## Estatística ----

modelo_anova_estatisticas <- modelo_anova %>% anova()

modelo_anova_estatisticas

anova_estatisticas <- stringr::str_glue("F-Crítico<sub>(GL = 2, 27, α = 0.05)</sub> = {f_critico %>% round(2)}, F<sub>({modelo_anova_estatisticas$Df[1]}, {modelo_anova_estatisticas$Df[2]})</sub> = {modelo_anova_estatisticas$`F value`[1] %>% round(3)}, p < 0.01")

anova_estatisticas

## Gráfico ----

theme_set(theme(axis.text = element_text(color = "black", size = 20, family = "Times New Roman"),
                axis.title = element_text(color = "black", size = 20, family = "Times New Roman"),
                panel.grid.major  = element_line(colour = "gray90", linewidth = 1),
                legend.text = element_text(color = "black", size = 20, family = "Times New Roman"),
                legend.title = element_text(color = "black", size = 20, family = "Times New Roman"),
                legend.position = "bottom",
                panel.border = element_rect(colour = 1, fill = "transparent", size = 1),
                panel.background = element_rect(colour = NA, fill = "white")))

dados_1_trat_long %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(dados_1_trat_long$Tratamento %>% unique)) %>%
  ggplot(aes(Tratamento, Diversidade, fill = Tratamento)) +
  geom_boxplot(color = "black", width = 0.25, show.legend = FALSE) +
  geom_jitter(color = "black", shape = 21, size = 5, width = 0.1, show.legend = FALSE) +
  ggtext::geom_richtext(aes(x = 2, y = 11, label = anova_estatisticas),
                        color = "black",
                        size = 7.5,
                        fill = NA,
                        label.color = NA,
                        family = "Times New Roman") +
  labs(x = NULL,
       y = "Índice de Hill para q = 1",
       fill = NULL) +
  scale_y_continuous(breaks = seq(4.5, 11, 0.5), limits = c(4.5, 11)) +
  scale_fill_manual(values = c("#BB7F4E", "#E4C62D", "#46CD57"))

ggsave("grafico_anova_atividade_5.png", height = 10, width = 12)

# Tabela do teste de Dunn ----

## Gernado o flextable ----

dunn_flextable <- teste_dunn$res %>%
  dplyr::mutate(Comparação = Comparison,
                Z = Z,
                p = P.adj %>% as.numeric(),
                p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p %>% round(3) %>% as.character())) %>%
  dplyr::select(c(5, 2, 6)) %>%
  flextable::flextable() %>%
  flextable::width(width = 2.15) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all")

dunn_flextable

## Salvando a tabela ----

dunn_flextable %>%
  flextable::save_as_docx(path = "tabela_dunn_Atividade_5.docx")


