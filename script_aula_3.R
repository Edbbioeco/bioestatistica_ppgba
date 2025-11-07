# Pacotes ----

library(tidyverse)

library(readxl)

library(report)

library(flextable)

library(FSA)

library(ggtext)

# Dados ----

## Importando ----

dados_1 <- readxl::read_xlsx("Atividade_1.xlsx",
                             sheet = 3)

dados_1

## gerando dados aleatórios ----

set.seed(123); runif(10, min = 5, max = 11) %>% round(1) -> com3

com3

## Tratando ----

dados_1_trat <- dados_1 %>%
  dplyr::select(6:7) %>%
  dplyr::mutate(`Floresta Nativa` = com3) %>%
  dplyr::rename(`Plantação` = `3`, `Restauração` = `...7`) %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Tratamento",
                      values_to = "Diversidade")

dados_1_trat

dados_1_trat_mw <- dados_1_trat %>%
  dplyr::filter(Tratamento != "Restauração")

dados_1_trat_mw

# Mann-Whitney ----

## Modelando ----

mann_whitney <- stats::wilcox.test(Diversidade ~ Tratamento,
                                   data = dados_1_trat_mw,
                                   exact = FALSE)

mann_whitney

# Signed-Rank ----

## Modelando ----

dados_1_trat_sr <- dados_1 %>%
  dplyr::select(6:7) %>%
  dplyr::mutate(`Floresta Nativa` = com3) %>%
  dplyr::rename(`Plantação` = `3`, `Restauração` = `...7`) %>%
  dplyr::select(c(1, 3))

dados_1_trat_sr

signed_rank <- stats::wilcox.test(dados_1_trat_sr$Plantação,
                                  dados_1_trat_sr$`Floresta Nativa`,
                                  paired = TRUE)

signed_rank

# Tabela de Mann-Whitney e Signed-Rank ----

## Calculando as medianas ----

medianas <- dados_1_trat %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(dados_1_trat$Tratamento %>% unique)) %>%
  dplyr::group_by(Tratamento) %>%
  dplyr::summarise(Mediana = Diversidade %>% median())

medianas

## Criando as tabelas ----

tabela_mannwhitney <- tibble::tibble(Teste = "Mann-Whitney",
                                     `Mediana do Grupo 1` = medianas$Mediana[1],
                                     `Mediana do Grupo 2` = medianas$Mediana[3],
                                     Estatística = paste0("W = ", mann_whitney$statistic),
                                     p = "< 0.001")

tebela_signedrank <- tibble::tibble(Teste = "Signed-Rank",
                                    `Mediana do Grupo 1` = medianas$Mediana[1],
                                   `Mediana do Grupo 2` = medianas$Mediana[3],
                                   Estatística = paste0("V = ", signed_rank$statistic),
                                   p = "< 0.001")

## Unindo ----

tabelas_unidas <- dplyr::bind_rows(tabela_mannwhitney, tebela_signedrank)

tabelas_unidas

## Criando e salvando o flextable ----

tabelas_unidas_flex <- tabelas_unidas %>%
  flextable::flextable() %>%
  flextable::width(width = 1.25) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all")

tabelas_unidas_flex

tabelas_unidas_flex %>%
  flextable::save_as_docx(path = "tabela_mannwhitney_signedrank_atividade_3.docx")

# Gráfico de Mann-Whitney e Signed-Rank ----

## Estatísticas

label = "Mann-Whitney: \n\nW = 95, p < 0.001 \n\nSigned Rank: \n\nV = 0, p < 0.001"

dados_1_trat %>%
  dplyr::filter(Tratamento != "Restauração") %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(c("Plantação", "Floresta Nativa"))) %>%
  ggplot(aes(Tratamento, Diversidade, fill = Tratamento)) +
  geom_boxplot(color = "black", width = 0.25, show.legend = FALSE) +
  geom_jitter(width = 0.1, color = "black", shape = 21, size = 5, show.legend = FALSE) +
  ggforce::geom_link(aes(x = 1, xend = 2, y = medianas$Mediana[1], yend = medianas$Mediana[3]),
                     color = "black",
                     linetype = "dashed") +
  ggtext::geom_richtext(aes(x = 0.75, y = 9.5, label = label, fontface = 2),
                        color = "black",
                        size = 6,
                        fill = NA,
                        label.color = NA,
                        family = "Times New Roman") +
  scale_fill_manual(values = c("#BB7F4E", "#46CD57")) +
  scale_y_continuous(breaks = seq(4.5, 10.5, 0.5), limits = c(4.5, 10.5)) +
  labs(x = NULL,
       y = "Índice de Diversidade de Hill para q = 1") +
  theme(axis.text = element_text(color = "black", size = 20, family = "Times New Roman"),
        axis.title = element_text(color = "black", size = 20, family = "Times New Roman"),
        panel.grid.major  = element_line(colour = "gray90", linewidth = 1),
        legend.text = element_text(color = "black", size = 20, family = "Times New Roman"),
        legend.title = element_text(color = "black", size = 20, family = "Times New Roman"),
        legend.position = "bottom",
        panel.border = element_rect(colour = 1, fill = "transparent", size = 1),
        panel.background = element_rect(colour = NA, fill = "white"))

ggsave("mannwhitney_signedrank_atividade_3.png", height = 10, width = 12)

# Kruskall Wallis ----

## Modelando ----

kruskall_wallis <- stats::kruskal.test(Diversidade ~ Tratamento, data = dados_1_trat)

kruskall_wallis

## Post-hoc ----

### Modelando ----

dunn <- FSA::dunnTest(Diversidade ~ Tratamento, data = dados_1_trat)

dunn

### Tabela ----

#### Criando ----

dunn_flex <- dunn$res %>%
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

dunn_flex

#### Salvando ----

dunn_flex %>%
  flextable::save_as_docx(path = "teste_dunn_atividade_3.docx")

## Tabela ----

### Criando ----

kruskall_wallis_flex <- tibble::tibble(`X²-crítico` = stats::qchisq(0.05, df = 2, lower.tail = FALSE) %>% round(3),
                                       `X²` = kruskall_wallis$statistic %>% round(3),
                                      `Graus de liberdade` = kruskall_wallis$parameter,
                                        p = "< 0.001") %>%
  flextable::flextable() %>%
  flextable::width(width = 1.25) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all")

kruskall_wallis_flex

### Salvando ----

kruskall_wallis_flex %>%
  flextable::save_as_docx(path = "tabela_kruskall_wallis_rank_atividade_3.docx")

## Gráfico ----

dados_1_trat %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(dados_1_trat$Tratamento %>% unique)) %>%
  ggplot(aes(Tratamento, Diversidade, fill = Tratamento)) +
  geom_boxplot(color = "black", width = 0.25) +
  geom_jitter(width = 0.1, color = "black", shape = 21, size = 5, show.legend = FALSE) +
  ggtext::geom_richtext(aes(x = 1.5, y = 10, label = "x² <sub>(2)</sub> = 16.55, p < 0.001"),
                        color = "black",
                        size = 7.5,
                        family = "Times New Roman",
                        label.color = NA,
                        fill = NA) +
  scale_y_continuous(breaks = seq(4.5, 10.5, 0.5), limits = c(4.5, 10.5)) +
  scale_fill_manual(values = c("#BB7F4E", "#E4C62D", "#46CD57")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  labs(x = NULL) +
  theme(axis.text = element_text(color = "black", size = 15, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      panel.grid.major  = element_line(colour = "gray90", linewidth = 1),
      legend.text = element_text(color = "black", size = 15, family = "Times New Roman"),
      legend.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      legend.position = "none",
      panel.border = element_rect(colour = 1, fill = "transparent", size = 1),
      panel.background = element_rect(colour = NA, fill = "white"))

ggsave("kuskarll_wallis_atividade_3.png", height = 10, width = 12)

aov(Diversidade ~ Tratamento, data = dados_1_trat) %>%
  performance::check_model()
