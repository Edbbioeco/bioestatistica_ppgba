# Pacotes ----

library(tidyverse)

library(readxl)

library(report)

library(ggtext)

library(DescTools)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados_qui_quadrado.xlsx", sheet = 2)

## Checando ----

dados %>%
  dplyr::glimpse()

dados %>%
  report::report()

## Tratando ----

### Qui-Quadrado de aderência ----

dados_q1 <- dados %>%
  dplyr::filter(Teste == "QUIQUADRADO ADERÊNCIA FRAÇÕES ESPERADAS IGUAIS")

dados_q1

### Qui-Quadrado frequências distintas ----

dados_q2 <- dados %>%
  dplyr::filter(Teste == "QUIQUADRADO ADERÊNCIA FRAÇÕES ESPERADAS DIFERENTES (25 - 75)")

dados_q2

### Qui-Quadrado de Independência ----

dados_q3 <- dados %>%
  dplyr::filter(Teste == "QUIQUADRADO INDEPENDENCIA (2X2)")

dados_q3

### Teste G ----

dados_g <- dados %>%
  dplyr::filter(Teste == "G TESTE (4 REPETIÇÕES)")

dados_g

# Teste Qui-Quadrado de Aderência ----

## Frequência esperada ----

freq_esp_x2_1 <- (dados_q1$Abundância %>% sum()) / dados_q1$Abundância %>% length()

freq_esp_x2_1

## Modelando ----

teste_x2_aderencia <- stats::chisq.test(dados_q1$Abundância)

teste_x2_aderencia

## X²-crítico ----

x2_aderencia_critico <- stats::qchisq(p = 0.05, df = 2, lower.tail = FALSE)

x2_aderencia_critico

## Gráfico ----

estatistica_x2_aderencia <- stringr::str_glue("X²-Crítico = {x2_aderencia_critico %>% round(2)}, X²<sub>({teste_x2_aderencia$parameter})</sub> = {teste_x2_aderencia$statistic %>% round(2)}, p = {teste_x2_aderencia$p.value %>% round(2)}")

estatistica_x2_aderencia

teste_x2_aderencia$p.value

theme_set(theme(axis.text = element_text(color = "black", size = 20, family = "Times New Roman"),
                axis.title = element_text(color = "black", size = 20, family = "Times New Roman"),
                panel.grid.major  = element_line(colour = "gray90", linewidth = 1),
                legend.text = element_text(color = "black", size = 20, family = "Times New Roman"),
                legend.title = element_text(color = "black", size = 20, family = "Times New Roman"),
                legend.position = "bottom",
                panel.border = element_rect(colour = 1, fill = "transparent", size = 1),
                panel.background = element_rect(colour = NA, fill = "white")))

dados_q1 %>%
  dplyr::mutate(Tratamento = paste0("Comunidade ", 1:3)) %>%
  ggplot(aes(Tratamento, Abundância, fill = Tratamento)) +
  geom_col(color = "black", width = 0.5) +
  geom_hline(yintercept = freq_esp_x2_1, color = "black", linetype = "dashed") +
  geom_text(aes(x = 2.75, y = freq_esp_x2_1 + 0.5, label = "Frequência Esperada"),
            size = 7.5,
            family = "Times New Roman") +
  ggtext::geom_richtext(aes(x = 2, y = 17, label = estatistica_x2_aderencia),
                        color = "black",
                        size = 7.5,
                        fill = NA,
                        label.color = NA,
                        family = "Times New Roman") +
  labs(x = NULL,
       fill = NULL) +
  scale_y_continuous(breaks = seq(0, 18, 2), limits = c(0, 18)) +
  scale_fill_manual(values = c("#BB7F4E", "#E4C62D", "#46CD57")) +
  theme(legend.position = "none")

ggsave("x2_aderencia_atividade_4.png", height = 10, width = 12)

# Teste Qui-Quadrado de Frequências Distintas ----

## Modelando ----

teste_x2_freddist <- stats::chisq.test(dados_q2$Abundância, p = c(0.25, 0.75))

teste_x2_freddist

## X²-crítico ----

x2_freddist_critico <- stats::qchisq(p = 0.05, df = 1, lower.tail = FALSE)

x2_freddist_critico

## Post-Hoc Resíduos Padronizados Ajustados ----

### Calculando o ponto de corte ----

(0.05 / teste_x2_freddist$parameter) %>% stats::qnorm(. / 2) %>% as.numeric()

### Checando os resíduos ajustados ----

teste_x2_freddist$stdres

## Gráfico ----

estatistica_x2_freddist <- stringr::str_glue("X²-Crítico = {x2_freddist_critico %>% round(2)}, X²<sub>({teste_x2_freddist$parameter})</sub> = {teste_x2_freddist$statistic %>% round(2)}, p < 0.01")

estatistica_x2_freddist

dados_q2 %>%
  dplyr::mutate(Tratamento = paste0("Comunidade ", 1:2),
                `Esperada` = c(25, 75)) %>%
  tidyr::pivot_longer(cols = 3:4,
                      names_to = "Tipo",
                      values_to = "Abundância") %>%
  dplyr::mutate(Tipo = dplyr::case_match(Tipo,
                                         "Abundância" ~ "Observada (contagem)",
                                         .default = "Esperada (%)")) %>%
  ggplot(aes(Tratamento, Abundância, fill = Tipo)) +
  geom_col(position = "dodge", color = "black", width = 0.5) +
  ggtext::geom_richtext(aes(x = 1.5, y = 80, label = estatistica_x2_freddist),
                        color = "black",
                        size = 7.5,
                        fill = NA,
                        label.color = NA,
                        family = "Times New Roman") +
  labs(x = NULL,
       fill = "Frequência") +
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_fill_manual(values = c("#BB7F4E", "#46CD57")) +
  guides(fill = guide_legend(title.position = "top",
                                 title.hjust = 0.5))

ggsave("x2_freqdist_atividade_4.png", height = 10, width = 12)

# Teste Qui-Quadrado de Independência ----

## Tratando os dados ----

valores <- dados_q3 %>% dplyr::pull(Abundância)

dados_q3_trat <- data.frame(Cond1A = valores[c(1, 3)],
                            Cond1B = valores[c(2, 4)],
                            row.names = paste0("Cond2", c("A", "B")))

dados_q3_trat

## Modelando ----

teste_x2_independencia <- stats::chisq.test(dados_q3_trat)

teste_x2_independencia

## X²-Crítico ----

stats::qchisq(p = 0.05, df = 1, lower.tail = FALSE)

# Teste G ----

## Tratando os dados ----

dados_g_trat <- dados_g %>%
  dplyr::mutate(Tratamento = dplyr::case_when(dados_g$Tratamento %>% stringr::str_detect("\\+") == TRUE ~ "Tratamento 1",
                                              .default = "Tratamento 2"),
                Repetição = rep(c(1:4), 2)) %>%
  tidyr::pivot_wider(names_from = c(Tratamento),
                     values_from = Abundância)

dados_g_trat

## Modelando ----

teste_g <- DescTools::GTest(dados_g_trat$`Tratamento 1`, dados_g_trat$`Tratamento 2`)

teste_g

## X²-Crítico ----

stats::qchisq(p = 0.05, df = 4, lower.tail = FALSE)

# Tabelas das estatísticas ----

## Criando a tabela ----

tabela_estatisticas <- tibble::tibble(Teste = c("X² de Aderência",
                                                "X² de Frequências Distintas",
                                                "X² de Independência",
                                                "Teste G"),
                                      `X²-Crítico` = c(x2_aderencia_critico %>% round(3),
                                                       x2_freddist_critico %>% round(3),
                                                       stats::qchisq(p = 0.05, df = 1, lower.tail = FALSE) %>% round(3),
                                                       stats::qchisq(p = 0.05, df = 4, lower.tail = FALSE)) %>% round(3),
                                     Estatística = c(stringr::str_glue("X² = {teste_x2_aderencia$statistic %>% round(3)}"),
                                                     stringr::str_glue("X² = {teste_x2_freddist$statistic %>% round(3)}"),
                                                     stringr::str_glue("X² = {teste_x2_independencia$statistic %>% round(3)}"),
                                                     stringr::str_glue("G = {teste_g$statistic %>% round(3)}")),
                                     `Graus de Liberdade` = c(teste_x2_aderencia$parameter %>% round(3),
                                                              teste_x2_freddist$parameter %>% round(3),
                                                              teste_x2_independencia$paramete %>% round(3),
                                                              teste_g$parameter) %>% round(3),
                                     p = c(teste_x2_aderencia$p.value %>% round(3),
                                          "< 0.01",
                                           teste_x2_independencia$p.value %>% round(3),
                                           teste_g$p.value %>% round(3)))

tabela_estatisticas

## Gerando o flextable ----

tabela_estatisticas_flex <- tabela_estatisticas %>%
  flextable::flextable() %>%
  flextable::width(width = 1.25) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all")

tabela_estatisticas_flex

## Salvando a tabela flextable ----

tabela_estatisticas_flex %>%
  flextable::save_as_docx(path = "tabela_estatísticas_atividade_4.docx")

