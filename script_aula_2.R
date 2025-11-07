# Pacotes ----

library(tidyverse)

library(readxl)

library(report)

library(performance)

library(flextable)

library(ggtext)

# Dados ----

## Importando ----

dados_2 <- readxl::read_xlsx("Atividade_1.xlsx",
                             sheet = 3)

dados_2

## Tratando

dados_2_trat <- dados_2 %>%
  dplyr::select(6:7) %>%
  dplyr::rename(`Pré-Restauração` = `3`, `Pós-Restauração` = `...7`)

dados_2_trat

## Checando ----

dados_2_trat %>%
  dplyr::glimpse()

dados_2_trat %>%
  report::report()

# Teste T para uma mostra ----

## Definindo o valor de forma aleatória ----

set.seed(123); runif(1, 6, 10) -> mu

mu

## Pré-Restauração ----

### Normalidade ----

dados_2_trat$`Pré-Restauração` %>%
  shapiro.test()

### Modelando ----

testet_1a_c1 <- t.test(dados_2_trat$`Pré-Restauração`, mu = mu)

testet_1a_c1

### t-Crítico ----

stats::qt(p = 0.05, df = 9, lower.tail = FALSE)

## Pós-Restauração ----

### Normalidade ----

dados_2_trat$`Pós-Restauração` %>%
  shapiro.test()

### Modelando ----

testet_1a_c2 <- t.test(dados_2_trat$`Pós-Restauração`, mu = mu)

testet_1a_c2

### t-Crítico ----

stats::qt(p = 0.05, df = 9, lower.tail = FALSE)

## Criando uma tabela unificada ----

### Pré-Restauração ----

tabela_1a_c1 <- tibble::tibble(Tratamento = "Pré-Restauração",
                               `x̅` = testet_1a_c1$estimate %>% as.numeric(),
                               t = testet_1a_c1$statistic %>% as.numeric() %>% round(2),
                               `graus de liberdade` = testet_1a_c1$parameter %>% as.numeric(),
                               p = if(testet_1a_c1$p.value < 0.01) { print("< 0.01") } else { print(testet_1a_c1$p.value %>% round(2)) },
                               `Intervalo de Confiança 95%` = paste0(testet_1a_c1$conf.int[1] %>% round(2),
                                                                      " – ",
                                                                      testet_1a_c1$conf.int[2] %>% round(2)))

tabela_1a_c1

### Pós-Restauração ----

tabela_1a_c2 <- tibble::tibble(Tratamento = "Pós-Restauração",
                               `x̅` = testet_1a_c2$estimate %>% as.numeric(),
                               t = testet_1a_c2$statistic %>% as.numeric() %>% round(2),
                               `graus de liberdade` = testet_1a_c2$parameter %>% as.numeric(),
                               p = if(testet_1a_c2$p.value < 0.01) { print("< 0.01") } else { print(testet_1a_c2$p.value %>% round(2) %>% as.character()) },
                               `Intervalo de Confiança 95%` = paste0(testet_1a_c2$conf.int[1] %>% round(2),
                                                                      " – ",
                                                                      testet_1a_c2$conf.int[2] %>% round(2)))

tabela_1a_c2

### Unindo e criando a tabela ----

tabelas_1a <- ls(pattern = "^tabela_1a_")

tabelas_unidas_1a <- mget(tabelas_1a) %>%
  dplyr::bind_rows()

tabelas_unidas_1a

### Criando e salvando a tabela flextable ----

tabelas_unidas_1a_flex <- tabelas_unidas_1a %>%
  flextable::flextable() %>%
  flextable::width(width = 1.15) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all") %>%
  flextable::set_caption(caption = paste0("t-crítico = 1.83, \u00b5 = 7.15"))

tabelas_unidas_1a_flex

tabelas_unidas_1a_flex %>%
  flextable::save_as_docx(path = "tabela_1a_atividade_2.docx")

## Gráfico ----

medias_1a <- tibble::tibble(Tratamento = c("Pré-Restauração", "Pós-Restauração"),
                            `Índice de Diversidade de Hill para q = 1` = c(5.31, 6.64)) %>%
              dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(c("Pré-Restauração", "Pós-Restauração")))

medias_1a

estatisticas_1a <- tibble::tibble(Tratamento = c("Pré-Restauração", "Pós-Restauração"),
                                  `Índice de Diversidade de Hill para q = 1` = 8.2,
                                  label = c("t <sub>(9)</sub> = -11.97, p < 0.01",
                                            "t <sub>(9)</sub> = -1.69, p = 0.13")) %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(c("Pré-Restauração", "Pós-Restauração")))


estatisticas_1a

dados_2_trat %>%
  tidyr::pivot_longer(cols = dplyr::contains("Restauração"),
                      values_to = "Índice de Diversidade de Hill para q = 1",
                      names_to = "Tratamento") %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(c("Pré-Restauração", "Pós-Restauração"))) %>%
  ggplot(aes(Tratamento, `Índice de Diversidade de Hill para q = 1`, fill = Tratamento)) +
  geom_boxplot(color = "black", show.legend = FALSE, width = 0.2) +
  geom_jitter(shape = 21, width = 0.1, color = "black", size = 5, show.legend = FALSE) +
  geom_point(aes(Tratamento, mu + 0.5), color = "black",  size = 5, show.legend = FALSE) +
  geom_point(data = medias_1a,
             aes(Tratamento, `Índice de Diversidade de Hill para q = 1`, fill = Tratamento),
             shape = 23,
             color = "black",
             stroke = 2,
             size = 5,
             fill = NA) +
  geom_point(data = medias_1a,
             aes(Tratamento, `Índice de Diversidade de Hill para q = 1`, color = "Tratamento"),
             shape = 23,
             color = "black",
             stroke = 2,
             size = 5) +
  ggtext::geom_richtext(data = estatisticas_1a,
                        aes(Tratamento,
                            `Índice de Diversidade de Hill para q = 1`,
                            label = label,
                            fontface = 2),
                        color = "black",
                        size = 7.5,
                        label.color = NA,
                        fill = NA,
                        family = "Times New Roman") +
  facet_wrap(~ Tratamento, scales = "free_x") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5),
         fill = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  labs(color = "Médias dos grupos",
       fill = "Médias dos grupos",
       x = NULL) +
  scale_y_continuous(breaks = seq(4.5, 8.5, 0.5), limits = c(4.5, 8.5)) +
  scale_fill_manual(values = c("#BB7F4E", "#46CD57")) +
  scale_color_manual(values = c("#BB7F4E", "#46CD57")) +
  theme(axis.text = element_text(color = "black", size = 20, family = "Times New Roman"),
        axis.title = element_text(color = "black", size = 20, family = "Times New Roman"),
        panel.grid.major  = element_line(colour = "gray90", linewidth = 1),
        legend.text = element_text(color = "black", size = 20, family = "Times New Roman"),
        legend.title = element_text(color = "black", size = 20, family = "Times New Roman"),
        legend.position = "bottom",
        panel.border = element_rect(colour = 1, fill = "transparent", size = 1),
        panel.background = element_rect(colour = NA, fill = "white"),
        strip.background = element_rect(color = "black", size = 1),
        strip.text = element_text(color = "black", size = 20, family = "Times New Roman"))

ggsave("teste_t_1_amostra_atividade_2.png", height = 10, width = 12)

# Teste T para duas amostras ----

## Tratando os dados ----

dados_2_trat_testt <- dados_2_trat %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Tratamento",
                      values_to = "Diversidade") %>%
  dplyr::mutate(Tratamento = Tratamento %>% forcats::fct_relevel(c("Pré-Restauração", "Pós-Restauração")))

dados_2_trat_testt

## Pressupostos ----

### Normalidade dos resíduos por Shapiro-Wilki ----

lm(Diversidade ~ Tratamento, data = dados_2_trat_testt) %>%
  residuals() %>%
  shapiro.test()

### Homogenidade de variância dos resíduos por Bartlett ----

bartlett.test(Diversidade ~ Tratamento, data = dados_2_trat_testt)

stats::qchisq(p = 0.05, d = 1, lower.tail = FALSE)

## Modelando ----

### Teste t para variâncias heterogêneas ----

#### Modelo ----

teste_t_var_dif <- t.test(Diversidade ~ Tratamento, data = dados_2_trat_testt, var.equal = FALSE)

teste_t_var_dif

#### t-crítico ----

stats::qt(p = 0.05, df = 13.188, lower.tail = FALSE)

### Teste t para variâncias homogêneas ----

#### Modelo ----

teste_t_var_eq <- t.test(Diversidade ~ Tratamento, data = dados_2_trat_testt, var.equal = TRUE)

teste_t_var_eq

#### t-crítico ----

stats::qt(p = 0.05, df = 18, lower.tail = FALSE)

## Tabela ----

### Estatísticas ----

estatisticas_2a_vardif <- tibble::tibble(Variância = "Heterogênea",
                                         `t-crítico` = "1.77",
                                         t = teste_t_var_dif$statistic %>% as.numeric() %>% round(3),
                                         `graus de liberdade` = teste_t_var_dif$parameter %>% round(3),
                                         p = "< 0.01",
                                         `Intervalo de Confiança 95%` = paste0(teste_t_var_dif$conf.int[1] %>% round(2),
                                                                               " – ",
                                                                               teste_t_var_dif$conf.int[2] %>% round(2)))

estatisticas_2a_vardif

estatisticas_2a_vareq <- tibble::tibble(Variância = "Homogênea",
                                        `t-crítico` = "1.73",
                                         t = teste_t_var_eq$statistic %>% as.numeric() %>% round(3),
                                         `graus de liberdade` = teste_t_var_eq$parameter %>% as.numeric() %>% round(0),
                                         p = "< 0.01",
                                         `Intervalo de Confiança 95%` = paste0(teste_t_var_eq$conf.int[1] %>% round(2),
                                                                               " – ",
                                                                               teste_t_var_eq$conf.int[2] %>% round(2)))

estatisticas_2a_vareq

### Unindo as tabela ----

tabelas_2a <- ls(pattern = "a_var")

tabelas_unidas_2a <- mget(tabelas_2a) %>%
  dplyr::bind_rows()

tabelas_unidas_2a

### Criando e salvando o flextable ----

tabelas_unidas_2a_flex <- tabelas_unidas_2a %>%
  flextable::flextable() %>%
  flextable::width(width = 1.15) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all") %>%
  flextable::set_caption(caption = stringr::str_glue("x\u0305 Pré-Restauração = {teste_t_var_eq$estimate[1]}; x\u0305 Pós-Restauração = {teste_t_var_eq$estimate[2]}"))

tabelas_unidas_2a_flex

tabelas_unidas_2a_flex %>%
  flextable::save_as_docx(path = "tabela_2a_atividade_2.docx")

## Gráfico ----

teste_t_2_estatisticas <- tibble::tibble(Tratamento = c(0.75, 1.5),
                                         Diversidade = 8.35,
                                         estatisticas = c("Variância heterogênea \n\nt <sub>(13.188)</sub> = -3.84, p < 0.01 \n\nt-crítico = 1.77",
                                                          "Variância homogênea \n\nt <sub>(18)</sub> = -3.84, p < 0.01 \n\nt-crítico = 1.73"))

teste_t_2_estatisticas

label = "Variância heterogênea: \n\nt <sub>(13.188)</sub> = -3.84, p < 0.01 \n\nt-crítico = 1.77 \n\n   \n\n   \n\nVariância homogênea: \n\nt <sub>(18)</sub> = -3.84, p < 0.01 \n\nt-crítico = 1.73"

pontos_medias_2a <- tibble::tibble(Tratamento = c("Pré-Restauração", "Pós-Restauração"),
                                   Diversidade = c(teste_t_var_eq$estimate[1], teste_t_var_eq$estimate[2]))

pontos_medias_2a

dados_2_trat_testt %>%
  ggplot(aes(Tratamento, Diversidade, fill = Tratamento)) +
  geom_boxplot(color = "black", show.legend = FALSE, width = 0.25) +
  geom_jitter(width = 0.1, color = "black", shape = 21, size = 5, show.legend = FALSE) +
  geom_point(data = pontos_medias_2a,
             aes(Tratamento, Diversidade),
             color = "black",
             size = 5,
             show.legend = FALSE) +
  ggforce::geom_link(aes(x = 1, xend = 2, y = teste_t_var_eq$estimate[1], yend = teste_t_var_eq$estimate[2]),
                     color = "black",
                     linetype = "dashed") +
  ggtext::geom_richtext(aes(x = 0.75, y = 8, label = label, fontface = 2),
                        color = "black",
                        size = 6,
                        fill = NA,
                        label.color = NA,
                        family = "Times New Roman") +
  scale_fill_manual(values = c("#BB7F4E", "#46CD57")) +
  scale_y_continuous(breaks = seq(4.5, 8.5, 0.5), limits = c(4.5, 8.5)) +
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

ggsave("teste_t_2_amostra_atividade_2.png", height = 10, width = 12)
