# Pacotes ----

library(tidyverse)

library(readxl)

library(report)

library(flextable)

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

# Calculando as estatísticas ----

## Criando a função para calcular Intervalo de confiança ----

calc_ic <- function(x, conf_int) {

  if(conf_int == 0.9) {
    mean_x <- mean(x)

    std_error <- sd(x) / sqrt(length(x))

    error_margin <- 1.645 * std_error

    lower_bound <- (mean_x - error_margin) %>% round(3)

    upper_bound <- (mean_x + error_margin) %>% round(3)

    p <- stringr::str_glue("{lower_bound}—{upper_bound}")

    return(p)

  } else if(conf_int == 0.95){

    mean_x <- mean(x)

    std_error <- sd(x) / sqrt(length(x))

    error_margin <- 1.96 * std_error

    lower_bound <- (mean_x - error_margin) %>% round(3)

    upper_bound <- (mean_x + error_margin) %>% round(3)

    p <- stringr::str_glue("{lower_bound}—{upper_bound}")

    return(p)

  } else if(conf_int == 0.99){

    mean_x <- mean(x)

    std_error <- sd(x) / sqrt(length(x))

    error_margin <- 2.575 * std_error

    lower_bound <- (mean_x - error_margin) %>% round(3)

    upper_bound <- (mean_x + error_margin) %>% round(3)

    p <- stringr::str_glue("{lower_bound}—{upper_bound}")

    return(p)

  }

}

## Calculando as estatísticas ----

### Tabela ----

flextable_tabela <- dados_1_trat %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Tratamento",
                      values_to = "Valores") %>%
  dplyr::group_by(Tratamento) %>%
  dplyr::summarise(`Valor mínimo` = min(Valores),
                   `Valor máximo` = max(Valores),
                   `Média ± Erro padrão` = paste0(mean(Valores) %>% round(2), " ± ", (sd(Valores) / sqrt(length(Valores))) %>% round(3)),
                   Mediana = median(Valores),
                   `Intervalo de confiança 95%`= calc_ic(Valores, conf_int = 0.95)) %>%
  dplyr::arrange(`Valor mínimo` %>% dplyr::desc()) %>%
  flextable::flextable() %>%
  flextable::width(width = 1.25) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Times New Roman", part = "all")  %>%
  flextable::fontsize(size = 12, part = "all")

flextable_tabela

### Exportando a tabela ----

flextable_tabela %>%
  flextable::save_as_docx(path = "tabela_atividade_1.docx")

## Gráfico ----

dados_hill <- dados_1_trat %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      values_to = "Intervalo de confiança 95%",
                      names_to = "Variável") %>%
  dplyr::mutate(Variável = Variável %>% forcats::fct_relevel(c("Plantação", "Restauração", "Floresta Nativa")))

dados_hill

dados_1_trat %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Variável",
                      values_to = "Valores") %>%
  dplyr::group_by(Variável) %>%
  dplyr::summarise(`Valor mínimo` = min(Valores),
                   `Valor máximo` = max(Valores),
                   `Média` = mean(Valores) %>% round(2),
                   `Erro padrão` = (sd(Valores) / sqrt(length(Valores))) %>% round(3),
                   `Intervalo de confiança 95%` = calc_ic(Valores, conf_int = 0.9)) %>%
  tidyr::separate_rows(`Intervalo de confiança 95%`, sep = "—") %>%
  dplyr::mutate(`Intervalo de confiança 95%` = `Intervalo de confiança 95%` %>% as.numeric,
                Variável = Variável %>% forcats::fct_relevel(c("Plantação", "Restauração", "Floresta Nativa"))) %>%
  as.data.frame() %>%
  ggplot(aes(`Intervalo de confiança 95%`, Variável, color = Variável)) +
  geom_line(linewidth = 25,
            alpha = 0.5) +
  geom_point(aes(Média, Variável),
             color = "black",
             show.legend = FALSE,
             size = 2.5,
             stroke = 1.5) +
  geom_jitter(data = dados_hill, aes(`Intervalo de confiança 95%`, Variável, fill = Variável),
              height = 0.15,
              shape = 21,
              color = "black",
              size = 5,
              show.legend = FALSE) +
  geom_errorbar(aes(xmax = Média + `Erro padrão`, xmin = Média - `Erro padrão`),
                color = "black",
                width = 0.1,
                linewidth = 1) +
  labs(x = "Diversidade de Hill para q = 1",
       y = NULL,
       color = "Intervalo de Confiança 95%") +
  scale_x_continuous(breaks = seq(4.5, 11, 0.5), limits = c(4.5, 11)) +
  scale_fill_manual(values = c("#BB7F4E", "#E4C62D", "#46CD57")) +
  scale_color_manual(values = c("#BB7F4E", "#E4C62D", "#46CD57")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  theme(axis.text = element_text(color = "black", size = 15, family = "Times New Roman", face = "bold"),
        axis.title = element_text(color = "black", size = 15, family = "Times New Roman", face = "bold"),
        panel.grid.major  = element_line(colour = "gray90", linewidth = 1),
        legend.text = element_text(color = "black", size = 15, family = "Times New Roman", face = "bold"),
        legend.title = element_text(color = "black", size = 15, family = "Times New Roman", face = "bold"),
        legend.position = "top",
        panel.border = element_rect(colour = 1, fill = "transparent", size = 1),
        panel.background = element_rect(colour = NA, fill = "white"))

ggsave(filename = "grafico_atividade1.png", height = 10, width = 12)

theme_bw()

dados_modelo <- dados_1_trat %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "tratamento",
                      values_to = "diversidade")

aov(diversidade ~ tratamento, data = dados_modelo) %>%
  performance::check_model()

aov(diversidade ~ tratamento, data = dados_modelo) %>%
  performance::check_normality()

aov(diversidade ~ tratamento, data = dados_modelo) %>%
  performance::check_homogeneity()

aov(diversidade ~ tratamento, data = dados_modelo) %>%
  report::report()

aov(diversidade ~ tratamento, data = dados_modelo) %>%
  summary()

qf(0.05, df1 = 2, df2 = 27, lower.tail = FALSE)

