library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Certifique-se de que o pacote dplyr está carregado
library(dplyr)

# -------------------------------------------------------------------------
# 2. CRIAR O BANCO DE DADOS DE FLORAÇÃO (flower)
# -------------------------------------------------------------------------
# Regra: Manter apenas os registros onde há botões (bu) OU flores abertas (fl)
flower <- master %>%
  filter(BU == 1 | FL == 1) 

# Se você quiser ser mais restrito e olhar APENAS para flores abertas (excluindo botões), 
# use apenas: filter(fl == 1)

# -------------------------------------------------------------------------
# 3. CRIAR O BANCO DE DADOS DE FRUTIFICAÇÃO (fruit)
# -------------------------------------------------------------------------
# Regra: Manter apenas registros com frutos imaturos (un) OU maduros (fr)
fruit <- master %>%
  filter(imfruit == 1 | mfruit == 1)

# Se quiser olhar APENAS para os frutos maduros prontos para dispersão:
# use apenas: filter(fr == 1)

# 1. CRIAÇÃO DA CATEGORIA, LIMPEZA DE NOMES E CÁLCULO DO N ---------------------

dados_feno <- flower %>%
  mutate(Sindrome_Final = case_when(
    Zoofilia == 1 & Anemofilia == 1 ~ "Ambofilia",
    Zoofilia == 1 ~ "Zoofilia",
    Anemofilia == 1 ~ "Anemofilia",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Sindrome_Final)) %>%
  mutate(
    data_formatada = as.Date(paste(year, month, "01", sep = "-")),
    Species_Curta = word(Species, 1, 2)
  )

n_amostral <- dados_feno %>%
  group_by(Species_Curta, Sindrome_Final) %>%
  summarize(n = n_distinct(Tag), .groups = 'drop')

dados_feno <- dados_feno %>%
  left_join(n_amostral, by = c("Species_Curta", "Sindrome_Final")) %>%
  mutate(Species_Label = paste0(Species_Curta, "(", n, ")"))

# 2. LÓGICA DE INTERVALOS (65 DIAS) -------------------------------------------

dados_intervalos <- dados_feno %>%
  # A CORREÇÃO ESTÁ AQUI: Garantimos que tudo está em ordem cronológica perfeita
  # ANTES de calcular a diferença de dias.
  arrange(Species_Label, Sindrome_Final, data_formatada) %>%
  group_by(Species_Label, Sindrome_Final) %>%
  mutate(
    diff_dias = as.numeric(data_formatada - lag(data_formatada, default = as.Date("1900-01-01"))),
    novo_bloco = ifelse(diff_dias > 65, 1, 0),
    id_bloco = cumsum(novo_bloco)
  ) %>%
  group_by(Species_Label, Sindrome_Final, id_bloco) %>%
  summarize(
    Inicio = min(data_formatada),
    Fim = max(data_formatada),
    .groups = 'drop'
  )

# Ordenação para manter o visual em escada
ordem_especies <- dados_intervalos %>%
  group_by(Species_Label) %>%
  summarize(primeiro_registro = min(Inicio)) %>%
  arrange(primeiro_registro) %>%
  pull(Species_Label)

dados_intervalos <- dados_intervalos %>%
  mutate(Species_Label = factor(Species_Label, levels = ordem_especies))


# 3. CONSTRUÇÃO DO GRÁFICO FINAL (COM EIXO X PERSONALIZADO) --------------------

ggplot(dados_intervalos) +
  geom_segment(aes(x = Inicio, xend = Fim, y = Species_Label, yend = Species_Label,
                   color = Sindrome_Final),
               linewidth = 1.2, alpha = 1) +
  
  geom_point(aes(x = Inicio, y = Species_Label, color = Sindrome_Final), size = 3) +
  geom_point(aes(x = Fim, y = Species_Label, color = Sindrome_Final), size = 3) +
  
  # CONFIGURAÇÃO MANUAL DO EIXO X
  scale_x_date(
    breaks = seq(as.Date("2022-07-01"), as.Date("2023-06-01"), by = "1 month"),
    labels = c("J", "A", "S\n                             2022", 
               "O", "N", "D", "J", "F", 
               "M\n                            2023", "A", "M", "J"),
    limits = c(as.Date("2022-06-15"), as.Date("2023-06-15")) # Margem leve para não cortar os pontos das pontas
  ) +
  
  scale_color_manual(values = c(
    "Zoofilia"   = "#51127C", 
    "Ambofilia"  = "#FB8861"  
  )) +
  
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, # Remove o título "Meses" para não poluir, já que o ano está ali
    y = "Espécies (n amostral)",
    color = "Síndrome de Polinização"
  ) +
  
  theme_minimal() +
  theme(
    # Removemos a inclinação de 45 graus para o ano ficar bem centralizado abaixo da letra
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 1.2),
    axis.text.y = element_text(face = "italic", size = 11), 
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("duracao_polinizacao.jpg", width = 10, height = 7, dpi = 300)


# ==============================================================================
# CÁLCULO DA DURAÇÃO MÉDIA, DESVIO PADRÃO E N AMOSTRAL (n >= 3)
# ==============================================================================

# 1. Preparar os dados com as síndromes e datas
dados_flor_limpos <- flower %>%
  mutate(
    Sindrome_Final = case_when(
      Zoofilia == 1 & Anemofilia == 1 ~ "Ambofilia",
      Zoofilia == 1 ~ "Zoofilia",
      Anemofilia == 1 ~ "Anemofilia",
      TRUE ~ NA_character_
    ),
    data_formatada = as.Date(paste(year, month, "01", sep = "-")),
    Species_Curta = word(Species, 1, 2)
  ) %>%
  filter(!is.na(Sindrome_Final))

# 2. Calcular a duração (em meses) e o número de indivíduos para CADA ESPÉCIE
duracao_especies_flor <- dados_flor_limpos %>%
  group_by(Species_Curta, Sindrome_Final) %>%
  summarize(
    n_individuos = n_distinct(Tag),
    meses_ativos = n_distinct(data_formatada), # Conta os meses que a espécie teve flor
    .groups = 'drop'
  ) %>%
  # O SEU FILTRO: Mantém apenas as espécies com 3 ou mais indivíduos amostrados
  filter(n_individuos >= 3)

# 3. Calcular a tabela final por SÍNDROME DE POLINIZAÇÃO
tabela_final_duracao_flor <- duracao_especies_flor %>%
  group_by(Sindrome_Final) %>%
  summarize(
    Total_Especies = n(),
    Total_Individuos = sum(n_individuos),
    Media_Meses = round(mean(meses_ativos), 2),
    Desvio_Padrao = round(sd(meses_ativos), 2)
  )

# 4. Exibir o resultado final
cat("\n--- TABELA DE DURAÇÃO DA FLORAÇÃO ---\n")
print(tabela_final_duracao)


###############################################################################
# 1. CRIAÇÃO DA CATEGORIA, LIMPEZA DE NOMES E CÁLCULO DO N ---------------------

dados_feno_fruto <- fruit %>%
  mutate(Sindrome_Final = case_when(
    Zoocoria == 1 ~ "Zoocoria",
    Anemocoria == 1 ~ "Anemocoria",
    Autocoria == 1 ~ "Autocoria",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Sindrome_Final)) %>%
  mutate(
    data_formatada = as.Date(paste(year, month, "01", sep = "-")),
    Species_Curta = word(Species, 1, 2)
  )

n_amostral_fruto <- dados_feno_fruto %>%
  group_by(Species_Curta, Sindrome_Final) %>%
  summarize(n = n_distinct(Tag), .groups = 'drop')

dados_feno_fruto <- dados_feno_fruto %>%
  left_join(n_amostral_fruto, by = c("Species_Curta", "Sindrome_Final")) %>%
  mutate(Species_Label = paste0(Species_Curta, "(", n, ")"))

# 2. LÓGICA DE INTERVALOS (65 DIAS) -------------------------------------------

dados_intervalos_fruto <- dados_feno_fruto %>%
  arrange(Species_Label, Sindrome_Final, data_formatada) %>%
  group_by(Species_Label, Sindrome_Final) %>%
  mutate(
    diff_dias = as.numeric(data_formatada - lag(data_formatada, default = as.Date("1900-01-01"))),
    novo_bloco = ifelse(diff_dias > 65, 1, 0),
    id_bloco = cumsum(novo_bloco)
  ) %>%
  group_by(Species_Label, Sindrome_Final, id_bloco) %>%
  summarize(
    Inicio = min(data_formatada),
    Fim = max(data_formatada),
    .groups = 'drop'
  )

# Ordenação para manter o visual em escada
ordem_especies_fruto <- dados_intervalos_fruto %>%
  group_by(Species_Label) %>%
  summarize(primeiro_registro = min(Inicio)) %>%
  arrange(primeiro_registro) %>%
  pull(Species_Label)

dados_intervalos_fruto <- dados_intervalos_fruto %>%
  mutate(Species_Label = factor(Species_Label, levels = ordem_especies_fruto))


# 3. CONSTRUÇÃO DO GRÁFICO FINAL (COM EIXO X PERSONALIZADO) --------------------

ggplot(dados_intervalos_fruto) +
  geom_segment(aes(x = Inicio, xend = Fim, y = Species_Label, yend = Species_Label, color = Sindrome_Final),
               linewidth = 1.2, alpha = 1) +
  
  geom_point(aes(x = Inicio, y = Species_Label, color = Sindrome_Final), size = 3) +
  geom_point(aes(x = Fim, y = Species_Label, color = Sindrome_Final), size = 3) +
  
  # CONFIGURAÇÃO MANUAL DO EIXO X
  scale_x_date(
    breaks = seq(as.Date("2022-07-01"), as.Date("2023-06-01"), by = "1 month"),
    labels = c("J", "A", "S\n                             2022", 
               "O", "N", "D", "J", "F", 
               "M\n                            2023", "A", "M", "J"),
    limits = c(as.Date("2022-06-15"), as.Date("2023-06-15")) # Margem leve
  ) +
  
  scale_color_manual(values = c(
    "Zoocoria"   = "#1D1147", 
    "Anemocoria" = "#B63679", 
    "Autocoria"  = "#FB8861"  
  )) +
  
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, 
    y = "Espécies (n amostral)",
    color = "Síndrome de Dispersão"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 1.2),
    axis.text.y = element_text(face = "italic", size = 11), 
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("duracao_dispersao.jpg", width = 10, height = 7, dpi = 300)


# ==============================================================================
# CÁLCULO DA DURAÇÃO MÉDIA, DESVIO PADRÃO E N AMOSTRAL (n >= 3)
# ==============================================================================

# 1. Preparar os dados com as síndromes e datas
dados_frutos_limpos <- fruit %>%
  mutate(
    Sindrome_Final = case_when(
      Zoocoria == 1 ~ "Zoocoria",
      Anemocoria == 1 ~ "Anemocoria",
      Autocoria == 1 ~ "Autocoria",
      TRUE ~ NA_character_
    ),
    data_formatada = as.Date(paste(year, month, "01", sep = "-")),
    Species_Curta = word(Species, 1, 2)
  ) %>%
  filter(!is.na(Sindrome_Final))

# 2. Calcular a duração (em meses) e o número de indivíduos para CADA ESPÉCIE
duracao_especies_fruto <- dados_frutos_limpos %>%
  group_by(Species_Curta, Sindrome_Final) %>%
  summarize(
    n_individuos = n_distinct(Tag),
    meses_ativos = n_distinct(data_formatada), # Conta os meses que a espécie teve flor
    .groups = 'drop'
  ) %>%
  # O SEU FILTRO: Mantém apenas as espécies com 3 ou mais indivíduos amostrados
  filter(n_individuos >= 3)

# 3. Calcular a tabela final por SÍNDROME DE POLINIZAÇÃO
tabela_final_duracao_fruto <- duracao_especies_fruto %>%
  group_by(Sindrome_Final) %>%
  summarize(
    Total_Especies = n(),
    Total_Individuos = sum(n_individuos),
    Media_Meses = round(mean(meses_ativos), 2),
    Desvio_Padrao = round(sd(meses_ativos), 2)
  )

# 4. Exibir o resultado final
cat("\n--- TABELA DE DURAÇÃO DA FRUTIFICAÇÃO ---\n")
print(tabela_final_duracao_fruto)
