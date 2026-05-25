#1 CARREGANDO PACOTES----------------------------------------------------------------

#Lista dos pacotes que serão utilizados no script
package.list <- c(
  
  # --- 1. ESTATÍSTICA CIRCULAR E MODELAGEM ---
  "circular",    # Base para dados circulares (ângulos, testes de Rayleigh)
  "CircMLE",     # Testes de Máxima Verossimilhança (modelos M1 a M5B)
  "NPCirc",      # Testes não-paramétricos circulares (Hermans-Rasson)
  "CircStats",   # Estatísticas circulares complementares
  "movMF",       # Modelos de misturas (usado no cálculo do Lambda)
  "bpnreg",      # Regressão bayesiana projetada para dados circulares
  
  # --- 2. ECOLOGIA, FENOLOGIA E TEMPO ---
  "phenology",   # Ferramentas específicas para sazonalidade e monitoramento
  "activity",    # Análise de padrões de atividade temporal
  "synchrony",   # Análise de sincronia em eventos ecológicos
  "phenocamr",   # Integração com dados de câmeras fenológicas
  
  # --- 3. LIMPEZA E ORGANIZAÇÃO DE DADOS ---
  "tidyverse",   # Pacote mestre de ciência de dados (carrega ggplot, dplyr, etc.)
  "dplyr",       # Manipulação e filtros (filter, select, mutate)
  "tidyr",       # Transformação de formato de tabelas (pivot_longer)
  "stringr",     # Manipulação de textos e nomes de espécies
  "lubridate",   # Conversão avançada de datas para dias do ano (yday)
  "here",        # Gerenciamento de diretórios sem caminhos absolutos
  "fastDummies", # Criação de variáveis binárias (1 e 0) para síndromes
  "writexl",     # Salvar tabelas geradas pelos códigos
  
  # --- 4. VISUALIZAÇÃO E GRÁFICOS ---
  "ggplot2",     # Motor principal de gráficos para duração e blocos
  "ggtext",      # Formatação de texto rica nos gráficos (itálico em eixos)
  "extrafont",   # Importação de fontes (ex: Times New Roman, Arial)
  "RColorBrewer",# Paletas de cores otimizadas e profissionais
  "corrplot",    # Matrizes visuais de correlação
  "pheatmap"     # Mapas de calor (heatmaps) fenológicos
)

# Instalando pacotes que ainda não estão no computador
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Carregando todos os pacotes
for(i in package.list){library(i, character.only = T)}


#2 CaARREGANDO DADOS--------------------------------------------------------------------
#nomeando e fornecendo localização do arquivo a ser aberto
feno <- readr::read_csv2(here::here("dados", "dados.csv"))

#nomeando e fornecendo localização do arquivo a ser aberto. Aqui é utilizado
#read_csv2 pois o arquivo CSV fica separado por vírgula
traits <- readr::read_csv(here::here("dados", "disp-poll_new.csv"))

# duracao_frutos <- readr::read_csv(here::here("dados", "duracao_frutos.csv"))
# 
# duracao_flores <- readr::read_csv(here::here("dados", "duracao_flores.csv"))
# 
# duracao_brotamento1 <- readr::read_csv(here::here("dados", 
#                                                   "duracao_brotamento1.csv"))
# 
# duracao_brotamento2 <- readr::read_csv(here::here("dados", 
#                                                   "duracao_brotamento2.csv"))
# 
# duracao_queda_foliar1 <- readr::read_csv(here::here("dados", 
#                                                     "duracao_queda_foliar1.csv"))
# 
# duracao_queda_foliar2 <- readr::read_csv(here::here("dados", 
#                                                     "duracao_queda_foliar2.csv"))

#3 LIMPEZA E ORGANIZAÇÃO DOS DADOS-------------------------------------------------

#selecionando colunas do arquivo original que serão utilizadas
feno <- feno %>% select(1:13)

#Seleciona colunas do arquivo original que serão utilizadas
traits <- traits %>% select(3:11)

#junta as tabelas feno e traits, organizando por espécie
master <- merge(x=feno, y=traits,
                by="Species", all.x =T)

#Transforma NA=0
master[is.na(master)] <- 0


#4 CALCULANDO MÊS DO ANO E TRANSFORMANDO EM GRAUS--------------------------------

class(master$DATE)
master$DATE <- as.Date(master$DATE, "%d/%m/%Y")

summary(master)
as_tibble(master)

master$year = format(as.Date(master$DATE), "%Y")
master$month = format(as.Date(master$DATE), "%m")

# Transforma a coluna de mês (texto) em número inteiro (1 a 12)
master$month_num = as.numeric(master$month)

# A CONVERSÃO CIRCULAR MENSAL:
# (Mês - 0.5) centraliza o dado. Ex: Jan (1) vira 0.5. Multiplicado por 30 (360/12) = 15°
master$angles = ((master$month_num - 0.5) * 360) / 12

master <- master %>%
  filter(year == "2022" & month %in% c("07", "08", "09", "10", "11", "12") |
           year == "2023" & month %in% c("01", "02", "03", "04", "05", "06") )


#5 FRUITING PHENOLOGY AND DISPERSAL---------------------------------------------

#5A FILTER DATA
#rename columns because current name format causes error
colnames(master)[11] ="imfruit"
colnames(master)[12] ="mfruit"
colnames(master)[9] = "BU"
colnames(master)[10] = "FL"

class(master$imfruit)

# Filtrar apenas o PRIMEIRO mês de frutificação de cada indivíduo
fruit_unit <- master %>%
  filter(imfruit == 1 | mfruit == 1) %>%
  group_by(Tag, Species) %>% # Agrupa por planta
  arrange(DATE) %>%          # Garante que as datas estão em ordem
  slice(1) %>%               # Pega apenas a primeira linha (o primeiro mês) de cada planta
  ungroup()


gerar_dados_e_grafico_mle <- function(dados, coluna_sindrome, nome_arquivo = NULL,
                                      cor_rosa, cor_borda = "black", salvar_arquivo = TRUE) {
  
  # 1. Filtro e preparação dos dados
  df <- as.data.frame(dados) %>% dplyr::filter(get(coluna_sindrome) == 1)
  
  # Trava de segurança inicial
  if(nrow(df) == 0) {
    message("Nenhum dado encontrado para ", coluna_sindrome)
    return(data.frame(Sindrome = coluna_sindrome, n = 0, Melhor_Modelo = NA, 
                      Distribuicao = NA))
  }
  
  radianos_dados <- (df$angles * pi) / 180
  
  # 2. Estatísticas Descritivas
  feno_circ <- circular(df$angles, units = "degrees", template = "none", 
                        modulo = "2pi", zero = pi/2, rotation = "clock")
  
  r_vetor <- rho.circular(feno_circ)
  mu_rad_obj <- mean.circular(feno_circ)
  mu_rad_val <- as.numeric(mu_rad_obj) * pi / 180 
  
  # 3. Análise de Máxima Verossimilhança (MLE)
  set.seed(42)
  res_mle <- CircMLE::circ_mle(radianos_dados)
  tabela_res <- as.data.frame(res_mle$results)
  melhor_modelo_nome <- row.names(tabela_res)[1]
  topo <- tabela_res[1, ]
  is_multimodal <- grepl("M3|M4|M5", melhor_modelo_nome)
  
  # 4. Extração Segura dos Parâmetros MLE
  safe_ext <- function(col) {
    if (!col %in% names(topo)) return(NA)
    val <- topo[[col]]
    if (is.null(val) || length(val) == 0 || is.na(val)) return(NA)
    return(round(as.numeric(val), 3))
  }
  
  v_q1  <- safe_ext("q1")
  v_k1  <- safe_ext("k1")
  v_q2  <- safe_ext("q2")
  v_k2  <- safe_ext("k2")
  v_lam <- safe_ext("lamda") 
  if (is.na(v_lam)) v_lam <- safe_ext("lambda")
  
  # 5. Testes de Hipótese 
  # O HR_test já está embutido no CircMLE e roda via bootstrap (9999 iterações)
  test_ray <- rayleigh.test(feno_circ)
  test_hr  <- CircMLE::HR_test(radianos_dados) 
  
  # 6. Cálculo da Data Média
  ang_ajustado <- (as.numeric(mu_rad_obj) %% 360)
  dia_ano <- (ang_ajustado * 365) / 360
  data_media <- as.Date(dia_ano, origin = "2022-01-01")
  
  # 7. Gráfico
  if(salvar_arquivo) {
    if(is.null(nome_arquivo)) stop("Forneça um nome de arquivo para salvar.")
    jpeg(nome_arquivo, width = 1800, height = 1800, res = 300, quality = 100)
    par(mar = c(4, 4, 4, 4))
  }
  
  plot(feno_circ, axes = FALSE, shrink = 1, stack = TRUE, bins = 365, 
       cex = 0, rotation = "clock", zero = pi/2, main = "")
  
  title(main = coluna_sindrome, cex.main = 3)
  
  circular::rose.diag(feno_circ, axes = FALSE, bins = 12, col = cor_rosa, 
                      border = cor_borda, add = TRUE, prop = 1.0, 
                      zero = pi/2, rotation = "clock")
  
  lines(density.circular(feno_circ, bw = 20), col = "black", lwd = 3)
  
  if(melhor_modelo_nome != "M1") {
    arrows.circular(mu_rad_obj, shrink = r_vetor, lwd = 3, col = "black", length = 0.1)
    
    if(!is.na(v_q1)) {
      pico1 <- circular(v_q1, units="radians", zero=pi/2, rotation="clock")
      arrows.circular(pico1, shrink=1, lty=2, lwd=2, col = "#363636", length=0.1)
    }
    
    if(!is.na(v_q2) && is_multimodal) {
      pico2 <- circular(v_q2, units="radians", zero=pi/2, rotation="clock")
      arrows.circular(pico2, shrink=1, lty=2, lwd=2, col = "#363636", length=0.1)
    }
  }
  
  axis.circular(at = circular(sort(seq(0, 11/6*pi, pi/6), decreasing = TRUE)), 
                labels = c("M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M","A"), 
                cex = 1.3)
  
  if(salvar_arquivo) dev.off()
  
  # --- RETORNO DA TABELA À PROVA DE FALHAS ---
  final_distribuicao <- 
    if(melhor_modelo_nome == "M1") "Uniforme" else if(is_multimodal) "Bimodal" else "Unimodal"
  
  final_teste <- 
    if(is_multimodal) "Hermans-Rasson" else "Rayleigh"
  
  # Ajuste na extração de resultados do test_hr, que retorna c(Estatistica, p-valor)
  final_estatistica <- 
    if(is_multimodal) as.numeric(test_hr[1]) else as.numeric(test_ray$statistic)
  
  final_pvalor <-
    if(is_multimodal) as.numeric(test_hr[2]) else as.numeric(test_ray$p.value)
  
  res_list <- list(
    Sindrome        = coluna_sindrome,
    Melhor_Modelo   = melhor_modelo_nome,
    Distribuicao    = final_distribuicao,
    n               = nrow(df),
    Teste_Escolha   = final_teste,
    Estatistica     = round(final_estatistica, 4),
    P_Valor         = round(final_pvalor, 4),
    r_vetor         = round(r_vetor, 3),
    Angulo_Med_Rad  = round(mu_rad_val, 3),
    Data_Media      = format(data_media, "%d/%m/%y"),
    q1_rad          = if(melhor_modelo_nome == "M1") NA else v_q1,
    k1_kappa        = if(melhor_modelo_nome == "M1") NA else v_k1,
    q2_rad          = if(is_multimodal) v_q2 else NA,
    k2_kappa        = if(is_multimodal) v_k2 else NA,
    lambda          = if(is_multimodal) v_lam else NA,
    AICc            = round(as.numeric(topo$AICc), 3),
    Peso_AIC        = round(as.numeric(topo$AIC_weights), 3)
  )
  
  return(as.data.frame(lapply(res_list, function(x) if(length(x)==0) NA else x)))
}

# Abre o arquivo para a composição
jpeg("sazonalidade_unida.jpg", width = 6000, height = 2000, res = 300, quality = 100)

# Configura layout: 1 linha, 3 colunas
par(mfrow = c(1, 3), mar = c(8, 2, 8, 2), xpd = TRUE)

# Chama a função com salvar_arquivo = FALSE
#Zoocoria
stats_zoo <- gerar_dados_e_grafico_mle(
  dados = fruit_unit,
  coluna_sindrome = "Zoocoria",
  cor_rosa = "#51127C",
  cor_borda = "#1D1147",
  salvar_arquivo = FALSE
)

#Anemocoria
stats_anemo <- gerar_dados_e_grafico_mle(
  dados = fruit_unit,
  coluna_sindrome = "Anemocoria",
  cor_rosa = "#B63679",
  cor_borda = "#822681",
  salvar_arquivo = FALSE
)

#Autocoria
stats_auto <- gerar_dados_e_grafico_mle(
  dados = fruit_unit,
  coluna_sindrome = "Autocoria",
  cor_rosa = "#FB8861",
  cor_borda = "#E65164",
  salvar_arquivo = FALSE
)

# Fecha o arquivo final
dev.off()

# Unir na tabela final
tabela_dispersao <- rbind(stats_zoo, stats_anemo, stats_auto)
print(tabela_dispersao)
write_xlsx(tabela_dispersao, here::here("dados", "tabela_dispersao_final.xlsx"))



# Filtrar apenas o PRIMEIRO mês de frutificação de cada indivíduo
flower_unit <- master %>%
  filter(BU == 1 | FL == 1) %>%
  group_by(Tag, Species) %>% # Agrupa por planta
  arrange(DATE) %>%          # Garante que as datas estão em ordem
  slice(1) %>%               # Pega apenas a primeira linha (o primeiro mês) de cada planta
  ungroup()

# 1. Definir o arquivo de saída para os gráficos unidos
# Aumentamos a largura para comportar os dois gráficos e a altura para evitar cortes
jpeg("floracao_unida.jpg", width = 4000, height = 2000, res = 300, quality = 100)

# 2. Configurar o layout (1 linha, 2 colunas) e margens generosas
# xpd = TRUE impede que a linha de densidade seja cortada nas bordas
par(mfrow = c(1, 2), mar = c(8, 2, 8, 2), xpd = TRUE)

# 3. Executar as funções com salvar_arquivo = FALSE
# Note que o argumento 'nome_arquivo' pode ser ignorado ou passar NULL aqui

# Zoofilia
stats_zoof <- gerar_dados_e_grafico_mle(
  dados = flower_unit, 
  coluna_sindrome = "Zoofilia", 
  cor_rosa = "#51127C",
  cor_borda = "#1D1147",
  salvar_arquivo = FALSE
)

# Anemofilia
stats_anemof <- gerar_dados_e_grafico_mle(
  dados = flower_unit, 
  coluna_sindrome = "Anemofilia", 
  cor_rosa = "#B63679",
  cor_borda = "#822681",
  salvar_arquivo = FALSE
)

# 4. Fechar o dispositivo para salvar o arquivo
dev.off()

# Unir na tabela final
tabela_polinizacao <- rbind(stats_zoof, stats_anemof)
print(tabela_polinizacao)
write_xlsx(tabela_polinizacao, here::here("dados", "tabela_polinizacao_final.xlsx"))

