library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)

# 1. FUNÇÃO PARA GERAR O CORRELOGRAMA (COM INTERPOLAÇÃO DE 65 DIAS) -----------

gerar_correlograma <- function(dados_base, nome_sindrome, nome_arquivo, min_indiv = 3) {
  
  # A. Filtra os dados e cria a data e nome curto
  dados_filtrados <- dados_base %>%
    filter(.data[[nome_sindrome]] == 1) %>%
    mutate(
      Species_Curta = word(Species, 1, 2),
      Data = as.Date(paste(year, month, "01", sep = "-"))
    )
  
  # B. PREENCHIMENTO DE LACUNAS (REGRA DOS 65 DIAS)
  # Aplica a lógica indivíduo por indivíduo (Tag) para gerar continuidade
  dados_continuos <- dados_filtrados %>%
    arrange(Species_Curta, Tag, Data) %>%
    group_by(Species_Curta, Tag) %>%
    mutate(
      # Calcula a diferença de dias entre o mês atual e o mês em que a árvore foi vista antes
      diff_dias = as.numeric(Data - lag(Data, default = as.Date("1900-01-01"))),
      novo_bloco = ifelse(diff_dias > 65, 1, 0),
      id_bloco = cumsum(novo_bloco)
    ) %>%
    # Isola cada "evento contínuo" da árvore
    group_by(Species_Curta, Tag, id_bloco) %>%
    summarise(
      Inicio = min(Data),
      Fim = max(Data),
      .groups = "drop"
    ) %>%
    # O pulo do gato: cria as linhas dos meses "escondidos" no meio do intervalo
    rowwise() %>%
    mutate(Data = list(seq(from = Inicio, to = Fim, by = "month"))) %>%
    unnest(Data) %>%
    select(Species_Curta, Tag, Data) %>%
    distinct() # Remove duplicatas por segurança
  
  # C. Calcula o "n" amostral E APLICA O FILTRO
  n_amostral <- dados_continuos %>%
    group_by(Species_Curta) %>%
    summarise(n = n_distinct(Tag), .groups = "drop") %>%
    filter(n >= min_indiv) # Corta as espécies que não atingem o mínimo
  
  # Trava de segurança caso o filtro remova espécies demais
  if(nrow(n_amostral) < 2) {
    message("Não há espécies suficientes (com n >= ", min_indiv, ") em: ", nome_sindrome)
    return(invisible(NULL))
  }
  
  # D. Prepara a Matriz (agora usando os dados contínuos sem lacunas)
  matriz <- dados_continuos %>%
    inner_join(n_amostral, by = "Species_Curta") %>% # inner_join aplica o filtro
    mutate(Species_Label = paste0(Species_Curta, " (", n, ")")) %>%
    group_by(Data, Species_Label) %>%
    summarise(Individuos_Ativos = n_distinct(Tag), .groups = "drop") %>%
    pivot_wider(names_from = Species_Label, values_from = Individuos_Ativos, values_fill = 0) %>%
    select(-Data)
  
  if(ncol(matriz) < 2) {
    message("Não há colunas suficientes para rodar correlação em: ", nome_sindrome)
    return(invisible(NULL))
  }
  
  # E. Correlação de Spearman e P-VALORES
  cor_matrix <- cor(matriz, method = "spearman")
  teste_sig <- suppressWarnings(cor.mtest(matriz, conf.level = 0.95, method = "spearman"))
  p_matrix <- teste_sig$p
  
  # F. Configuração da Imagem
  jpeg(nome_arquivo, width = 3500, height = 3500, res = 300)
  
  # G. Desenho do Gráfico Base
  grafico <- corrplot::corrplot(
    cor_matrix,
    method = "color",       
    type = "lower",         
    diag = FALSE,           
    tl.col = "black",       
    tl.cex = 0.8,           
    tl.srt = 90,            
    tl.pos = "ld",          
    tl.offset = 0.5, 
    
    order = "hclust",       
    hclust.method = "ward.D2", 
    addgrid.col = "gray50", 
    
    # 1º Filtro: Apaga as cores dos quadrados não-significativos
    p.mat = p_matrix,       
    sig.level = 0.05,
    insig = "blank",             
    
    col = colorRampPalette(c("#ca0020", "#f7f7f7", "#0571b0"))(24),
    cl.length = 9,          
    
    title = paste("Matriz de Sincronia -", nome_sindrome),
    mar = c(0, 0, 4, 0)     
  )
  
  # H. Adicionando os textos APENAS nos quadrados significativos
  # Extrai as coordenadas de todos os quadrados
  pos <- grafico$corrPos
  
  # 2º Filtro: Isola apenas as coordenadas onde o p-valor é menor que 0.05
  pos_sig <- subset(pos, p.value < 0.05)
  
  # Imprime o Valor de R (r_s) apenas nas coordenadas filtradas
  text(pos_sig$x, pos_sig$y + 0.20, labels = round(pos_sig$corr, 2), cex = 0.7, font = 2)
  
  # Imprime o P-valor exato abaixo do R
  text(pos_sig$x, pos_sig$y - 0.20, labels = paste0("p=", round(pos_sig$p.value, 3)), cex = 0.6)
  
  dev.off()
  message("Pronto! Gráfico salvo apenas com cores e valores significativos: ", nome_arquivo)
}

# 2. GERANDO OS GRÁFICOS SEPARADOS ---------------------------------------------

# Gráfico de Zoofilia
gerar_correlograma(
  dados_base = flower, 
  nome_sindrome = "Zoofilia", 
  nome_arquivo = "sincronia_zoofilia.jpg"
)

# Gráfico de Anemofilia
gerar_correlograma(
  dados_base = flower, 
  nome_sindrome = "Anemofilia", 
  nome_arquivo = "sincronia_anemofilia.jpg"
)

# 3. GERANDO OS GRÁFICOS PARA SÍNDROMES DE DISPERSÃO ---------------------------

# Gráfico de Zoocoria
gerar_correlograma(
  dados_base = fruit,          # Substitua pelo nome do seu dataframe de frutos
  nome_sindrome = "Zoocoria",  # Verifique a grafia exata da sua coluna
  nome_arquivo = "sincronia_zoocoria.jpg"
)

# Gráfico de Anemocoria
gerar_correlograma(
  dados_base = fruit,          
  nome_sindrome = "Anemocoria", 
  nome_arquivo = "sincronia_anemocoria.jpg"
)

# Gráfico de Autocoria (se houver no seu estudo)
gerar_correlograma(
  dados_base = fruit,          
  nome_sindrome = "Autocoria", 
  nome_arquivo = "sincronia_autocoria.jpg"
)




# 1. FUNÇÃO PARA EXPORTAR OS VALORES DE r E p PARA CSV -------------------------

extrair_tabela_sincronia <- function(dados_base, nome_sindrome, nome_arquivo_csv, min_indiv = 3) {
  
  # A e B. Prepara os dados contínuos (exatamente como no gráfico)
  dados_continuos <- dados_base %>%
    filter(.data[[nome_sindrome]] == 1) %>%
    mutate(
      Species_Curta = word(Species, 1, 2),
      Data = as.Date(paste(year, month, "01", sep = "-"))
    ) %>%
    arrange(Species_Curta, Tag, Data) %>%
    group_by(Species_Curta, Tag) %>%
    mutate(
      diff_dias = as.numeric(Data - lag(Data, default = as.Date("1900-01-01"))),
      novo_bloco = ifelse(diff_dias > 65, 1, 0),
      id_bloco = cumsum(novo_bloco)
    ) %>%
    group_by(Species_Curta, Tag, id_bloco) %>%
    summarise(Inicio = min(Data), Fim = max(Data), .groups = "drop") %>%
    rowwise() %>%
    mutate(Data = list(seq(from = Inicio, to = Fim, by = "month"))) %>%
    unnest(Data) %>%
    select(Species_Curta, Tag, Data) %>%
    distinct()
  
  # C e D. Filtro e Matriz
  n_amostral <- dados_continuos %>%
    group_by(Species_Curta) %>%
    summarise(n = n_distinct(Tag), .groups = "drop") %>%
    filter(n >= min_indiv)
  
  matriz <- dados_continuos %>%
    inner_join(n_amostral, by = "Species_Curta") %>%
    mutate(Species_Label = paste0(Species_Curta, " (", n, ")")) %>%
    group_by(Data, Species_Label) %>%
    summarise(Individuos_Ativos = n_distinct(Tag), .groups = "drop") %>%
    pivot_wider(names_from = Species_Label, values_from = Individuos_Ativos, values_fill = 0) %>%
    select(-Data)
  
  # E. Extrai r e p
  cor_matrix <- cor(matriz, method = "spearman")
  teste_sig <- suppressWarnings(cor.mtest(matriz, conf.level = 0.95, method = "spearman"))
  p_matrix <- teste_sig$p
  
  # F. Transforma as matrizes numa tabela legível (Pairwise)
  # Transforma em formato longo
  r_df <- as.data.frame(as.table(cor_matrix))
  p_df <- as.data.frame(as.table(p_matrix))
  
  tabela_resultados <- data.frame(
    Especie_1 = r_df$Var1,
    Especie_2 = r_df$Var2,
    r_Spearman = round(r_df$Freq, 3), # Arredonda para 3 casas decimais
    p_valor = round(p_df$Freq, 4)     # Arredonda para 4 casas decimais
  )
  
  # G. Limpa a tabela (remove correlações da espécie com ela mesma e pares duplicados)
  tabela_resultados <- tabela_resultados %>%
    filter(as.character(Especie_1) < as.character(Especie_2)) %>%
    # Cria uma coluna para facilitar a leitura: é significativo ou não?
    mutate(Significativo = ifelse(p_valor < 0.05, "Sim (*)", "Não")) %>%
    arrange(p_valor) # Ordena colocando os mais significativos no topo
  
  # H. Guarda o CSV e imprime no R
  write.csv2(tabela_resultados, nome_arquivo_csv, row.names = FALSE)
  message("Tabela guardada com sucesso: ", nome_arquivo_csv)
  
  return(tabela_resultados)
}

# 2. EXECUTANDO A FUNÇÃO -------------------------------------------------------

# Extrair valores para Zoofilia
tabela_zoofilia <- extrair_tabela_sincronia(
  dados_base = flower, 
  nome_sindrome = "Zoofilia", 
  nome_arquivo_csv = "valores_sincronia_zoofilia.csv"
)

# Imprimir os primeiros resultados no ecrã para ver agora mesmo
head(tabela_zoofilia, 10)







# 1. FUNÇÃO PARA GERAR O CORRELOGRAMA (COM FILTRO DE N MÍNIMO CORRIGIDO) -------

gerar_correlograma <- function(dados_base, nome_sindrome, nome_arquivo, min_indiv = 3) {
  
  # A. Filtra os dados e cria a data e nome curto
  dados_filtrados <- dados_base %>%
    filter(.data[[nome_sindrome]] == 1) %>%
    mutate(
      Species_Curta = word(Species, 1, 2),
      Data = as.Date(paste(year, month, "01", sep = "-"))
    )
  
  # B. Calcula o "n" amostral E APLICA O FILTRO
  n_amostral <- dados_filtrados %>%
    group_by(Species_Curta) %>%
    summarise(n = n_distinct(Tag), .groups = "drop") %>%
    filter(n >= min_indiv) # Corta as espécies que não atingem o mínimo
  
  # Trava de segurança caso o filtro remova espécies demais
  if(nrow(n_amostral) < 2) {
    message("Não há espécies suficientes (com n >= ", min_indiv, ") em: ", nome_sindrome)
    return(invisible(NULL))
  }
  
  # C. Prepara a Matriz
  matriz <- dados_filtrados %>%
    # inner_join garante que apenas as espécies aprovadas no filtro continuem na análise
    inner_join(n_amostral, by = "Species_Curta") %>% 
    mutate(Species_Label = paste0(Species_Curta, " (", n, ")")) %>%
    group_by(Data, Species_Label) %>%
    summarise(Individuos_Ativos = n_distinct(Tag), .groups = "drop") %>%
    pivot_wider(names_from = Species_Label, values_from = Individuos_Ativos, values_fill = 0) %>%
    select(-Data)
  
  if(ncol(matriz) < 2) {
    message("Não há colunas suficientes na matriz para: ", nome_sindrome)
    return(invisible(NULL))
  }
  
  # D. Correlação de Spearman e P-VALORES
  cor_matrix <- cor(matriz, method = "spearman")
  teste_sig <- suppressWarnings(cor.mtest(matriz, conf.level = 0.95, method = "spearman"))
  p_matrix <- teste_sig$p
  
  # E. Configuração da Imagem
  jpeg(nome_arquivo, width = 3500, height = 3500, res = 300)
  
  # F. Desenho do Gráfico
  corrplot::corrplot(
    cor_matrix,
    method = "color",       
    type = "lower",         
    diag = FALSE,           
    tl.col = "black",       
    tl.cex = 0.8,           
    tl.srt = 90,            
    tl.pos = "ld",          
    tl.offset = 0.5,        
    
    order = "hclust",       
    hclust.method = "ward.D2", 
    addgrid.col = "gray50", 
    
    p.mat = p_matrix,       
    sig.level = 0.05,       
    insig = "blank",        
    
    col = colorRampPalette(c("#ca0020", "#f7f7f7", "#0571b0"))(24),
    cl.length = 9,          
    
    title = paste("Matriz de Sincronia -", nome_sindrome),
    mar = c(0, 0, 4, 0)     
  )
  
  dev.off()
  message("Pronto! Gráfico salvo como: ", nome_arquivo)
}

# A) Gráfico de Zoocoria (Dispersão Biótica)
gerar_correlograma(
  dados_base = fruit,          
  nome_sindrome = "Zoocoria", 
  nome_arquivo = "sincronia_zoocoria2.jpg",
  min_indiv = 3 
)

## 1. Cria a nova coluna unindo as dispersões abióticas
fruit <- fruit %>%
  mutate(
    Nao_Zoocoria = ifelse(Anemocoria == 1 | Autocoria == 1, 1, 0)
  )

# 2. Agora sim, roda a função (o R já vai encontrar a coluna!)
gerar_correlograma(
  dados_base = fruit,          
  nome_sindrome = "Nao_Zoocoria", 
  nome_arquivo = "sincronia_nao_zooc.jpg",
  min_indiv = 3
)

