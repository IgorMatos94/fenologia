#sync
#filter again to only include fruting individuals (with immature or mature 
#fruits)
fruit <- gr8r_3 %>%
  filter(imfruit == 1 | mfruit == 1)

#filter to only include zoochorous species 
fruit_zooc <- fruit %>%
  filter(Zoocoria == 1)

#calculate the number of fruiting individuals per species, per month
fruit_zooc_new <- fruit_zooc %>%
  group_by(Species, month) %>%
  summarise(ind = n_distinct(Tag))

#filter to only include zoochorous tree species 
zooc <- gr8r_3 %>%
  filter(Zoocoria == 1)

#calculate total number of zoochorous fruiting individuals 
fruit_zooc_new2 <- zooc %>%
  group_by(Species, month) %>%
  summarise(ind = n_distinct(Tag))

#see which months we didn't survey any individuals of that species, we need 
#to have the number of indiviudals for every month, even if it's 0
fruit_zooc_new3 <- fruit_zooc_new2 %>%
  group_by(Species) %>%
  summarise(month = n_distinct(month))

#arrange sheet so its easier to see which species need fixing 
fruit_zooc_new4 <- fruit_zooc_new3 %>%
  arrange(month)

#create a new dataframe with the missing data 
list(fruit_zooc_new4$Species)

Species <- c( "Cupania vernalis Cambess.", 
              "Cupania vernalis Cambess.",
              "Cupania vernalis Cambess.",
              "Andira fraxinifolia Benth.",
              "Andira fraxinifolia Benth.",
              "Ocotea puberula (Rich.) Nees", 
              "Ocotea puberula (Rich.) Nees",                             
              "Eugenia pyriformis Cambess.",                              
              "Lafoensia pacari A.St.-Hil.",                             
              "Lithraea molleoides (Vell.) Engl.",                       
              "Myrcia tomentosa (Aubl.) DC.",                             
              "Tapirira guianensis Aubl.",                                
              "Zanthoxylum monogynum A.St.-Hil.",                         
              "Zanthoxylum riedelianum Engl.")

month <- c("02", "05", "10", "02", "12", "03", "05", "05", 
           "12", "05", "12", "12", "12", "05")


ind <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

missing_data <- data.frame(Species, month, ind)


#bind missing data to original data set 
full.data <- bind_rows(fruit_zooc_new2, missing_data)

#merge the spreadsheet with the total number of individuals and the number of
#fruiting individuals 
merge <- merge(x=full.data, y=fruit_zooc_new,
               by=c("Species", "month"), all.x =T)

#calculate the proportion of fruiting individuals 
merge$prop_zooc <- (merge$ind.y/merge$ind.x)

#fill the NAs with 0
merge[is.na(merge)] <- 0


#select the three columns that we want to transform into a matrix 
merge1 <- merge %>% select(1, 2, 5)

merge1$prop_zooc <- as.numeric(merge1$prop_zooc)


#create the matrix where each column is a species and each row is a month
matrix_zooc <- merge1 %>%
  pivot_wider(names_from = Species,
              values_from = prop_zooc) 

#excluding columns with no activity
matrix_zooc_n <- matrix_zooc[, -c(3, 4, 7, 9, 10, 11, 13, 18, 19, 20,
                                  21, 23, 27)]


#make the first column the row names
matrix_zooc_df <- matrix_zooc_n[, -1]

#run communitmatrix_n#run community sync test
community.sync(matrix_zooc_df, nrands = 100)

#create a correlation matrix --> but how to we deal with the NAs?? Why are 
#there so many NAs??
correlation_zooc_matrix <- cor(matrix_zooc_df, method = "pearson")

# Criar o heatmap ajustando a escala
par(pty = "s")
image(1:nrow(correlation_zooc_matrix), 1:ncol(correlation_zooc_matrix), correlation_zooc_matrix, col = colorRampPalette(c("#BCDE22", "#80A016", "#3B5708"))(100), axes = FALSE, main = "Heatmap de Correlação de Pearson", xlab = "", ylab = "")

# Inicializar a matriz de resultados
n_var <- ncol(matrix_zooc_df)
matriz_zooc_df_res <- matrix("", nrow = n_var, ncol = n_var)

# Calcular a correlação e o valor-p entre todas as variáveis
for (i in 1:n_var) {
  for (j in 1:n_var) {
    correlation_zooc_matrix_teste <- cor.test(matrix_zooc_df[[i]], matrix_zooc_df[[j]])
    matriz_zooc_df_res[i, j] <- paste(sprintf("%.2f", correlation_zooc_matrix_teste$estimate), sprintf("(p = %.3f)", correlation_zooc_matrix_teste$p.value), sep = "\n")
  }
}

# Adicionar os valores da correlação ao gráfico
for (i in 1:nrow(correlation_zooc_matrix)) {
  for (j in 1:ncol(correlation_zooc_matrix)) {
    text(j, i, matriz_zooc_df_res[i, j], cex = 0.5, col = "black", font = 2)
  }
}

# Adicionar rótulos dos eixos x e y
# Adicionar rótulos dos eixos x na vertical
par(plt = c(0.1, 0.9, 0.1, 0.9))
axis(1, ncol(correlation_zooc_matrix), labels = FALSE)
for (i in 1:ncol(correlation_zooc_matrix)) {
  text(i, par("usr")[3] - 0.5, colnames(correlation_zooc_matrix)[i], srt = 90, adj = 1, xpd = TRUE, cex.axis = 0.8)
}

# Adicionar rótulos dos eixos y na horizontal
par(plt = c(0.1, 0.9, 0.1, 0.9))
axis(2, nrow(correlation_zooc_matrix), labels = FALSE)
for (i in 1:nrow(correlation_zooc_matrix)) {
  text(par("usr")[1] - 0.5, i, rownames(correlation_zooc_matrix)[i], srt = 0, adj = 1, xpd = TRUE, cex.axis = 0.8)
}

