library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


##########################################################################
#Q7 Faça o split do datafa frama Iris para treino e teste
##########################################################################

#Criand dataframe de teste e treino
set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
df_teste <- iris[test_index,]
df_treino <- iris[-test_index,]

##########################################################################
#Q8 Identifique a melhor variavel para correlação das flores
##########################################################################

# Função para procurar a melhor para da flor para classificação 
funcao_acuracia <- function(tamanho_parte_da_flor,parte_da_flor,informacoes_da_flor) {
  predicao <- ifelse(informacoes_da_flor[[parte_da_flor]] > tamanho_parte_da_flor, "virginica", "versicolor")
  mean(predicao == informacoes_da_flor$Species)
}

# Características a serem avaliadas
partes_da_flor <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Data frame para armazenar os melhores resultados
melhores_resultados <- data.frame(Parte_Flor = character(), Melhor_Corte = numeric(), Melhor_Acuracia = numeric(), stringsAsFactors=FALSE)



# Procurar o tamanho ideal para idenificar cada parte da flor

for (parte_da_flor in partes_da_flor){
  melhor_acuracia <- 0
  melhor_corte <- NA
  
  tamanhos_partes_da_flor <- seq(min(df_treino[[parte_da_flor]]), max(df_treino[[parte_da_flor]]), by=0.1)
  for (tamanho_parte_da_flor in tamanhos_partes_da_flor) {
    acuracia <- funcao_acuracia(tamanho_parte_da_flor, parte_da_flor, df_treino)
    if (acuracia > melhor_acuracia) {
      melhor_acuracia <- acuracia
      melhor_corte <- tamanho_parte_da_flor
    }
  }
  
  # Adicionar os melhores resultados ao data frame
  melhores_resultados <- rbind(melhores_resultados, data.frame(Parte_Flor = parte_da_flor, Melhor_Corte = melhor_corte, Melhor_Acuracia = melhor_acuracia))
}

# Exibir os resultados
print(melhores_resultados)

##########################################################################
#Q9 Utilize o melhor tamanho/corte da flor encontrado no treino para calcular a acuracia na BASE DE TESTE
##########################################################################

parte_da_flor_statico <- "Petal.Width" 
melhor_corte_statico <- 1.5
predicao <- ifelse(df_teste[[parte_da_flor_statico]] > melhor_corte_statico, "virginica", "versicolor")
mean(predicao == df_treino$Species)

##########################################################################
#Q10 Identifique qual a segunda melhor parte da flor para prever o tipo de flor
##########################################################################

# Exibir os resultados
print(melhores_resultados)



##########################################################################
#Q11 Função para calcular a precisão com base nos cortes de Petal.Length e Petal.Width
##########################################################################

plot(iris, pch=21, bg=iris$Species)

#Função para calcular a precisão com base nos cortes de Petal.Length e Petal.Width
calc_accuracy <- function(length_cutoff, width_cutoff, data) {
  predictions <- ifelse(data$Petal.Length > length_cutoff & data$Petal.Width > width_cutoff, "virginica", "versicolor")
  mean(predictions == data$Species)
}

# Procurar os melhores cortes
length_cutoffs <- seq(min(train_set$Petal.Length), max(train_set$Petal.Length), by=0.1)
width_cutoffs <- seq(min(train_set$Petal.Width), max(train_set$Petal.Width), by=0.1)

best_length_cutoff <- NULL
best_width_cutoff <- NULL
best_accuracy <- 0

for (length_cutoff in length_cutoffs) {
  for (width_cutoff in width_cutoffs) {
    accuracy <- calc_accuracy(length_cutoff, width_cutoff, train_set)
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_length_cutoff <- length_cutoff
      best_width_cutoff <- width_cutoff
    }
  }
}

# Prever no conjunto de teste usando os melhores cortes
test_predictions <- ifelse(test_set$Petal.Length > best_length_cutoff & test_set$Petal.Width > best_width_cutoff, "virginica", "versicolor")

# Calcular a precisão no conjunto de teste
test_accuracy <- mean(test_predictions == test_set$Species)

# Mostrar a precisão no conjunto de teste
cat("Precisão geral no conjunto de teste: ", test_accuracy, "\n")

