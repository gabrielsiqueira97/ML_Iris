# Carregar o conjunto de dados Iris
data(iris)

# Verificar a estrutura do conjunto de dados
str(iris)


# Definir uma semente para reprodutibilidade
set.seed(123)

# Dividir os dados em 70% treino e 30% teste
index <- sample(1:nrow(iris), round(0.7 * nrow(iris)))
conjunto_treino <- iris[index, ]
conjunto_teste <- iris[-index, ]

# Verificar as primeiras linhas dos conjuntos de treino e teste
head(conjunto_treino)
head(conjunto_teste)

# Renomear variáveis para português
names(iris) <- c("Comprimento_Sépala", "Largura_Sépala", "Comprimento_Pétala", "Largura_Pétala", "Espécie")

# Atualizar os conjuntos de treino e teste
conjunto_treino <- iris[index, ]
conjunto_teste <- iris[-index, ]

# Carregar a biblioteca necessária
library(class)

# Selecionar as características e os rótulos
caracteristicas_treino <- subset(conjunto_treino, select = c("Comprimento_Sépala", "Comprimento_Pétala"))
caracteristicas_teste <- subset(conjunto_teste, select = c("Comprimento_Sépala", "Comprimento_Pétala"))
rotulos_treino <- conjunto_treino$Espécie

##############################
# Passo 3 Treinar o modelo KNN
##############################

# Predições na base de treino
pred_treino <- knn(train = caracteristicas_treino, test = caracteristicas_treino, cl = rotulos_treino, k = 3)

# Matriz de confusão na base de treino
matriz_confusao_treino <- confusionMatrix(pred_treino, conjunto_treino$Espécie)

# Exibir a matriz de confusão
print(matriz_confusao_treino$table)

# Calcular métricas por classe na base de treino
metricas_treino <- data.frame(
  Classe = rownames(matriz_confusao_treino$byClass),
  Sensibilidade = matriz_confusao_treino$byClass[, "Sensitivity"],
  Especificidade = matriz_confusao_treino$byClass[, "Specificity"],
  F1_Score = matriz_confusao_treino$byClass[, "F1"]
)

# Exibir as métricas por classe na base de treino
print(metricas_treino)

# Calcular e exibir precisão geral na base de treino
precisao_treino <- matriz_confusao_treino$overall['Accuracy']
cat("Precisão na base de treino: ", precisao_treino, "\n")


##############################
# Passo 4: Avaliar o Modelo
##############################
# Carregar a biblioteca necessária
library(caret)

# Predições
pred <- modelo_knn

# Matriz de confusão
matriz_confusao <- confusionMatrix(pred, conjunto_teste$Espécie)

# Matriz de confusão
print(matriz_confusao$table)

# Calcular métricas por classe
metricas <- data.frame(
  Classe = rownames(matriz_confusao$byClass),
  Sensibilidade = matriz_confusao$byClass[, "Sensitivity"],
  Especificidade = matriz_confusao$byClass[, "Specificity"],
  F1_Score = matriz_confusao$byClass[, "F1"]
)

# Exibir as métricas por classe
print(metricas)

# Calcular e exibir precisão geral
precisao <- matriz_confusao$overall['Accuracy']
cat("Precisão: ", precisao, "\n")

############################
#Passo 5 - Olhando resultado linha a linha
############################

# Criar um data frame com as predições e os valores reais
resultado_treino <- data.frame(
  Comprimento_Sépala = conjunto_treino$Comprimento_Sépala,
  Comprimento_Pétala = conjunto_treino$Comprimento_Pétala,
  Espécie_Real = conjunto_treino$Espécie,
  Espécie_Prevista = pred_treino
)

# Exibir os primeiros resultados
head(resultado_treino)


