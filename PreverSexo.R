# Carrega as bibliotecas necessárias
library(tidyverse)
library(caret)
library(dslabs)

# Carrega o conjunto de dados "heights" do pacote dslabs
data(heights)

# Define o resultado (y) e o preditor (x)
# y representa o sexo (masculino ou feminino) e x representa a altura
y <- heights$sex
x <- heights$height

# Gera os conjuntos de treino e teste
# Define a semente para garantir reprodutibilidade nos resultados
set.seed(2007)
# Divide os dados aleatoriamente em 50% para treino e 50% para teste
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]  # Conjunto de teste
train_set <- heights[-test_index, ] # Conjunto de treino

# Adivinha o resultado aleatoriamente
# Aqui estamos apenas tentando adivinhar o sexo sem usar o preditor (altura)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# Calcula a precisão da adivinhação
# Verifica a proporção de acertos comparando y_hat com o valor real do sexo no conjunto de teste
mean(y_hat == test_set$sex)

# Compara as alturas médias e o desvio padrão entre homens e mulheres no conjunto de dados
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# Tenta prever "Masculino" se a altura for maior que 62, caso contrário, prevê "Feminino"
# Essa abordagem usa uma simples regra de corte
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
# Calcula a precisão desta abordagem comparando com os valores reais
mean(y == y_hat)

# Examina a precisão para 10 pontos de corte diferentes
# Aqui testamos vários valores de corte para ver qual deles oferece maior precisão
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

# Cria um gráfico mostrando a precisão para cada ponto de corte
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line()

# Encontra o ponto de corte com a melhor precisão
max(accuracy)

# Define o melhor ponto de corte baseado na precisão máxima obtida
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Aplica o melhor ponto de corte ao conjunto de teste e calcula a precisão
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

# Calcula a precisão final no conjunto de teste
mean(y_hat == test_set$sex)
