
#Package for reading xlsx files
install.packages("readxl")
library(readxl)
library("ggplot2")

library(dplyr)
library(plyr)

#read the data
dados <- read_excel("FEV-data-Excel.xlsx")

#DataFrame dimensions
dim(dados)
#Visualize the Dataframe
View(dados)
table(dados$Make)
length(dados)
str(dados)

#How many incomplete lines?
not_complete_cases <- sum(!complete.cases(dados))
not_complete_cases

class(dados)

#Renaming the variables
MyColumns <- colnames(dados)

MyColumns[1] <- "NomeCarro"
MyColumns[2] <- "Marca"
MyColumns[3] <- "Modelo"
MyColumns[4] <- "PrecoMinimo"
MyColumns[5] <- "PotenciaMotor"
MyColumns[6] <- "TorqueMaximo"
MyColumns[7] <- "TiposFreio"
MyColumns[8] <- "TipoAcionamento"
MyColumns[9] <- "CapacidadeBateria"
MyColumns[10] <- "Intervalo"
MyColumns[11] <- "BaseRoda"
MyColumns[12] <- "Comprimento"
MyColumns[13] <- "Largura"
MyColumns[14] <- "Altura"
MyColumns[15] <- "PesoMinimoVazio"
MyColumns[16] <- "PesoBrutoPermissivel"
MyColumns[17] <- "CapacidadeMaximaCarga"
MyColumns[18] <- "NumeroAssentos"
MyColumns[19] <- "NumeroPortas"
MyColumns[20] <- "TamanhoPneu"
MyColumns[21] <- "VelocidadeMaxima"
MyColumns[22] <- "CapacidadeCarga"
MyColumns[23] <- "Aceleracao0a100kph"
MyColumns[24] <- "PotenciaMaximaCargaDC"
MyColumns[25] <- "ConsumoEnergiaMedia"


colnames(dados) <- MyColumns
colnames(dados)

#Free memory
rm(MyColumns)

View(dados)

#Variable Target
dados$ConsumoEnergiaMedia

#Exploring the data in the dataset
str(dados)

summary(dados$ConsumoEnergiaMedia)
summary(dados[c('PrecoMinimo','ConsumoEnergiaMedia')])

#Dealing with Null Values
#How many lines have the complete data?
complete_cases <- sum(complete.cases(dados))
complete_cases

#How many lines are missing?
not_complete_cases <- sum(!complete.cases(dados))
not_complete_cases

#Percentage of missing data
percentual <- (not_complete_cases/complete_cases) * 100
percentual

#Null Data
nulos <- colwise(function(x){ sum(is.na(x))})
nulos(dados)


#Omit Null Data
dados2 <- na.omit(dados)
dados2

#Dimensions
dim(dados)
dim(dados2)

table(dados2$TipoAcionamento)

quantile(dados2$ConsumoEnergiaMedia)

#Graphically Viewing

# Boxplot
boxplot(dados2$PrecoMinimo, main = "Boxplot para os Preços Minimo dos Carros", ylab = "Preço (R$)")
boxplot(dados2$ConsumoEnergiaMedia, main = "Boxplot para o Consumo Médio", ylab = "KWh (KWh / 100km)")

# Histogram
hist(dados2$PrecoMinimo, main = "Histograma para os Preços Carros", xlab = "Preço (R$)")
hist(dados2$PotenciaMotor, main = "Histograma para a Potencia dos carros", breaks = 5,  ylab = "Potencia (KM)")
hist(dados2$ConsumoEnergiaMedia, main = "Histograma para o Consumo dos carros", breaks = 5, ylab = "Consumo (Km)")

# Scatterplot Price x Km
# Using price as the dependent variable (y)
plot(x = dados2$PotenciaMotor, y = dados2$ConsumoEnergiaMedia,
     main = "Scatterplot - Potencia x Consumo Medio",
     xlab = "Potencia",
     ylab = "Consumo")

# Scatterplot Price x Average consumption
# Using price as the dependent variable (y)
plot(x = dados2$PrecoMinimo, y = dados2$ConsumoEnergiaMedia,
     main = "Scatterplot - Preço x Consumo Medio",
     xlab = "Preço Minimo",
     ylab = "Consumo")

####################### CORRELATION ################################################### 

#Extracting the numerical variables
numeric_variable_list <- sapply(dados2, is.numeric)
numeric_variable_list
numerical_data <- dados2[numeric_variable_list]
numerical_data

#Correlation Matrix
cor(numerical_data)

colnames(numerical_data)

#Correlation Plot
pairs(numerical_data)
pairs(numerical_data[1:5], labels = colnames(numerical_data[1:5]))
pairs(numerical_data[6:10], labels = colnames(numerical_data[6:10]))


#Correlation Categorical Variable
install.packages("gmodels")
library(gmodels)
?CrossTable

CrossTable(x = dados2$TiposFreio, y = dados2$TipoAcionamento)

############################ Correlation Graphically ############################################## 
colnames(dados2)
cols <- c("PrecoMinimo", "PotenciaMotor", "TorqueMaximo",        
         "CapacidadeBateria", "Intervalo", "BaseRoda",
         "Comprimento", "Largura", "Altura", "PesoMinimoVazio", "PesoBrutoPermissivel",
         "CapacidadeMaximaCarga", "NumeroAssentos", "NumeroPortas", "TamanhoPneu", "VelocidadeMaxima",
         "CapacidadeCarga", "Aceleracao0a100kph", "PotenciaMaximaCargaDC", "ConsumoEnergiaMedia" )

# Vector with the correlation methods
metodos <- c("pearson", "spearman")

#Applying the correlation methods with the cor() function
cors <- lapply(metodos, function(method) 
  (cor(dados2[, cols], method = method)))

head(cors)

# Preparing the plot
require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Correlation Map
Map(plot.cors, cors, metodos)

#View data
View(dados2)

####################################### BOX PLOTS ###############################
# Average consumption x potential predictor variables
labels <- list("Boxplots - Consumo por preço",
               "Boxplots - Potencia Motor",
               "Boxplots - Torque Maximo",
               "Boxplots - Capacidade Bateria",
               "Boxplots - Intervalo",
               "Boxplots - Base Roda",
               "Boxplots - Comprimento",
               "Boxplots - Largura",
               "Boxplots - Altura",
               "Boxplots - Peso Minimo Vazio",
               "Boxplots - Capacidade Maxima Carga",
               "Boxplots - Numero assentos",
               "Boxplots - Numero Portas",
               "Boxplots - TamanhoPneu",
               "Boxplots - Velocidade Maxima",
               "Boxplots - Capacidade Carga",
               "Boxplots - Aceleracao0a100kph",
               "Boxplots - Potencia Maxima CargaDC"
               )

xAxis <- list("PrecoMinimo", "PotenciaMotor", "TorqueMaximo", "CapacidadeBateria", "Intervalo",
              "BaseRoda", "Comprimento", "Largura", "Altura", "PesoMinimoVazio", "PesoBrutoPermissivel",
              "CapacidadeMaximaCarga", "NumeroAssentos", "NumeroPortas", "TamanhoPneu", "VelocidadeMaxima",
              "CapacidadeCarga", "Aceleracao0a100kph", "PotenciaMaximaCargaDC")

# Function for Density Plots
plot.scatter <- function(X, label){ 
  ggplot(dados2, aes_string(x = X, y = "ConsumoEnergiaMedia")) + 
    geom_point(aes_string(colour = "ConsumoEnergiaMedia"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) 
}

Map(plot.scatter, xAxis, labels)

#########################################################################################

# Creating a model to identify the most important attributes for the predictive model
require(randomForest)

# Evaluating the importance of all variables
modelo <- randomForest(ConsumoEnergiaMedia ~ . , 
                       data = dados2, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

# Plotting the variables by level of importance
varImpPlot(modelo)

colnames(dados2)

# Removing collinear variables
modelo <- randomForest(ConsumoEnergiaMedia ~ . - TamanhoPneu
                       - NumeroPortas
                       - Altura
                       - Intervalo
                       - NumeroAssentos
                       - Largura
                       - Marca
                       - Modelo
                       - TiposFreio
                       - TipoAcionamento
                       - NomeCarro,
                       data = dados2, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)


# Plotting the variables by level of importance
varImpPlot(modelo)

# Saving the result
df_saida <- dados2[, c("ConsumoEnergiaMedia", rownames(modelo$importance))]

View(df_saida)
dim(df_saida)

df_teste <- df_saida
df_teste

#################################### Linear Model #################################
#training the model
install.packages("caTools")
library(caTools)

set.seed(101)
?sample.split

#Spliting
amostra <- sample.split(df_teste$PotenciaMotor, SplitRatio = 0.70)
amostra
#70% training data
treino = subset(df_teste, amostra = TRUE)

#30% test data
teste = subset(df_teste, amostra = FALSE)

#Generating the model
modelo_v1 <- lm(ConsumoEnergiaMedia ~ ., treino)
modelo_v2 <- lm(ConsumoEnergiaMedia ~ PrecoMinimo + PotenciaMotor, treino)
modelo_v3 <- lm(ConsumoEnergiaMedia ~ PrecoMinimo, treino)
modelo_v4 <- lm(ConsumoEnergiaMedia ~ PotenciaMotor, treino)

summary(modelo_v1) #0.9066
summary(modelo_v2) #0.6807
summary(modelo_v3) #0.6391
summary(modelo_v4) #0.68

#Visualizing the Model and Forecasting
#Residues
res <- residuals(modelo_v1)

#COnverting the object to a dataframe
res <- as.data.frame(res)
head(res)

#Histogram of the residues
ggplot(res, aes(res))+
  geom_histogram(fill  = "blue", alpha = 0.5, binwidth = 1)

# Model Plot
plot(modelo_v1)

modelo <- lm(ConsumoEnergiaMedia ~., treino)

prevendo_ConsumoEnergiaMedia <- predict(modelo_v1, teste)

prevendo_ConsumoEnergiaMedia

#Viewing the predicted values
resultados <- cbind(prevendo_ConsumoEnergiaMedia, teste$ConsumoEnergiaMedia)
colnames(resultados) <- c('Previsto', 'Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados)

#Function to handle 0 values
trata_zeros <- function(x){
  if(x < 0){
    return(0)
  }else{
    return(x)
  }
}

resultados$Previsto <- sapply(resultados$Previsto, trata_zeros)
resultados$Previsto

#mean error
#MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
mse

#RMSE
rmse <- mse^0.5
rmse

#R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(df_teste$ConsumoEnergiaMedia)- resultados$Real)^2)

#R-Squared ajuda a calcular o nivel de precisao do modelo. Quanto maior, melhor
R2 = 1 - (SSE/SST)
R2

#0.9066

############################## Redes Neurais ########################################################

# Loading the Neural Networks package
install.packages("neuralnet")
library(neuralnet)

#Maxs and Mins
maxs <- apply(df_teste, 2, max)
mins <- apply(df_teste, 2, min)

#printing the values
maxs
mins

#Normalization
dados_normalizados <- as.data.frame(scale(df_teste, center = mins, scale = maxs -mins))
head(dados_normalizados)

#Creating the training and test data
#install.packages("caTools")
library(caTools)
split = sample.split(dados_normalizados$ConsumoEnergiaMedia, SplitRatio = 0.70)

treino = subset(dados_normalizados, split == TRUE)
teste = subset(dados_normalizados, split == FALSE)

#Getting the column names
coluna_nomes <- names(treino)
coluna_nomes

#Aggregating
formula <- as.formula(paste("ConsumoEnergiaMedia ~", paste(coluna_nomes[!coluna_nomes %in% "ConsumoEnergiaMedia"], collapse = "+" )))
formula

#Training the model
rede_neural <- neuralnet(formula, data = treino, hidden = c(5,3), linear.output = TRUE)

#Plot
plot(rede_neural)

#Making predictions
rede_neural_prev <- compute(rede_neural, teste[1:13])
rede_neural_prev

#Converting the test data
previsoes <- rede_neural_prev$net.result * (max(df_teste$ConsumoEnergiaMedia) - min(df_teste$ConsumoEnergiaMedia)) + min(df_teste$ConsumoEnergiaMedia)
teste_convert <- (teste$ConsumoEnergiaMedia) * (max(df_teste$ConsumoEnergiaMedia) - min(df_teste$ConsumoEnergiaMedia)) + min(df_teste$ConsumoEnergiaMedia)

#Calculating the Mean Squared Error
MSE.nn <- sum((teste_convert - previsoes)^2/nrow(teste))
MSE.nn

#Errors and predictions
error.df <- data.frame(teste_convert, previsoes)

head(error.df)

#Plot dos erros
library(ggplot2)
ggplot(error.df, aes(x = teste_convert, y=previsoes)) +
  geom_point() + stat_smooth()
