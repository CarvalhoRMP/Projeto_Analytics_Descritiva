###############################################################################
#                                                                             #
#                                                                             #
#                               PROJETO FINAL                                 #
#                                                                             #
#     DISCIPLINA: ANALYTICS DESCRITIVA                                        #
#     BASE DE DADOS: https://www.kaggle.com/harlfoxem/housesalesprediction    #
#     DESCRIÇÃO DA BASE DE DADOS:                                             #
#         This dataset contains house sale prices for King County, which      #
#         includes Seattle. It includes homes sold between May 2014 and       #
#         May 2015. It's a great dataset for evaluating simple regression     #
#         models.                                                             #
#                                                                             #
#                                                                             #
###############################################################################


# Carregando bibliotecas
require(tidyverse)
require(ggplot2)
require(GGally)
require(readr)
require(lubridate)
require(ggmap)
require(psych)
require(gridExtra)

# Definindo diretório da base de dados
setwd("DADOS")
getwd()

# Carregando base de dados
houses <- read.csv("kc_house_data.csv")

# Visualizando Cabeçalho 
head(houses)

# Inspecionando base de dados - Informações Gerais
dim(houses)
str(houses)

# Verificando se existem valores em branco na base de dados
sum(is.na(houses))

#REALIZANDO TRATAMENTO DE DADOS
# Eliminando coluna "id"
houses$id <- NULL
houses$zipcode <- NULL

# Modificando os títulos das colunas
# Por não compreender o significado das colunas "yr_renovated",
# "sqft_living15" e "sqft_lot15" o nome destas colunas não foi modificado.
names(houses) <- c("Data", "Preco", "Num_Quartos", "Num_Banheiros", "Area_Construida",
                   "Area_Total", "Num_Andares", "Beira_Mar", "Vista_Mar", "Condicao",
                   "Avaliacao", "Area_Sotao", "Area_Porao", "Ano_Contrucao",
                   "yr_renovated", "Latitude", "Longitude",
                   "sqft_living15", "sqft_lot15")

# Ajustando a coluna Data para o formato correto
houses$Data <- substr(houses$Data, 1, 8)
houses$Data <- ymd(houses$Data)

# Modificando o tipo de dado da coluna "Codigo_Postal" de inteiro para o caracter
#houses$Codigo_Postal <- as.character(houses$Codigo_Postal)
# Visualizando o dataset após as modificações
str(houses)
dim(houses)

# DESCREVENDO VARIÁVEIS NUMÉRICAS
# Resumindo os dados
summary(houses)

# Convertendo dados numéricos para uma única coluna
houses_pivot <- pivot_longer(houses %>% select_if(is.numeric)
                             %>% select(-c("Beira_Mar", "Vista_Mar", "Condicao",
                                           "Avaliacao")),
                             cols = everything(), names_to = "Atributos_Numericos",
                             values_to = "Valores")

# Visualizando variáveis numéricas através de um boxplot
ggplot(houses_pivot, mapping = aes(x=Atributos_Numericos, y=Valores, fill="red")) +
  geom_boxplot(show.legend = FALSE) + 
  facet_wrap(.~Atributos_Numericos, ncol = 5, scales = "free") +
  theme(strip.text.x = element_blank(), text = element_text(size = 9))

# Criando dataframe apenas com as colunas numéricas
# e sem as colunas "Latitude" e "Longitude"
houses_cols_num <- houses %>% select_if(is.numeric) %>% select(-c("Beira_Mar",
                                                                  "Vista_Mar",
                                                                  "Condicao",
                                                                  "Avaliacao",
                                                                  "Latitude",
                                                                  "Longitude"))

# Avaliando correlação entre pares de variáveis
ggpairs(houses_cols_num, upper = list(continuous = wrap("cor", size = 3))) +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey95"))

# Criando mapa de calor para a tabela de correlações
corPlot(houses_cols_num, xlas=2, main="Matriz de Correlações", scale=F,
        gr=colorRampPalette(c("White", "yellow2", "#00B6EB", "blue3")), cex=0.7,
        cex.axis=0.7)

# Relacionando Dados Categóricos e Numéricos
# Criando dataset com dados de interesse
houses_cat <- houses %>% select(c("Beira_Mar", "Vista_Mar", "Condicao",
                                  "Avaliacao", "Preco"))

# Visualizando relação entre variáveis categóricas e preço através
# Criando objeto gráfico que conta a quantidade de casas por localização - Barplot
plot1 <- ggplot(houses_cat, mapping=aes(x=as.factor(Beira_Mar),
                               fill=as.factor(Beira_Mar))) +
  geom_bar(show.legend=F) +
  labs(x="Localização a beira-mar", y="Quantidade de casas",
       title= "Qtd de casas x Localização a beira-mar") +
  theme(text = element_text(size=6))

# Criando objeto gráfico que conta a quantidade de casas Com cômodos
# com vista para o mar - Barplot
plot2 <- ggplot(houses_cat, mapping=aes(x=as.factor(Vista_Mar),
                               fill=as.factor(Vista_Mar))) +
  geom_bar(show.legend=F) +
  labs(x="Qtd de cômodos com vista para o mar", y="Quantidade de casas",
       title= "Qtd de casas x Cômodos com vista para o mar") +
  theme(text = element_text(size=6))

# Criando objeto gráfico que conta a quantidade de casas por condição - Barplot
plot3 <- ggplot(houses_cat, mapping=aes(x=as.factor(Condicao),
                                        fill=as.factor(Condicao))) +
  geom_bar(show.legend=F) +
  labs(x="Condição do Imóvel", y="Quantidade de casas",
       title= "Qtd de casas x Condição do Imóvel") +
  theme(text = element_text(size=6))

# Criando objeto gráfico que conta a quantidade de casas por Avaliação - Barplot
plot4 <- ggplot(houses_cat, mapping=aes(x=as.factor(Avaliacao),
                                        fill=as.factor(Avaliacao))) +
  geom_bar(show.legend=F) +
  labs(x="Avaliação do Imóvel", y="Quantidade de casas",
       title= "Qtd de casas x Avaliação do Imóvel") +
  theme(text = element_text(size=6))

# Criando objeto gráfico relacionando Preço x Localização - Boxplot
plot5 <- ggplot(houses_cat, mapping = aes(x=as.factor(Beira_Mar), y=Preco,
                                          fill = as.factor(Beira_Mar))) +
  geom_boxplot(show.legend=F) +
  labs(x="Localização a beira-mar", y="Preço - Dólar ($)",
       title= "Variação do Preço x Localização a Beira-Mar") +
  theme(text = element_text(size=6))

# Criando objeto gráfico relacionando Preço x Quantidade de cômodos
# com vista para o mar - Boxplot
plot6 <- ggplot(houses_cat, mapping = aes(x=as.factor(Vista_Mar), y=Preco,
                                          fill = as.factor(Vista_Mar))) +
  geom_boxplot(show.legend=F) +
  labs(x="Qtd de cômodos com vista para o mar", y="Preço - Dólar ($)",
       title="Variação do preço x Vista para o mar") +
  theme(text = element_text(size=6))

# Criando objeto gráfico relacionando Preço x Condição do Imóvel - Boxplot
plot7 <- ggplot(houses_cat, mapping = aes(x=as.factor(Condicao), y=Preco,
                                          fill = as.factor(Condicao))) +
  geom_boxplot(show.legend=F) +
  labs(x="Condição do Imóvel", y="Preço - Dólar ($)",
       title= "Variação do Preço x Condição do Imóvel") +
  theme(text = element_text(size=6))

# Criando objeto gráfico relacionando Preço x Avaliação do Imóvel - Boxplot
plot8 <- ggplot(houses_cat, mapping = aes(x=as.factor(Avaliacao), y=Preco,
                                          fill = as.factor(Avaliacao))) +
  geom_boxplot(show.legend=F) +
  labs(x="Avaliação do Imóvel", y="Preço - Dólar ($)",
       title= "Variação do Preço x Avaliação do Imóvel") +
  theme(text = element_text(size=6))

# Criando tabela que relaciona o preço médio com a localização
PrecoMedioLocal <- houses_cat %>% group_by(Beira_Mar) %>% summarise(Preco_L = mean(Preco))

# Criando objeto gráfico que relaciona Preço Médio x Localização - Barplot
plot9 <- ggplot(PrecoMedioLocal, mapping=aes(x=as.factor(Beira_Mar), y=Preco_L,
                                                fill=as.factor(Beira_Mar))) + 
  geom_bar(stat = "identity", show.legend=F) +
  labs(x="Localização a beira-mar", y="Preco Médio ($)",
       title="Preço Médio do Imóvel x Localização a Beira-Mar") +
  theme(text = element_text(size=6))

# Criando tabela que relaciona o preço médio com a quantidade de cômodos
# com vista para a beira-mar
PrecoMedioVista <- houses %>% group_by(Vista_Mar) %>% summarise(Preco_V = mean(Preco))

# Criando objeto gráfico que relaciona Preço Médio x Quantidade de cômodos
# com vista para o mar - Barplot
plot10 <- ggplot(PrecoMedioVista, mapping=aes(x=as.factor(Vista_Mar), y=Preco_V,
                                                fill=as.factor(Vista_Mar))) + 
  geom_bar(stat = "identity", show.legend=F) +
  labs(x="Qtd de cômodos com vista para o mar", y="Preco Médio ($)",
       title="Preço Médio do Imóvel x Vista para o Mar") +
  theme(text = element_text(size=6))

# Criando tabela que relaciona o preço médio com a localização
PrecoMedioCond <- houses_cat %>% group_by(Condicao) %>% summarise(Preco_C = mean(Preco))

# Criando objeto gráfico que relaciona Preço Médio x Localização - Barplot
plot11 <- ggplot(PrecoMedioCond, mapping=aes(x=as.factor(Condicao), y=Preco_C,
                                             fill=as.factor(Condicao))) + 
  geom_bar(stat = "identity", show.legend=F) +
  labs(x="Condição do Imóvel", y="Preco Médio ($)",
       title="Preço Médio do Imóvel x Condição do Imóvel") +
  theme(text = element_text(size=6))

# Criando tabela que relaciona o preço médio com a localização
PrecoMedioAval <- houses_cat %>% group_by(Avaliacao) %>% summarise(Preco_A = mean(Preco))

# Criando objeto gráfico que relaciona Preço Médio x Localização - Barplot
plot12 <- ggplot(PrecoMedioAval, mapping=aes(x=as.factor(Avaliacao), y=Preco_A,
                                             fill=as.factor(Avaliacao))) + 
  geom_bar(stat = "identity", show.legend=F) +
  labs(x="Avaliação do Imóvel", y="Preco Médio ($)",
       title="Preço Médio do Imóvel x Avaliação do Imóvel") +
  theme(text = element_text(size=6))

# Plotando objetos gráficos na mesma área
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
             plot10, plot11, plot12, nrow=3, ncol=4)

# Criando Mapas para visualizar posição das casas

# Definindo área do gráfico
lat_max <- round(max(houses$Latitude) + 0.05, 2)
lat_min <- round(min(houses$Latitude) - 0.05, 2)
long_max <- round(max(houses$Longitude) + 0.05, 2)
long_min <- round(min(houses$Longitude) - 0.05, 2)

# Gerando mapa geral da região onde os dados foram coletados
us <- c(left = long_min, bottom = lat_min, right = long_max, top = lat_max)

# Mapa da região onde os dados foram coletados
get_stamenmap(us, zoom = 8, maptype = "terrain") %>% ggmap()

# Mapa contemplando a localização dos imóveis
qmplot(Longitude, Latitude, data=houses, maptype = "toner-lite", color = I("red"))
