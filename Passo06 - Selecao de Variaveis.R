####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPÉCIES (SDM)         ####
####  SELEÇÃO DE VARIAVEIS AMBIENTAIS PARA REDUCAO DE AUTOCORRELACAO AMBIENTAL  ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVOS: SELEÇÃO DE VARIAVEIS AMBIENTAIS
#######               A. Extrair os valores das camadas ambientais
#######               B. Selecionar as variaveis pela correlacao e VIF
#######               C. Selecionar as variaveis pela Análise de Componente Princital





############ 1. INSTALACAO DOS PACOTES E CARREGAR AS FUNCOES AUXILIARES ############
# Carregar os pacotes necessarios:
library(rgdal)
library(raster)
library(dismo)
library(usdm)
library(vegan)

#delimitar uma projecao espacial para lat/long e para UTM:
longlat_WGS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
UTM_proj = CRS("+proj=utm +zone=22 +south +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#ou criar uma a partir de codigo EPGS
A = make_EPSG() 
LL_prj = as.character(subset(A, A$code=="4326")[3]) 
UTM_prj = as.character(subset(A, A$code=="5383")[3]) 





#################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ###########################
#carregar todas os arquivos rasters de uma pasta, selecionando pelo formato. Recursive = TRUE incluirá todas as subpastas, As camadas devem estar na mesma resolução. Mude o p"pattern" para o formato dos rasters
camadas = list.files(path="./asc_presente/", pattern =".asc", full.names=TRUE)
camadas = stack(camadas)
#definir a projecao:
crs(camadas) = longlat_WGS
#Verificar
camadas

# Carregar os pontos de ocorrencia:
pontos_ocorrencia = read.csv("./pontos/psimonsi_corrigido.csv")
head(pontos_ocorrencia)
str(pontos_ocorrencia)
#adicionar uma projecao
coordinates(pontos_ocorrencia) = ~Long+Lat #nome das colunas.
crs(pontos_ocorrencia) =  longlat_WGS





#################### 3. EXTRAIR OS VALORES DAS CAMADAS AMBIENTAIS ##################
#Extrair os valores:
valores = as.data.frame(extract(camadas, pontos_ocorrencia))
class(valores)
head(valores)
summary(valores)





################ 4. SELECIONAR AS VARIAVEIS PELO VIF/CORRELACAO ####################
#maximo de variáveis no modelo: entre 3 e 5 variáveis.
length(pontos_ocorrencia)/10 # 
length(pontos_ocorrencia)/15 # 

#A. Selecionar as variaveis usando um valor máximo de correlação de 0.7
vif_variaveis = vifcor(valores, th = 0.7)
vif_variaveis
# 5 variaveis com correlacao <0.7
# todas com VIF < 5
# maxima correlacao entre as varaiveis = 0.48

#B. Selecionar as variaveis usando um valor máximo de VIF de 5
vif_variaveis2 = vifstep(valores, th = 5)
vif_variaveis2
# 5 variaveis com correlacao <0.7
# todas com VIF < 5
# maxima correlacao entre as varaiveis = 0.68

#C. Selecioanr as variaveis manualmente usando um valor máximo de VIF de 5
options(scipen = 999) #desligar numeros em notação cientifica
vif_variaveis3 = vif(valores)
vif_variaveis3

#Remover manualmente, mantendo a variavel bio6
names(valores)
valores2 = valores[, -17]
names(valores2)

#Refazer os calculos e retirar até sobrar apenas variaveis com VIF < 5
vif_variaveis3 = vif(valores2)
vif_variaveis3
valores2 = valores2[, -2]
names(valores2)

####

#escolher um dos conjuntos de dados:
vif_variaveis
vif_variaveis2
vif_variaveis3

#salvar o resultado, no caso escolhi o primeiro conjunto
write.csv(as.data.frame(vif_variaveis@results), file = "./Resultados/Vif_variaveis.csv")





####################### 5. SELECIONAR AS VARIAVEIS PELA PCA ########################
# fazer a PCA com os valores extraidos:
pca_variaveis = prcomp(valores, center=TRUE, scale=TRUE)
summary(pca_variaveis) #verificar a contribuicao dos PC

#A1. Selecionar o numero de PCs pela Broken Stick Model:
screeplot(pca_variaveis, bstick=TRUE, type="lines")
screeplot(pca_variaveis, bstick=TRUE, type="barplot") #3 PC
#Broken Stick Model: PC devem ser retidos contanto que o valor observado de autovalores (eigenvalues/ordination) sejam maiores que os valores esperados pelo modelo randômico do Broken Stick components. Jackson 1993 & Legendre & Legendre 2012
summary(pca_variaveis)
#3PCs are 92.27% of variance

#A2. Selecionar o numero de PCs que somem mais de 90% da variância explicada (Wang et al. 2016 - Study on selecting sensitive environmental variables in modelling species spatial distribution).
summary(pca_variaveis)
#3PCs are 92.27% of variance

#B1. Selecionar as variaveis mais importantes para cada PC selecioando. São as variaveis que tem valor absoluto maior que 0.32 - o que seria referente a 10% da variacia do PC (Tabachnick and Fidell 1989):

#definir numero de PCs:
pc = 3
#definir os eixos principais
pca_rotation = abs(pca_variaveis$rotation) #don't matter the direction

lista = list()
for (i in 1:pc) {posicao = pca_rotation[pca_rotation[, i] > 0.32, ]
                 lista[[i]]=row.names(posicao)}

variaveis_sel = unlist(lista)
variaveis_sel = unique(variaveis_sel)
variaveis_sel #4 variaveis

#salvar os resultados
write.csv(as.data.frame(variaveis_sel), file = "./Resultados/PCA_variaveis.csv")

#B2. Selecionar variavel com a maior contribução em cada PC selecionado
PC1_var = names(which.max(abs(pca_variaveis$rotation[,1])))
PC1_var
PC2_var = names(which.max(abs(pca_variaveis$rotation[,2])))
PC2_var
PC3_var = names(which.max(abs(pca_variaveis$rotation[,3])))
PC3_var

#salvar os resultados:
as.data.frame(rbind(PC1_var, PC2_var, PC3_var))
write.csv(as.data.frame(rbind(PC1_var, PC2_var, PC3_var)), file = "./Resultados/PCA_variaveis_maximocontribuicao.csv")


#END
