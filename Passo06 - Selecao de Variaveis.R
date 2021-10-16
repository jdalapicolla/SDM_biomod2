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


#Verificar a projeção 
rgdal::rgdal_extSoftVersion()

#Para GDAL >3 e PROJ >6
(LL_prj <- CRS("+init=epsg:4326"))
cat(strwrap(gsub(",", ", ", (comment(LL_prj)))), sep="\n")
LL_prj

(UTM_prj <- CRS("+init=epsg:5383"))
cat(strwrap(gsub(",", ", ", (comment(UTM_prj)))), sep="\n")
UTM_prj


#ou  GDAL <3 e PROJ <6
A = rgdal::make_EPSG() 
LL_prj = as.character(subset(A, A$code=="4326")[3]) 
UTM_prj = as.character(subset(A, A$code=="5383")[3]) 




#################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ###########################
#carregar todas os arquivos rasters de uma pasta, selecionando pelo formato. Recursive = TRUE incluirá todas as subpastas, As camadas devem estar na mesma resolução. Mude o p"pattern" para o formato dos rasters
camadas = list.files(path="./asc_presente/", pattern =".asc", full.names=TRUE)
camadas = stack(camadas)
#definir a projecao:
crs(camadas) = LL_prj
#Verificar
camadas

# Carregar os pontos de ocorrencia:
pontos_ocorrencia = read.csv("./simonsi/points/points_filtered_sim.csv")
head(pontos_ocorrencia)
str(pontos_ocorrencia)
#adicionar uma projecao
coordinates(pontos_ocorrencia) = ~longitude+latitude #nome das colunas.
crs(pontos_ocorrencia) =  LL_prj





#################### 3. EXTRAIR OS VALORES DAS CAMADAS AMBIENTAIS ##################
#Extrair os valores:
valores = as.data.frame(raster::extract(camadas, pontos_ocorrencia))
class(valores)
head(valores)
summary(valores)


################ 4. SELECIONAR AS VARIAVEIS PELO VIF/CORRELACAO ####################
#maximo de variáveis no modelo: entre 3 e 4 variáveis.
length(pontos_ocorrencia)/10 
length(pontos_ocorrencia)/15  

#A. Selecionar as variaveis usando um valor máximo de correlação de 0.6
vif_variaveis = vifcor(valores, th = 0.6)
vif_variaveis
# todas com VIF < 2
# maxima correlacao entre as varaiveis = -0.487
# minima correlacao entre as varaiveis = 0.171

#B. Selecionar as variaveis usando um valor máximo de VIF de 5
vif_variaveis2 = vifstep(valores, th = 3)
vif_variaveis2
# 5 variaveis com correlacao <0.5
# todas com VIF < 2
# maxima correlacao entre as varaiveis = 0.488

#C. Selecioanr as variaveis manualmente usando um valor máximo de VIF de 5
options(scipen = 999) #desligar numeros em notação cientifica
vif_variaveis3 = vifstep(valores, th=5)
vif_variaveis3

#Remover manualmente, mantendo a variavel bio6
names(valores)
valores2 = valores[, as.character(vif_variaveis3@results$Variables)]
valores2 = valores2[, -2]
names(valores2)

#Refazer os calculos e retirar até sobrar apenas variaveis com VIF < 5
vif_variaveis3 = vifstep(valores2, th = 5)
vif_variaveis3

valores2 = valores2[, -3]
names(valores2)

#correlation = 0.1773325 to -0.48752

####

#escolher um dos conjuntos de dados:
vif_variaveis
vif_variaveis2
vif_variaveis3

#salvar o resultado, no caso escolhi o primeiro conjunto
write.csv(as.data.frame(vif_variaveis@results), file = "./simonsi/Vif_variaveis_cormax.csv")
write.csv(as.data.frame(vif_variaveis2@results), file = "./simonsi/Vif_variaveis_vifmax.csv")
write.csv(as.data.frame(vif_variaveis3@results), file = "./simonsi/Vif_variaveis_vifmanual.csv")



####################### 5. SELECIONAR AS VARIAVEIS PELA PCA ########################
# fazer a PCA com os valores extraidos:
pca_variaveis = prcomp(valores, center=TRUE, scale=TRUE)
summary(pca_variaveis) #verificar a contribuicao dos PC

#A1. Selecionar o numero de PCs pela Broken Stick Model:
screeplot(pca_variaveis, bstick=TRUE, type="lines")
screeplot(pca_variaveis, bstick=TRUE, type="barplot") #3 PC
#Broken Stick Model: PC devem ser retidos contanto que o valor observado de autovalores (eigenvalues/ordination) sejam maiores que os valores esperados pelo modelo randômico do Broken Stick components. Jackson 1993 & Legendre & Legendre 2012
summary(pca_variaveis)
#3PCs are 87.94% of variance

#A2. Selecionar o numero de PCs que somem mais de 90% da variância explicada (Wang et al. 2016 - Study on selecting sensitive environmental variables in modelling species spatial distribution).
summary(pca_variaveis)
#4PCs are 93.79% of variance

#B2. Selecionar variavel com a maior contribução em cada PC selecionado
PC1_var = names(which.max(abs(pca_variaveis$rotation[,1])))
PC1_var
PC2_var = names(which.max(abs(pca_variaveis$rotation[,2])))
PC2_var
PC3_var = names(which.max(abs(pca_variaveis$rotation[,3])))
PC3_var
PC4_var = names(which.max(abs(pca_variaveis$rotation[,4])))
PC4_var
#PC5_var = names(which.max(abs(pca_variaveis$rotation[,5])))
#PC5_var


#salvar os resultados:
pca_A = c(PC1_var, PC2_var, PC3_var)
pca_A = vifcor(valores[, pca_A]) #max cor = -0.123 to -0.288
pca_A


pca_B = c(PC1_var, PC2_var, PC3_var,PC4_var)
pca_B = vifcor(valores[, pca_B]) #max cor = -0.123 to -0.464
pca_B

write.csv(as.data.frame(pca_A@results), file = "./Vif_variaveis_pcaA.csv")
write.csv(as.data.frame(pca_B@results), file = "./Vif_variaveis_pcaB.csv")


#C1. Selecionar as variaveis mais importantes para cada PC selecionado. São as variaveis que tem valor absoluto maior que 0.32 - o que seria referente a 10% da variacia do PC (Tabachnick and Fidell 1989):

#definir numero de PCs pelo BSR:
pc = 4
#definir os eixos principais
pca_rotation = abs(pca_variaveis$rotation) #don't matter the direction

lista = list()
for (i in 1:pc) {posicao = pca_rotation[pca_rotation[, i] > 0.32, ]
lista[[i]]=row.names(posicao)}

variaveis_sel = unlist(lista)
variaveis_sel = unique(variaveis_sel)
variaveis_sel #6 variaveis

pca_C = vif(valores[, variaveis_sel])
pca_C
pca_C = vif(valores[, as.vector(pca_C$Variables[-2])])
pca_C


#salvar os resultados
write.csv(as.data.frame(pca_C), file = "./Vif_variaveis_pcaC.csv")



#END
