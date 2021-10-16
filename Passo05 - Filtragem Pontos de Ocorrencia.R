####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPÉCIES (SDM)         ####
####        FILTRAGEM DOS PONTOS PARA REDUCAO DE AUTOCORRELACAO ESPACIAL        ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVOS: FILTRAR OS PONTOS DE OCORRENCIA PARA REDUZIR A AUTOCORRELACAO ESPACIAL
#######               A. Remover pontos duplicados
#######               B. Remover pontos fora da mascara
#######               C. Escolher um ponto de ocorrencia dentro de um buffer





########### 1. INSTALACAO DOS PACOTES E CARREGAR AS FUNCOES AUXILIARES #############
# Carregar os pacotes necessarios:
library(rgdal)
library(raster)
library(dismo)
library(rangeBuilder)


#delimitar uma projecao espacial para lat/long e para UTM:
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



####################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ########################
# Carregar os pontos de ocorrencia:
pontos_brutos = read.csv("./simonsi/points/points_raw_sim.csv")
head(pontos_brutos)
str(pontos_brutos)

# Carregar a camada ambiental em 'asc' ja editada para servir como modelo
variavel = raster("./asc_presente/annualPET.asc")
crs(variavel) = LL_prj
variavel
plot(variavel)





########################## 3. REMOVER PONTOS DUPLICADOS ############################
#numero de pontos brutos:
length(pontos_brutos[, 1]) #164

#remover duplicados
pontos_unicos = pontos_brutos[!duplicated(pontos_brutos[c("longitude","latitude")]), ]

#numero de pontos unicos:
length(pontos_unicos[, 1]) #60





##################### 4. REMOVER PONTOS FORA DA AREA DE ESTUDO #####################
#numero de pontos unicos:
length(pontos_unicos[, 1]) #60

#selecionar apenas as colunas de lat e long:
names(pontos_unicos)
ocorrencia = pontos_unicos[,2:3] # lon/lat columns
head(ocorrencia)
str(ocorrencia)

#adicionar uma projecao
coordinates(ocorrencia) = ~longitude+latitude #nome das colunas.
crs(ocorrencia) =  LL_prj

#extrair os valores da camada
valores = extract(variavel, ocorrencia)
head(valores)

#achar as posições onde não há valor, é um ponto fora da camada. NoData nos arquivos '.asc' é normalmente '-9999', mas você pode ser NA alterar o valor. testar os dois.
i = which(valores != "-9999")

i = which(valores != "NA")
i #lines in the point_raw

#update the points raw and create a SpatialPoints for ocorrence points.
pontos_unicos_area = pontos_unicos[i,]
#numero de pontos restantes:
length(pontos_unicos_area[, 1]) #56




####### 5. REMOVER VIES AMOSTRAL QUE PODE LEVAR A AUTOCORRELACAO ESPACIAL ##########
#transformar os pontos em SpatialPoints
names(pontos_unicos_area)
ocorrencia = pontos_unicos_area[,2:3] # lon/lat columns
coordinates(ocorrencia) = ~longitude+latitude #nome das colunas.


#selecionar pontos que tenham 10Km de distância de cada
sel = filterByProximity(ocorrencia@coords, dist = 10, mapUnits = FALSE, returnIndex = FALSE)

#verificar o numero de pontos restantes:
length(pontos_unicos_area[,1]) #56
length(sel[,1]) #41

#salvar os pontos de ocorrencia corrigidos
sel = as.data.frame(sel)
write.csv(sel, "./simonsi/points/points_filtered_sim.csv", row.names = FALSE)


#END
