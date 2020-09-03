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

#delimitar uma projecao espacial para lat/long e para UTM:
longlat_WGS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
UTM_proj = CRS("+proj=utm +zone=22 +south +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#ou criar uma a partir de codigo EPGS
A = make_EPSG() 
LL_prj = as.character(subset(A, A$code=="4326")[3]) 
UTM_prj = as.character(subset(A, A$code=="5383")[3]) 





####################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ########################
# Carregar os pontos de ocorrencia:
pontos_brutos = read.csv("./pontos/psimonsi.csv")
head(pontos_brutos)
str(pontos_brutos)

# Carregar a camada ambiental em 'asc' ja editada para servir como modelo
variavel = raster("./asc_presente/wc2.1_30s_bio_1.asc")
crs(variavel) = longlat_WGS
variavel
plot(variavel)





########################## 3. REMOVER PONTOS DUPLICADOS ############################
#numero de pontos brutos:
length(pontos_brutos[, 1]) #148

#remover duplicados
pontos_unicos = pontos_brutos[!duplicated(pontos_brutos[c("Long","Lat")]), ]

#numero de pontos unicos:
length(pontos_unicos[, 1]) #55





##################### 4. REMOVER PONTOS FORA DA AREA DE ESTUDO #####################
#numero de pontos unicos:
length(pontos_unicos[, 1]) #55

#selecionar apenas as colunas de lat e long:
names(pontos_unicos)
ocorrencia = pontos_unicos[,3:4] # lon/lat columns
head(ocorrencia)
str(ocorrencia)

#adicionar uma projecao
coordinates(ocorrencia) = ~Long+Lat #nome das colunas.
crs(ocorrencia) =  longlat_WGS

#extrair os valores da camada
valores = extract(variavel, ocorrencia)
head(valores)

#achar as posições onde não há valor, é um ponto fora da camada. NoData nos arquivos '.asc' é normalmente '-9999', mas você pode alterar o valor
i = which(valores != "-9999")
i #lines in the point_raw

#update the points raw and create a SpatialPoints for ocorrence points.
pontos_unicos_area = pontos_unicos[i,]
#numero de pontos restantes:
length(pontos_unicos_area[, 1]) #51





####### 5. REMOVER VIES AMOSTRAL QUE PODE LEVAR A AUTOCORRELACAO ESPACIAL ##########
#transformar os pontos em SpatialPoints
names(pontos_unicos_area)
ocorrencia = pontos_unicos_area[,3:4] # lon/lat columns
coordinates(ocorrencia) = ~Long+Lat #nome das colunas.
crs(ocorrencia) = longlat_WGS

#criar um buffer de 10Km ao redor dos pontos
buffer = circles(ocorrencia, d = 10000, lonlat=TRUE) #d é o raio do circulo em metros
plot(buffer)
class(buffer)

#converter os círculos em polígonos
buffer = polygons(buffer)
#rasterizar os circulos
buffer= rasterize(buffer, variavel)

#selecionar 1 ponto por cada circulo
sel = gridSample(ocorrencia, buffer, n=1)

#verificar o numero de pontos restantes:
length(pontos_unicos_area[,1]) #51
length(sel[,1]) #47

#salvar os pontos de ocorrencia corrigidos
sel = as.data.frame(sel)
write.csv(sel, "./pontos/psimonsi_corrigido.csv", row.names = FALSE)


#END
