####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPECIES (SDM)         ####
####               CONSTRUCAO DA MASCARA PARA A AREA DE ESTUDO                  ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVOS: CONSTRUCAO DA MASCARA PARA A AREA DE ESTUDO
#######               A. Baseado em um shapefile previo
#######               B. Baseado em uma area retangular
#######               C. Baseado em uma area circular/buffer





############# 1. INSTALACAO DOS PACOTES E CARREGAR AS FUNCOES AUXILIARES #########
# Carregar os pacotes necessarios:
library(rgdal)
library(raster)
library(maptools)
library(sp)
library(rgeos)
library(sampSurf)


#delimitar uma projecao espacial para lat/long e para UTM:
#Verificar a projeção 
rgdal::rgdal_extSoftVersion()

#Para GDAL >3 e PROJ >6
(longlat_WGS = CRS("+init=epsg:4326"))
cat(strwrap(gsub(",", ", ", (comment(longlat_WGS)))), sep="\n")
LL_prj

(UTM_prj = CRS("+init=epsg:5383"))
cat(strwrap(gsub(",", ", ", (comment(UTM_prj)))), sep="\n")
UTM_prj


#ou  GDAL <3 e PROJ <6
A = rgdal::make_EPSG() 
longlat_WGS = as.character(subset(A, A$code=="4326")[3]) 
UTM_prj = as.character(subset(A, A$code=="5383")[3])




####################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ########################
# Carregar os pontos de ocorrencia brutos:
pontos_brutos = read.csv("./pontos/psimonsi.csv")
head(pontos_brutos)
str(pontos_brutos)

#Projetar os pontos de long e lat:
plot(pontos_brutos$Long, pontos_brutos$Lat)

#salvar apenas long e lat como novo objeto SpatialPoints
names(pontos_brutos)
longlat = pontos_brutos[, c(3:4)]
coordinates(longlat) = ~Long+Lat #similar to SpatialPoints
crs(longlat) = longlat_WGS




############## 3. CRIAR MASCARAS A PARTIR DE UM SHAPEFILE EXISTENTE ################
# carregar o shapefile dos estados brasileiros
brasil = shapefile("./shapefiles/Brazil_Estados.shp")
#verificar se carregou corretamente
plot(brasil)
class(brasil)

#verificar a tabela de atributos do shapefile:
names(brasil@data) #nomes das colunas
length(brasil@data[,1]) #numero de linhas/poligonos independentes
head(brasil@data) # verificar o conteudo da tabela

#objetivo e criar uma mascara do estado do AM, AC e RO
#informacao sobre os estados está nessa coluna:
brasil@data$postal

#selecionar um estado ou atributo da tabela
AM = brasil[brasil$postal == "AM",]
plot(AM)

#selecionar todos os atributos exceto o Para
sempara = brasil[brasil$postal != "PA",]
plot(sempara)

#selecionar multiplos atributos
amazonia_ocidental = brasil[brasil$postal == "AM" | brasil$postal == "R.",] #RO e AC tem o mesmo codigo "R."
plot(amazonia_ocidental)

#necessario dissolver as divisas entre os estados. Para isso temos que escolher uma coluna de atributo que tenho o mesmo valor em todas as linhas
head(amazonia_ocidental@data)

#mais facil e pratico e criar uma coluna para dissolver com o mesmo valor:
amazonia_ocidental@data$dissolve = "A"

#dissolver utilizando como 'id' a coluna criada com os mesmos valores: 
amazonia_ocidental = gUnaryUnion(amazonia_ocidental, id = amazonia_ocidental$dissolve)
plot(amazonia_ocidental)
class(amazonia_ocidental)

#para salvar como shapefile o poligono tem que ser da classe "SpatialPolygonsDataFrame", converta-o:
amazonia_ocidental = as(amazonia_ocidental, "SpatialPolygonsDataFrame" )
crs(amazonia_ocidental) = longlat_WGS #acrescente a projecao
class(amazonia_ocidental)

#salvar na pasta 'mascaras':
writeOGR(amazonia_ocidental, "./mascaras/", "mascara_AM_AC_RO", driver="ESRI Shapefile", overwrite_layer = T)





#################### 4. CRIAR MASCARAS A PARTIR DE UM RETANGULO ####################
#Projetar os pontos de long e lat:
plot(pontos_brutos$Long, pontos_brutos$Lat)
#escolha os pontos extremos para representar a sua area de estudo. Para carajas, por exemplo
norte = -2
sul = -14
oeste = -75
leste = -62

#crie os vertices do retangulo:
x1 = c(oeste, norte)
x2 = c(oeste, sul)
x3 = c(leste, sul)
x4 = c(leste, norte)

#crie com os vertices uma matriz
coords = as.matrix(rbind(x1,x2,x3,x4))
coords #veficar

#transforme a matriz de pontos em um poligono
pol = Polygon(coords)
pols = Polygons(list(pol), ID="1")
polss = SpatialPolygons(list(pols))
polssD = as(polss, "SpatialPolygonsDataFrame" )
crs(polssD) = longlat_WGS #acrescente a projecao
class(polssD)

#projete o poligono e os pontos para ver se os pontos estão dentro do poligono
plot(polssD)
points(longlat$Long, longlat$Lat)

#nao e necessario dissolver e ele já esta na classe "SpatialPolygonsDataFrame"

#salvar na pasta 'mascaras':
writeOGR(polssD, "./mascaras/", "mascara_retangular", driver="ESRI Shapefile", overwrite_layer = T)





############ 5. CRIAR A MASCARA CIRCULAR A PARTIR DE UM PONTO CENTRAL ##############
#Projetar os pontos de long e lat:
plot(pontos_brutos$Long, pontos_brutos$Lat)
#escolher o ponto central em "centerPoint", spUnits está em graus decimais, se usar UTM estará em metros, o raio = 7 na projecao lat/long significa 7 grau decimal algo em torno de 800 Km (1 grau ~ 110 Km no Equador).
mascara_circular = spCircle(7, centerPoint=c(x=-70,y=-8), spUnits = longlat_WGS, spID='1')

# verificar
plot(mascara_circular$spCircle)
plot(mascara_circular$location, add=TRUE, pch=3)
points(longlat$Long, longlat$Lat)

#para salvar como shapefile o poligono tem que ser da classe "SpatialPolygonsDataFrame", converta-o:
mascara_circular = as(mascara_circular$spCircle, "SpatialPolygonsDataFrame" )
crs(mascara_circular) = longlat_WGS #acrescente a projecao
class(mascara_circular)

#salvar na pasta 'mascaras':
writeOGR(mascara_circular, "./mascaras/", "mascara_circular", driver="ESRI Shapefile",overwrite_layer = T)


##END
