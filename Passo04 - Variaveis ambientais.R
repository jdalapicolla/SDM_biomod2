####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPECIES (SDM)         ####
####                   PADRONIZACAO DOS DADOS AMBIENTAIS                        ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVOS: PREPARAR AS VARIÁVEIS AMBIENTAIS PARA INPUT DOS MODELOS
#######               A. Cortar as rasters usando uma mascara
#######               B. Reprojetar as variaveis
#######               C. Converter os rasters para o formato '.asc'





############ 1. INSTALACAO DOS PACOTES E CARREGAR AS FUNCOES AUXILIARES #############
# Carregar os pacotes necessarios:
library(rgdal)
library(raster)

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


 



  

####################### 2. CARREGAR OS ARQUIVOS NECESSARIOS #########################
#carregar um raster para representar a resolução que você deseja, no caso escolhemos a 'bio1.tiff' com quadrículas de 1Km²:
exemplo_res = raster("./preditores_presente/wc2.1_30s_bio_1.tif")
crs(exemplo_res) = longlat_WGS

#carregar todas os arquivos rasters de uma pasta, selecionando pelo formato. Recursive = TRUE incluirá todas as subpastas, As camadas devem estar na mesma resolução. Mude o p"pattern" para o formato dos rasters
camadas = list.files(path="./preditores_presente/", pattern =".tif", full.names=TRUE)
camadas = stack(camadas)
#definir a projecao:
crs(camadas) = longlat_WGS
#Verificar
camadas

#carregar a mascara para representar a area de estudo:
mascara = shapefile("./mascaras/mascara_circular.shp")
#define a projection, the same one for all!!!!
crs(mascara) = longlat_WGS
plot(mascara)





################# 3. ALTERAR A RESOLUCAO DAS VARIAVEIS AMBIENTAIS ###################
#reduzir para a area de estudo a camada usada como exemplo para a resolucao. Isso reduzira o tempo das analises:
exemplo_res_red = crop(exemplo_res, extent(mascara), snap="out") #cria uma area retangular
plot(exemplo_res_red)

#Mudar a resolucao das variaveis ambientais. Essa etapa pode levar muito tempo dependendo da area amostrada.
camadas_res = resample(camadas, exemplo_res_red, method="bilinear", bylayer=TRUE, progress='text', snap="out")
camadas_res
plot(camadas_res[[1]])





############ 4. CORTAR AS VARIAVEIS AMBIENTAIS PARA A FORMA DA MASCARA ##############
#Clip the resampled layers with the study area:
camadas_res_mas = mask(camadas_res, mascara, bylayer=TRUE) #exactamente da forma da mascara
plot(camadas_res_mas[[1]])





######## 5. CONVERTER E SALVAR AS VARIAVEIS AMBIENTAIS PARA O FORMATO ASC ###########
#Salvar como .ascii em uma nova pasta
writeRaster(camadas_res_mas, paste0("./asc_presente/", paste0(names(camadas_res_mas),".asc")), driver='ascii', bylayer=TRUE)


##END
