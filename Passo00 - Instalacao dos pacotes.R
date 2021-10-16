####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPÉCIES (SDM)         ####
####          INSTALACAO AUTOMATICA DOS PACOTES E SOFTWARES NECESSARIOS         ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVOS: INSTALAR OS PACOTES DE R E BAIXAR O ARQUIVO DO MAXENT



#### 1. INSTALAÇÃO AUTOMATICA DOS PACOTES EM R NECESSÁRIOS ####
#executar os comandos abaixo para verificar se os pacotes já foram instalados. Se um pacote não foi instalado, ele será instalado automaticamente:
#Pacotes básicos para instalar outros pacotes:
if (!require('remotes'))      install.packages('remotes');           library('remotes')
if (!require('BiocManager'))  install.packages('BiocManager');       library('BiocManager')
if (!require('pacman'))       install.packages('pacman');            library('pacman')
if (!require('devtools'))     install.packages('devtools');          library('devtools')

#Pacotes do CRAN R:
if (!require('tidyverse'))     install.packages("tidyverse");        library('tidyverse')
if (!require('biomod2'))       install.packages("biomod2");          library('biomod2')
if (!require('dismo'))         install.packages("dismo");            library('dismo')
if (!require('ENMeval'))       install.packages("ENMeval");          library('ENMeval')
if (!require('maptools'))      install.packages("maptools");         library('maptools')
if (!require('plyr'))          install.packages("plyr");             library('plyr')
if (!require('rgdal'))         install.packages("rgdal");            library('rgdal')
if (!require('raster'))        install.packages("raster");           library('raster')
if (!require('sf'))            install.packages("sf");               library('sf')
if (!require('rgeos'))         install.packages('rgeos');            library('rgeos')
if (!require('rJava'))         install.packages('rJava');            library('rJava')
if (!require('sampSurf'))      install.packages('sampSurf');         library('sampSurf')
if (!require('sp'))            install.packages('sp');               library('sp')
if (!require('usdm'))          install.packages('usdm');             library('usdm')
if (!require('vegan'))         install.packages('vegan');            library('vegan')
if (!require('rangeBuilder'))  install.packages('rangeBuilder');     library('rangeBuilder')


#verificar se todos os pacotes estão careegando:
library(biomod2)
library(dismo)
library(ENMeval)
library(maptools)
library(parallel)
library(plyr)
library(raster)
library(rgdal)
library(rgeos)
library(rJava)
library(sampSurf)
library(sp)
library(tidyverse)
library(usdm)
library(vegan)
library(rangeBuilder)
library(sf)



##### 2. DOWNLOAD DO MAXENT ####
#A. Vá para a página: https://biodiversityinformatics.amnh.org/open_source/maxent/

#B. No retângulo "DOWNLOAD" há o botão "SUBMIT" e baixo dele a frase "I prefer to download without providing this information", clique nessa frase.

#C. Apacerá o botão "DOWNLOAD MAXENT", clique e espere o download terminar.

#D. Descompactar o arquivo "maxent.jar"

#E. É necessário copiar o arquivo "maxent.jar" para a pasta 'R/x86_64-pc-linux-gnu-library/3.6/dismo/java/', isto é dentro, da pasta do pacote 'dismo'. O endereço dessa pasta pode mudar de acordo com o sistema operacional e com a versao do R. Para achar o caminho para a pasta 'dismo', execute o comando:

system.file(package="dismo")

#F. Após copiar o arquivo para lá rode o comando abaixo para ver se você fez corretamente:
jar = paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar) & require(rJava)){
  print("'maxent.jar' está na pasta correta")
}else{
  print("'maxent.jar' precisa ser movido para a pasta dismo/java")
}

##END
