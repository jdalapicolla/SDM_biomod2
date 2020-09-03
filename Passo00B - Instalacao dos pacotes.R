####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPÉCIES (SDM)         ####
####          INSTALACAO AUTOMATICA DOS PACOTES E SOFTWARES NECESSARIOS         ####

#### Scripts by Jeronymo Dalapicolla and Jamille Viega ####

####### OBJETIVOS: INSTALAR OS PACOTES DE R E BAIXAR O ARQUIVO DO MAXENT



#### 1. INSTALAÇÃO AUTOMATICA DOS PACOTES EM R NECESSÁRIOS ####
#executar os comandos abaixo para verificar se os pacotes já foram instalados. Se um pacote não foi instalado, ele será instalado automaticamente:

if("devtools" %in% rownames(installed.packages()) == FALSE){install.packages("devtools")
} else {print (paste0("'devtools' já está instalado na biblioteca dessa versão do R"))}

if("biomod2" %in% rownames(installed.packages()) == FALSE){install.packages("biomod2")
} else {print (paste0("'biomod2' já está instalado na biblioteca dessa versão do R"))}

if("dismo" %in% rownames(installed.packages()) == FALSE){install.packages("dismo")
} else {print (paste0("'dismo' já está instalado na biblioteca dessa versão do R"))}

if("ENMeval" %in% rownames(installed.packages()) == FALSE){install.packages("ENMeval")
} else {print (paste0("'ENMeval' já está instalado na biblioteca dessa versão do R"))}

if("maptools" %in% rownames(installed.packages()) == FALSE){install.packages("maptools")
} else {print (paste0("'maptools' já está instalado na biblioteca dessa versão do R"))}

if("parallel" %in% rownames(installed.packages()) == FALSE){install.packages("parallel")
} else {print (paste0("'parallel' já está instalado na biblioteca dessa versão do R"))}

if("plyr" %in% rownames(installed.packages()) == FALSE){install.packages("plyr")
} else {print (paste0("'plyr' já está instalado na biblioteca dessa versão do R"))}

if("raster" %in% rownames(installed.packages()) == FALSE){install.packages("raster")
} else {print (paste0("'raster' já está instalado na biblioteca dessa versão do R"))}

if("rgdal" %in% rownames(installed.packages()) == FALSE){install.packages("rgdal")
} else {print (paste0("'rgdal' já está instalado na biblioteca dessa versão do R"))}

if("rgeos" %in% rownames(installed.packages()) == FALSE){install.packages("rgeos")
} else {print (paste0("'rgeos' já está instalado na biblioteca dessa versão do R"))}

if("rJava" %in% rownames(installed.packages()) == FALSE){install.packages("rJava")
} else {print (paste0("'rJava' já está instalado na biblioteca dessa versão do R"))}

if("sampSurf" %in% rownames(installed.packages()) == FALSE){install.packages("sampSurf")
} else {print (paste0("'sampSurf' já está instalado na biblioteca dessa versão do R"))}

if("sp" %in% rownames(installed.packages()) == FALSE){install.packages("sp")
} else {print (paste0("'sp' já está instalado na biblioteca dessa versão do R"))}

if("tidyverse" %in% rownames(installed.packages()) == FALSE){install.packages("tidyverse")
} else {print (paste0("'tidyverse' já está instalado na biblioteca dessa versão do R"))}

if("usdm" %in% rownames(installed.packages()) == FALSE){install.packages("usdm")
} else {print (paste0("'usdm' já está instalado na biblioteca dessa versão do R"))}

if("vegan" %in% rownames(installed.packages()) == FALSE){install.packages("vegan")
} else {print (paste0("'vegan' já está instalado na biblioteca dessa versão do R"))}



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
