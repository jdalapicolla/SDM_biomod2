####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPECIES (SDM)         ####
####                    ESCOLHA DOS PARAMETROS DO MAXENT                        ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVO: ESCOLHER QUAIS FEATURE CLASS E PARAMETRO DE REGULARIZAÇÃO SÃO IDEAIS PARA O NOSSO CONJUNTO DE DADOS






############### 1. INSTALACAO DOS PACOTES E CARREGAR AS FUNCOES AUXILIARES #########
# Carregar os pacotes necessarios:
#library(rJava)
library(dismo)
library(ENMeval)
library(plyr)
library(raster)
library(rgdal)
library(parallel)
library(randomForest)

## É necessário copiar o arquivo "maxent.jar" na pasta 'R/x86_64-pc-linux-gnu-library/3.6/dismo/java/'. Você já deve ter feito isso no passo 00B. Rode o comando abaixo para ver se você fez corretamente:
  jar = paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
  if (file.exists(jar) & require(rJava)){
    print("'maxent.jar' está na pasta correta")
  }else{
    print("'maxent.jar' precisa ser movida para a pasta dismo/java do R")
  }

#aumentar a memoria do Java no R
options(java.parameters = "-Xmx5g") #5GB



######################## 2. CARREGAR OS ARQUIVOS NECESSARIOS #######################
#carregar os pontos de ocorrencia corrigidos e filtrados, após o passo 5. O arquivo só pode ter as colunas lat e long
ocorrencia = read.csv("steerei/points/points_filtered_ste.csv")
head(ocorrencia)
length(ocorrencia[,1]) #47 pontos

#carregar as variaveis selecionadas no passo 6. Criar umas para separada para elas:
camadas = list.files(path="./steerei/vif_manual", pattern =".asc", full.names=TRUE)
camadas = stack(camadas)
camadas #verificar se as camadas foram carregadas corretamente

#criar 10 mil pontos de pseudoausência a partir da área de estudo (background pontos)
bg_pnts = randomPoints(camadas, n = 10000)




######################### 3. ESCOLHER OS PARÂMETROS DO RF ###########################
#extrair os valores dos pontos de ocorrência e dos pontos de fundo das camadas:
bg = raster::extract(camadas, bg_pnts)
bfc = raster::extract(camadas, ocorrencia)

#criar um único dataframe, 1 para pontos de presença, 0 para pontos de fundo.
d = rbind(cbind(pa=1, bfc), cbind(pa=0, bg))
d = data.frame(d)
dim(d)

# escolher o valor de mtry a partir do número de trees, maior que 1000
trf = tuneRF(d[, 2:ncol(d)], d[, 'pa'], ntreeTry=2000,
              stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)






######################### 4. RODAR A FUNCAO ENMevaluate  ###########################
#testar os seguintes parametros:
FC = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT") #feature class
RM = seq(0.5, 3, 0.5) #beta/regularizacao

#Valores aceitaveis para fc são: L=linear, Q=quadratic, P=product, T=threshold, e H=hinge
#4 FC e 4 valores de RM demoraram 5 min usando Maxent, e no Maxnet trava depois de 15 min.

#testar o numero de cores do computador:
detectCores()

#rodar a análise com maxent:
EVALme = ENMevaluate (ocorrencia,
                      camadas,
                      bg.coords = bg_pnts,
                      RMvalues = RM,
                      fc = FC,
                      algorithm = "maxent.jar",
                      method = "block",
                      clamp = FALSE,
                      overlap= FALSE,
                      rasterPreds = TRUE,
                      progbar  = TRUE,
                      updateProgress = TRUE,
                      parallel = TRUE,
                      numCores = 3)

#rodar a análise com maxnet. Não recomendavel em um computador basico:
EVALme = ENMevaluate (ocorrencia,
                      camadas,
                      bg.coords = bg_pnts,
                      RMvalues = RM,
                      fc = FC,
                      # algorithm = "maxent.jar",
                      method = "block",
                      clamp = FALSE,
                      overlap= FALSE,
                      rasterPreds = TRUE,
                      progbar  = TRUE,
                      parallel = TRUE,
                      numCores = 3)

#verificar os resultados:
EVALme

#salvar o objeto como Rdata/rds
saveRDS(EVALme, file = "./ParametrosMaxEnt_steerei_vifmanual.rds")

#carregar o arquivo RDS se for necessário
EVALme = readRDS("./ParametrosMaxEnt_steerei_vifmanual.rds")





#### 5. ESCOLHER OS MELHORES VALORES DE FEATURE CLASS AND REGULARIZACAO (BETA) #####
#Resultados da performance dos modelos:
EVALme@results

#organizar pelo valor de AICc
res = arrange(EVALme@results, EVALme@results$AICc)

#salvar e considerar apenas os melhores modelos = deltaAICc<2 e desconsiderar NA
best_models = res[res$delta.AICc<2  & !is.na(res$delta.AICc), ]
best_models

#save the table
write.csv(best_models, file="./Melhores_Parametros_MaxEnt_steerei_vifmanual.csv", row.names = F)


##END
