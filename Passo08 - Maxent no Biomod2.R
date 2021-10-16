####        INSTITUTO TECNOLOGICO VALE - DESENVOLVIMENTO SUSTENTAVEL            ####
#### P0S-GRADUACAO EM USO SUSTENTAVEL DE RECURSOS NATURAIS EM REGIOES TROPICAIS ####

####          AULA PRATICA DE MODELOS DE DISTRIBUIÇÃO DE ESPECIES (SDM)         ####
####                    RODAR O MAXENT DENTRO DO BIOMOD2                        ####

#### Scripts by Jeronymo Dalapicolla ####

####### OBJETIVO: CRIAR OS MODELOS DE DISTRIBUICAO DE ESPÉCIES NO MAXENT DENTRO DO PACOTE BIOMOD2


############# 1. INSTALACAO DOS PACOTES E CARREGAR AS FUNCOES AUXILIARES ###########
# Carregar os pacotes necessarios:
library(tidyverse)
library(dismo)
library(maptools)
library(sp)
library(raster)
library(rgeos)
library(biomod2)

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




######################### 2. CARREGAR OS ARQUIVOS NECESSARIOS ######################
#carregar todas os arquivos rasters de uma pasta, selecionando pelo formato. Recursive = TRUE incluirá todas as subpastas, As camadas devem estar na mesma resolução. Mude o p"pattern" para o formato dos rasters
camadas = list.files(path="./steerei/vif_manual/", pattern =".asc", full.names=TRUE)
camadas = stack(camadas)
#definir a projecao:
crs(camadas) = LL_prj
#Verificar
camadas
#usar o primeiro arquivo .asc como modelo de nossa mascara
mascara = raster(camadas[1]) 

# Carregar os pontos de ocorrencia:
pontos_ocorrencia = read.csv("steerei/points/points_filtered_ste.csv")
head(pontos_ocorrencia)
str(pontos_ocorrencia)
#adicionar uma projecao
coordinates(pontos_ocorrencia) = ~longitude+latitude #nome das colunas.
crs(pontos_ocorrencia) = LL_prj

#Formatacao dos Dados para usar no biomod2
name = "steerei_vifmanual" #definir o nome para especie
expl = camadas #definir as camadas
xy = pontos_ocorrencia@coords #lon/lat
resp = rep(1, length(pontos_ocorrencia)) # a vector containing species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminate ) that will be used to build the species distribution models.





############### 3. DEFINIR OS PONTOS DE PSEUDOAUSÊNCIA PARA O MODELO ###############
#definir os pontos de background/pseudoausencia (PA), concentrar os pontos entre 20 e 500 km de distancia dos pontos de presenca
back = BIOMOD_FormatingData (resp.var=resp, #1 para presenca
                             expl.var=expl, # variaveis ambientais
                             resp.xy=xy, #coordenadas lon + lat
                             resp.name=name, #nome da especie
                             PA.nb.rep=1,	#numero de particoes de PA
                             PA.nb.absences=10000,	#n de PA's em cada particao
                             PA.strategy='disk',
                             PA.dist.min=10000,	#distancia minima para gerar PA's (em m)
                             PA.dist.max=500000,	#distancia m?xima para gerar PA's (em m)
                             na.rm=TRUE)

#ver os valores das camadas ambientaispara os pontos de PA
head(back@data.env.var)

#todos so pontos: PA + os de presenca
length(back@coord[,1]) #10041





######################## 4. DEFINIR OS PARAMETROS DO MAXENT ########################
#Mudar os parametros para rodar o MaxEnt. Basicamente o Feature Class e o RM do passo 7, o resto vamos deixar no default/padrão
#lembrar que colocar o MaxEnt na pasta que o R esta lendo
options = BIOMOD_ModelingOptions(
  MAXENT.Phillips = list(
    path_to_maxent.jar = getwd(), #deixar o arquivo maxente.jar na pasta de trabalho
    memory_allocated = NULL, #usar o determinado pelo rJava
    maximumiterations=5000, #numero de interacoes
    linear=T, #L
    quadratic=T, #Q
    product=F, #P
    threshold=F, #T
    hinge=F, #H
    lq2lqptthreshold=80,
    l2lqthreshold=10,
    hingethreshold=15,
    beta_threshold=-1,
    beta_categorical=-1,
    beta_lqp=-1,
    beta_hinge=-1,
    betamultiplier=0.5, #valor do RM 
    defaultprevalence=0.5))


########################### 5. RODAR O MODELO NO MAXENT ###########################
modelo_maxent = BIOMOD_Modeling(back,	#ponto de PA
                                models=c("MAXENT.Phillips"), #qual algoritmo usar
                                models.options = options, #parametros para o modelo
                                NbRunEval=50,	#numero de replicacoes
                                DataSplit=70,	#% de dados de presenca para treino/calibracao
                                Prevalence=0.5,
                                VarImport=100, #permutacoes para estimar a importancia das variaveis
                                models.eval.meth=c("TSS","ROC"),
                                SaveObj=TRUE,
                                rescal.all.models=TRUE,
                                do.full.models=FALSE,
                                modeling.id=paste(name))





############################ 6. VALIDACAO DOS MODELOS ##############################
#calcular os parametros de validacao
modelos_validacao = get_evaluations(modelo_maxent)
modelos_validacao

#A. Organizar os resultados do TSS
modelos_validacao["TSS","Testing.data",,,]
modelos_validacao["TSS","Sensitivity",,,]
modelos_validacao["TSS","Specificity",,,]
modelos_validacao["TSS","Cutoff",,,]

#criar uma tabela com os resultados de TSS para Maxent:
TSS_resultados_mx = as.data.frame(cbind(modelos_validacao["TSS","Testing.data",,,], modelos_validacao["TSS","Sensitivity",,,], modelos_validacao["TSS","Specificity",,,], modelos_validacao["TSS","Cutoff",,,]))

#mudar o nome das colunas
colnames(TSS_resultados_mx) = c("TSS", "Sensitivity", "Specificity", "Threshold")
head(TSS_resultados_mx)

#salvar os resultados
write.csv(TSS_resultados_mx, "./steerei_TSS_tabela.csv")



#B. Organizar os resultados do ROC/AUC
modelos_validacao["ROC","Testing.data",,,]
modelos_validacao["ROC","Sensitivity",,,]
modelos_validacao["ROC","Specificity",,,]
modelos_validacao["ROC","Cutoff",,,]

#criar uma tabela com os resultados de AUC para maxent:
AUC_resultados_mx = as.data.frame(cbind(modelos_validacao["ROC","Testing.data",,,], modelos_validacao["ROC","Sensitivity",,,], modelos_validacao["ROC","Specificity",,,], modelos_validacao["ROC","Cutoff",,,]))

#mudar o nome das colunas
colnames(AUC_resultados_mx) = c("AUC", "Sensitivity", "Specificity", "Threshold")
head(AUC_resultados_mx)

#salvar os resultados
write.csv(AUC_resultados_mx, "./steerei_AUC_tabela.csv")



##### 7. ESCOLHER O MELHOR MODELO/REPLICA PARA A PROJECAO NO ESPAÇO GEOGRÁFICO #####
#Escolher os melhores modelos:
#TSS > 0.4
#AUC > 0.75
#Sensitivity mais próximo de 100 melhor
#Specificity mais próximo de 100 melhor


#testar diferentes valores de TSS e AUC e verificar quantas réplicas/modelos permanecem. Pelo menos 20:
length(AUC_resultados_mx[which(AUC_resultados_mx[,1] > 0.7), ][,1]) #21 models
length(TSS_resultados_mx[which(TSS_resultados_mx[,1] > 0.4), ][,1]) #20


#selecionar os 30 melhores modelos em cada métrica de acurácia
run_names_tss_mx = rownames(arrange(TSS_resultados_mx, -TSS_resultados_mx[,1]))[1:20] #20 models
run_names_auc_mx = rownames(arrange(AUC_resultados_mx, -AUC_resultados_mx[,1]))[1:20] #20 models


#transformar as strings em posições das réplicas:
posicao_modelos_tss_mx = as.numeric(str_remove(run_names_tss_mx,"RUN"))
posicao_modelos_auc_mx = as.numeric(str_remove(run_names_auc_mx,"RUN"))

#comparar os melhores modelos em cada métrica
posicao_modelos_tss_mx
posicao_modelos_auc_mx

#ver a diferenças:
setdiff(posicao_modelos_tss_mx, posicao_modelos_auc_mx)
setdiff(posicao_modelos_auc_mx, posicao_modelos_tss_mx)
#4 modelos de 30 modelos, cerca de 8% de diferenças

identical(posicao_modelos_tss_mx, posicao_modelos_auc_mx)
identical(posicao_modelos_auc_mx, posicao_modelos_tss_mx)


#selecionar o nome dos melhores modelos e do melhor modelo segundo uma das duas métricas, fazer para as duas é uma opção, mas como é feito um modelo médio, a diferença é pequena entre os dois:
#No caso TSS tem menos problemas que AUC:
#verificar os nomes
modelo_maxent@models.computed

#nome dos melhores modelos
melhores_modelos_auc = modelo_maxent@models.computed[posicao_modelos_auc_mx]
melhores_modelos_tss = modelo_maxent@models.computed[posicao_modelos_tss_mx]
#nome do melhor modelo que no meu caso foi o numero 5
melhor_modelo_auc = modelo_maxent@models.computed[posicao_modelos_auc_mx[1]]
melhor_modelo_tss = modelo_maxent@models.computed[posicao_modelos_tss_mx[1]]

#são os mesmos modelos. Escolher um modelos:
melhor_modelo_auc
melhor_modelo_tss




################### 8. CALCULAR A IMPORTANCIA DAS VARIAVEIS  #######################
#calcular a importancia e criar uma tabela com os resultados, sempre com o TSS:
importancia_vars = t(as.data.frame(get_variables_importance(modelo_maxent)))
head(importancia_vars)

#salvar os resultados de todos os modelos
write.csv(importancia_vars, "./steerei_importancia_variaveis_mx.csv")

#selecionar a importancia considerando só os melhores modelos pelo TSS
importa_melhores = importancia_vars[posicao_modelos_tss_mx, ]

#calcular a media de importancia
media = c(mean(importa_melhores[,1]), mean(importa_melhores[,2]), mean(importa_melhores[,3]), mean(importa_melhores[,4])) #numero de variaveis

#adicionar a media como ultima linha:
importa_melhores_media = rbind(importa_melhores, media)
head(importa_melhores_media)
tail(importa_melhores_media)

#salvar os resultados só dos melhores modelos
write.csv(importa_melhores_media, "./steerei_importancia_variaveis_melhores_tss_mx.csv")




#visualizar a importancia de todos os modelos em um gráfico:
ggplot(gather(as.data.frame(importancia_vars)),aes(x = reorder(key, value, fun = median,), y = value)) + 
  geom_boxplot() + 
  scale_x_discrete(name="Variáveis")+
  scale_y_continuous(name="Importância (%)")+
  theme_bw(base_size = 14)

#visualizar a importancia apenas dos melhores modelos em um gráfico:
ggplot(gather(as.data.frame(importa_melhores)),aes(x = reorder(key, value, fun = median,), y = value)) + 
  geom_boxplot() + 
  scale_x_discrete(name="Variáveis")+
  scale_y_continuous(name="Importância (%)")+
  theme_bw(base_size = 14)





############# 9. CURVA DE RESPOSTA DAS VARIAVEIS PARA OS MELHORES MODELOS ##########
#carregar todas os modelos/replicas como objetos
meus_modelos = BIOMOD_LoadModels(modelo_maxent, models = "MAXENT.Phillips")
#verificar
meus_modelos

#criar as curvas de resposta para os melhores modelos para auc:
curvas_melhores_modelos =
  response.plot2(
    models = meus_modelos[posicao_modelos_tss_mx], # escolher os melhores modelos pelo tss 
    Data = get_formal_data(modelo_maxent, 'expl.var') , #RasterStack com as variaveis
    show.variables = get_formal_data(modelo_maxent, 'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = "mean",
    save.file = "pdf", #formato do arquivo de imagem
    name = "./response_curve_steerei_melhores_modelos_tss_mx", #nome do modelo
    ImageSize = 480, #resolucao da imagem
   # col = c("blue", "red", "black", "gray"), #cores para as curvas de acordo com o número de modelos
    legend = TRUE,
    data_species = get_formal_data(modelo_maxent, 'resp.var'))

#criar as curvas de resposta para o melhor modelo:
curvas_melhor =
  response.plot2(
    models = melhor_modelo_tss, # escolher o melhor modelo 
    Data = get_formal_data(modelo_maxent, 'expl.var') , #RasterStack com as variaveis
    show.variables = get_formal_data(modelo_maxent, 'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = "mean",
    save.file = "pdf", #formato do arquivo de imagem
    name = "./response_curve_steerei_melhor_modelo_tss_mx", #nome do modelo
    ImageSize = 600, #resolucao da imagem
    col = c("blue"), #cores para a curva
    legend = TRUE,
    data_species = get_formal_data(modelo_maxent, 'resp.var'))





###################### 10. PROJETAR PARA UMA ÁREA GEOGRÁFICA ######################
#Carregar um modelo anterior:
(bm_out_file = load("./steerei.vifmanual/steerei.vifmanual.steerei_vifmanual.models.out"))
ls()
my_bm_model = get(bm_out_file)
rm(list = c(bm_out_file, 'bm_out_file'))
ls()

modelo_maxent = my_bm_model


#carregar as camadas da projeção:
camadas_proj = list.files(path="./steerei/vif_manual_full/", pattern =".asc", full.names=TRUE)
camadas_proj = stack(camadas_proj)
#definir a projecao:
crs(camadas_proj) = LL_prj
#Verificar
camadas_proj

camadas_proj = expl # projetar para a mesma area que os modelos foram construidos. Se for para o passado, futuro ou outras areas (espécie invasoras) colocar aqui as variaveis (tem que ser variaveis com o mesmo nome e resolucao que foram usadas na construção dos modelos)

proj_present = BIOMOD_Projection(modeling.output=modelo_maxent,
                                 new.env=camadas_proj,
                                 proj.name='current',
                                 selected.models= melhores_modelos_tss, #selecionar os modelos bons
                                 compress=FALSE,
                                 build.clamping.mask=FALSE,
                                 output.format='.img',
                                 do.stack=TRUE)

#verificar os modelos
proj_present
plot(proj_present)

#transformar as projecões em raster 
rasters_presente = get_predictions(proj_present)
plot(rasters_presente[[1]])

#salvar um modelo específico
writeRaster(rasters_presente[[1]], filename="./steerei_melhor_modelo.asc", format="ascii")

#fazer um modelo médio de todas as projeções
raster_medio =  calc(rasters_presente, fun=mean)
#salvar o modelo medio
writeRaster(raster_medio, filename="./steerei_modelo_medio.asc", format="ascii")





########## 11. CONSTRUIR O MAPA FINAL CORTADO PELO THRESHOLD/LIMIAR ################
#fazer um Mapa Binario (presenca/ausencia) a partir de um valor limiar/threshold e do mapa médio:
#para achar o threshold ótimo de um modelo usamos a função 'Find.Optim.Stat", mas ele já é calculado automaticamnte pela função get_evaluations, no passo #6 desse script. Precisamos apenas calcular apenas o valor médio dos melhores modelos:

#selecionar os limiares/threshold dos melhores modelos:
limiares = as.data.frame(TSS_resultados_mx[posicao_modelos_tss_mx, ][4])
#calcular o threshold medio:
liminar_medio = mean(limiares$Threshold)
liminar_medio

#criar o mapa binario. Pixels acima do valor do limiar terão valor 1, e abaixo, valor 0.
mapa_binario = BinaryTransformation(raster_medio, liminar_medio)

#salvar o mapa binario
writeRaster(mapa_binario, filename="./Resultados/gperuanus_mapa_binario_mx.asc", format="ascii", overwrite=TRUE)

#Para criar o mapa final tem que multiplicar o mapa binario pelo mapa médio dos melhores modelos.
raster_final = raster_medio*mapa_binario
raster_final

#salvar o mapa final
writeRaster(raster_final, filename="./Resultados/gperuanus_mapa_final_mx.asc", format="ascii")





######## 12. RECLASSIFICAR O MAPA FINAL E ESTIMAR A ÁREA ADEQUADA EM KM² ##########
#definir a area  em Km² da célula/pixel:
celula = 1

#Calcular a área adequada:
area_adequada = as.data.frame(tapply(area(mapa_binario), mapa_binario[], sum)*celula)
rownames(area_adequada)  =c("Não-adequada", "Adequada")
colnames(area_adequada) = c("Área (Km²)")
area_adequada

#salvar os resultados só dos melhores modelos
write.csv(area_adequada, "./Resultados/gperuanus_area_adequada_mx.csv")


#Calcular a área adequada por classes:
#A) Propor uma divisão de classes. Saber os valores máximo e mininos
raster_final #0 e 1000

#B) Propor a divisao em classes:
#DE 0 AO liminar = CLASSE 0 (INADEQUADA)
#DE liminar AO 750 = CLASSE 1 (MÉDIA)
#DE 750 AO 900 = CLASSE 2 (ALTA)
#DE 900 AO 1000 = CLASSE 3 (MUITO ALTA)

#C) Criar uma data.frame (df) de reclassificação de acordo com a proposta:
reclass_df = c(0, liminar_medio, 0,
               liminar_medio, 750, 1,
               750, 900, 2,
               900, 1000, 3)
#verificar
reclass_df

#D) Converter o df em uma matriz:
reclass_m = matrix(reclass_df,
                   ncol = 3,
                   byrow = TRUE)
#verificar
reclass_m

#E) Criar o raster reclassificado a partir da matriz:
raster_classificado = reclassify(raster_final,reclass_m)

#F) Calcular a área adequada por classes:
area_adequada_classes = as.data.frame(tapply(area(raster_classificado), raster_classificado[], sum)*celula)
rownames(area_adequada_classes)  =c("Não-adequada", "Média", "Alta", "Muito Alta")
colnames(area_adequada_classes) = c("Área (Km²)")
area_adequada_classes

#salvar os resultados só dos melhores modelos
write.csv(area_adequada_classes, "./Resultados/gperuanus_area_adequada_classes_mx.csv")

#salvar o raster reclassificado
writeRaster(raster_classificado, filename="./Resultados/gperuanus_mapa_final_reclassificado_mx.asc", format="ascii")










######################## 13. DEFINIR OS PARAMETROS DO RANDOM FOREST ########################
#Mudar os parametros para rodar o MaxEnt. Basicamente o Feature Class e o RM do passo 7, o resto vamos deixar no default/padrão
#lembrar que colocar o MaxEnt na pasta que o R esta lendo
options = BIOMOD_ModelingOptions(
 RF = list(
    do.classif=TRUE,
    ntree=1000, #more accurate predictions are likely to be obtained by choosing a large number of trees. It is interesting to note that picking a large ntree does not lead to overfitting, since finite forests converge to infinite ones (Breiman, 2001). Recomend more than 1,000.
    #mtry= default, #Dıaz-Uriarte and de Andres (2006), who show that this parameter has a little impact on the performance of the method, though larger values may be associated with a reduction in the predictive performance. On the other hand, Genuer et al. (2010) claim that the default value of mtry is either optimal or too small. Therefore, a conservative approach is to take mtry as large as possible (limited by available computing resources). default for regrission is p\3 where p is predictors
    nodesize=5, #1 for classification and 5 for regression
    maxnodes=NULL))


########################### 5. RODAR O MODELO NO MAXENT ###########################
modelo_maxent = BIOMOD_Modeling(back,	#ponto de PA
                                models=c("RF"), #qual algoritmo usar
                                models.options = options, #parametros para o modelo
                                NbRunEval=50,	#numero de replicacoes
                                DataSplit=70,	#% de dados de presenca para treino/calibracao
                                Prevalence=0.5,
                                VarImport=100, #permutacoes para estimar a importancia das variaveis
                                models.eval.meth=c("TSS","ROC"),
                                SaveObj=TRUE,
                                rescal.all.models=TRUE,
                                do.full.models=FALSE,
                                modeling.id=paste(name))



############################ 6. VALIDACAO DOS MODELOS ##############################
#calcular os parametros de validacao
modelos_validacao = get_evaluations(modelo_maxent)
modelos_validacao

#A. Organizar os resultados do TSS
modelos_validacao["TSS","Testing.data",,,]
modelos_validacao["TSS","Sensitivity",,,]
modelos_validacao["TSS","Specificity",,,]
modelos_validacao["TSS","Cutoff",,,]

#criar uma tabela com os resultados de TSS para Maxent:
TSS_resultados_rf = as.data.frame(cbind(modelos_validacao["TSS","Testing.data",,,], modelos_validacao["TSS","Sensitivity",,,], modelos_validacao["TSS","Specificity",,,], modelos_validacao["TSS","Cutoff",,,]))

#mudar o nome das colunas
colnames(TSS_resultados_rf) = c("TSS", "Sensitivity", "Specificity", "Threshold")
head(TSS_resultados_rf)

#salvar os resultados
write.csv(TSS_resultados_rf, "./Resultados/gperuanus_TSS_tabela_rf.csv")



#B. Organizar os resultados do ROC/AUC
modelos_validacao["ROC","Testing.data",,,]
modelos_validacao["ROC","Sensitivity",,,]
modelos_validacao["ROC","Specificity",,,]
modelos_validacao["ROC","Cutoff",,,]

#criar uma tabela com os resultados de AUC para maxent:
AUC_resultados_rf = as.data.frame(cbind(modelos_validacao["ROC","Testing.data",,,], modelos_validacao["ROC","Sensitivity",,,], modelos_validacao["ROC","Specificity",,,], modelos_validacao["ROC","Cutoff",,,]))

#mudar o nome das colunas
colnames(AUC_resultados_rf) = c("AUC", "Sensitivity", "Specificity", "Threshold")
head(AUC_resultados_rf)

#salvar os resultados
write.csv(AUC_resultados_rf, "./Resultados/gperuanus_AUC_tabela_rf.csv")



##### 7. ESCOLHER O MELHOR MODELO/REPLICA PARA A PROJECAO NO ESPAÇO GEOGRÁFICO #####
#Escolher os melhores modelos:
#TSS > 0.4
#AUC > 0.75
#Sensitivity mais próximo de 100 melhor
#Specificity mais próximo de 100 melhor


#testar diferentes valores de TSS e AUC e verificar quantas réplicas/modelos permanecem. Pelo menos 20:
length(AUC_resultados_rf[which(AUC_resultados_rf[,1] > 0.7), ][,1]) #49 models
length(TSS_resultados_rf[which(TSS_resultados_rf[,1] > 0.4), ][,1]) #50


#selecionar os 30 melhores modelos em cada métrica de acurácia
run_names_tss_rf = rownames(arrange(TSS_resultados_rf, -TSS_resultados_rf[,1]))[1:30] #30 models
run_names_auc_rf = rownames(arrange(AUC_resultados_rf, -AUC_resultados_rf[,1]))[1:30] #30 models


#transformar as strings em posições das réplicas:
posicao_modelos_tss_rf = as.numeric(str_remove(run_names_tss_rf,"RUN"))
posicao_modelos_auc_rf = as.numeric(str_remove(run_names_auc_rf,"RUN"))

#comparar os melhores modelos em cada métrica
posicao_modelos_tss_rf
posicao_modelos_auc_rf

#ver a diferenças:
setdiff(posicao_modelos_tss_rf, posicao_modelos_auc_rf)
setdiff(posicao_modelos_auc_rf, posicao_modelos_tss_rf)
#4 modelos de 30 modelos, cerca de 8% de diferenças

identical(posicao_modelos_tss_mx, posicao_modelos_auc_rf)
identical(posicao_modelos_auc_mx, posicao_modelos_tss_rf)


#selecionar o nome dos melhores modelos e do melhor modelo segundo uma das duas métricas, fazer para as duas é uma opção, mas como é feito um modelo médio, a diferença é pequena entre os dois:
#No caso TSS tem menos problemas que AUC:
#verificar os nomes
modelo_maxent@models.computed

#nome dos melhores modelos
melhores_modelos_auc = modelo_maxent@models.computed[posicao_modelos_auc_rf]
melhores_modelos_tss = modelo_maxent@models.computed[posicao_modelos_tss_rf]
#nome do melhor modelo que no meu caso foi o numero 5
melhor_modelo_auc = modelo_maxent@models.computed[posicao_modelos_auc_rf[1]]
melhor_modelo_tss = modelo_maxent@models.computed[posicao_modelos_tss_rf[1]]

#são os mesmos modelos. Escolher um modelos:
melhor_modelo_auc
melhor_modelo_tss




################### 8. CALCULAR A IMPORTANCIA DAS VARIAVEIS  #######################
#calcular a importancia e criar uma tabela com os resultados, sempre com o TSS:
importancia_vars = t(as.data.frame(get_variables_importance(modelo_maxent)))
head(importancia_vars)

#salvar os resultados de todos os modelos
write.csv(importancia_vars, "./Resultados/gperuanus_importancia_variaveis_rf.csv")

#selecionar a importancia considerando só os melhores modelos pelo TSS
importa_melhores = importancia_vars[posicao_modelos_tss_rf, ]

#calcular a media de importancia
media = c(mean(importa_melhores[,1]), mean(importa_melhores[,2]), mean(importa_melhores[,3])) #numero de variaveis

#adicionar a media como ultima linha:
importa_melhores_media = rbind(importa_melhores, media)
head(importa_melhores_media)
tail(importa_melhores_media)

#salvar os resultados só dos melhores modelos
write.csv(importa_melhores_media, "./Resultados/gperuanus_importancia_variaveis_melhores_tss_rf.csv")




#visualizar a importancia de todos os modelos em um gráfico:
ggplot(gather(as.data.frame(importancia_vars)),aes(x = reorder(key, value, fun = median,), y = value)) + 
  geom_boxplot() + 
  scale_x_discrete(name="Variáveis")+
  scale_y_continuous(name="Importância (%)")+
  theme_bw(base_size = 14)

#visualizar a importancia apenas dos melhores modelos em um gráfico:
ggplot(gather(as.data.frame(importa_melhores)),aes(x = reorder(key, value, fun = median,), y = value)) + 
  geom_boxplot() + 
  scale_x_discrete(name="Variáveis")+
  scale_y_continuous(name="Importância (%)")+
  theme_bw(base_size = 14)





############# 9. CURVA DE RESPOSTA DAS VARIAVEIS PARA OS MELHORES MODELOS ##########
#carregar todas os modelos/replicas como objetos
meus_modelos = BIOMOD_LoadModels(modelo_maxent, models = "RF")
#verificar
meus_modelos

#criar as curvas de resposta para os melhores modelos para auc:
curvas_melhores_modelos =
  response.plot2(
    models = meus_modelos[posicao_modelos_tss_mx], # escolher os melhores modelos pelo tss 
    Data = get_formal_data(modelo_maxent, 'expl.var') , #RasterStack com as variaveis
    show.variables = get_formal_data(modelo_maxent, 'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = "mean",
    save.file = "pdf", #formato do arquivo de imagem
    name = "./Resultados/response_curve_gperuanus_melhores_modelos_tss_rf", #nome do modelo
    ImageSize = 480, #resolucao da imagem
    # col = c("blue", "red", "black", "gray"), #cores para as curvas de acordo com o número de modelos
    legend = TRUE,
    data_species = get_formal_data(modelo_maxent, 'resp.var'))

#criar as curvas de resposta para o melhor modelo:
curvas_melhor =
  response.plot2(
    models = melhor_modelo_tss, # escolher o melhor modelo 
    Data = get_formal_data(modelo_maxent, 'expl.var') , #RasterStack com as variaveis
    show.variables = get_formal_data(modelo_maxent, 'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = "mean",
    save.file = "pdf", #formato do arquivo de imagem
    name = "./Resultados/response_curve_gperuanus_melhor_modelo_tss_rf", #nome do modelo
    ImageSize = 600, #resolucao da imagem
    col = c("blue"), #cores para a curva
    legend = TRUE,
    data_species = get_formal_data(modelo_maxent, 'resp.var'))





###################### 10. PROJETAR PARA UMA ÁREA GEOGRÁFICA ######################
camadas_proj = expl # projetar para a mesma area que os modelos foram construidos. Se for para o passado, futuro ou outras areas (espécie invasoras) colocar aqui as variaveis (tem que ser variaveis com o mesmo nome e resolucao que foram usadas na construção dos modelos)

proj_present = BIOMOD_Projection(modeling.output=modelo_maxent,
                                 new.env=camadas_proj,
                                 proj.name='current',
                                 selected.models= melhores_modelos_tss, #selecionar os modelos bons
                                 compress=FALSE,
                                 build.clamping.mask=FALSE,
                                 output.format='.img',
                                 do.stack=TRUE)

#verificar os modelos
proj_present
plot(proj_present)

#transformar as projecões em raster 
rasters_presente = get_predictions(proj_present)
plot(rasters_presente[[1]])

#salvar um modelo específico
writeRaster(rasters_presente[[1]], filename="./Resultados/gperuanus_melhor_modelo_rf.asc", format="ascii")

#fazer um modelo médio de todas as projeções
raster_medio =  calc(rasters_presente, fun=mean)
#salvar o modelo medio
writeRaster(raster_medio, filename="./Resultados/gperuanus_modelo_medio_rf.asc", format="ascii")





########## 11. CONSTRUIR O MAPA FINAL CORTADO PELO THRESHOLD/LIMIAR ################
#fazer um Mapa Binario (presenca/ausencia) a partir de um valor limiar/threshold e do mapa médio:
#para achar o threshold ótimo de um modelo usamos a função 'Find.Optim.Stat", mas ele já é calculado automaticamnte pela função get_evaluations, no passo #6 desse script. Precisamos apenas calcular apenas o valor médio dos melhores modelos:

#selecionar os limiares/threshold dos melhores modelos:
limiares = as.data.frame(TSS_resultados_mx[posicao_modelos_tss_mx, ][4])
#calcular o threshold medio:
liminar_medio = mean(limiares$Threshold)
liminar_medio

#criar o mapa binario. Pixels acima do valor do limiar terão valor 1, e abaixo, valor 0.
mapa_binario = BinaryTransformation(raster_medio, liminar_medio)

#salvar o mapa binario
writeRaster(mapa_binario, filename="./Resultados/gperuanus_mapa_binario_rf.asc", format="ascii", overwrite=TRUE)

#Para criar o mapa final tem que multiplicar o mapa binario pelo mapa médio dos melhores modelos.
raster_final = raster_medio*mapa_binario
raster_final

#salvar o mapa final
writeRaster(raster_final, filename="./Resultados/gperuanus_mapa_final_rf.asc", format="ascii")





######## 12. RECLASSIFICAR O MAPA FINAL E ESTIMAR A ÁREA ADEQUADA EM KM² ##########
#definir a area  em Km² da célula/pixel:
celula = 1

#Calcular a área adequada:
area_adequada = as.data.frame(tapply(area(mapa_binario), mapa_binario[], sum)*celula)
rownames(area_adequada)  =c("Não-adequada", "Adequada")
colnames(area_adequada) = c("Área (Km²)")
area_adequada

#salvar os resultados só dos melhores modelos
write.csv(area_adequada, "./Resultados/gperuanus_area_adequada_rf.csv")


#Calcular a área adequada por classes:
#A) Propor uma divisão de classes. Saber os valores máximo e mininos
raster_final #0 e 1000

#B) Propor a divisao em classes:
#DE 0 AO liminar = CLASSE 0 (INADEQUADA)
#DE liminar AO 750 = CLASSE 1 (MÉDIA)
#DE 750 AO 900 = CLASSE 2 (ALTA)
#DE 900 AO 1000 = CLASSE 3 (MUITO ALTA)

#C) Criar uma data.frame (df) de reclassificação de acordo com a proposta:
reclass_df = c(0, liminar_medio, 0,
               liminar_medio, 750, 1,
               750, 900, 2,
               900, 1000, 3)
#verificar
reclass_df

#D) Converter o df em uma matriz:
reclass_m = matrix(reclass_df,
                   ncol = 3,
                   byrow = TRUE)
#verificar
reclass_m

#E) Criar o raster reclassificado a partir da matriz:
raster_classificado = reclassify(raster_final,reclass_m)

#F) Calcular a área adequada por classes:
area_adequada_classes = as.data.frame(tapply(area(raster_classificado), raster_classificado[], sum)*celula)
rownames(area_adequada_classes)  =c("Não-adequada", "Média", "Alta", "Muito Alta")
colnames(area_adequada_classes) = c("Área (Km²)")
area_adequada_classes

#salvar os resultados só dos melhores modelos
write.csv(area_adequada_classes, "./Resultados/gperuanus_area_adequada_classes_rf.csv")

#salvar o raster reclassificado
writeRaster(raster_classificado, filename="./Resultados/gperuanus_mapa_final_reclassificado_rf.asc", format="ascii")


#END;;

#raster maxent
raster_mx = raster("./Resultados/VifManual_MX/gperuanus_mapa_final.asc")
plot(raster_mx)

raster_rf = raster("./Resultados/VifManual_RF/gperuanus_mapa_final_rf.asc")
plot(raster_rf)


#binario do desmatamento
loss2019 = raster("./asc_presente/not/LossCover2019.asc")
loss2019_binario = BinaryTransformation(loss2019, 1)
plot(loss2019_binario)

#inverter os valores
loss2019_binario[loss2019_binario == 0] = 2
loss2019_binario[loss2019_binario == 1] = 0
loss2019_binario[loss2019_binario == 2] = 1
plot(loss2019_binario)

#Para criar o mapa final tem que multiplicar o mapa binario pelo mapa médio dos melhores modelos.
raster_final_mx = raster_mx*loss2019_binario
raster_final_mx
plot(raster_final_mx)

raster_final_rf = raster_rf*loss2019_binario
raster_final_rf
plot(raster_final_rf)

writeRaster(raster_final_mx, filename="./Resultados/gperuanus_mapa_final_loss2019_mx.asc", format="ascii")
writeRaster(raster_final_rf, filename="./Resultados/gperuanus_mapa_final_loss2019_rf.asc", format="ascii")





#Calcular a área adequada por classes:
#A) Propor uma divisão de classes. Saber os valores máximo e mininos
raster_final_mx #0 e 1000
raster_final_rf


#B) Propor a divisao em classes:
#DE 0 AO liminar = CLASSE 0 (INADEQUADA)
#DE liminar AO 750 = CLASSE 1 (MÉDIA)
#DE 750 AO 900 = CLASSE 2 (ALTA)
#DE 900 AO 1000 = CLASSE 3 (MUITO ALTA)

liminar_medio = 603.4

#C) Criar uma data.frame (df) de reclassificação de acordo com a proposta:
reclass_df = c(0, liminar_medio, 0,
               liminar_medio, 750, 1,
               750, 900, 2,
               900, 1000, 3)
#verificar
reclass_df

#D) Converter o df em uma matriz:
reclass_m = matrix(reclass_df,
                   ncol = 3,
                   byrow = TRUE)
#verificar
reclass_m

#E) Criar o raster reclassificado a partir da matriz:
raster_classificado = reclassify(raster_final_mx,reclass_m)

#F) Calcular a área adequada por classes:
area_adequada_classes = as.data.frame(tapply(area(raster_classificado), raster_classificado[], sum)*celula)
rownames(area_adequada_classes)  =c("Não-adequada", "Média", "Alta", "Muito Alta")
colnames(area_adequada_classes) = c("Área (Km²)")
area_adequada_classes

#salvar os resultados só dos melhores modelos
write.csv(area_adequada_classes, "./Resultados/gperuanus_area_adequada_classes_loss2019_mx.csv")

#salvar o raster reclassificado
writeRaster(raster_classificado, filename="./Resultados/gperuanus_mapa_final_reclassificado_mx_loss2019.asc", format="ascii")


#B) Propor a divisao em classes:
#DE 0 AO liminar = CLASSE 0 (INADEQUADA)
#DE liminar AO 750 = CLASSE 1 (MÉDIA)
#DE 750 AO 900 = CLASSE 2 (ALTA)
#DE 900 AO 1000 = CLASSE 3 (MUITO ALTA)

liminar_medio = 215.53

#C) Criar uma data.frame (df) de reclassificação de acordo com a proposta:
reclass_df = c(0, liminar_medio, 0,
               liminar_medio, 750, 1,
               750, 900, 2,
               900, 1000, 3)
#verificar
reclass_df

#D) Converter o df em uma matriz:
reclass_m = matrix(reclass_df,
                   ncol = 3,
                   byrow = TRUE)
#verificar
reclass_m

#E) Criar o raster reclassificado a partir da matriz:
raster_classificado = reclassify(raster_final_rf,reclass_m)

#F) Calcular a área adequada por classes:
area_adequada_classes = as.data.frame(tapply(area(raster_classificado), raster_classificado[], sum)*celula)
rownames(area_adequada_classes)  =c("Não-adequada", "Média", "Alta", "Muito Alta")
colnames(area_adequada_classes) = c("Área (Km²)")
area_adequada_classes

#salvar os resultados só dos melhores modelos
write.csv(area_adequada_classes, "./Resultados/gperuanus_area_adequada_classes_loss2019_rf.csv")

#salvar o raster reclassificado
writeRaster(raster_classificado, filename="./Resultados/gperuanus_mapa_final_reclassificado_rf_loss2019.asc", format="ascii")
