# SDM_biomod2
Scripts with comments in Portuguese for performing Species Distribution Models (SDM) using MaxEnt in 'biomod2' package


#######################   PASSO-A-PASSO DOS SDM  #########################

## PRE-ANALISE:
    #1. AQUISICAO DE PONTOS DE OCORRENCIA (Não abordado aqui - Ver Material Externo)
    #2. AQUISICAO DE DADOS ABIOTICOS/VARIAVEIS CLIMATICAS (Não abordado aqui - Ver Material Externo)
    #3. CONSTRUCAO DA MASCARA PARA REPRESENTAR A AREA DE ESTUDO
    #4. PADRONIZACAO DOS DADOS AMBIENTAIS
    #5. FILTRAGEM DOS PONTOS PARA REDUCAO DE AUTOCORRELACAO ESPACIAL
    #6. SELEÇÃO DE VARIAVEIS AMBIENTAIS PARA REDUCAO DE AUTOCORRELACAO AMBIENTAL


## ANALISE:
    #6. ESCOLHA DOS PARAMETROS DO MAXENT
    #7. RODAR O MAXENT DENTRO DO BIOMOD2


## POS-ANALISE:
    #8. VALIDACAO DOS MODELOS
    #9. MAPAS BINARIOS E THRESHOLD
    #10. CURVAS DE RESPOSTA PARA AS VARIAVEIS
    #11. CATEGORIZACAO DE AREAS
    #12. CALCULO EM KM² DE AREA ADEQUADA
  
 
#########################  MATERIAL EXTERNO  ############################

#1. TUTORIAL DE MODELOS DE DISTRIBUIÇÃO DE ESPÉCIES: GUIA TEÓRICO: https://www.researchgate.net/publication/312553609_TUTORIAL_DE_MODELOS_DE_DISTRIBUICAO_-_GUIA_TEORICO

AQUI VOCÊ ENCONTRA O PASSO A PASSO DOS PASSOS 1 E 2:
#2. TUTORIAL DE MODELOS DE DISTRIBUIÇÃO DE ESPÉCIES: GUIA PRÁTICO USANDO O MAXENT E O ARCGIS 10 https://www.researchgate.net/publication/312553932_TUTORIAL_DE_MODELOS_DE_DISTRIBUICAO_DE_ESPECIES_-_GUIA_PRATICO


#Dalapicolla, J. 2016. Tutorial de modelos de distribuição de espécies: guia prático usando o MaxEnt e  o ArcGIS 10. Laboratório  de  Mastozoologia  e  Biogeografia,  Universidade  Federal  do  Espírito Santo, Vitória. Disponível em: https://blog.ufes.br/lamab/?page_id=186 


#Tutorial oficial do MaxEnt: https://biodiversityinformatics.amnh.org/open_source/maxent/Maxent_tutorial2017.pdf

