# ---------------------------------------------------------------------------- #
# Curso Online: Dos dados ao mapa: Modelando a distribuição                    #
# das espécies                                                                 #
#                                                                              #
# Criado por: Dr. Carlos de Oliveira                                           #
# Data: 21-02-2026                                                             #
# Contato: carlos.prof.bio@gmail.com                                           #
#                                                                              #
# Descrição: o script representa o processo geral de implementação de          #
# uma rotina de modelagem da adequabilidade ambiental, para espécies marinhas  # 
# no contexto da Modelagem de Distribuição de Especies (MDE).                  #
# Todos os procedimentos podem ser modificados conforme o escopo e necessidade # 
# da pesquisa.                                                                 #
#                                                                              #
# Notas:                                                                       #
# - eventuais erros podem surgir no script devido atualizações dos             #
# pacotes utilizados pelo script.                                              #
# - as pastas de origem e destino dos arquivos devem ser atualizadas           #
# conforme o computador onde serão realizados os processos de modelagem.       #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#
# Vídeo-aula 1 - Apresentação
#
# Vídeo-aula 2 - Instalar os pacotes necessários
#              - Extrair os pontos de ocorrência da toninha (Pontoporia blainvillei)
#              - GBIF (Global Biodiversity Information Facility)
#              - Tratar (selecionar) os melhores pontos (até o momento).
#              - Visualizar esse pontos
#
# Vídeo-aula 3 - Obter mapa da área de estudo (shapefile)  
#
# Vídeo-aula 4 - Obter e processar os dados ambientais (camadas) - Bio-Oracle 
#
# Vídeo-aula 5 - Extrair valores das camadas ambientais (pontos de presença)
#                
# Vídeo-aula 6 - Gerar as ausências/pseudoausências
#                Extrair valores das camadas ambientais
#
# Vídeo-aula 7 - Verificar colinearidade
#
# Vídeo-aula 8 - Iniciar com a formatação dos dados para o pacote Biomod2
#
# Vídeo-aula 9 - Modelar a distribuição potencial atual da toninha 
#              - Visualizar as variáveis mais importantes
#              - Visualizar as principais métricas
#
# Vídeo-aula 10 - Modelar a distribuição potencial atual (Ensemble)
#               - Visualizar as variáveis mais importantes
#               - Visualizar as curvas de respostas para essas variáveis
#
# Vídeo-aula 11 - Rodar SDM - Cenário futuro 2090-2100 (SSP5-8.5)
#               - Baixar as camadas ambientais
#
# Vídeo-aula 12 - Projetar a distribuição potencial futura da toninha
#               - Comparar os mapas de adequabilidade (Atual x Futuro)
#
# Vídeo-aula 13 - Calcular a perda ou ganho de adequabilidade (Atual x Futuro)
#               
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Vídeo-aula 2 - "Instalar" e carregar os pacotes
#              - Extrair os pontos de ocorrência da toninha (Pontoporia blainvillei)
#              - GBIF (Global Biodiversity Information Facility)
#              - Tratar (selecionar) os melhores pontos (até o momento).
#              - Visualizar esse pontos

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Instalar pacotes

#### Comentar sobre os pacotes (onde buscar)

install.packages("raster")     # Leitura, manipulação, análise e visualização de dados raster (camadas ambientais, mapas contínuos, etc.)
install.packages("tidyverse")  # Conjunto de pacotes para ciência de dados (dplyr, tidyr, ggplot2, readr, etc.)
install.packages("dplyr")      # Manipulação de dados tabulares: filtrar, selecionar, criar variáveis e resumir dados
install.packages("dismo")      # Ferramentas para modelagem de distribuição de espécies (SDM),
install.packages("psych")      # Funções estatísticas exploratórias, incluindo correlação, análise de colinearidade e PCA
install.packages("devtools")   # Ferramentas para desenvolvimento e instalação de pacote
library(devtools)
devtools::install_github("biomodhub/biomod2", dependencies = TRUE)  # Instalação do biomod2 diretamente do GitHub
devtools::install_github("bio-oracle/biooracler")  # Interface para acessar dados ambientais marinhos do Bio-ORACLE
install.packages("sf")         # Manipulação moderna de dados espaciais vetoriais (shapefiles, geopackages)
install.packages("sp")         # Estruturas clássicas de dados espaciais (SpatialPoints, SpatialPolygons)
install.packages("readxl")     # Leitura de arquivos Excel (.xls e .xlsx)
install.packages("writexl")    # Escrita de arquivos Excel (.xlsx)
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata")) # Baixar Shapefiles

# Carregar pacotes

library(rnaturalearth)
library(rnaturalearthdata)
library(raster)    
library(tidyverse) 
library(dplyr)     
library(psych)     
library(biomod2)
library(ggplot2)
library(maps)
library(sf)        
library(sp)        
library(biooracler)
library(writexl)   
library(readxl)

# ---------------------------------------------------------------------------- #

options(scipen = 999) # remover notação científica dos dados

pal1 <- c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred") # paleta de cores

# ---------------------------------------------------------------------------- #

# 01. Obter dados presença

# https://www.gbif.org/

## Download ou carregamento das ocorrências -----
sp_toninha_full <- dismo::gbif(
  genus = "Pontoporia",           # Define o gênero da espécie a buscar no GBIF
  species = "blainvillei",        # Define a espécie
  geo = TRUE,                     # Filtra apenas registros com coordenadas (lat/long)
  removeZeros = TRUE,             # Remove registros com coordenadas inválidas
  download = TRUE                 # Faz o download diretamente do GBIF
)

# ---------------------------------------------------------------------------- #

# Dados do mapa mundi
world_map <- map_data("world")

# Plot do mapa global com os pontos
g1 <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray60",
    linewidth = 0.2
  ) +
  geom_point(
    data = sp_toninha_full,
    aes(x = lon, y = lat),
    color = "red",
    size = 2
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Ocorrências de Pontoporia blainvillei",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

g1

# ---------------------------------------------------------------------------- #

# sp_toninha <- read.csv("nome do arquivo.csv")             # Alternativa: ler ocorrências de um arquivo CSV
# sp_toninha <- readxl::read_excel("nome do arquivo.xlsx")  # Alternativa: ler ocorrências de um Excel

# ---------------------------------------------------------------------------- #

names(sp_toninha_full)    # Mostra os nomes das colunas do objeto 'sp_toninha_full'
sp_toninha_full$country   # Exibe todos os dados baixados por país
nrow(sp_toninha_full)     # Número de registros (observações = linhas)

# ---------------------------------------------------------------------------- #

### Tratamento dos dados -----
sp_toninha <- sp_toninha_full %>%
  dplyr::filter(country %in% c("Brazil", "Argentina", "Uruguay")) %>%  # Mantém ocorrências nos países escolhidos
  dplyr::select(lon, lat)                                              # Mantém apenas colunas de interesse

nrow(sp_toninha)  # Número de registros (observações = linhas)

# ---------------------------------------------------------------------------- #

sp_toninha <- sp_toninha %>%
  distinct() %>%      # Remove registros duplicados
  drop_na()           # Remove registros com valores faltantes

nrow(sp_toninha)      # Número de registros (observações = linhas)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# 01. Obter mapa da área de estudo (shapefile)

oceano <- ne_download(                          # Baixa dados geográficos do Natural Earth
  scale = 10,                                   # Define a resolução (10 = alta resolução)
  type = "ocean",                               # Tipo de feição: oceanos
  category = "physical",                        # Categoria física (elementos naturais)
  returnclass = "sf"                            # Retorna o objeto no formato sf (simple features)
)

#### Comentar sobre as funções (onde buscar)

plot(st_geometry(oceano))                       # Plota apenas a geometria do objeto oceano

g1 # Mostrar mapa com todos os pontos de ocorrência

# Recortar área de estudo
coord_limit <- c(-70, -35, -60, -10)            # Define limites: xmin, xmax, ymin, ymax (lon, lat)

bbox <- st_bbox(                                # Cria um bounding box (caixa espacial)
  c(xmin = coord_limit[1],                      # Limite mínimo de longitude
    xmax = coord_limit[2],                      # Limite máximo de longitude
    ymin = coord_limit[3],                      # Limite mínimo de latitude
    ymax = coord_limit[4]),                     # Limite máximo de latitude
  crs = st_crs(oceano)                          # Usa o mesmo sistema de coordenadas do oceano
)

bbox_sf <- st_as_sfc(bbox)                      # Converte o bounding box em objeto espacial sf
                                                # transforma o bounding box em um polígono espacial no mapa 
                                                # e não apenas um retângilo números.

#sf::sf_use_s2(FALSE)                            # Desativa o motor esférico S2 para operações espaciais

ocean_crop <- st_intersection(oceano, bbox_sf)  # Recorta o oceano usando o bounding box

plot(st_geometry(ocean_crop))                   # Plota o oceano já recortado

axis(2, at = seq(-60, -10, by = 5))             # Adiciona eixo Y com marcações de 5 em 5 graus

axis(1, at = seq(-70, -35, by = 5))             # Adiciona eixo X com marcações de 5 em 5 graus

points(sp_toninha$lon, sp_toninha$lat,          # Plota os pontos de ocorrência
       pch = 16,                                # Define o símbolo do ponto (círculo sólido)
       col = "red",                             # Define a cor dos pontos
       cex = 1)                                 # Define o tamanho dos pontos

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# 03. Obter dados e processar dados ambientais

# https://www.bio-oracle.org/
# https://onlinelibrary.wiley.com/doi/full/10.1111/geb.13813
# https://onlinelibrary.wiley.com/doi/10.1111/j.1466-8238.2011.00656.x

# ---------------------------------------------------------------------------- #

list_layers()                                      # Visualizar a descrição das camadas ambientais

#list_layers("tas_baseline_2000_2020_depthsurf")   # Listar camada indivídual

camadas <- list_layers()                           # Salvar todas as camadas em uma variável
write_xlsx(camadas, "informacoes_camadas.xlsx")    # Salvar em .xlsx

info_layer("tas_baseline_2000_2020_depthsurf")     # Informação sobre camadas indivídual

# ---------------------------------------------------------------------------- #

chl_baseline_surf <- "chl_baseline_2000_2018_depthsurf" ### mg m-3
mld_baseline_surf <- "mlotst_baseline_2000_2019_depthsurf" ### m
tsm_baseline_surf <- "thetao_baseline_2000_2019_depthsurf" ### °C
sal_baseline_surf <- "so_baseline_2000_2019_depthsurf" ### -
swd_baseline_surf <- "swd_baseline_2000_2019_depthsurf" ### Graus
sws_baseline_surf <- "sws_baseline_2000_2019_depthsurf" ### m.s-1
produt_baseline_surf <- "phyc_baseline_2000_2020_depthsurf" ### Total Phytoplankton - mmol . m-3
bathy_baseline <- "terrain_characteristics" ### metros  
iron_baseline_surf <- "dfe_baseline_2000_2018_depthsurf" ### mmol.m-3
nitrate_baseline_surf <- "no3_baseline_2000_2018_depthsurf" ### mmol.m-3
oxygen_baseline_surf <- "o2_baseline_2000_2018_depthsurf" ### mmol.m-3
ph_baseline_surf <- "ph_baseline_2000_2018_depthsurf" ### -
phosphate_baseline_surf <- "po4_baseline_2000_2018_depthsurf" ### mmol.m-3
silicate_baseline_surf <- "si_baseline_2000_2018_depthsurf" ### mmol.m-3

info_layer("chl_baseline_2000_2018_depthsurf") # Informação sobre camadas indivídual
info_layer("terrain_characteristics")          # Informação sobre camadas indivídual

# "Definir restrições (constraints)" de tempo (time), latitude e longitude. 
# As restrições devem ser fornecidas como uma lista nomeada contendo pelo menos um dos seguintes itens: 
# tempo (time), latitude ou longitude.

time_bathy = c('1970-01-01T00:00:00Z', '1970-01-01T00:00:00Z')  # Intervalo temporal da batimetria - variável estática, sem variação temporal real
time = c('2000-01-01T00:00:00Z', '2000-01-01T00:00:00Z')        # Intervalo temporal das variáveis ambientais
                                                                # time = c('2010-01-01T00:00:00Z', '2010-01-01T00:00:00Z')
latitude = c(-60,-10)                                           # Domínio espacial
longitude = c(-70, -35)                                         # Domínio espacial

# Criar as listas de restrições (constraints) para consulta de dados.

constraints_bathy = list(time_bathy, latitude, longitude)     # Cria uma lista com restrições espaciais/temporais para batimetria
constraints = list(time, latitude, longitude)                 # Cria outra lista de restrições (tempo + coordenadas)
names(constraints) = c("time", "latitude", "longitude")       # Define nomes dos elementos da lista 'constraints'
names(constraints_bathy) = c("time", "latitude", "longitude") # Define nomes dos elementos da lista 'constraints_bathy'

constraints_bathy                                             # Mostra o conteúdo da lista de batimetria
constraints                                                   # Mostra o conteúdo da lista principal

# Selecionar as variáveis (camadas) de interesse

info_layer("chl_baseline_2000_2018_depthsurf") # Informação sobre camadas indivídual
info_layer("terrain_characteristics")          # Informação sobre camadas indivídual

variables_chl_baseline_surf = c("chl_mean")
variables_bathy_baseline = c("bathymetry_mean")
variables_mld_baseline_surf = c("mlotst_mean")
variables_tsm_baseline_surf = c("thetao_mean")
variables_sal_baseline_surf = c("so_mean")
variables_swd_baseline_surf = c("swd_mean")
variables_sws_baseline_surf = c("sws_mean")
variables_produt_baseline_surf = c("phyc_mean")
variables_iron_baseline_surf = c("dfe_mean")
variables_nitrate_baseline_surf = c("no3_mean")
variables_oxygen_baseline_surf = c("o2_mean")
variables_ph_baseline_surf = c("ph_mean")
variables_phosphate_baseline_surf = c("po4_mean")
variables_silicate_baseline_surf = c("si_mean")

# Fazer download das camadas ambientais

chl_baseline_surf_2000_2010 <- download_layers(chl_baseline_surf, variables_chl_baseline_surf, constraints)
mld_baseline_surf_2000_2010 <- download_layers(mld_baseline_surf, variables_mld_baseline_surf, constraints)
tsm_baseline_surf_2000_2010 <- download_layers(tsm_baseline_surf, variables_tsm_baseline_surf, constraints)
sal_baseline_surf_2000_2010 <- download_layers(sal_baseline_surf, variables_sal_baseline_surf, constraints)
swd_baseline_surf_2000_2010 <- download_layers(swd_baseline_surf, variables_swd_baseline_surf, constraints)
sws_baseline_surf_2000_2010 <- download_layers(sws_baseline_surf, variables_sws_baseline_surf, constraints)
produt_baseline_surf_2000_2010 <- download_layers(produt_baseline_surf, variables_produt_baseline_surf, constraints)
bathy_baseline_2000_2010 <- download_layers(bathy_baseline, variables_bathy_baseline, constraints_bathy)
iron_baseline_2000_2010 <- download_layers(iron_baseline_surf, variables_iron_baseline_surf, constraints)
nitrate_baseline_2000_2010 <- download_layers(nitrate_baseline_surf, variables_nitrate_baseline_surf, constraints)
oxygen_baseline_2000_2010 <- download_layers(oxygen_baseline_surf, variables_oxygen_baseline_surf, constraints)
ph_baseline_2000_2010 <- download_layers(ph_baseline_surf, variables_ph_baseline_surf, constraints)
phosphate_baseline_2000_2010 <- download_layers(phosphate_baseline_surf, variables_phosphate_baseline_surf, constraints)
silicate_baseline_2000_2010 <- download_layers(silicate_baseline_surf, variables_silicate_baseline_surf, constraints)

chl_baseline_surf_2000_2010 # Visualizar informações das camadas
bathy_baseline_2000_2010    # Visualizar informações das camadas

# Criar RasterLayer a partir dos SpatRaster

# Compatibilidade entre pacotes no R (Terra e Raster)

chl_surf_raster <- raster(chl_baseline_surf_2000_2010)
mld_surf_raster <- raster(mld_baseline_surf_2000_2010)
tsm_surf_raster <- raster(tsm_baseline_surf_2000_2010)
sal_surf_raster <- raster(sal_baseline_surf_2000_2010)
swd_surf_raster <- raster(swd_baseline_surf_2000_2010)
sws_surf_raster <- raster(sws_baseline_surf_2000_2010)
produt_surf_raster <- raster(produt_baseline_surf_2000_2010)
bathy_raster <- raster(bathy_baseline_2000_2010)
iron_surf_raster <- raster(iron_baseline_2000_2010)
nitrate_surf_raster <- raster(nitrate_baseline_2000_2010)
oxygen_surf_raster <- raster(oxygen_baseline_2000_2010)
ph_surf_raster <- raster(ph_baseline_2000_2010)
phosphate_surf_raster <- raster(phosphate_baseline_2000_2010)
silicate_surf_raster <- raster(silicate_baseline_2000_2010)

chl_surf_raster # Visualizar informações das camadas Raster
bathy_raster    # Visualizar informações das camadas Raster

# ---------------------------------------------------------------------------- #

# resolução em graus
res_grau <- 0.05

# conversão 1 grau -> km
km_por_grau <- 111
pixel_km2 <- (res_grau * km_por_grau)^2

pixel_km2

# ---------------------------------------------------------------------------- #

# Empilhar os RasterLayer em um RasterStack

bio <- stack(chl_surf_raster, mld_surf_raster, tsm_surf_raster, sal_surf_raster, swd_surf_raster, sws_surf_raster, 
             produt_surf_raster, bathy_raster, iron_surf_raster, nitrate_surf_raster, phosphate_surf_raster, 
             silicate_surf_raster, ph_surf_raster, oxygen_surf_raster)

print(bio) # Visualizar informações das camadas Raster empilhadas

plot(bio)  # Plotar as camadas Raster
plot(bio)  # Plotar as camadas Raster

# ---------------------------------------------------------------------------- #

#bio <- crop(bio, ocean_crop) # recorte da área de estudo
bio <- mask(bio, ocean_crop) # máscara fora da área de estudo

names(bio)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# 04. Extrair valores das variáveis ambientais

head(sp_toninha)                                     # Visualizar as 6 primeiras linhas
nrow(sp_toninha)                                     # Número de registros (observações = linhas)

toninha_var <- raster::extract(bio, sp_toninha)      # Extrair valores das variáveis ambientais

summary(toninha_var)                                 # Resumir os dados (quartis)

g1                                                   # Plotar gráfico com pontos de ocorrência

nrow(sp_toninha)                                     # Número de registros (observações = linhas)

# Restaram 103 observações (registros de ocorrência)

# ---------------------------------------------------------------------------- #

# Concatenar "sp_toninha e toninha_var"

head(toninha_var)                                # Visualizar as 6 primeiras linhas
head(sp_toninha)                                 # Visualizar as 6 primeiras linhas

toninha_concat <- cbind(sp_toninha, toninha_var) # cbind(): "column bind" = concatenar por colunas
                                                 # junta objetos lado a lado, e cria uma matriz/data frame com novas colunas

head(toninha_concat)                             # Visualizar as 6 primeiras linhas
summary(toninha_concat)                          # Resumir os dados (quartis)

# ---------------------------------------------------------------------------- #
# Excluir NAs

toninha_sem_na <- na.omit(toninha_concat) 

str(toninha_sem_na)         # str() = structure -> mostrar a estrutura interna do objeto

write_xlsx(                 # Salvar objeto em um arquivo .xlsx (excel)
  toninha_sem_na,
  "toninha_sem_na.xlsx"
)

# ---------------------------------------------------------------------------- #

# Plotar dois gráficos com os pontos
par(mfrow = c(1, 2))

plot(st_geometry(ocean_crop), 
     col = "lightblue",
     main = "Distribuição das ocorrências sem filtragem dos dados")  # Título centralizado

axis(2, at = seq(-60, -10, by = 5))
axis(1, at = seq(-70, -35, by = 5))

points(sp_toninha$lon, sp_toninha$lat,
       pch = 16,
       col = "red",
       cex = 1)

plot(st_geometry(ocean_crop), 
     col = "lightblue",
     main = "Distribuição das ocorrências com filtragem dos dados")  # Título centralizado

axis(2, at = seq(-60, -10, by = 5))
axis(1, at = seq(-70, -35, by = 5))

points(toninha_sem_na$lon, toninha_sem_na$lat,
       pch = 16,
       col = "red",
       cex = 1)

# ---------------------------------------------------------------------------- #

# Remove pontos indesejados
toninha_sem_na <- toninha_sem_na[-c(99), ]

# ---------------------------------------------------------------------------- #

# Plotar um gráfico com os pontos
par(mfrow = c(1, 1))

plot(st_geometry(ocean_crop), 
     col = "lightblue",
     main = "Distribuição das ocorrências com filtragem dos dados")  # Título centralizado

axis(2, at = seq(-60, -10, by = 5))
axis(1, at = seq(-70, -35, by = 5))

points(toninha_sem_na$lon, toninha_sem_na$lat,
       pch = 16,
       col = "red",
       cex = 1)

# ---------------------------------------------------------------------------- #

nrow(sp_toninha_full)        # Número de registros (baixadas do GBIF)
nrow(sp_toninha)             # Número de registros (após primeiras filtragens)
nrow(toninha_sem_na)         # Número de registros (que iremos modelar)

summary(toninha_sem_na)      # Resumir os dados (quartis)

# ---------------------------------------------------------------------------- #

# 05. Gerar as ausências/pseudoausências 

# Calcular distância entre pontos de presença e ausência
# Pontos de ausência com 2° de distância dos pontos de presença

# Converter o meu dataset (toninha_sem_na) para um objeto espacial do pacote sf
# Agora cada linha vira um ponto no espaço

toninha_pres_sf <- st_as_sf(      # Converter o meu dataset (toninha_sem_na) para um objeto espacial do pacote sf
                                  # Agora cada linha vira um ponto no espaço
  toninha_sem_na,                 # Data frame com os dados de ocorrência
  coords = c("lon", "lat"),       # Colunas que representam longitude e latitude
  crs = 4326                      # Sistema de referência de coordenadas usado no mapa (EPSG:4326 = WGS84)
)

oceans_sf <- st_as_sf(ocean_crop) # Converter o meu mapa (ocean_crop) para um objeto espacial do pacote sf

# ---------------------------------------------------------------------------- #

# Sortear pontos candidatos (mais do que 102) -> pontos de ausência com 2° de distância dos pontos de presença

set.seed(123)                          # Reprodutibilisdade

candidatos_sf <- st_sample(            # Função do pacote sf que  sorteia pontos dentro de uma geometria
  oceans_sf,                           # Mapa convertido para objeto espacial do pacote sf
  size = 5000,                         # quanto maior, melhor
  type = "random"
)

#candidatos_sf <- st_as_sf(candidatos)

# ---------------------------------------------------------------------------- #

#install.packages("lwgeom")   # pacote complementar ao pacote sf, com operações que o sf puro não cobre bem.

library(lwgeom) 

# Calcular distância até os pontos de presença
dist_matrix <- st_distance(candidatos_sf, toninha_pres_sf)

# distância mínima de cada candidato até qualquer presença
dist_min <- apply(dist_matrix, 1, min)

# Filtrar pontos com distância ≥ 2° = 222 km
ausencias_sf_2 <- st_as_sf(candidatos_sf)[dist_min >= 2, ]

ausencias_sf_222 <- st_as_sf(candidatos_sf)[dist_min >= 222, ]

# Selecionar 120 pontos
if (nrow(ausencias_sf_2) < 120) {
  stop("Poucos pontos disponíveis. Aumente o número de candidatos.")
}

ausencias_sf_2 <- ausencias_sf_2 %>%
  slice_sample(n = 102)

# Selecionar 120 pontos
if (nrow(ausencias_sf_222) < 120) {
  stop("Poucos pontos disponíveis. Aumente o número de candidatos.")
}

ausencias_sf_222 <- ausencias_sf_222 %>%
  slice_sample(n = 102)

# ---------------------------------------------------------------------------- #

# Plotar um gráfico com os pontos
par(mfrow = c(1, 2))

plot(st_geometry(ocean_crop), col = "lightblue")

axis(2, at = seq(-60, -10, by = 5))
axis(1, at = seq(-70, -35, by = 5))

plot(st_geometry(toninha_pres_sf), add = TRUE, col = "blue", pch = 16)
plot(st_geometry(ausencias_sf_2), add = TRUE, col = "red", pch = 16)

plot(st_geometry(ocean_crop), col = "lightblue")

axis(2, at = seq(-60, -10, by = 5))
axis(1, at = seq(-70, -35, by = 5))

plot(st_geometry(toninha_pres_sf), add = TRUE, col = "blue", pch = 16)
plot(st_geometry(ausencias_sf_222), add = TRUE, col = "red", pch = 16)

# ---------------------------------------------------------------------------- #

# Extrair latitude e longitude
ausencias_df <- ausencias_sf %>%
  st_coordinates() %>%
  as.data.frame()

colnames(ausencias_df) <- c("lon", "lat")

# ---------------------------------------------------------------------------- #

# Extrair valores das variaveis de ausência

toninha_ausencias <- raster::extract(bio, ausencias_sf)

summary(toninha_ausencias)

# ---------------------------------------------------------------------------- #

# Inserir coluna "species" com valor 0 para ausência e coluna "lon" e "lat" 

toninha_ausencias_df <- cbind(
  species = 0,
  ausencias_df[, c("lon", "lat")],
  toninha_ausencias
)

str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

#toninha_ausencias_df <- na.omit(as.data.frame(toninha_ausencias_df))

#str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

# Define semente para reprodutibilidade (opcional)
set.seed(123)

# Sorteia 5 linhas aleatórias para remover
linhas_remover <- sample(seq_len(nrow(toninha_ausencias_df)), 9)

# Remove as linhas sorteadas
toninha_ausencias_df <- toninha_ausencias_df[-linhas_remover, ]

str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

# Concatenar dados de presença e ausências 

colnames(toninha_ausencias_df)
colnames(toninha_sem_na)

# Inserir coluna "species" com valor 0 para ausência e coluna "lon" e "lat" 

toninha_sem_na <- cbind(
  species = 1,
  toninha_sem_na
)

colnames(toninha_ausencias_df)
colnames(toninha_sem_na)

# ---------------------------------------------------------------------------- #

# Concatenar

toninha_final <- rbind(
  toninha_sem_na,
  toninha_ausencias_df
)

str(toninha_final)

# ---------------------------------------------------------------------------- #

# Visualização base
plot(st_geometry(ocean_crop), col = "lightblue")

axis(2, at = seq(-60, -10, by = 5))
axis(1, at = seq(-70, -35, by = 5))

# Definir cores por presença/ausência
cols <- ifelse(toninha_final$species == 1, "blue", "red")

# Plotar pontos
points(
  toninha_final$lon,
  toninha_final$lat,
  col = cols,
  pch = 16,
  cex = 0.8
)

# ---------------------------------------------------------------------------- #

write_xlsx(
  toninha_final,
  path = "dados_toninha_final.xlsx"
)

# ---------------------------------------------------------------------------- #

# 06. Verificar colinearidade -----

toninha_colin <- toninha_final %>%
  dplyr::select(-species, -lon, -lat)

toninha_colin <- toninha_final %>%
  dplyr::select(-species, -lon, -lat, -phyc_mean, -mlotst_mean, -dfe_mean, -no3_mean, -po4_mean, -o2_mean, -ph_mean)

pairs.panels(
  toninha_colin,
  cex = 10,        # tamanho geral da fonte (números, correlações)
  cex.labels = 1.5 # tamanho dos nomes das variáveis
)

# ---------------------------------------------------------------------------- #

# 07. Rodar SDM Biomod2 - Atual -----

# https://cran.r-project.org/web/packages/biomod2/biomod2.pdf

# ---------------------------------------------------------------------------- #

# Dados de presença / ausência -----

toninha_final <- read_xlsx("dados_toninha_final.xlsx")

toninha_colin <- toninha_final %>%
  dplyr::select(-phyc_mean, -mlotst_mean, -dfe_mean, -no3_mean, -po4_mean, -o2_mean, -ph_mean)

# Checagens básicas
str(toninha_colin)
summary(toninha_colin)

# ---------------------------------------------------------------------------- #

# Nome da espécie
myResp <- toninha_colin$species
myRespName <- "species"

# Coordenadas (data.frame simples)
myRespXY <- toninha_colin[, c("lon", "lat")]

# ---------------------------------------------------------------------------- #

# Dados ambientais -----

# Empilhar os RasterLayer em um RasterStack
bio_colin <- stack(chl_surf_raster, tsm_surf_raster, sal_surf_raster, swd_surf_raster, sws_surf_raster, bathy_raster, silicate_surf_raster)

# Converter Brick → Stack
bio_colin <- raster::stack(bio_colin)

bio_colin

# ---------------------------------------------------------------------------- #

# Formatação BIOMOD (com ausências reais) -----

myBiomodData <- BIOMOD_FormatingData(
  resp.var  = myResp,              # ✅ vetor 0/1
  expl.var  = bio_colin,
  resp.xy   = myRespXY,
  resp.name = myRespName
)

myBiomodData
myBiomodData@coord
head(myBiomodData@data.env.var)
#plot(myBiomodData)

# ---------------------------------------------------------------------------- #

ModelsTable # Visualizar algoritmos

#allModels <- c('ANN', 'CTA', 'DNN', 'FDA', 'GAM', 'GBM', 'GLM', 'MARS'
#               , 'MAXENT', 'MAXNET', 'RF', 'RFd', 'SRE', 'XGBOOST')

allModels <- c('GAM', 'GLM', 'RF', 'XGBOOST', 'MAXENT')

toninha_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = allModels,
  strategy = 'default',
  bm.format = myBiomodData
)

toninha_model <- BIOMOD_Modeling(
  bm.format    = myBiomodData,
  modeling.id = 'AllModels',
  models      = c('GAM','GLM','RF','XGBOOST', 'MAXENT'),
  CV.strategy = 'block',    # validação cruzada espacial (em blocos)
  CV.perc     = 0.7,
  OPT.strategy = 'default',
  metric.eval = c('TSS','AUCroc'),
  var.import  = 3,
  seed.val    = 42
)

# ---------------------------------------------------------------------------- #

# Obter scores de avaliação e importância das variáveis

bm_PlotEvalMean(
  toninha_model,
  metric.eval = c('TSS','AUCroc'),
  dataset = "validation",
  group.by = "algo",
  do.plot = TRUE
)

# ---------------------------------------------------------------------------- #

toninha_model_var_imp <- get_variables_importance(toninha_model)

# Calcula a média da importância das variáveis pelas colunas "expl.var"
mean_var_imp <- aggregate(var.imp ~ expl.var, data = toninha_model_var_imp, FUN = mean)

# Exibe o resultado
print(mean_var_imp)

# Ordena o dataframe mean_var_imp do mais importante para o menos importante
mean_var_imp <- mean_var_imp[order(mean_var_imp$var.imp, decreasing = TRUE), ]

# Gera o gráfico de barras invertido
barplot(mean_var_imp$var.imp, names.arg = mean_var_imp$expl.var, 
        xlab = "Variáveis Explicativas", ylab = "Importância",
        col = "blue")

# ---------------------------------------------------------------------------- #

## then call Projection function
toninha_projection <- BIOMOD_Projection(toninha_model,
                                        new.env = bio_colin,
                                        proj.name = 'current_new',
                                        selected.models = 'all',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)

# ---------------------------------------------------------------------------- #

mods <- get_built_models(toninha_model)

toninha_ens <- BIOMOD_EnsembleModeling(
  toninha_model,
  models.chosen = mods,
  em.by = "all",
  em.algo = c('EMmean'),
  metric.select = c('TSS'),
  metric.select.thresh = c(0.8),
  metric.eval = c("TSS"),
  var.import = 5,
  EMci.alpha = 0.05,
  EMwmean.decay = "proportional",
  nb.cpu = 1,
  seed.val = 123,
  do.progress = TRUE,
)

# ---------------------------------------------------------------------------- #

rcurve_toninha_ens <- 
  bm_PlotResponseCurves(
    toninha_ens,
    models.chosen = get_built_models(toninha_ens),
    new.env = get_formal_data(toninha_ens, "expl.var"),
    show.variables = get_formal_data(toninha_ens, "expl.var.names"),
    do.bivariate = FALSE,
    fixed.var = "mean",
    do.plot = TRUE,
    do.progress = TRUE)

# Get evaluation scores & variables importance
get_evaluations(toninha_ens)

get_variables_importance(toninha_ens)

# # Represent variables importance
bm_PlotVarImpBoxplot(bm.out = toninha_ens, group.by = c('expl.var', 'algo', 'algo'))

# Caminho para o arquivo
caminho_arquivo <- "C:/Cursos/Modelagem/Vídeo-aula/sdm_toninha_biomod2/species/proj_current_new/proj_current_new_species.tif"

# Carregar a imagem TIFF
imagem_tiff <- raster(caminho_arquivo)

# Visualizar a imagem
plot(imagem_tiff, col = pal1)

atual <- imagem_tiff / 1000

# Visualizar a imagem
plot(atual, col = pal1)

# ---------------------------------------------------------------------------- #

toninha_ens_ens <- BIOMOD_EnsembleForecasting(toninha_ens,
                                              projection.output = toninha_projection,
                                              new.env = bio_colin,
                                              selected.models = 'all',
                                              proj.name = "ensemble_new_current",
                                              binary.meth = "TSS")
# Caminho para o arquivo
caminho_arquivo_ens <- "C:/Cursos/Modelagem/Vídeo-aula/sdm_toninha_biomod2/species/proj_ensemble_new_current/proj_ensemble_new_current_species_ensemble.tif"

# Carregar a imagem TIFF
imagem_tiff_ens <- raster(caminho_arquivo_ens)

# Visualizar a imagem
plot(imagem_tiff_ens, col = pal1)

atual_ens <- imagem_tiff_ens / 1000

# Visualizar a imagem
plot(atual_ens, col = pal1)

# ---------------------------------------------------------------------------- #

# Extrair as coordenadas de myBiomodData
coordenadas <- myBiomodData@coord

valores_extraidos <- raster::extract(atual_ens, coordenadas)

# Visualizar os primeiros valores extraídos
head(valores_extraidos)

# Se desejar salvar esses valores em um arquivo .xlsx, use o pacote openxlsx

# Criar um dataframe com as coordenadas e os valores extraídos
dados_extraidos <- data.frame(coordenadas, valores_extraidos)

head(dados_extraidos)
str(dados_extraidos)

toninha_colin$valores_extraidos <- dados_extraidos$valores_extraidos

str(toninha_colin)

# Salvar os dados em um arquivo .xlsx
writexl::write_xlsx(
  toninha_colin,
  path = "toninha_colin.xlsx"
)

## Download ou carregamento das ocorrências -----
adequab <- readxl::read_excel("toninha_colin.xlsx")

head(adequab)
summary(adequab)

# ---------------------------------------------------------------------------- #

# Defina uma função para remover outliers usando o método do IQR
remove_outliers <- function(df, col_name) {
  Q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df[df[[col_name]] >= lower_bound & df[[col_name]] <= upper_bound, ]
}

# Remova os outliers da variável "so_mean.2"
adequab_clean_bathy <- remove_outliers(adequab, "bathymetry_mean")

bathy <- ggplot(
  adequab_clean_bathy,
  aes(x = bathymetry_mean, y = valores_extraidos)
) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    x = "Batimetria (m)",
    y = "Adequabilidade"
  ) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    breaks = seq(
      floor(min(adequab_clean_bathy$bathymetry_mean) / 500) * 500,
      ceiling(max(adequab_clean_bathy$bathymetry_mean) / 500) * 500,
      by = 500
    )
  )

plot(bathy)

# Remova os outliers da variável "so_mean.2"
adequab_clean_sst <- remove_outliers(adequab, "thetao_mean")

# Crie o gráfico de "curva de resposta" sem os outliers na variável x
sst <- ggplot(
  adequab_clean_sst,
  aes(x = thetao_mean, y = valores_extraidos)
) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    x = "SST (°C)",
    y = "Adequabilidade"
  ) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(breaks = seq(0, 30, by = 1))

plot(sst)

# Remova os outliers da variável "so_mean.2"
adequab_clean_sali <- remove_outliers(adequab, "so_mean")

# Crie o gráfico de "curva de resposta" sem os outliers na variável x
sal <- ggplot(adequab_clean_sali, aes(x = so_mean, y = valores_extraidos)) +
  #geom_point() + 
  geom_smooth(method = "loess") + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +  # Adiciona linha pontilhada em y = 0.5
  labs(x = "Salinidade",
       y = "Adequabilidade") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(breaks = seq(0, 40, by = 1))

plot(sal)

# ---------------------------------------------------------------------------- #

library(gridExtra)

# Combine os gráficos em um painel 2x2
grid.arrange(bathy, sst, sal, ncol = 3)

# ---------------------------------------------------------------------------- #

# 08. Rodar SDM - Cenário futuro -----

# ---------------------------------------------------------------------------- #

#chl_baseline_surf <- "chl_baseline_2000_2018_depthsurf" ### mg m-3

chl_ssp585_surf <- "chl_ssp585_2020_2100_depthsurf" ### mg m-3
silicate_ssp585_surf <- "si_ssp585_2020_2100_depthsurf" ### m
tsm_ssp585_surf <- "thetao_ssp585_2020_2100_depthsurf" ### °C
sal_ssp585_surf <- "so_ssp585_2020_2100_depthsurf" ### PSU
swd_ssp585_surf <- "swd_ssp585_2020_2100_depthsurf" ### Graus
sws_ssp585_surf <- "sws_ssp585_2020_2100_depthsurf" ### m s**-1
bathy_ssp585 <- "terrain_characteristics" ### metros  

### Definir período de tempo, latitude e longitude 

time_bathy_ssp585 = c('1970-01-01T00:00:00Z', '1970-01-01T00:00:00Z') ## Não mudar nada
time_2100_ssp585 = c('2090-01-01T00:00:00Z', '2090-01-01T00:00:00Z') ## Não mudar nada
latitude_ssp585 = c(-60, -10) ## Não mudar nada
longitude_ssp585 = c(-70, -35) ## Não mudar nada

constraints_bathy_ssp585 = list(time_bathy_ssp585, latitude_ssp585, longitude_ssp585) ## Não mudar nada
constraints_2100_ssp585 = list(time_2100_ssp585, latitude_ssp585, longitude_ssp585) ## Não mudar nada

#names(constraints_2050_ssp585) = c("time", "latitude", "longitude") ## Não mudar nada
names(constraints_2100_ssp585) = c("time", "latitude", "longitude") ## Não mudar nada
names(constraints_bathy_ssp585) = c("time", "latitude", "longitude") ## Não mudar nada

variables_chl_ssp585_surf = c("chl_mean") ## Não mudar nada
variables_silicate_ssp585_surf = c("si_mean") ## Não mudar nada
variables_tsm_ssp585_surf = c("thetao_mean") ## Não mudar nada
variables_sal_ssp585_surf = c("so_mean") ## Não mudar nada
variables_swd_ssp585_surf = c("swd_mean") ## Não mudar nada
variables_sws_ssp585_surf = c("sws_mean") ## Não mudar nada
variables_bathy_ssp585 = c("bathymetry_mean") ## Não mudar nada

chl_ssp585_surf_2100 <- download_layers(chl_ssp585_surf, variables_chl_ssp585_surf, constraints_2100_ssp585)
silicate_ssp585_surf_2100 <- download_layers(silicate_ssp585_surf, variables_silicate_ssp585_surf, constraints_2100_ssp585)
tsm_ssp585_surf_2100 <- download_layers(tsm_ssp585_surf, variables_tsm_ssp585_surf, constraints_2100_ssp585)
sal_ssp585_surf_2100 <- download_layers(sal_ssp585_surf, variables_sal_ssp585_surf, constraints_2100_ssp585)
swd_ssp585_surf_2100 <- download_layers(swd_ssp585_surf, variables_swd_ssp585_surf, constraints_2100_ssp585)
sws_ssp585_surf_2100 <- download_layers(sws_ssp585_surf, variables_sws_ssp585_surf, constraints_2100_ssp585)
bathy_ssp585_2100 <- download_layers(bathy_ssp585, variables_bathy_ssp585, constraints_bathy_ssp585)

# Criar RasterLayer a partir dos SpatRaster
chl_ssp585_surf_raster_2100 <- raster(chl_ssp585_surf_2100)
silicate_ssp585_surf_raster_2100 <- raster(silicate_ssp585_surf_2100)
tsm_ssp585_surf_raster_2100 <- raster(tsm_ssp585_surf_2100)
sal_ssp585_surf_raster_2100 <- raster(sal_ssp585_surf_2100)
swd_ssp585_surf_raster_2100 <- raster(swd_ssp585_surf_2100)
sws_ssp585_surf_raster_2100 <- raster(sws_ssp585_surf_2100)
bathy_ssp585_raster_2100 <- raster(bathy_ssp585_2100)

# Empilhar os RasterLayer em um RasterStack

bio_ssp585_2100 <- stack(chl_ssp585_surf_raster_2100, silicate_ssp585_surf_raster_2100, tsm_ssp585_surf_raster_2100, sal_ssp585_surf_raster_2100, 
                         swd_ssp585_surf_raster_2100, sws_ssp585_surf_raster_2100, bathy_ssp585_raster_2100)


# Verificar a estrutura do RasterStack resultante
print(bio_ssp585_2100)
plot(bio_ssp585_2100)

# ---------------------------------------------------------------------------- #

bio_ssp585_2100 <- crop(bio_ssp585_2100, ocean_crop) # recorte da área de estudo
bio_ssp585_2100 <- mask(bio_ssp585_2100, ocean_crop) # máscara fora da área de estudo

names(bio_ssp585_2100)

# ---------------------------------------------------------------------------- #

## then call Projection function
toninha_ssp585_2100_projection <- BIOMOD_Projection(toninha_model,
                                                    new.env = bio_ssp585_2100,
                                                    proj.name = 'ssp585_2100',
                                                    selected.models = 'all',
                                                    compress = FALSE,
                                                    build.clamping.mask = FALSE)

toninha_ssp585_2100_ens_ens <- BIOMOD_EnsembleForecasting(toninha_ens,
                                                          projection.output = toninha_ssp585_2100_projection,
                                                          new.env = bio_ssp585_2100,
                                                          selected.models = 'all',
                                                          proj.name = "ensemble_new_ssp585_2100",
                                                          binary.meth = "TSS")

# Caminho para o arquivo
caminho_arquivo_ens_ssp585_2100 <- "C:/Cursos/Modelagem/Vídeo-aula/sdm_toninha_biomod2/species/proj_ensemble_new_ssp585_2100/proj_ensemble_new_ssp585_2100_species_ensemble.tif"

# Carregar a imagem TIFF
imagem_tiff_ens_ssp585_2100 <- raster(caminho_arquivo_ens_ssp585_2100)

ssp585_2100_ens <- imagem_tiff_ens_ssp585_2100 / 1000

# ---------------------------------------------------------------------------- #

# Dividir a tela em 2 colunas e 1 linha
par(mfrow = c(1, 2))

# gráfico: projetado com ensemble
plot(atual_ens, zlim = c(0, 1), col = pal1,
     main = "Projetado atual\n(Ensemble)")

# gráfico: projetado no futuro
plot(ssp585_2100_ens, zlim = c(0, 1), col = pal1,
     main = "Projetado no futuro\n(SSP5-8.5 - 2090/2100)")

# ---------------------------------------------------------------------------- #

# Calculando a perda ou ganho de adequabilidade atual x futuro

# Raster atual
n_atual_0.9_1.0 <- cellStats(
  atual_ens >= 0.9 & atual_ens <= 1.0,
  stat = 'sum'
)

# Raster futuro
n_futuro_0.9_1.0 <- cellStats(
  ssp585_2100_ens >= 0.9 & ssp585_2100_ens <= 1.0,
  stat = 'sum'
)

n_atual_0.9_1.0
n_futuro_0.9_1.0

# ---------------------------------------------------------------------------- #

# Raster atual
n_atual_0.7_0.9 <- cellStats(
  atual_ens > 0.7 & atual_ens <= 0.9,
  stat = 'sum'
)

# Raster futuro
n_futuro_0.7_0.9 <- cellStats(
  ssp585_2100_ens > 0.7 & ssp585_2100_ens <= 0.9,
  stat = 'sum'
)

n_atual_0.7_0.9
n_futuro_0.7_0.9

# ---------------------------------------------------------------------------- #

# Raster atual
n_atual_0.5_0.7 <- cellStats(
  atual_ens > 0.5 & atual_ens <= 0.7,
  stat = 'sum'
)

# Raster futuro
n_futuro_0.5_0.7 <- cellStats(
  ssp585_2100_ens > 0.5 & ssp585_2100_ens <= 0.7,
  stat = 'sum'
)

n_atual_0.5_0.7
n_futuro_0.5_0.7

# ---------------------------------------------------------------------------- #

# Raster atual
n_atual_0.3_0.5 <- cellStats(
  atual_ens > 0.3 & atual_ens <= 0.5,
  stat = 'sum'
)

# Raster futuro
n_futuro_0.3_0.5 <- cellStats(
  ssp585_2100_ens > 0.3 & ssp585_2100_ens <= 0.5,
  stat = 'sum'
)

n_atual_0.3_0.5
n_futuro_0.3_0.5

# ---------------------------------------------------------------------------- #

# resolução em graus
res_grau <- 0.05

# conversão grau -> km
km_por_grau <- 111
pixel_km2 <- (res_grau * km_por_grau)^2

pixel_km2

# ---------------------------------------------------------------------------- #

area_atual_km2_0.9_1.0  <- n_atual_0.9_1.0  * pixel_km2
area_futuro_km2_0.9_1.0 <- n_futuro_0.9_1.0 * pixel_km2
area_perda_ganho_km2_0.9_1.0  <- -(area_atual_km2_0.9_1.0 - area_futuro_km2_0.9_1.0)

area_atual_km2_0.9_1.0
area_futuro_km2_0.9_1.0
area_perda_ganho_km2_0.9_1.0

# ---------------------------------------------------------------------------- #

perda_ganho_perda_percentual_0.9_1.0 <- area_perda_ganho_km2_0.9_1.0 / area_atual_km2_0.9_1.0 * 100
perda_ganho_perda_percentual_0.9_1.0

# ---------------------------------------------------------------------------- #

area_atual_km2_0.7_0.9  <- n_atual_0.7_0.9  * pixel_km2
area_futuro_km2_0.7_0.9 <- n_futuro_0.7_0.9 * pixel_km2
area_perda_ganho_km2_0.7_0.9  <- -(area_atual_km2_0.7_0.9 - area_futuro_km2_0.7_0.9)

area_atual_km2_0.7_0.9
area_futuro_km2_0.7_0.9
area_perda_ganho_km2_0.7_0.9

# ---------------------------------------------------------------------------- #

perda_ganho_perda_percentual_0.7_0.9 <- area_perda_ganho_km2_0.7_0.9 / area_atual_km2_0.7_0.9 * 100
perda_ganho_perda_percentual_0.7_0.9

# ---------------------------------------------------------------------------- #

area_atual_km2_0.5_0.7  <- n_atual_0.5_0.7  * pixel_km2
area_futuro_km2_0.5_0.7 <- n_futuro_0.5_0.7 * pixel_km2
area_perda_ganho_km2_0.5_0.7  <- -(area_atual_km2_0.5_0.7 - area_futuro_km2_0.5_0.7)

area_atual_km2_0.5_0.7
area_futuro_km2_0.5_0.7
area_perda_ganho_km2_0.5_0.7

# ---------------------------------------------------------------------------- #

perda_ganho_perda_percentual_0.5_0.7 <- area_perda_ganho_km2_0.5_0.7 / area_atual_km2_0.5_0.7 * 100
perda_ganho_perda_percentual_0.5_0.7

# ---------------------------------------------------------------------------- #

area_atual_km2_0.3_0.5  <- n_atual_0.3_0.5  * pixel_km2
area_futuro_km2_0.3_0.5 <- n_futuro_0.3_0.5 * pixel_km2
area_perda_ganho_km2_0.3_0.5  <- -(area_atual_km2_0.3_0.5 - area_futuro_km2_0.3_0.5)

area_atual_km2_0.3_0.5
area_futuro_km2_0.3_0.5
area_perda_ganho_km2_0.3_0.5

# ---------------------------------------------------------------------------- #

perda_ganho_perda_percentual_0.3_0.5 <- area_perda_ganho_km2_0.3_0.5 / area_atual_km2_0.3_0.5 * 100
perda_ganho_perda_percentual_0.3_0.5

# ---------------------------------------------------------------------------- #

df_perda_ganho <- data.frame(
  Classe = factor(
    c("0.9–1.0", "0.7–0.9", "0.5–0.7", "0.3–0.5"),
    levels = c("0.9–1.0", "0.7–0.9", "0.5–0.7", "0.3–0.5")
  ),
  Area_km2 = c(
    area_perda_ganho_km2_0.9_1.0,
    area_perda_ganho_km2_0.7_0.9,
    area_perda_ganho_km2_0.5_0.7,
    area_perda_ganho_km2_0.3_0.5
  ),
  Percentual = c(
    perda_ganho_perda_percentual_0.9_1.0,
    perda_ganho_perda_percentual_0.7_0.9,
    perda_ganho_perda_percentual_0.5_0.7,
    perda_ganho_perda_percentual_0.3_0.5
  )
)

# Identificar ganho ou perda
df_perda_ganho$Tipo <- ifelse(df_perda_ganho$Area_km2 >= 0, "Ganho", "Perda")

# ---------------------------------------------------------------------------- #

ggplot(df_perda_ganho, aes(x = Classe, y = Area_km2, fill = Tipo)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(
    aes(label = paste0(
      format(round(Area_km2, 0), big.mark = ".", scientific = FALSE),
      " km²\n(",
      round(Percentual, 1), "%)"
    )),
    vjust = ifelse(df_perda_ganho$Area_km2 >= 0, -0.5, 1.5),
    size = 5
  ) +
  scale_fill_manual(values = c("Ganho" = "#1F78B4", "Perda" = "#E31A1C")) +
  labs(
    title = "Ganho e perda de áreas de adequabilidade ambiental",
    subtitle = "Comparação entre o cenário atual e o futuro (SSP5-8.5 – 2100)",
    x = "Intervalo de adequabilidade",
    y = expression("Variação de área (km"^2*")")
  ) +
  coord_cartesian(ylim = c(-40000, 250000)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #