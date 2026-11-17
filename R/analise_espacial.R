####################################################################################################
#                                                                                                  #
#               Análises espaciais para a Reunião de Análise Crítica do IPeC                       #
#                                                                                                  #
####################################################################################################

# Load libraries
library(tidyverse)
library(mapview)
library(sf)
library(geobr)

# ~~~~~~~~~~~~~~~~~~~~~~~ Load data
dat_1 <- read.csv("Data/Processed/O_fauna_alvo_individual_cinco_1.csv", 
                  stringsAsFactors = TRUE,
                  h = TRUE,
                  dec = ",",
                  sep = ";")
dat_2 <- read.csv("Data/Processed/O_fauna_alvo_individual_cinco_2.csv", 
                  stringsAsFactors = TRUE,
                  h = TRUE,
                  dec = ",",
                  sep = ";")
dat_3 <- read.csv("Data/Processed/fauna_alvo_individual_2020.csv", 
                  stringsAsFactors = TRUE,
                  h = TRUE,
                  dec = ",",
                  sep = ";")

# ~~~~~~~~~~~~~~~~~~~~~~~ Check the data

glimpse(dat_1)
glimpse(dat_2)
glimpse(dat_3)

# ~~~~~~~~~~~~~~~~~~~~~~~ Join the databases
dat <- rbind(dat_1, dat_2, dat_3)
glimpse(dat)

# ~~~~~~~~~~~~~~~~~~~~~~~ Selecting the data

dat <- data.frame(dat[,c(2,6,8,16,20,21,22,36,37,38,39,42,46,69,70,71,75,76)])
glimpse(dat)

# ~~~~~~~~~~~~~~~~~~~~~~~ Renaming the variables

colnames(dat) <- c("fai","cidade","trecho","tipo.monitoramento", "data", "lat", "long", "condicao","cond.corporea","int.fisica","pres.oleo", "cod","cond.final", "ofai.sexo","ofai.estag.desenv","ofai.classe", "ofai.genero","ofai.sp")

# ~~~~~~~~~~~~~~~~~~~~~~~ Re coding the variable
dat$data <- as.Date(dat$data, format = "%d/%m/%Y")

dat$mes <- lubridate::month(dat$data)
dat$ano <- lubridate::year(dat$data)

dat$tri <- ifelse(dat$mes<=3 & dat$mes>=1,1,
                  ifelse(dat$mes>=4 & dat$mes<=6,2,
                         ifelse(dat$mes>=7 & dat$mes<=9,3,4)))

dat$mes <- as.factor(dat$mes)
dat$ano <- as.factor(dat$ano)
dat$tri <- as.factor(dat$tri)

# ~~~~~~~~~~~~~~~~~~~~~~~ Corrigindo dados geoespaciais
corrige_coord <- function(x) {
  
  x <- as.character(x)
  
  sinal <- ifelse(grepl("-", x), -1, 1)
  
  digitos <- gsub("[^0-9]", "", x)
  
  sinal * as.numeric(digitos) / 
    (10^(nchar(digitos) - 2))
}

dat <- dat |> 
  mutate(
    lat = corrige_coord(lat),
    long = corrige_coord(long)
  )

summary(dat$lat)
summary(dat$long)

# ~~~~~~~~~~~~~~~~~~~~~~~ Análise espacial

# Determinar a coordenada padrão
CRS_wgs <- 4326 # Coordenada padrão (WGS84 - long/lat)
CRS_lea <- "+proj=laea +lat_0=-25 +lon_0=-20 +datum=WGS84 +units=m +no_defs" # Projeção LEA centrada no AS

# ~~~~~~~~~~~~~~~~~~~~~~~ Recorte do polígono

# Selecionando os estados brasileiro
brasil <- read_state(showProgress = FALSE,
                     year = 2025)

# Selecionando o Estado de São Paulo
estado_sp <- brasil |> 
  filter(abbrev_state == "SP")

# Selecionando os municípios de Iguape, Cananéia e Ilha Comprida com base nos códigos do municipio do IBGE
litoral_sul <- read_municipality(code_muni = 
                                   c(3520426, 3520301, 3509908),
                                 year = 2025)

# Verificando os objetos
glimpse(brasil)
glimpse(estado_sp)
glimpse(litoral_sul)

# Visualizando
brasil |> 
  ggplot() +
  geom_sf() +
  theme_test()

estado_sp |> 
  ggplot() +
  geom_sf() +
  theme_test()

litoral_sul |> 
  ggplot() +
  geom_sf() +
  theme_test()

litoral_sul$name_muni <- as.factor(litoral_sul$name_muni)

# Transformando os dados geográficos em geometria
dfsp <- st_as_sf(dat, 
                 coords = c('long','lat'), 
                 crs = CRS_wgs) # As coordenadas estão originalmente em LAT/LONG; portanto, usar primeiro o CRS padrão (WGS84)

class(dfsp)
head(as.data.frame(dfsp))

# Determina a linha de costa como o limite do polígono
costa <- st_boundary(st_union(litoral_sul))
# Transforma para a mesma coordenada geográfica
costa <- st_transform(costa, CRS_wgs)

# Calcular a distância dos pontos a linha de costa
dfsp$dist_costa <- as.numeric(st_distance(dfsp, costa))

# Filtrar mantendo os pontos até 500 km

limite <- 500

dfsp_dentro <- dfsp |> 
  filter(dist_costa <= limite)

dfsp_fora <- dfsp |> 
  filter(dist_costa > limite)

# Verificando os pontos fora
mapview(dfsp_fora)

# Visualizando as ocorrências na área de estudo
ggplot(dfsp_dentro) +
  geom_sf(data = litoral_sul,
          fill = "gray",
          color = "white") +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_alpha_binned()+
  geom_sf(col = '#e07a5f',
          alpha = 0.3) +
  theme_minimal() 

# Visualizando a ocorrência por classe
ggplot(dfsp_dentro) +
  geom_sf(data = litoral_sul,
          fill = "gray",
          color = "white") +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_alpha_binned()+
  geom_sf(aes(color = ofai.classe),
          alpha = 0.3) +
  theme_minimal() +
  facet_grid(ofai.classe~ano)

# Visualizando encalhe de pinguins vivos
dat_pingu <- dfsp_dentro |> 
  filter(ofai.sp == "Spheniscus magellanicus",
         condicao == "Vivo")

ggplot(dat_pingu) +
  geom_sf(data = litoral_sul,
          fill = "gray",
          color = "white") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Distribuição espacial de encalhe de pinguins vivos") +
  scale_alpha_binned()+
  geom_sf(col = '#e07a5f',
          alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "None") +
  facet_wrap(~ano)

ggsave("Output/pinguim_ano.png",
       dpi = 300,
       height = 6,
       width = 10)

# Visualizando encalhe de tartaruga verde vivos
dat_cmydas <- dfsp_dentro |> 
  filter(ofai.sp == "Chelonia mydas",
         condicao == "Vivo")

ggplot(dat_cmydas) +
  geom_sf(data = litoral_sul,
          fill = "gray",
          color = "white") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Distribuição espacial de encalhe de tartarugas verdes") +
  scale_alpha_binned()+
  geom_sf(col = '#306a1f',
          alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "None") +
  facet_wrap(~ano)

ggsave("Output/chelonia_mydas.png",
       dpi = 300,
       height = 6,
       width = 10)

# Visualizando encalhe de lobos verde vivos
dat_lobo <- dfsp_dentro |> 
  filter(ofai.genero == "Arctocephalus",
         condicao == "Vivo") |> 
  mutate(ofai.genero = droplevels(ofai.genero),
         ofai.sp = droplevels(ofai.sp),
         condicao    = droplevels(condicao))

dat_lobo$ofai.sp <- ifelse(dat_lobo$ofai.sp == "Arctocephalus australis", "Arctocephalus australis",
                           ifelse(dat_lobo$ofai.sp == "Arctocephalus tropicalis", "Arctocephalus tropicalis", "Arctocephalus sp"))

ggplot(dat_lobo) +
  geom_sf(data = litoral_sul,
          fill = "gray",
          color = "white") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Distribuição espacial de encalhe de lobo marinho do gênero Arctocephalus sp") +
  geom_sf(aes(color = ofai.sp),
          alpha = 0.3) +
  theme_minimal() +
  theme(legend.text = element_text(face = "italic"),
        legend.position = "bottom")+
  facet_wrap(~ano)

ggsave("Output/lobos.png",
       dpi = 300,
       height = 7,
       width = 10)

# Visualizando em densidade
ggplot() +
  geom_sf(data = litoral_sul ,fill = "gray", color = "white") +
  geom_sf(data = dfsp_dentro, fill = "gray95", color = "gray") +
  stat_density_2d(
    data = dat,
    aes(x = long, y = lat, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.5,
    h = c(0.04, 0.04),
    n = 200,
    contour_var = "ndensity") + # com "ndensity" não fica comparável entre os anos, mas apenas dentro dele mesmo.
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Densidade (KDE)",
       title = "Densidade de ecalhe ao longo do litoral sul") +
  scale_fill_viridis_c(option = "magma") +
  coord_sf() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  facet_wrap(~ano)

ggsave("Output/densidade.png",
       dpi = 300,
       height = 7,
       width = 10)
