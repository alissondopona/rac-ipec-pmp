####################################################################################################
#                                                                                                  #
#               Script para a Reunião de Análise Crítica do Ipec                                   #
#                                                                                                  #
####################################################################################################

# Load libraries
library(tidyverse)

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

dat <- data.frame(dat[,c(2,6,8,16,20,21,22,36,37,38,39,42,46,69,70,71,76)])
glimpse(dat)

# ~~~~~~~~~~~~~~~~~~~~~~~ Renaming the variables

colnames(dat) <- c("fai","cidade","trecho","tipo.monitoramento", "data", "lat", "long", "condicao","cond.corporea","int.fisica","pres.oleo", "cod","cond.final", "ofai.sexo","ofai.estag.desenv","ofai.classe","ofai.sp")

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

# ~~~~~~~~~~~~~~~~~~~~~~~ Exploring the data

summary(dat$ofai.sp)

dat <- dat |> 
  add_count(ofai.sp) |> 
  filter(n >= 10) |> 
  select(-n) |> 
  droplevels()

dat <- dat |> 
  filter(ofai.sp != "") |> 
  droplevels()

summary(dat$cidade)

dat <- dat |> 
  filter(cidade != "") |> 
  droplevels()

levels(dat$cidade) <- c("Cananéia", 
                          "Iguape", 
                          "Ilha Comprida",
                          "Ilha Comprida", 
                          "Ilha Comprida")

# ~~~~~~~~~~~~~~~~~~~~~~~ Plot

ggplot(dat, aes(x = ano)) +
  geom_bar() +
  facet_grid(~ofai.classe) +
  labs(x = "Ano",
       y = "Frequência de encalhe") +
  theme_bw()

ggsave("Output/classe_ano.png",
       dpi = 300,
       height = 4,
       width = 9)
# --
ggplot(dat, aes(y = fct_rev(fct_infreq(ofai.sp)))) +
  geom_bar() +
  facet_grid(~ano) +
  labs(x = "Frequência de encalhe",
       y = "Espécie") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic")) 

ggsave("Output/sp_ano.png",
       dpi = 300,
       height = 4,
       width = 10)

# --
ggplot(dat, aes(y = fct_rev(fct_infreq(ofai.sp)))) +
  geom_bar() +
  facet_grid(~cidade) +
  labs(x = "Frequência de encalhe",
       y = "Espécie") +
  theme_bw()+
  theme(axis.text.y = element_text(face = "italic"))

ggsave("Output/sp_cidade.png",
       dpi = 300,
       height = 4,
       width = 9)
  
# --
ggplot(dat, aes(x = estag.desenv)) +
  geom_bar() +
  labs(x = "Estágio de desenvoldimento",
       y = "Frequência de encalhe") +
  theme_bw() 

ggplot(dat, aes(x = ano)) +
  geom_bar() +
  facet_grid(~estag.desenv)+ 
  labs(x = "Estágio de desenvoldimento",
       y = "Frequência de encalhe") +
  theme_bw()

ggplot(dat, aes(x = ano)) +
  geom_bar() +
  facet_grid(~cod) +
  labs(x = "Estágio de desenvoldimento",
       y = "Frequência de encalhe") +
  theme_bw()

# ~~~~~~~~~~~~~~~~~~~~~~~ Spheniscus magellanicus
dat_pingu <- dat |> 
  filter(ofai.sp == "Spheniscus magellanicus")

dat_pingu |> 
  group_by(ano) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = ano, y = count, group = 1)) + 
  geom_point() +
  geom_line()+
  labs(x = "Ano",
       y = "Frequência de ocorrência") +
  theme_bw()

ggsave("Output/sm_ano.png",
       dpi = 300,
       height = 3,
       width = 4)

# -- 
dat_pingu |> 
  group_by(ano, cidade, cod) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = ano, y = count, group = 1)) + 
  geom_point() +
  geom_line()+
  labs(x = "Ano",
       y = "Frequência de ocorrência") +
  facet_grid(cod~cidade) +
  theme_bw()

ggsave("Output/sp_ano_cidade.png",
       dpi = 300,
       height = 8,
       width = 7)

# ~~~~~~~~~~~~~~~~~~~~~~~ Turtle

dat_tarta <- dat |> 
  filter(ofai.classe == "Reptilia")

ggplot(dat_tarta, aes( x = ano))+
  geom_bar() +
  facet_wrap(~ofai.sp,
             scale = 'free') +
  labs(x = "Ano",
       y = " Frequência de encalhe") +
  theme_bw()

ggsave("Output/sp_tarta.png",
       dpi = 300,
       height = 6,
       width = 6)
# ~~~~~~~~~~~~~~~~~~~~~~~ Aves

dat_ave <- dat |> 
  filter(ofai.classe == "Aves")

ggplot(dat_ave, aes( x = ano))+
  geom_bar() +
  facet_wrap(~ofai.sp,
             scale = 'free') +
  labs(x = "Ano",
       y = " Frequência de encalhe") +
  theme_bw()


ggsave("Output/sp_aves.png",
       dpi = 300,
       height = 9,
       width = 9)
# ~~~~~~~~~~~~~~~~~~~~~~~ Mammals

dat_mamma <- dat |> 
  filter(ofai.classe == "Mammalia")

ggplot(dat_mamma, aes( x = ano))+
  geom_bar() +
  facet_wrap(~ofai.sp,
             scale = 'free') +
  labs(x = "Ano",
       y = " Frequência de encalhe") +
  theme_bw()
  
ggsave("Output/sp_tarta.png",
       dpi = 300,
       height = 6,
       width = 6)