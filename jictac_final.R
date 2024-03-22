# Bibliotecas ====
# Carregar o pacote com as informações geográficas do Brasil(estados, municipios e bairros)
library(geobr)
# Carregar o pacote tidyverse, que inclui uma coleção de pacotes para manipulação e visualização de dados.
library(tidyverse)
# Carregar o pacote readxl para leitura de arquivos do Excel.
library(readxl)
# Carregar o pacote paletteer para paletas de cores.
library(paletteer)
# Carregar o pacote ggplot2 para criar gráficos e visualizações avançados.
library(ggplot2)
# Carregar o pacote dplyr para tarefas de manipulação de dados.
library(dplyr)
# Carregar o pacote classInt para funções de classificação de dados.
library(classInt)
# Carregar o pacote raster para trabalhar com dados raster.
library(raster)
# Carregar o pacote maps para exibir mapas.
library(maps)
# Carregar o pacote sp para classes e métodos de dados espaciais.
library(sp)
# Carregar o pacote colorspace para manipulação de cores.
library(colorspace)
# Carregar o pacote cowplot para criar ggplots complexos e arranjar gráficos.
library(cowplot)
# Carregar o pacote sf para features simples e manipulação de dados espaciais.
library(sf)
# Carregar o pacote gridExtra para arranjar múltiplos gráficos na mesma página.
library(gridExtra)
# Carregar o pacote ggnewscale para adicionar múltiplas escalas de cores em um único gráfico ggplot.
library(ggnewscale)
#simplifica tarefas como instalação de pacotes diretamente do GitHub e desenvolvimento de pacotes R
library(devtools)
devtools::install_github("yutannihilation/ggsflabel")
#Permite a adição e personalização de etiquetas em gráficos criados com o ggplot2.
library(ggsflabel)

# Objetos ====
# Dados de Febre Amarela retirados do OpenDataSUS 
FAcasoshumanos <- read.csv("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_casoshumanos_1994-2021.csv", header = TRUE, sep = ";")

FAepizootias <- read.csv("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_epizpnh_1999-2021.csv", header = TRUE, sep = ";")


#Dados espaciais do Brasil do pacote "geobr"
estados <- geobr::read_state()

bairros <- geobr::read_neighborhood()

municipios <- geobr::read_municipality()

#Arquivo para compatibilizar os códigos municipais de 6 dígitos do DataSUS com os 
#códigos municipais de 7 dígitos do pacote "geobr"

dicmunicipios <- read.csv("https://github.com/luanajohas/jictac/raw/main/dicionario_municipios.csv")


# Epizootias====
#Converte a coluna COD_MUN_OCOR, da base FAepizootias, para o tipo de dado caracter
FAepizootias$COD_MUN_OCOR <- as.character(FAepizootias$COD_MUN_OCOR)

#Converte a coluna D_MUNICIP, da base dicmunicipios, para o tipo de dado caracter
dicmunicipios$ID_MUNICIP <- as.character(dicmunicipios$ID_MUNICIP)

#Agrupa os dados por COD_MUN_OCOR (código do município de ocorrência) e ANO_OCOR (ano de ocorrência) e, em seguida, 
#resume os dados, contando o número de ocorrências em cada grupo, armazenando o resultado na nova coluna chamada contzoo.
FAepizootias<-FAepizootias |>
  group_by(COD_MUN_OCOR,ANO_OCOR)|>
  summarise(contzoo=n())

#Realiza uma junção à esquerda entre dois dataframes, FAepizootias e dicmunicipios, 
#utilizando a correspondência entre as colunas COD_MUN_OCOR do dataframe FAepizootias e ID_MUNICIP do dataframe dicmunicipios.
FAepizootias<-left_join(FAepizootias, dicmunicipios,
                        by = c('COD_MUN_OCOR'= 'ID_MUNICIP'))

#Converte a coluna code_muni, da base FAepizootias, para o tipo de dado caracter
FAepizootias$code_muni <- as.character(FAepizootias$code_muni)

#Converte a coluna code_muni, da base municipios, para o tipo de dado caracter
municipios$code_muni <- as.character(municipios$code_muni)

#Realiza uma junção à direita entre dois dataframes, FAepizootias e municipios, utilizando a correspondência 
#entre as colunas code_muni do dataframe FAepizootias e code_muni do dataframe municipios
FAepizootias<-right_join(FAepizootias, municipios,
                         by= c('code_muni'='code_muni'))

#Cria uma nova coluna chamada "classe" no dataframe FAepizootias. Se o ano de ocorrência (ANO_OCOR) for maior ou igual 
#a 2017, a classe é "Recentes"; caso contrário, é definida como "Antigos".
FAepizootias <- FAepizootias|>
  mutate(classe = case_when(
    ANO_OCOR >= 2017 ~ "Recentes",
    ANO_OCOR < 2017 ~ "Antigos"
  ))

#Desenvolve um mapa de epizootias de febre amarela no Brasil utilizando o pacote ggplot2 para visualização.
#As epizootias recentes (ocorridas desde 2017) são representadas em azul escuro, enquanto as epizootias mais antigas 
#(anteriores a 2017) são representadas em tons de laranja. O mapa também inclui linhas de contorno dos estados do Brasil.
#A legenda do mapa mostra o número de casos de epizootias para cada período de tempo.
Faepizootiasbr <- FAepizootias
Faepizootiasbr|>
  ggplot() +
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepizootias, classe == "Recentes"),
          linewidth=0.00000001,
          color="#adb5bd00",
          alpha = 0.6)+
  scale_fill_gradientn(colours = c("skyblue", "navyblue"),
                       guide="colorbar",na.value="white", name = 'Contagem de Óbitos \n 2017-2021') +
  new_scale_fill() +
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepizootias, classe == "Antigos"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#f9dc5c88","#ffa400bb")    ,
                       guide="colorbar",na.value="white", name = 'Contagem de Óbitos \n 1999-2016') +
  geom_sf(data=estados,
          fill='white',
          alpha= 0,
          linewidth=0.01,
          color='#000000',
          linetype=1)+
  labs(title = 'Mapa de epizootias \nFebre Amarela',
       caption = 'https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_epizpnh_1999-2021.csv') +
  theme_void()+
  theme(text = element_text(size = 25))

#Desenvolve o mesmo mapa de epizootias, mas com um filtro de acordo com as coordenadas 
Faepizootiaszoom <- FAepizootias
Faepizootiaszoom|>
  ggplot() +
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepizootias, classe == "Recentes"),
          linewidth=0.00000001,
          color="#adb5bd00", 
          alpha = 0.6)+
  scale_fill_gradientn(colours = c("skyblue", "navyblue"),
                       guide="colorbar",na.value="white", name = 'Contagem de Óbitos \n 2017-2021' ) +
  new_scale_fill()+
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepizootias, classe == "Antigos"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#f9dc5c88","#ffa400bb")    ,
                       guide="colorbar",na.value="white", name = 'Contagem de Óbitos\n 1999-2016') +
  geom_sf(data=estados,
          fill='white',
          alpha= 0,
          linewidth=0.01,
          color='#000000',
          linetype=1)+
  coord_sf(xlim = c(-55, -35), ylim = c(-35, -10)) +
  labs(title='Mapa de epizootias \nFebre Amarela',
       x='longitude',
       y='latitude',
       caption='https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_epizpnh_1999-2021.csv')+ 
  theme_classic()+
  theme(text = element_text(size = 25))


#Casos Humanos ====
#Converte a coluna COD_MUN_LPI, da base FAcasoshumanos, para o tipo de dado caracter
FAcasoshumanos$COD_MUN_LPI<-as.character(FAcasoshumanos$COD_MUN_LPI)

#Agrupa os dados por código do município, ano e ocorrência de óbito, e em seguida, calcula a contagem total de casos humanos 
#e a contagem de casos fatais
FAcasoshumanos<-FAcasoshumanos |>
  group_by(COD_MUN_LPI, ANO_IS, OBITO) |>  
  summarise(conthum=n(), 
            contobitos = sum(OBITO == "SIM")) 

#Realiza uma junção à esquerda entre dois dataframes, FAcasoshumanos e dicmunicipios, 
#utilizando a correspondência entre as colunas COD_MUN_LPI do dataframe FAcasoshumanos e ID_MUNICIP do dataframe dicmunicipios.
FAcasoshumanos<-left_join(FAcasoshumanos, dicmunicipios,
                           by = c('COD_MUN_LPI'='ID_MUNICIP'))

#Converte a coluna code_muni, da base FAcasoshumanos, para o tipo de dado caracter
FAcasoshumanos$code_muni<-as.character(FAcasoshumanos$code_muni)

#Realiza uma junção à direita entre dois dataframes, FAcasoshumanos e municipios, utilizando a correspondência 
#entre as colunas code_muni do dataframe FAcasoshumanos e code_muni do dataframe municipios
FAcasoshumanos<-right_join(FAcasoshumanos,municipios,
                            by= c('code_muni'='code_muni'))

#Cria uma nova coluna chamada "classe" no dataframe FAcasoshumanos. Se o ano de ocorrência (ANO_IS) for maior ou igual 
#a 2017, a classe é "Recentes"; caso contrário, é definida como "Antigos".
FAcasoshumanos <- FAcasoshumanos|>
  mutate(classe = case_when(
    ANO_IS >= 2017 ~ "Recentes",
    ANO_IS < 2017 ~ "Antigos"
  ))

#Desenvolve um mapa de casos humanos de febre amarela no Brasil utilizando o pacote ggplot2 para visualização.
#Os casos recentes (ocorridos desde 2017) são representadas em roxo, enquanto as epizootias mais antigas 
#(anteriores a 2017) são representadas em tons de verde. O mapa também inclui linhas de contorno dos estados do Brasil.
#A legenda do mapa mostra o número de casos em humanos para cada período de tempo.


#COMENTAR ESSA PARTE 
FAcasoshumanosbr <- FAcasoshumanos

muni_seat <- geobr::read_municipal_seat()|>
  mutate(code_muni = as.character(code_muni))

FAcasoshumanosbr_municipal_seat <- FAcasoshumanosbr|>
  group_by(code_muni, classe)|>
  summarise(obitos_municipal_seat = sum(contobitos))|>
  filter(obitos_municipal_seat > 0)

FAcasoshumanosbr_municipal_seat <- left_join(FAcasoshumanosbr_municipal_seat,
                                       muni_seat,
                                       by = c("code_muni"))

FAcasoshumanosbr_municipal_seat <- FAcasoshumanosbr_municipal_seat|>
  mutate(
    label = case_when(
      classe == "Recentes" ~ paste0(name_muni,
                                    ",\n",
                                    obitos_municipal_seat,
                                    " (2017-2021)"),
      classe == "Antigos" ~ paste0(name_muni,
                                   ",\n",
                                   obitos_municipal_seat,
                                   " (1994-2016)")),
    x_coord = st_coordinates(geom)[,1],
    y_coord = st_coordinates(geom)[,2])

FAcasoshumanosbr|>
  ggplot() +
  geom_sf(aes(fill=conthum, geometry=geom),
          filter(FAcasoshumanosbr, classe == "Recentes"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#d8b9d577", "#6a0dad"),
                       guide="colorbar",na.value="white", name = 'Número de casos \n 2017-2021') +
  new_scale_fill()+
  geom_sf(aes(fill=conthum,geometry=geom),
          filter(FAcasoshumanosbr, classe == "Antigos"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#95d5b277","#1b4332bb")    ,
                       guide="colorbar",na.value="white", name = 'Número de casos \n 1994-2017') +
  # new_scale_fill()+
  geom_sf(aes(geometry = geom, size = obitos_municipal_seat, color = classe),
          data = FAcasoshumanosbr_municipal_seat,
          alpha = 0.5)+
  scale_color_manual(values = c("#1b4332bb","#6a0dadbb"))+
  geom_sf(data=estados,
          fill='white',
          alpha= 0,
          linewidth=0.01,
          color='#000000',
          linetype=1)+
  geom_sf_label_repel(# https://ggrepel.slowkow.com/articles/examples
                      # https://github.com/yutannihilation/ggsflabel
    data = FAcasoshumanosbr_municipal_seat|>
      filter(obitos_municipal_seat>8),
    aes(x = x_coord,
        y = y_coord,
        geometry = geom,
        label = label),
    fill = '#ffffff',
    max.overlaps = 20,
    force = 1200,
    segment.inflect = FALSE,
    segment.square = TRUE,
    segment.curvature = 1,
    segment.shape = 0)+
  labs(title='Mapa de casos humanos \nFebre Amarela',
       size = "Óbitos por município",
       color = "Antes/depois de 2017",
       caption='https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_casoshumanos_1994-2021.csv')+ 
  theme_void() #+
  theme(text = element_text(size = 25))  



#Desenvolve o mesmo mapa de casos humanos, mas com um filtro de acordo com as coordenadas 
  FAcasoshumanosbr|>
    ggplot() +
    geom_sf(aes(fill=conthum, geometry=geom),
            filter(FAcasoshumanosbr, classe == "Recentes"),
            linewidth=0.00000001,
            color="#adb5bd00")+
    scale_fill_gradientn(colours = c("#d8b9d577", "#6a0dad"),
                         guide="colorbar",na.value="white", name = 'Número de casos \n 2017-2021') +
    new_scale_fill()+
    geom_sf(aes(fill=conthum,geometry=geom),
            filter(FAcasoshumanosbr, classe == "Antigos"),
            linewidth=0.00000001,
            color="#adb5bd00")+
    scale_fill_gradientn(colours = c("#95d5b277","#1b4332bb")    ,
                         guide="colorbar",na.value="white", name = 'Número de casos \n 1994-2017') +
    # new_scale_fill()+
    geom_sf(aes(geometry = geom, size = obitos_municipal_seat, color = classe),
            data = FAcasoshumanosbr_municipal_seat,
            alpha = 0.5)+
    scale_color_manual(values = c("#1b4332bb","#6a0dadbb"))+
    geom_sf(data=estados,
            fill='white',
            alpha= 0,
            linewidth=0.01,
            color='#000000',
            linetype=1)+
    coord_sf(xlim = c(-55, -35), ylim = c(-35, -10)) +
    labs(title='Mapa de casos humanos \nFebre Amarela',
         size = "Óbitos por município",
         color = "Antes/depois de 2017",
         caption='https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_casoshumanos_1994-2021.csv',
         x = "longitude",
         y = "latitude") + 
    theme_classic()
  theme(text = element_text(size = 25))  
  
  
  
  
  
  
  
  


