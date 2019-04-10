
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(stringi)
library(magrittr)
library(lubridate)

# Lendo os arquivos -------------------------------------------------------

##-- tweets desde o final de semana até 17h de 09/04
tweets <- read_csv("./tweets.csv")

##-- membros do primeiro escalao do governo com conta no Twitter
usuarios <- read_csv("./usuarios.csv")


# Checando se ha mencoes --------------------------------------------------

##-- checando tweets que contem certos termos
tweets %>% 
  mutate(pronunciamento = str_detect(tolower(stri_trans_general(texto, "Latin-ASCII")), 
                                     paste(c("80 ", "tiros", " rio", "rio ",
                                           "evaldo", "musico"), collapse="|"))) %>% 
  filter(pronunciamento) %>% 
  {. ->> tweets2}

##-- revisao manual dos tweets que contem os termos acima
print(tweets2$texto)

##-- como nenhum se refere ao episodio do fuzilamento, 
##-- pronuncimaneto, variel binaria, é 0 para todos os posts
tweets %<>%
  transform(pronunciamento = 0)
  

# Merge das tabelas -------------------------------------------------------

##-- como alguns ministros nao postaram nada no periodo
##-- é necessario pegar a lista de todos os ministros em outra fonte
tweets %<>%
  full_join(usuarios, by=c('conta'))

##-- na tabela tambem tem alguns dados que vao serr usados nos graficos

# Graficos ----------------------------------------------------------------

##-- Criando variavel de diferenca de horas
##-- Filtrando apenas posts apos 07/04/19, 15hrs
tweets %<>% 
  mutate(data = ymd_hms(data), 
         ocorrido = ymd_hms("2019-04-07 15:00:00"),
         diferenca = round(as.numeric((data - ocorrido)/60))) %>% 
  filter(diferenca >= 0 | is.na(diferenca))


##-- grafico de todos os posts
tweets %>% 
  group_by(usuario, ordem, diferenca) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(usuario, ordem), y=diferenca, fill=as.character(n))) +
  geom_tile(col="black") +
  scale_fill_brewer() +
  coord_flip()

##-- grafico postando no twitter
horas <- data.frame(hora=1:50, id=1)

tweets %>% 
  mutate(pronunciamento=0) %>% 
  distinct(usuario, ordem, pronunciamento) %>%
  mutate(pronunciamento = factor(pronunciamento, 
                                 levels=c("0", "1" , "2", "3"))) %>% 
  mutate(id=1) %>% 
  left_join(horas, by=c("id")) %>% 
  ggplot(aes(x=reorder(usuario, -ordem), y=hora, fill=pronunciamento)) +
  geom_tile(col="black") +
  scale_fill_manual(values = c("#E0E0E0", "#EE2C2C", "#CD2626", "#8B1A1A"),
                    "Número de pronunciamentos", drop=F) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  labs(title="Pronunciamento sobre o fuzilamento\nde um carro de família no Rio - Twitter",
       subtitle="Número de menções ao episódio a cada hora após o ocorrido") +
  theme(axis.title = element_text()) + ylab('Horas passadas') + xlab('')










