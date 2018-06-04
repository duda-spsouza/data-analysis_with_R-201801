# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)
library(stringr)



# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
                              
subset_dicionario <- read_csv("C:/Users/alu201830146/data-analysis_with_R-201801/aula-05/data/ted_main.csv.gz")
head(subset_dicionario, 20);


# Visualize o resumo dos dados do dataframe. Verifique os m??nimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
summary(subset_dicionario)
###---------duration-------###
min(subset_dicionario$comments) 
max(subset_dicionario$comments) 
mean(subset_dicionario$comments) 
median(subset_dicionario$comments) 

min(subset_dicionario$comments) 
max(subset_dicionario$comments) 
mean(subset_dicionario$comments) 
median(subset_dicionario$comments) 

min(subset_dicionario$duration) 
max(subset_dicionario$duration) 
mean(subset_dicionario$duration) 
median(subset_dicionario$duration) 

min(subset_dicionario$film_date) 
max(subset_dicionario$film_date) 
mean(subset_dicionario$film_date) 
median(subset_dicionario$film_date) 

min(subset_dicionario$languages) 
max(subset_dicionario$languages) 
mean(subset_dicionario$languages) 
median(subset_dicionario$languages) 

min(subset_dicionario$num_speaker) 
max(subset_dicionario$num_speaker) 
mean(subset_dicionario$num_speaker) 
median(subset_dicionario$num_speaker) 

min(subset_dicionario$published_date) 
max(subset_dicionario$published_date) 
mean(subset_dicionario$published_date) 
median(subset_dicionario$published_date) 

min(subset_dicionario$views) 
max(subset_dicionario$views) 
mean(subset_dicionario$views) 
median(subset_dicionario$views) 

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

subset_dicionario %>%
  mutate(duracao = as.duration(duration)) %>%
  mutate(data_fil = as_datetime(film_date)) %>%
  mutate(publica_data = as_datetime(published_date)) ->subset_datas_time


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

subset_datas_time %>%
  mutate(evento = factor(event)) %>%
  mutate(ocup = factor(speaker_occupation)) -> subset_event_ocup
  summary(subset_event_ocup)



# Retire do dataframe a variável name
  
    subset(subset_event_ocup, select = -name) -> subset_sem_nome



# Visualize novamente o resumo dos dados do dataframe. Verifique os m??nimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
  summary(subset_sem_nome)
  
  min(subset_sem_nome$duracao) 
  max(subset_sem_nome$duracao) 
  mean(subset_sem_nome$duracao) 
  median(subset_sem_nome$duracao) 
  
  min(subset_sem_nome$data_film) 
  max(subset_sem_nome$data_film) 
  mean(subset_sem_nome$data_film) 
  median(subset_sem_nome$data_film) 
  
  min(subset_sem_nome$data_public) 
  max(subset_sem_nome$data_public) 
  mean(subset_sem_nome$data_public) 
  median(subset_sem_nome$data_public) 
  

# Verifique quais registros possuem a menor quantidade de l??nguas. Corrija para que possuam no m??nimo 1 idioma.

  subset_sem_nome %>% arrange(languages)
  
  subset_sem_nome %>% mutate(linguagens = if_else( languages == 0, 1L, languages ))-> subset_minlang 
  
# Verifique os 15 registros com menor data de filmagem. 

  subset_minlang %>% arrange(data_film)%>%select(data_film)%>%head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

  subset_minlang %>% 
    group_by(year(data_film))%>%
    count() -> subset_pres_year
  subset_pres_year

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

quantile(subset_pres_year$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))


# Verifique novamente o resumo dos dados do dataframe

subset_pres_year%>%
  filter(n > quantile(subset_pres_year$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))[4][1])%>%
  pull(year(data_film)) -> anos


# Verifique os 10 registros com maior duração.

subset_pres_year %>% filter( year(data_film) %in% anos )


# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
summary(subset_pres_year)



# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil




# Visualize os 10 quantis da quantidade de visualizações

subset_pres_year %>%
  arrange(desc(duracao))%>%
  head(10)


# Existem apresentaÃ§Ãµes com duraÃ§Ã£o maior que 3 desvios padrÃ£o acima da mÃ©dia? Liste elas

subset_dicionario %>%
  filter(duration > (3*sd(duration))) %>%
  select( duration, event, film_date, main_speaker)


# Calcule os 4 quartis e o IQR da duraÃ§Ã£o das apresentaÃ§Ãµes. Liste as apresentaÃ§Ãµes cuja duraÃ§Ã£o supera 1.5 * o IQR + o terceiro quartil
quantile(subset_dicionario$duration)


# Visualize os 10 quantis da quantidade de visualizaÃ§Ãµes
quantile(subset_dicionario$views, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))


# Compare as seguintes estatÃ?sticas descritivas da quantidade de visualizaÃ§Ãµes:
#   * MÃ©dia e Mediana. Qual Ã© maior?
#   * Desvio Absoluto da Mediana e Desvio PadrÃ£o. Qual Ã© maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR Ã© maior que o Desvio Absoluto da Mediana?
#   * Com base na mÃ©dia e na mediana, e na razÃ£o entre o IQR e o Desvio Absoluto da Mediana, 
#     vocÃª conclui que as quantidades de visualizaÃ§Ã£o estÃ£o distribuidas de forma simÃ©trica em torno da mÃ©dia?

mean(subset_dicionario$views)
median(subset_dicionario$views)
# a media eh maior

(dam_visual <- median( abs( subset_dicionario$views - median( subset_dicionario$views ))))
sd(subset_dicionario$views)
# o Desvio Absoluto da Mediana eh maior

quantile(subset_dicionario$views)
IQR <- c(quantile(ted_main$views))[3] - c(quantile(subset_dicionario$views))[1]
IQR/dam_visual

# Calcule a mÃ©dia, o desvio padrÃ£o, a mediana e o IQR da quantidade de lÃ?nguas dos seguintes grupos:
#     * 10% de vÃ?deos com maior nÃºmero de visualizaÃ§Ãµes
#     * 10% de vÃ?deos com menor nÃºmero de visualizaÃ§Ãµes

subset_dicionario%>%
  arrange(desc(languages))%>%
  select(languages)%>%
  head(10) -> dez_maior_visual  

mean(dez_maior_visual$languages)
median(dez_maior_visual$languages)
(dam_visual <- median( abs( dez_maior_visual$languages - median( dez_maior_visual$languages ))))
sd(dez_maior_visual$languages)
IQR <- c(quantile(dez_maior_visual$languages))[3] - c(quantile(dez_maior_visual$languages))[1]

subset_dicionario %>%
  arrange(desclanguages) %>%
  select(languages)%>%
  head(10) -> dez_menor_visual  

mean(dez_menor_visual$languages)
median(dez_menor_visual$languages)
(dam_visual <- median( abs( dez_menor_visual$languages - median( dez_menor_visual$languages ))))
sd(dez_maior_visual$languages)
IQR <- c(quantile(dez_menor_visual$languages))[3] - c(quantile(dez_menor_visual$languages))[1]


# Determine a quantidade de apresentaÃ§Ãµes por evento cujo nome inicie com TED. Utilize a funÃ§Ã£o str_detect para este filtro

subset_dicionario %>%
  filter( str_detect(subset_dicionario$name, pattern = "TED"))%>%
  select(name, event) %>% head(10)

subset_dicionario %>%
  filter(str_detect(subset_dicionario$name, pattern = "TED"))%>%
  group_by(event)%>%
  summarise( qtde_apres = n())%>%
  ungroup()%>%
  arrange(desc(qtde_apres))%>%
  head(20)



# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizaÃ§Ãµes dos vÃ?deos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentaÃ§Ãµes resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicaÃ§Ã£o)
#   * a quantidade mÃ©dia de lÃ?nguas das apresentaÃ§Ãµes
#   * o desvio padrÃ£o da quantidade de lÃ?nguas
#   * o coeficiente de variaÃ§Ã£o da quantidade de lÃ?nguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÃÃES




# Calcule e classifique as seguintes correlaÃ§Ãµes
#     * Quantidade de visualizaÃ§Ãµes e Quantidade de lÃ?nguas
#     * Quantidade de visualizaÃ§Ãµes e DuraÃ§Ã£o
#     * Quantidade de visualizaÃ§Ãµes e Quantidade de ComentÃ¡rios
#     * Quantidade de ComentÃ¡rios e Quantidade de lÃ?nguas

library(ggcorrplot)

corr <-
  subset_dicionario %>% 
  select_if(is_numeric) %>%
  mutate( views = as.numeric(views)
          , languages = as.numeric(languages)) %>%
  select(views, languages) %>%
  cor() %>% 
  round(2)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)


corr <-
  subset_dicionario %>% 
  select_if(is_numeric) %>%
  mutate( views = as.numeric(views)
          , duration = as.numeric(duration)) %>%
  select(views, duration) %>%
  cor() %>% 
  round(2)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)


# Descarte os vÃ?deos cuja duraÃ§Ã£o seja maior que 3 desvios padrÃµes da mÃ©dia. Calcule novamente as 5 correlaÃ§Ãµes solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duraÃ§Ã£o dos vÃ?deos por ano de filmagem. Calcule a correlaÃ§Ã£o entre o ano e a mediana da duraÃ§Ã£o
# e interprete o resultado



# bibliotecas utilizadas
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")

library(magrittr)
library(Hmisc)

subset_dicionario %>%
  mutate(duracao =duration(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = seq( from = 1970, to = 2020, by = 5 )) +
  theme_bw()


ted_talks_recentes <- ted_main %>%
  mutate(data_film = as_datetime(film_date))%>%
  filter(data_film >= ymd(20050101)) %>%
  mutate(languages = if_else(languages == 0, 1L, languages))


ted_talks_recentes %>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = 2005:2017) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_talks_recentes %>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  stat_summary(fun.data = mean_sdl) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = -10, to = 60, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


# ATIVIDADE
# Repetir os gráficos de pontos e de sumário utilizando o ano de publicação no eixo x e a duração no eixo y.


subset_dicionario %>%
  mutate(duracao =as.numeric(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_public )) %>%
  ggplot( aes( x = year, y = duracao )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = seq( from = 2000, to = 2020, by = 1 )) +
  theme_bw()

#select(duracao,data_film,data_public,year)

subset_dicionario %>%
  mutate(duracao =as.numeric(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_public )) %>%
  ggplot( aes( x = year, y = duracao )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = 2000:2020) +
  labs( x = "Ano de publicacao"
        , y = "Duracao em segundos"
        , title = "Evolução da Duracao por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2000."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


subset_dicionario %>%
  mutate(duracao =as.numeric(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_public )) %>%
  ggplot( aes( x = year, y = duracao )) +
  stat_summary(fun.data = mean_sdl) +
  scale_x_continuous( breaks = 2000:2020) +
  scale_y_continuous( breaks = seq(from = 0, to = 3000, by = 500 )) +
  labs( x = "Ano de publicacao"
        , y = "Duracao em segundos"
        , title = "Evolução da Duracao por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2000."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()




#Gráficos de barras

ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  group_by(year) %>%
  summarise(sum_views = sum(views)) %>%
  ungroup() %>%
  ggplot( aes( x = year, y = sum_views )) +
  geom_col(fill="blue", alpha=0.6) +
  scale_x_continuous(breaks = 2005:2017) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  labs( x = "Ano de filmagem"
        , y = "Total de visualizações de apresentações"
        , title = "Exemplo com geom_col"
        , subtitle = "Exibição do total de visualizações de apresentações de um mesmo ano de filmagem") +
  theme_bw()

ggplot(ted_talks_recentes, aes( x = year( data_film ))) +
  geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq( from = 50, to = 300, by = 50 )) +
  labs( x = "Ano de filmagem"
        , y = "Total de apresentações publicadas"
        , title = "Exemplo com geom_bar" ) +
  theme_bw()

ted_talks_recentes %>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( ano = year( data_public ), mes = month( data_public, label = TRUE )) %>%
  ggplot(aes( x = ano, fill = mes )) +
  geom_bar( alpha=0.6, color="black" ) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq( from = 50, to = 300, by = 50 )) +
  labs( x = "Ano de filmagem"
        , y = "Total de apresentações"
        , fill = "Mês do ano"
        , title = "Publicações por mês em cada ano" ) +
  theme_bw()


ted_talks_recentes %>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( ano = year( data_public ), mes = month( data_public, label = TRUE )) %>%
  ggplot(aes( x = ano )) +
  geom_bar( alpha=0.6 ) +
  scale_x_continuous( breaks = 2005:2017 ) +
  facet_wrap (~ mes, ncol = 3 ) +
  labs( x = "Ano de filmagem"
        , y = "Total de apresentações"
        , fill = "Mês do ano"
        , title = "Publicações por mês em cada ano" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages, group = year )) +
  geom_boxplot() +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 100, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages, group = year )) +
  geom_jitter(alpha = .2, height = 0, width = 0.3) +
  geom_boxplot(outlier.color = "red", outlier.alpha = 0.8, alpha = 0.2) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 100, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  geom_jitter(alpha = .2, height = 0, width = 0.3) +
  stat_summary(fun.data = mean_sdl, color="red") +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = -10, to = 80, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação.\n O ponto é a média no ano e a barra vertical representa o intervalo de 2 desvios acima e abaixo da média."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()

ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  group_by(year) %>%
  mutate(low = mean(languages) - 2 * sd(languages), hi = mean(languages) + 2 * sd(languages)) %>%
  ungroup() %>%
  ggplot( aes( x = year, y = languages, ymin = low, ymax = hi )) +
  geom_ribbon(fill = "lightgray", alpha = 0.5) +
  geom_jitter(alpha = .2, height = 0, width = 0.5) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = -10, to = 80, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da quantidade de línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação.\n A faixa cinza correponde ao intervalo de 2 desvios padrão acima e abaixo da média, calculados ano a ano."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()

library(ggcorrplot)

corr <-
  ted_talks_recentes %>%
  select_if(is_numeric) %>%
  mutate( duration = as.numeric(duration)
          , published_date = as.numeric(published_date)
          , film_date = as.numeric(film_date)) %>%
  select(languages,views,comments,duration,num_speaker,film_date,published_date) %>%
  cor() %>% round(2)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)