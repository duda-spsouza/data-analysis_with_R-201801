# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)



# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
                              
subset_dicionario <- read_csv("C:/Users/alu201830146/data-analysis_with_R-201801/aula-05/data/ted_main.csv.gz")
head(subset_dicionario, 20);


# Visualize o resumo dos dados do dataframe. Verifique os m�?nimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

###---------duration-------###

min(subset_dicionario$duration)   
max(subset_dicionario$duration)
mean(subset_dicionario$duration)
median(subset_dicionario$duration)


###---------film_date-------###
min(subset_dicionario$film_date)   
max(subset_dicionario$film_date)
mean(subset_dicionario$film_date)
median(subset_dicionario$film_date)

###---------published_date-------###
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



# Visualize novamente o resumo dos dados do dataframe. Verifique os m�?nimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
  summary(subset_sem_nome)
  
  ###---------data_fil-------###
  
  min(subset_sem_nome$data_fil)   
  max(subset_sem_nome$data_fil)
  mean(subset_sem_nome$data_fil)
  median(subset_sem_nome$data_fil)
  
  ###---------publica_data-------###
  
  min(subset_sem_nome$publica_data)   
  max(subset_sem_nome$publica_data)
  mean(subset_sem_nome$publica_data)
  median(subset_sem_nome$publica_data)
  
  ###---------DURACAO-------###
  min(subset_sem_nome$duracao)   
  max(subset_sem_nome$duracao)
  mean(subset_sem_nome$duracao)
  median(subset_sem_nome$duracao)
  

# Verifique quais registros possuem a menor quantidade de l�?nguas. Corrija para que possuam no m�?nimo 1 idioma.

  subset_sem_nome %>% arrange(languages)

  subset_sem_nome %>% mutate(languages = (if_else(languages==0, 1L,languages)))
  
# Verifique os 15 registros com menor data de filmagem. 

  subset_sem_nome %>%
  mutate(min(data_fil))
  head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo



# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.




# Verifique novamente o resumo dos dados do dataframe




# Verifique os 10 registros com maior duração.




# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas




# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil




# Visualize os 10 quantis da quantidade de visualizações




# Compare as seguintes estat�?sticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de l�?nguas dos seguintes grupos:
#     * 10% de v�?deos com maior número de visualizações
#     * 10% de v�?deos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro




# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos v�?deos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de l�?nguas das apresentações
#   * o desvio padrão da quantidade de l�?nguas
#   * o coeficiente de variação da quantidade de l�?nguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de l�?nguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de l�?nguas




# Descarte os v�?deos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos v�?deos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




