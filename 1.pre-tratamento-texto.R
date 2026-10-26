library(tidyverse)
library(dplyr)

livros = readRDS("livros.RDS")

dim(livros)
glimpse(livros)

vazios = which(livros$texto == "")
vazios

x = livros$texto
i = 233
x[i]

#transforar tudo em minísculo
x = x |> tolower()
x[i]

#retirar os numeros
library(tm)
x = x |> removeNumbers()
x[i]

##############
#a funcao gsub
##############

# Remover caracteres que não são letras e substitui por espaço
x <- gsub("[^\\p{L}\\p{M}]+", " ", x, perl = TRUE)
x[i]

#identifica 1 ou mais espaços consecutivos e troca por espaço simples
x <- gsub("\\s+", " ", x)
x[i]


#carrega uma lista de stop words
stop_words_pt = readRDS("stop_words_pt.rds")
#carregar o pacote e arquivo responsavel pela lematizacao
library(udpipe)
#modelo_pt = udpipe_download_model(language = "portuguese")
ud_model = udpipe_load_model("portuguese-bosque-ud-2.5-191206.udpipe")



#para cada posicao de x queremos fazer o seguinte:
# - tirar stop words
# - levatizar
# - tirar stop words
# veja primeiro o exemplo para a posicao i, mas será feito para todas depois com um applay

library(stringr)

#1) separar o paragrafo em palavras
paragrafo = x[[i]]
(palavras = str_split(paragrafo, "\\s+"))
palavras = palavras[[1]]
#2) identificar e retirar as stop words
indices_stop = palavras %in% stop_words_pt
indices_stop
length(indices_stop)
length(palavras)
palavras_sem_stop <- palavras[!indices_stop]
#3)criar tabela de lema e trocar palavra pro lema
tabela_lemma = udpipe::udpipe_annotate(ud_model, x = palavras_sem_stop) |> 
  as.data.frame() |> select(token, lemma) |> rename(palavras = token)
head(tabela_lemma)
palavras_sem_stop
palavras_sem_stop_lemma = tabela_lemma$lemma
#4) identificar e retirar as stop words, novamente
indices_stop = palavras_sem_stop_lemma %in% stop_words_pt
palavras_sem_stop_lemma_sem_stop <- palavras_sem_stop_lemma[!indices_stop]
#5) juntar tudo em um paragrafo novamente
paragrafo_sem_stop_lemma_sem_stop = paste(palavras_sem_stop_lemma_sem_stop,collapse = " ")

#repetir os passos 1, 2, 3, 4 e 5 em todas as posicoes de x a partir de um applay

x_sem_stop_lemma_sem_stop = lapply(x,
  function(paragrafo){
    #1) separar o paragrafo em palavras
    palavras = str_split(paragrafo, "\\s+")
    palavras = palavras[[1]]
    #2) identificar e retirar as stop words
    indices_stop = palavras %in% stop_words_pt
    palavras_sem_stop <- palavras[!indices_stop]
    #3)criar tabela de lema e trocar palavra pro lema
    tabela_lemma = 
      udpipe::udpipe_annotate(ud_model, x = palavras_sem_stop) |> 
      as.data.frame() |> 
      select(token, lemma) |> 
      rename(palavras = token)
    palavras_sem_stop_lemma = tabela_lemma$lemma
    #4) identificar e retirar as stop words, novamente
    indices_stop = palavras_sem_stop_lemma %in% stop_words_pt
    palavras_sem_stop_lemma_sem_stop <- palavras_sem_stop_lemma[!indices_stop]
    #5) juntar tudo em um paragrafo novamente
    paragrafo_sem_stop_lemma_sem_stop = paste(palavras_sem_stop_lemma_sem_stop,collapse = " ")
    return(paragrafo_sem_stop_lemma_sem_stop)
  })

x_sem_stop_lemma_sem_stop = unlist(x_sem_stop_lemma_sem_stop)
x_sem_stop_lemma_sem_stop[i]
x[i]

livros$texto = x_sem_stop_lemma_sem_stop

vazios = which(livros$texto == "")
livros = livros[-vazios,]
glimpse(livros)

saveRDS(livros,"livros_fim_pre_tratamento.RDS")


