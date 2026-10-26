library(tidyverse)
library(dplyr)
library(tidytext)

livros = readRDS("livros_fim_pre_tratamento.RDS")
glimpse(livros)

livros_tokens = livros |> select(id,autor,livro,texto) |> unnest_tokens(token, texto)
livros_tokens = tibble(livros_tokens)
livros_tokens

###########################
### Contagem palavras
###########################

### tirar o token nao para a analise unigram

livros_tokens = livros_tokens |> filter(token != "não")

contagem_tokens = livros_tokens |> 
  select(token) |>
  group_by(token) |> 
  mutate(n = n()) |>  
  ungroup() |> 
  arrange(desc(n)) |> 
  unique()

#######################################
### Palavras mais comuns em todo corpus
#######################################

top = 10

barplot(rev(contagem_tokens$n[1:top]),
        horiz = T,
        names.arg = rev(contagem_tokens$token[1:top]), 
        las=1,
        main="Contagem de Tokens no Corpus")

########################
### Nuvens de palavras
########################

library(wordcloud)

windows(100,100)
wordcloud(words = contagem_tokens$token,
          scale = c(3,0.3),
          freq = contagem_tokens$n,
          max.words = 100,
          colors = brewer.pal(6,"Dark2"),
          rot.per = 0.2)


########################################
#### Palavras positivas x negativas
########################################

#install.packages("lexiconPT")
library(tidytext)
library(lexiconPT)

# Carregar o léxico SentiLex
data("sentiLex_lem_PT02")
glimpse(sentiLex_lem_PT02)

table(sentiLex_lem_PT02$polarity)

sentiLex_lem_PT02 |> filter(!(polarity %in% c(-1,0,1) ))

token_polaridade = sentiLex_lem_PT02 |> 
  select(term,polarity) |> 
  rename(token = term) |> 
  mutate(polaridade = ifelse(polarity > 1, 1, ifelse(polarity < -1, -1, polarity))) |>
  select(token, polaridade) |> unique()

head(token_polaridade)
dim(token_polaridade)
length(token_polaridade$token)
length(unique(token_polaridade$token))

token_polaridade |> group_by(token) |> 
  mutate(rep = n()) |> 
  ungroup() |> 
  arrange(token) |>
  filter(rep>1)

which(token_polaridade$token == "ingenuo" & token_polaridade$polaridade  == 0)
token_polaridade = token_polaridade[-4149,]
which(token_polaridade$token == "vivido" & token_polaridade$polaridade  == 0)
token_polaridade = token_polaridade[-6944,]

glimpse(token_polaridade)
glimpse(contagem_tokens)
contagem_tokens_com_polaridade = contagem_tokens |> left_join(token_polaridade)


################################################
### Palavras mais comuns: positivas x negativas
################################################

contagem_positivos = 
  contagem_tokens_com_polaridade |> 
  filter(polaridade == 1) |> 
  select(-polaridade) |> 
  arrange(desc(n))
contagem_negativos = 
  contagem_tokens_com_polaridade |> 
  filter(polaridade == -1) |> 
  select(-polaridade)  |> 
  arrange(desc(n))

par(mfrow = c(1, 2), 
    oma = c(0, 0, 3, 0), 
    mar = c(5, 6, 4, 2))

max = max(contagem_positivos$n[1],contagem_negativos$n[1])
barplot(rev(contagem_positivos$n[1:top]),
        horiz = T,
        names.arg = rev(contagem_positivos$token[1:top]),
        las=1,
        main = "Positivos",
        xlab = "Freqência",
        xlim=c(0,max+1),
        col = "darkgreen")

barplot(rev(contagem_negativos$n[1:top]),
        horiz = T,
        names.arg = rev(contagem_negativos$token[1:top]),
        las=1,
        main = "Negativos",
        xlab = "Freqência",
        xlim=c(0,max+1),
        col = "brown")

mtext(paste("Termos mais frequentes - positivo x negativo"),
      side = 3, 
      outer = TRUE, cex = 2)



################################################
### Nuvens de palavras: positivas x negativas
################################################

library(wordcloud)

wordcloud(words = contagem_positivos$token,
          scale = c(3,0.3),
          freq = contagem_positivos$n,
          max.words = 50,
          random.order = F,
          rot.per = 0.2,
          colors = c("lightgreen","green4","darkgreen"),
          gridSize = 1
)

wordcloud(words = contagem_negativos$token,
          scale = c(3,0.3),
          freq = contagem_negativos$n,
          max.words = 50,
          random.order = F,
          rot.per = 0.2,
          colors = c("brown1","brown","brown4")
)


