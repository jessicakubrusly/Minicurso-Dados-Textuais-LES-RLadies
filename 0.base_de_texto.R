

remotes::install_url(
  "https://github.com/sillasgonzaga/literaturaBR/archive/refs/heads/master.zip"
)

library(literaturaBR)
library(tidyverse)
library(dplyr)

data(package = "literaturaBR")


# Carregar Dom Casmurro
data("dom_casmurro")

# Examinar os dados
head(dom_casmurro)
dim(dom_casmurro)
glimpse(dom_casmurro)

bases <- data(package = "literaturaBR")$results
bases
bases[,"Item"]
dim(bases)
glimpse(bases)

#juntar tudo em uma base única
livros <- load_all_books() |>
  mutate(
    autor = case_when(
      book_name %in% c(
        "O Alienista",
        "Dom Casmurro",
        "Memórias Póstumas de Brás Cubas"
      ) ~ "Machado de Assis",
      grepl("Ateneu", book_name) ~ "Raul Pompeia",
      grepl("Cortiço", book_name) ~ "Aluísio Azevedo",
      grepl("A escrava Isaura", book_name) ~ "Bernardo Guimarães",
      grepl("Sargento de Milícias", book_name) ~
        "Manuel Antônio de Almeida",
      grepl("Noite na Taverna", book_name) ~
        "Álvares de Azevedo",
      TRUE ~ NA_character_
    )
  ) |>
  relocate(autor, .before = book_name)

# Conferir autores e livros
livros |>
  count(autor, book_name)
livros = livros |> select(-url)
colnames(livros) 
colnames(livros) = c("autor","livro","cap","paragrafo","texto")
glimpse(livros) 
saveRDS(livros,"livros.RDS")

library(httr2)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

importar_livro <- function(pagina, autor, livro) {
  
  resposta <- request("https://pt.wikisource.org/w/api.php") |>
    req_url_query(
      action = "parse",
      page = pagina,
      prop = "text",
      format = "json",
      formatversion = 2,
      redirects = 1
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  if (!is.null(resposta$error)) {
    stop("Página não encontrada: ", pagina)
  }
  
  html <- read_html(resposta$parse$text)
  
  paragrafos <- html |>
    html_elements(".mw-parser-output p") |>
    html_text2()
  
  tibble(
    autor = autor,
    livro = livro,
    cap = pagina,
    paragrafo = seq_along(paragrafos),
    texto = paragrafos
  ) |>
    filter(nchar(texto) > 30)
}




buscar_paginas <- function(termo, limite = 100) {
  
  resposta <- request("https://pt.wikisource.org/w/api.php") |>
    req_url_query(
      action = "query",
      list = "search",
      srsearch = paste0('intitle:"', termo, '"'),
      srnamespace = 0,
      srlimit = limite,
      format = "json"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  tibble(
    pagina = resposta$query$search$title
  )
}

livros |> select(autor,livro) |> 
  unique() |> group_by(autor) |> mutate(n=n())


au = "Aluísio Azevedo"
ti = "Casa de Pensão"

au = c(au,"Bernardo Guimarães")
ti = c(ti,"O Seminarista")

au = c(au,"José de Alencar")
ti = c(ti,"Iracema")

au = c(au,"Bernardo Guimarães")
ti = c(ti,"O Garimpeiro")

au = c(au,"Raul Pompeia")
ti = c(ti,"As Joias da Coroa")

au = c(au,"Álvares de Azevedo")
ti = c(ti,"Macário")

au = c(au,"Manuel Antônio de Almeida")
ti = c(ti,"Dois Amores")

au = c(au,"José de Alencar")
ti = c(ti,"Senhora")

au = c(au,"Júlia Lopes de Almeida")
ti = c(ti,"A Falência")

au = c(au,"Júlia Lopes de Almeida")
ti = c(ti,"A Intrusa")

au = c(au,"Lima Barreto")
ti = c(ti,"Triste Fim de Policarpo Quaresma")

au = c(au,"Lima Barreto")
ti = c(ti,"Clara dos Anjos")

au = c(au,"Eça de Queirós")
ti = c(ti,"O Primo Basílio")

au = c(au,"Eça de Queirós")
ti = c(ti,"Os Maias")

au = c(au,"Machado de Assis")
ti = c(ti,"Quincas Borba")

au = "Mário de Andrade"
ti = "Macunaíma"
#1928

au = c(au,"Júlia Lopes de Almeida")
ti = c(ti,"A Viúva Simões")
#1897

au = c(au,"Graciliano Ramos")
ti = c(ti,"Vidas Secas")
#1938

length(au)
length(ti)

glimpse(livros)


for(j in 3:length(au)){
  a = au[j]
  t = ti[j]
  
  pags = buscar_paginas(t)
  novo_livro = NULL
  
  for(i in 1:length(pags$pagina)){
    
    print(i)
    p <- pags$pagina[i]
    
    repeat {
      
      novo <- tryCatch(
        importar_livro(
          pagina = p,
          autor = a,
          livro = t
        ),
        error = function(e) {
          message("Erro na página ", p, ": ", conditionMessage(e))
          NULL
        }
      )
      
      # Se funcionou, sai do repeat
      if (!is.null(novo)) break
      
      # Se houve erro, aguarda 10 segundos e tenta novamente
      message("Aguardando 10 segundos...")
      Sys.sleep(10)
    }
    
    novo_livro <- rbind(novo_livro, novo)
    
    # Pequena pausa entre requisições
    Sys.sleep(2)
  }

  livros = livros |> rbind(novo_livro)
  print(dim(livros))
  saveRDS(livros,"livros.RDS")
}

livros = livros |> unique()
livros |> select(autor,livro) |> 
  unique() |> group_by(autor) |> mutate(n=n()) |> arrange(autor)
livros |> select(autor) |> 
  group_by(autor) |> 
  mutate(n=n()) |> unique() |> arrange(autor)

saveRDS(livros,"livros.RDS")

livros$texto = iconv(livros$texto, from = "", to = "UTF-8")
sum(!stringi::stri_enc_isutf8(livros$texto ),na.rm = T)
sum(is.na(!stringi::stri_enc_isutf8(livros$texto )))
indices = which(is.na(!stringi::stri_enc_isutf8(livros$texto )))
indices
# livros$texto[indices]
# livros = livros[-indices,]

saveRDS(livros,"livros.RDS")





glimpse(livros)
livros = livros |> mutate(id = 1:nrow(livros))
rownames(livros) = livros$id
livros <- livros |>
  mutate(
    ano = case_when(
      livro == "O Alienista" ~ 1881L,
      livro == "O Ateneu" ~ 1888L,
      livro == "O Cortiço" ~ 1890L,
      livro == "Dom Casmurro" ~ 1899L,
      livro == "A escrava Isaura" ~ 1875L,
      livro == "Memórias de um Sargento de Milícias" ~ 1852L,
      livro == "Memórias Póstumas de Brás Cubas" ~ 1880L,
      livro == "Noite na Taverna" ~ 1855L,
      livro == "Casa de Pensão" ~ 1884L,
      livro == "O Seminarista" ~ 1872L,
      livro == "Iracema" ~ 1865L,
      livro == "O Garimpeiro" ~ 1872L,
      livro == "As Joias da Coroa" ~ 1882L,
      livro == "Macário" ~ 1855L,
      livro == "Dois Amores" ~ 1861L,
      livro == "Senhora" ~ 1875L,
      livro == "A Falência" ~ 1901L,
      livro == "A Intrusa" ~ 1908L,
      livro == "Triste Fim de Policarpo Quaresma" ~ 1911L,
      livro == "Clara dos Anjos" ~ 1923L,
      livro == "O Primo Basílio" ~ 1878L,
      livro == "Os Maias" ~ 1888L,
      livro == "Quincas Borba" ~ 1886L,
      livro == "Macunaíma" ~ 1928L,
      livro == "A Viúva Simões" ~ 1897L,
      livro == "Vidas Secas" ~1938L,
      TRUE ~ NA_integer_
    )
  )
glimpse(livros)


saveRDS(livros,"livros.RDS")

