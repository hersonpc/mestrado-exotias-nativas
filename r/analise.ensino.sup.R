###
## Carregando bibliotecas
###
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(gridExtra)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')

carregarDados <- function() {
    url <- 
        list(
            jogo = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSqejm8BK7tvPHasgEcNu3BnlGmnT9HxDZlaBsiwuIb04zkKkaY6yrxd6TYTKkZO9jYkuaVFGzljPoH/pub?gid=718358497&single=true&output=csv",
            jogo.gabarito = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQqltencC-1KU1X4nz-CJsANjcne8yAu_9Y4D0aBbr_AA-FX1Y627CpsFEpLdBObmAKpsrITYutPrWg/pub?gid=0&single=true&output=csv",
            questionario = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQCqV4F4m1BA8dh3A6sjZX9YPgFmSQkImUWc8EDNE_oOd68-eqvikjvH5svJo5s0OQ-TCoCBU5nmz_F/pub?gid=1126355755&single=true&output=csv"
        )
    dados <- 
        list(
            jogo = read.csv(url$jogo, stringsAsFactors = FALSE, encoding = "UTF-8") %>% tbl_df,
            jogo.gabarito = read.csv(url$jogo.gabarito, stringsAsFactors = FALSE, encoding = "UTF-8") %>% tbl_df,
            questionario = read.csv(url$questionario, stringsAsFactors = FALSE, encoding = "UTF-8") %>% tbl_df
        )
    ###
    ## Ajustando estrutura dos dados do jogo de imagens...
    ###
    # padronizando o nome das variaveis
    names(dados$jogo) <- c('data', 'codigo', 'instituicao', 'curso', 'periodo', 'modelo', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8', 'q9', 'q10', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10')
    # redefinindo campos...
    dados$jogo <-
        dados$jogo %>%
        filter(!is.na(codigo)) %>%
        mutate(data = dmy_hms(data),
               modelo = as.numeric(substr(modelo, 8,8))
        )
    
    return(dados)
}

aplicar_gabarito <- function(dados) {

    resultado <- NULL
    grupos <- NULL
    nativas <- NULL
    especies <- NULL
    
    # percorrendo todas as respostas
    for(i in 1:nrow(dados$jogo)) {
        # selecionando a resposta da vez
        ficha <- dados$jogo[i,]
        
        # obtendo gabarito do modelo
        gabarito <- 
            dados$jogo.gabarito %>% 
            filter(modelo == ficha$modelo)
        
        # verificando se aluno acertou as perguntas
        gabarito$codigo <- ficha$codigo
        gabarito$aluno.nomes <- (ficha %>% select(q1:q10))[1,] %>% as.character()
        gabarito$aluno.origem <- (ficha %>% select(s1:s10))[1,] %>% as.character()
        gabarito <- 
            gabarito %>%
            mutate(acertou.nome = ifelse(resposta == aluno.nomes, 1, 0),
                   acertou.origem = ifelse(nativo_sn == aluno.origem, 1, 0))
        
        # acumula os resultado para retorno da funcao 
        resultado <- rbind(resultado, gabarito)
        
        # # determinando a quantidade de acertos por grupo...
        # grupos <- 
        #     rbind(grupos,
        #             gabarito %>%
        #             group_by(grupo) %>%
        #             summarise(acertos = sum(acertou)) %>%
        #             mutate(codigo = i) %>%
        #             select(codigo, variavel = grupo, acertos)
        #     )
        # 
        # # determinando a quantidade de acertos de nativas e exóticas...
        # nativas <- 
        #     rbind(nativas,
        #             gabarito %>%
        #             group_by(nativo_sn) %>%
        #             summarise(acertos = sum(acertou)) %>%
        #             mutate(codigo = i) %>%
        #             select(codigo, variavel = nativo_sn, acertos)
        #     )
        # 
        # # determinando a quantidade de acertos por especie...
        # especies <- 
        #     rbind(especies,
        #             gabarito %>%
        #             group_by(especie) %>%
        #             summarise(acertos = sum(acertou)) %>%
        #             mutate(codigo = i) %>%
        #             select(codigo, variavel = especie, acertos)
        #     )
    }
    return(resultado) 
    
    
    grupos <- resultado %>%
        group_by(codigo, nativo_sn) %>%
        summarise(acertos = sum(acertou.nome),
                  p.acertos = acertos / 5) %>%
        select(codigo, variavel = nativo_sn, acertos, p.acertos)
    
    
    turmas <- dados$jogo %>%
        mutate(turma = group_indices_(dados$jogo, .dots=c("instituicao", "curso", "periodo")) ) %>%
        select(codigo, turma)
    
    
    merge(turmas, resultado, by = "codigo") %>%
        mutate(nome = ifelse(nativo_sn == "Sim", "nativo", "exotico")) %>%
        group_by(turma, nome) %>%
        summarise(alunos = n_distinct(codigo),
                  acertos = sum(acertou.nome),
                  acertos.esperados = (alunos*5),
                  p.acertos = acertos / acertos.esperados)
}

dados <- carregarDados()

names(dados$jogo)



