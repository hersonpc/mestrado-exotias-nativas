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
    
    # Identifica turmas
    turmas <- dados$jogo %>%
      mutate(turma = group_indices_(dados$jogo, .dots=c("instituicao", "curso", "periodo")) ) %>%
      select(codigo, turma)
    
    # Junta a informação das turmas com o resultado
    resultado <- merge(turmas, resultado, by = "codigo") %>% tbl_df
    
    return(resultado) 
}

extratifica_pesquisa <- function(resultado) {
  extrato_por_aluno <- 
    resultado %>% 
    mutate(ponto.nome.nativo = ifelse(nativo_sn == "Sim" & acertou.nome == 1, 1, 0),
           ponto.nome.exotico = ifelse(nativo_sn == "Não" & acertou.nome == 1, 1, 0),
           ponto.origem.nativo = ifelse(nativo_sn == "Sim" & acertou.origem == 1, 1, 0),
           ponto.origem.exotico = ifelse(nativo_sn == "Não" & acertou.origem == 1, 1, 0),
           ponto.indice.nativo = ifelse(ponto.nome.nativo == 1 & ponto.origem.nativo == 1, 1, 0),
           ponto.indice.exotico = ifelse(ponto.nome.exotico == 1 & ponto.origem.exotico == 1, 1, 0)
    ) %>%
    select(-grupo, -especie) %>% 
    group_by(turma, codigo) %>%
    summarise(t.nome.nativo = sum(ponto.nome.nativo),
              t.nome.exotico = sum(ponto.nome.exotico),
              t.origem.nativo = sum(ponto.origem.nativo),
              t.origem.exotico = sum(ponto.origem.exotico),
              t.indice.nativo = sum(ponto.indice.nativo),
              t.indice.exotico = sum(ponto.indice.exotico)
    ) %>%
    tbl_df
  
  extrato_por_turma <- 
    extrato_por_aluno %>%
    group_by(turma) %>%
    summarise(alunos = n(),
              acertos.esperados = (alunos * 5), # tanto para nome quanto para origem são 5 acertos por aluno
              tg.nome.nativo = sum(t.nome.nativo),
              tg.nome.exotico = sum(t.nome.exotico),
              tg.origem.nativo = sum(t.origem.nativo),
              tg.origem.exotico = sum(t.origem.exotico),
              tg.indice.nativo = sum(t.indice.nativo),
              tg.indice.exotico = sum(t.indice.exotico),
              p.nome.nativo = tg.nome.nativo / acertos.esperados,
              p.nome.exotico = tg.nome.exotico / acertos.esperados,
              p.origem.nativo = tg.origem.nativo / acertos.esperados,
              p.origem.exotico = tg.origem.exotico / acertos.esperados,
              p.indice.nativo = tg.indice.nativo / acertos.esperados,
              p.indice.exotico = tg.indice.exotico / acertos.esperados
    ) %>%
    tbl_df
  
  return(list(
    por_aluno = extrato_por_aluno,
    por_turma = extrato_por_turma
  ))
}

############################################################################################################################################
## Processando os dados
############################################################################################################################################
dados <- carregarDados()
resultado.pesquisa <- aplicar_gabarito(dados)
extrato.pesquisa <- extratifica_pesquisa(resultado.pesquisa)

############################################################################################################################################
## exportando os dados...
############################################################################################################################################
write.csv2(extrato.pesquisa$por_aluno, "extrato.por.aluno.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(extrato.pesquisa$por_turma, "extrato.por.turma.csv", fileEncoding = "UTF-8", row.names = FALSE)




############################################################################################################################################
# testando normalidade    
############################################################################################################################################

normalidade(extrato.pesquisa$por_turma$p.nome.nativo)
normalidade(extrato.pesquisa$por_turma$p.nome.exotico)
normalidade(extrato.pesquisa$por_turma$p.origem.nativo)
normalidade(extrato.pesquisa$por_turma$p.origem.exotico)
normalidade(extrato.pesquisa$por_turma$p.indice.nativo)
normalidade(extrato.pesquisa$por_turma$p.indice.exotico)


############################################################################################################################################
############################################################################################################################################
############################################################################################################################################

## Teste de hipotese 1
hipotese1 <- list(
  parametrico =
    t.test(extrato.pesquisa$por_turma$p.nome.exotico,
           extrato.pesquisa$por_turma$p.nome.nativo, 
           alternative = "two.sided", paired = TRUE, conf.level = .95),
  nao.parametrico =
    wilcox.test(
      extrato.pesquisa$por_turma$p.nome.exotico, 
      extrato.pesquisa$por_turma$p.nome.nativo, 
      alternative = "two.sided", paired = TRUE)
)

## Teste de hipotese 2
hipotese2 <- list(
  parametrico =
    t.test(extrato.pesquisa$por_turma$p.origem.exotico,
           extrato.pesquisa$por_turma$p.origem.nativo, 
           alternative = "two.sided", paired = TRUE, conf.level = .95),
  nao.parametrico =
    wilcox.test(
      extrato.pesquisa$por_turma$p.origem.exotico, 
      extrato.pesquisa$por_turma$p.origem.nativo, 
      alternative = "two.sided", paired = TRUE)
)
## Teste de hipotese 3
hipotese3 <- list(
  parametrico =
    t.test(extrato.pesquisa$por_turma$p.indice.exotico,
           extrato.pesquisa$por_turma$p.indice.nativo, 
           alternative = "two.sided", paired = TRUE, conf.level = .95),
  nao.parametrico =
    wilcox.test(
      extrato.pesquisa$por_turma$p.indice.exotico, 
      extrato.pesquisa$por_turma$p.indice.nativo, 
      alternative = "two.sided", paired = TRUE)
)


print(hipotese1)
print(hipotese2)
print(hipotese3)

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################