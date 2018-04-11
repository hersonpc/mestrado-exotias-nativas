###
## Carregando bibliotecas
###
library(lubridate)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

###
## Preparando dados dos formularios de consulta
###
dados <- read.csv(file = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSqejm8BK7tvPHasgEcNu3BnlGmnT9HxDZlaBsiwuIb04zkKkaY6yrxd6TYTKkZO9jYkuaVFGzljPoH/pub?gid=718358497&single=true&output=csv', 
                  stringsAsFactors = FALSE, fileEncoding = 'UTF-8')
gabarito <- read.csv(file = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQqltencC-1KU1X4nz-CJsANjcne8yAu_9Y4D0aBbr_AA-FX1Y627CpsFEpLdBObmAKpsrITYutPrWg/pub?gid=0&single=true&output=csv', 
                     stringsAsFactors = FALSE, fileEncoding = 'UTF-8')

## Trocando o nome das colunas para facilitar o uso no R
names(dados) <- c('data', 'codigo', 'instituicao', 'curso', 'periodo', 'modelo', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8', 'q9', 'q10', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10')
names(gabarito) <- c('modelo', 'q', 'r', 's', 'grupo')

## Padronizar o campo modelo dos dados
dados$modelo <- dados$modelo %>% str_replace('Modelo ', '') %>% as.integer()

## Funcao para validar a resposta no gabarito
gabaritou_questao <- function(modeloQuestionario, numQuestao, letraResposta) {
    resposta_no_gabarito <- (gabarito %>% filter(modelo == modeloQuestionario, q == numQuestao))$r
    return (resposta_no_gabarito == as.character(letraResposta))
}

# 1 = Sim, 0 = Não
gabaritou_nativa <- function(modeloQuestionario, numQuestao, snResposta) {
    especie_nativa_no_gabarito <- (gabarito %>% filter(modelo == modeloQuestionario, q == numQuestao))$s
    return (
        ifelse(especie_nativa_no_gabarito == "Sim",
               ifelse((especie_nativa_no_gabarito == as.character(snResposta)), 'ns', 'nn'),
               ifelse((especie_nativa_no_gabarito == as.character(snResposta)), 'es', 'en')
               )
        )
}

gabarito_grupo <- function(modeloQuestionario, numQuestao) {
    grupo_no_gabarito <- (gabarito %>% filter(modelo == modeloQuestionario, q == numQuestao))$g
    return (grupo_no_gabarito)
}

## aplicar o gabarito nos dados coletados

for (i in seq(1:nrow(dados))) {
    formAtual <- dados[i,]
    # Determinando acerto ou erro
    dados[i,'qr1'] <- gabaritou_questao(formAtual$modelo, 1, formAtual$q1)
    dados[i,'qr2'] <- gabaritou_questao(formAtual$modelo, 2, formAtual$q2)
    dados[i,'qr3'] <- gabaritou_questao(formAtual$modelo, 3, formAtual$q3)
    dados[i,'qr4'] <- gabaritou_questao(formAtual$modelo, 4, formAtual$q4)
    dados[i,'qr5'] <- gabaritou_questao(formAtual$modelo, 5, formAtual$q5)
    dados[i,'qr6'] <- gabaritou_questao(formAtual$modelo, 6, formAtual$q6)
    dados[i,'qr7'] <- gabaritou_questao(formAtual$modelo, 7, formAtual$q7)
    dados[i,'qr8'] <- gabaritou_questao(formAtual$modelo, 8, formAtual$q8)
    dados[i,'qr9'] <- gabaritou_questao(formAtual$modelo, 9, formAtual$q9)
    dados[i,'qr10'] <- gabaritou_questao(formAtual$modelo, 10, formAtual$q10)

    # Determinando se especie nativa
    dados[i,'nat1'] <- gabaritou_nativa(formAtual$modelo, 1, formAtual$s1)
    dados[i,'nat2'] <- gabaritou_nativa(formAtual$modelo, 2, formAtual$s2)
    dados[i,'nat3'] <- gabaritou_nativa(formAtual$modelo, 3, formAtual$s3)
    dados[i,'nat4'] <- gabaritou_nativa(formAtual$modelo, 4, formAtual$s4)
    dados[i,'nat5'] <- gabaritou_nativa(formAtual$modelo, 5, formAtual$s5)
    dados[i,'nat6'] <- gabaritou_nativa(formAtual$modelo, 6, formAtual$s6)
    dados[i,'nat7'] <- gabaritou_nativa(formAtual$modelo, 7, formAtual$s7)
    dados[i,'nat8'] <- gabaritou_nativa(formAtual$modelo, 8, formAtual$s8)
    dados[i,'nat9'] <- gabaritou_nativa(formAtual$modelo, 9, formAtual$s9)
    dados[i,'nat10'] <- gabaritou_nativa(formAtual$modelo, 10, formAtual$s10)
    
    # determinar quantos acertos obteve na classificação entre exoticas e nativas
    nativatbl <- c(gabaritou_nativa(formAtual$modelo, 1, formAtual$s1), 
      gabaritou_nativa(formAtual$modelo, 2, formAtual$s2),
      gabaritou_nativa(formAtual$modelo, 3, formAtual$s3),
      gabaritou_nativa(formAtual$modelo, 4, formAtual$s4),
      gabaritou_nativa(formAtual$modelo, 5, formAtual$s5),
      gabaritou_nativa(formAtual$modelo, 6, formAtual$s6),
      gabaritou_nativa(formAtual$modelo, 7, formAtual$s7),
      gabaritou_nativa(formAtual$modelo, 8, formAtual$s8),
      gabaritou_nativa(formAtual$modelo, 9, formAtual$s9),
      gabaritou_nativa(formAtual$modelo, 10, formAtual$s10)
    ) %>% table
    
    dados[i,'acertou_exoticas'] <- ifelse(!is.na(nativatbl['es']),nativatbl['es'],0)
    dados[i,'acertou_nativas'] <- ifelse(!is.na(nativatbl['ns']),nativatbl['ns'],0)
    
    # Determinando grupo das especies
    dados[i,'g1'] <- gabarito_grupo(formAtual$modelo, 1)
    dados[i,'g2'] <- gabarito_grupo(formAtual$modelo, 2)
    dados[i,'g3'] <- gabarito_grupo(formAtual$modelo, 3)
    dados[i,'g4'] <- gabarito_grupo(formAtual$modelo, 4)
    dados[i,'g5'] <- gabarito_grupo(formAtual$modelo, 5)
    dados[i,'g6'] <- gabarito_grupo(formAtual$modelo, 6)
    dados[i,'g7'] <- gabarito_grupo(formAtual$modelo, 7)
    dados[i,'g8'] <- gabarito_grupo(formAtual$modelo, 8)
    dados[i,'g9'] <- gabarito_grupo(formAtual$modelo, 9)
    dados[i,'g10'] <- gabarito_grupo(formAtual$modelo, 10)
}



###
## Gerando graficos
###

acertostbl <- 
    dados %>%
        select(acertou_exoticas:acertou_nativas) %>%
        colSums() %>%
        melt() 
    #group_by(variable) %>%
    #summarise(total = sum(value)) %>%
    #ungroup()

acertostbl["acerto" ] <- rownames(acertostbl)

ggplot(acertostbl, aes(x = acerto, y = value)) +
    geom_col() +
    theme_bw() +
    labs(title = 'Quantidade de acertos entre espécies')


# Determinando a distribuição dos acertos dentre a turma
dados %>%
    select("Exóticas" = acertou_exoticas, "Nativas" = acertou_nativas) %>% 
    boxplot(main = "Distribuição dos acertos de classificação das espécies",
            xlab = 'Espécies', ylab = 'Número de acertos')

dados %>%
  select(curso, "Exóticas" = acertou_exoticas, "Nativas" = acertou_nativas) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value, fill = curso)) +
  geom_boxplot(show.legend = F) +
  facet_grid(~curso) +
  theme_bw() +
  labs(title = "Distribuição dos acertos de classificação das espécies por curso",
          x = 'Espécies', y = 'Número de acertos')

