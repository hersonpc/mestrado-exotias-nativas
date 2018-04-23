###
## Carregando bibliotecas
###
library(lubridate)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

###
## Preparando esup_dados dos formularios de consulta
###
esup_dados <- read.csv(file = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSqejm8BK7tvPHasgEcNu3BnlGmnT9HxDZlaBsiwuIb04zkKkaY6yrxd6TYTKkZO9jYkuaVFGzljPoH/pub?gid=718358497&single=true&output=csv', stringsAsFactors = FALSE)
esup_gabarito <- read.csv(file = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQqltencC-1KU1X4nz-CJsANjcne8yAu_9Y4D0aBbr_AA-FX1Y627CpsFEpLdBObmAKpsrITYutPrWg/pub?gid=0&single=true&output=csv', stringsAsFactors = FALSE)

## Trocando o nome das colunas para facilitar o uso no R
names(esup_dados) <- c('data', 'codigo', 'instituicao', 'curso', 'periodo', 'modelo', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8', 'q9', 'q10', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10')
names(esup_gabarito) <- c('modelo', 'q', 'r', 's', 'grupo')

## Padronizar o campo modelo dos esup_dados
esup_dados$modelo <- esup_dados$modelo %>% str_replace('Modelo ', '') %>% as.integer()

## Remove esup_dados incompletos da importação, caso existir
esup_dados <- esup_dados[complete.cases(esup_dados),]


###
## Determinar para cada aluno: 
##      1. A qtde de acertos e erros
##      2. A qtde de especies acertadas 
##      3. 
###

## Funcao para validar a resposta no esup_gabarito
esup_gabaritou_questao <- function(modeloQuestionario, numQuestao, letraResposta) {
    resposta_no_esup_gabarito <- (esup_gabarito %>% filter(modelo == modeloQuestionario, q == numQuestao))$r
    return (resposta_no_esup_gabarito == as.character(letraResposta))
}

# 1 = Sim, 0 = Não
esup_gabaritou_nativa <- function(modeloQuestionario, numQuestao, snResposta) {
    especie_nativa_no_esup_gabarito <- (esup_gabarito %>% filter(modelo == modeloQuestionario, q == numQuestao))$s
    return (
        ifelse(especie_nativa_no_esup_gabarito == "Sim",
               ifelse((especie_nativa_no_esup_gabarito == as.character(snResposta)), 'ns', 'nn'),
               ifelse((especie_nativa_no_esup_gabarito == as.character(snResposta)), 'es', 'en')
        )
    )
}

esup_gabarito_grupo <- function(modeloQuestionario, numQuestao) {
    grupo_no_esup_gabarito <- (esup_gabarito %>% filter(modelo == modeloQuestionario, q == numQuestao))$g
    return (grupo_no_esup_gabarito)
}

## aplicar o esup_gabarito nos esup_dados coletados

for (i in seq(1:nrow(esup_dados))) {
    formAtual <- esup_dados[i,]
    # Determinando acerto ou erro
    esup_dados[i,'qr1'] <- esup_gabaritou_questao(formAtual$modelo, 1, formAtual$q1)
    esup_dados[i,'qr2'] <- esup_gabaritou_questao(formAtual$modelo, 2, formAtual$q2)
    esup_dados[i,'qr3'] <- esup_gabaritou_questao(formAtual$modelo, 3, formAtual$q3)
    esup_dados[i,'qr4'] <- esup_gabaritou_questao(formAtual$modelo, 4, formAtual$q4)
    esup_dados[i,'qr5'] <- esup_gabaritou_questao(formAtual$modelo, 5, formAtual$q5)
    esup_dados[i,'qr6'] <- esup_gabaritou_questao(formAtual$modelo, 6, formAtual$q6)
    esup_dados[i,'qr7'] <- esup_gabaritou_questao(formAtual$modelo, 7, formAtual$q7)
    esup_dados[i,'qr8'] <- esup_gabaritou_questao(formAtual$modelo, 8, formAtual$q8)
    esup_dados[i,'qr9'] <- esup_gabaritou_questao(formAtual$modelo, 9, formAtual$q9)
    esup_dados[i,'qr10'] <- esup_gabaritou_questao(formAtual$modelo, 10, formAtual$q10)
    
    # Determinando se especie nativa
    esup_dados[i,'nat1'] <- esup_gabaritou_nativa(formAtual$modelo, 1, formAtual$s1)
    esup_dados[i,'nat2'] <- esup_gabaritou_nativa(formAtual$modelo, 2, formAtual$s2)
    esup_dados[i,'nat3'] <- esup_gabaritou_nativa(formAtual$modelo, 3, formAtual$s3)
    esup_dados[i,'nat4'] <- esup_gabaritou_nativa(formAtual$modelo, 4, formAtual$s4)
    esup_dados[i,'nat5'] <- esup_gabaritou_nativa(formAtual$modelo, 5, formAtual$s5)
    esup_dados[i,'nat6'] <- esup_gabaritou_nativa(formAtual$modelo, 6, formAtual$s6)
    esup_dados[i,'nat7'] <- esup_gabaritou_nativa(formAtual$modelo, 7, formAtual$s7)
    esup_dados[i,'nat8'] <- esup_gabaritou_nativa(formAtual$modelo, 8, formAtual$s8)
    esup_dados[i,'nat9'] <- esup_gabaritou_nativa(formAtual$modelo, 9, formAtual$s9)
    esup_dados[i,'nat10'] <- esup_gabaritou_nativa(formAtual$modelo, 10, formAtual$s10)
    
    nativatbl <- c(esup_gabaritou_nativa(formAtual$modelo, 1, formAtual$s1), 
                   esup_gabaritou_nativa(formAtual$modelo, 2, formAtual$s2),
                   esup_gabaritou_nativa(formAtual$modelo, 3, formAtual$s3),
                   esup_gabaritou_nativa(formAtual$modelo, 4, formAtual$s4),
                   esup_gabaritou_nativa(formAtual$modelo, 5, formAtual$s5),
                   esup_gabaritou_nativa(formAtual$modelo, 6, formAtual$s6),
                   esup_gabaritou_nativa(formAtual$modelo, 7, formAtual$s7),
                   esup_gabaritou_nativa(formAtual$modelo, 8, formAtual$s8),
                   esup_gabaritou_nativa(formAtual$modelo, 9, formAtual$s9),
                   esup_gabaritou_nativa(formAtual$modelo, 10, formAtual$s10)
    ) %>% table
    
    esup_dados[i,'acertou_exoticas'] <- ifelse(!is.na(nativatbl['es']),nativatbl['es'],0)
    esup_dados[i,'acertou_nativas'] <- ifelse(!is.na(nativatbl['ns']),nativatbl['ns'],0)
    
    # Determinando grupo das especies
    esup_dados[i,'g1'] <- esup_gabarito_grupo(formAtual$modelo, 1)
    esup_dados[i,'g2'] <- esup_gabarito_grupo(formAtual$modelo, 2)
    esup_dados[i,'g3'] <- esup_gabarito_grupo(formAtual$modelo, 3)
    esup_dados[i,'g4'] <- esup_gabarito_grupo(formAtual$modelo, 4)
    esup_dados[i,'g5'] <- esup_gabarito_grupo(formAtual$modelo, 5)
    esup_dados[i,'g6'] <- esup_gabarito_grupo(formAtual$modelo, 6)
    esup_dados[i,'g7'] <- esup_gabarito_grupo(formAtual$modelo, 7)
    esup_dados[i,'g8'] <- esup_gabarito_grupo(formAtual$modelo, 8)
    esup_dados[i,'g9'] <- esup_gabarito_grupo(formAtual$modelo, 9)
    esup_dados[i,'g10'] <- esup_gabarito_grupo(formAtual$modelo, 10)
    
    animaisAcertos <- data.frame("animal" = unique(esup_gabarito$grupo)
                                 , "acertos" = rep(0, length(unique(esup_gabarito$grupo)))
                                 , "erros" = rep(0, length(unique(esup_gabarito$grupo)))
    )
    for (questao in seq(1:10)) {
        grupoAnimal <- esup_gabarito_grupo(formAtual$modelo, questao)
        if (esup_gabaritou_questao(formAtual$modelo, questao, formAtual$q1)) {
            message(paste("+1 para", grupoAnimal))
            animaisAcertos[animaisAcertos$animal == grupoAnimal, ]$acertos <- 
                animaisAcertos[animaisAcertos$animal == grupoAnimal, ]$acertos + 1
            #aux <- esup_dados[i, grupoAnimal]
            #if(is.null(aux))
            #    aux <- 0
            #esup_dados[i, esup_gabarito_grupo(formAtual$modelo, questao)] <- aux + 1
        } else {
            message(paste("-1 para", grupoAnimal))
            animaisAcertos[animaisAcertos$animal == grupoAnimal, ]$erros <- 
                animaisAcertos[animaisAcertos$animal == grupoAnimal, ]$erros + 1
        }
    }
    # print(animaisAcertos)
}



###
## Gerando graficos
###


acertostbl <- 
    esup_dados %>%
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
esup_dados %>%
    select("Exóticas" = acertou_exoticas, "Nativas" = acertou_nativas) %>% 
    boxplot(main = "Distribuição dos acertos de classificação das espécies",
            xlab = 'Espécies', ylab = 'Número de acertos')

analise1a <- 
    rbind(
        esup_dados %>%
            select(instituicao, curso, periodo, "acertos" = acertou_exoticas) %>% 
            #mutate(tipo = "exoticas")
            group_by(instituicao, curso, periodo, acertos) %>%
            summarise(qtde = n(), tipo = "exoticas")
        
        ,
        esup_dados %>%
            select(instituicao, curso, periodo, "acertos" = acertou_nativas) %>% 
            #mutate(tipo = "nativas")
            group_by(instituicao, curso, periodo, acertos) %>%
            summarise(qtde = n(), tipo = "nativas")
    ) %>%
    select(instituicao, curso, periodo, tipo, acertos, qtde) %>%
    inner_join(
        analise1a %>%
            group_by(instituicao, curso) %>%
            summarise(tot = sum(qtde))
    ) %>%
    mutate(fr = qtde * 100 / tot)

analise1a %>%    
    ggplot(aes(acertos, fr, fill = tipo)) +
    facet_grid(curso+instituicao~tipo+periodo) +
    geom_col() +
    scale_x_continuous(breaks = 0:5) +
    theme_bw()


table(esup_dados$instituicao, esup_dados$curso)
table(esup_dados$curso, esup_dados$periodo)

ggplot(aes(curso, n(), fill = tipo)) +
    geom_col() 
