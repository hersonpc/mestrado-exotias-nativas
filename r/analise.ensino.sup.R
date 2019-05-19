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

ic.media <- function(vetorDados){
    ###
    ## Calculando o intervalo de confiança
    ###
    media <- mean(vetorDados) #média da amostra
    s <- sd(vetorDados) #desvio padrão
    n <- length(vetorDados) #tamanho da amostra
    erroPadrao <- qnorm(0.975)*s/sqrt(n)
    limite <- 1.96 * erroPadrao
    error.inferior <- media - erroPadrao
    error.superior <- media + erroPadrao
    limite.inferior <- media - limite
    limite.superior <- media + limite
    return(data.frame(
        media, 
        desvio = s, 
        erroPadrao, 
        error.inferior, error.superior, 
        limite.inferior, limite.superior))
}

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
            jogo.gabarito = read.csv(url$jogo.gabarito, stringsAsFactors = FALSE, encoding = "UTF-8") %>% select(modelo:especie) %>% tbl_df,
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
                   acertou.origem = ifelse(nativo_sn == aluno.origem, 1, 0),
                   acertou.ambos = ifelse(acertou.nome == 1 & acertou.origem == 1, 1, 0),
                   acertou.ave.nat = ifelse(grupo == "Ave" & nativo_sn == "Sim" & acertou.ambos == 1, 1, 0),
                   acertou.ave.exo = ifelse(grupo == "Ave" & nativo_sn == "Não" & acertou.ambos == 1, 1, 0),
                   acertou.inv.nat = ifelse(grupo == "Invertebrado" & nativo_sn == "Sim" & acertou.ambos == 1, 1, 0),
                   acertou.inv.exo = ifelse(grupo == "Invertebrado" & nativo_sn == "Não" & acertou.ambos == 1, 1, 0),
                   acertou.mam.nat = ifelse(grupo == "Mamifero" & nativo_sn == "Sim" & acertou.ambos == 1, 1, 0),
                   acertou.mam.exo = ifelse(grupo == "Mamifero" & nativo_sn == "Não" & acertou.ambos == 1, 1, 0),
                   acertou.pei.nat = ifelse(grupo == "Peixe" & nativo_sn == "Sim" & acertou.ambos == 1, 1, 0),
                   acertou.pei.exo = ifelse(grupo == "Peixe" & nativo_sn == "Não" & acertou.ambos == 1, 1, 0),
                   acertou.rep.nat = ifelse(grupo == "Réptil" & nativo_sn == "Sim" & acertou.ambos == 1, 1, 0),
                   acertou.rep.exo = ifelse(grupo == "Réptil" & nativo_sn == "Não" & acertou.ambos == 1, 1, 0)
            )
        
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
                  t.indice.exotico = sum(ponto.indice.exotico),
                  t.ave.nat = sum(acertou.ave.nat),
                  t.ave.exo = sum(acertou.ave.exo),
                  t.inv.nat = sum(acertou.inv.nat),
                  t.inv.exo = sum(acertou.inv.exo),
                  t.mam.nat = sum(acertou.mam.nat),
                  t.mam.exo = sum(acertou.mam.exo),
                  t.pei.nat = sum(acertou.pei.nat),
                  t.pei.exo = sum(acertou.pei.exo),
                  t.rep.nat = sum(acertou.rep.nat),
                  t.rep.exo = sum(acertou.rep.exo)
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
                  tg.ave.nat = sum(t.ave.nat),
                  tg.ave.exo = sum(t.ave.exo),
                  tg.inv.nat = sum(t.inv.nat),
                  tg.inv.exo = sum(t.inv.exo),
                  tg.mam.nat = sum(t.mam.nat),
                  tg.mam.exo = sum(t.mam.exo),
                  tg.pei.nat = sum(t.pei.nat),
                  tg.pei.exo = sum(t.pei.exo),
                  tg.rep.nat = sum(t.rep.nat),
                  tg.rep.exo = sum(t.rep.exo),
                  p.nome.nativo = tg.nome.nativo / acertos.esperados,
                  p.nome.exotico = tg.nome.exotico / acertos.esperados,
                  p.origem.nativo = tg.origem.nativo / acertos.esperados,
                  p.origem.exotico = tg.origem.exotico / acertos.esperados,
                  p.indice.nativo = tg.indice.nativo / acertos.esperados,
                  p.indice.exotico = tg.indice.exotico / acertos.esperados,
                  p.ave.nat = tg.ave.nat / alunos,
                  p.ave.exo = tg.ave.exo / alunos,
                  p.inv.nat = tg.inv.nat / alunos,
                  p.inv.exo = tg.inv.exo / alunos,
                  p.mam.nat = tg.mam.nat / alunos,
                  p.mam.exo = tg.mam.exo / alunos,
                  p.pei.nat = tg.pei.nat / alunos,
                  p.pei.exo = tg.pei.exo / alunos,
                  p.rep.nat = tg.rep.nat / alunos,
                  p.rep.exo = tg.rep.exo / alunos
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

## Teste de hipotese 1 ######################################################################################################
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

## Teste de hipotese 2 ######################################################################################################
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
## Teste de hipotese 3 ######################################################################################################
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

## Teste de hipotese 4 ######################################################################################################
plotAnaliseTaxonomicosGenovart <- function() {
    
    mediaExoticas <- mean(extrato.pesquisa$por_turma$p.nome.exotico)
    mediaNativas <- mean(extrato.pesquisa$por_turma$p.nome.nativo)
    
    tmp <- rbind(
        cbind(grupo = "Aves", origem = "exotica", ic.media(extrato.pesquisa$por_turma$p.ave.exo)),
        cbind(grupo = "Invertebrados", origem = "exotica", ic.media(extrato.pesquisa$por_turma$p.inv.exo)),
        cbind(grupo = "Mamíferos", origem = "exotica", ic.media(extrato.pesquisa$por_turma$p.mam.exo)),
        cbind(grupo = "Peixes", origem = "exotica", ic.media(extrato.pesquisa$por_turma$p.pei.exo)),
        cbind(grupo = "Répteis", origem = "exotica", ic.media(extrato.pesquisa$por_turma$p.rep.exo)),
        
        cbind(grupo = "Aves", origem = "nativa", ic.media(extrato.pesquisa$por_turma$p.ave.nat)),
        cbind(grupo = "Invertebrados", origem = "nativa", ic.media(extrato.pesquisa$por_turma$p.inv.nat)),
        cbind(grupo = "Mamíferos", origem = "nativa", ic.media(extrato.pesquisa$por_turma$p.mam.nat)),
        cbind(grupo = "Peixes", origem = "nativa", ic.media(extrato.pesquisa$por_turma$p.pei.nat)),
        cbind(grupo = "Répteis", origem = "nativa", ic.media(extrato.pesquisa$por_turma$p.rep.nat))
    )
    limitesEscala <- c(0,1)
    
    p <- 
        ggplot(tmp, aes(grupo, media, colour = origem)) +
        geom_crossbar(aes(ymin = error.inferior, ymax = error.superior), width = 0.1) +
        geom_errorbar(aes(ymin = limite.inferior, ymax = limite.superior), width = 0.1) +
        geom_point(aes(grupo, media)) +
        scale_y_continuous(limits = limitesEscala, labels = scales::percent) +
        labs(title = "Reconhecimento das espécies",
             x = "",
             y = "\nTaxa de respostas corretas\n",
             colour = 'Origem:') #+ 
        #my_theme #+ theme(legend.position="none")
    if(mediaExoticas > 0) {
        p <- 
            p +
            geom_hline(yintercept = mediaExoticas, linetype = 2,
                       colour="orangered")
    }
    if(mediaNativas > 0) {
        p <- 
            p +
            geom_hline(yintercept = mediaNativas, linetype = 2, 
                       colour="darkblue")
    }
    return(p)
}
anovaTaxonomicos <- function() {
    tmp <- extrato.pesquisa$por_turma %>%
        select(p.ave.nat:p.rep.exo) %>%
        melt() %>%
        tbl_df
    # anova(lm(value ~ variable, tmp))
    tmp <- cbind(tmp, colsplit(tmp$variable, "\\.", c("p", "origem", "grupo"))) %>%
        select(origem, grupo, value) %>%
        mutate(
            origem = ifelse(origem == "ave", "Ave", 
                            ifelse(origem == "inv", "Invertebrado",
                                   ifelse(origem == "mam", "Mamifero",
                                          ifelse(origem == "pei", "Peixe",
                                                 ifelse(origem == "rep", "Réptil", "Desconhecido"))))),
            grupo = ifelse(grupo == "nat", "Nativa", "Exótica"))
    return (list(anova = anova(lm(value ~ grupo * origem, tmp)),
                 tukey = TukeyHSD(aov(lm(value ~ grupo * origem, tmp))) )
            )
}
plotAnaliseTukey <- function(tukey) {
    tky <- as.data.frame(tukey$`grupo:origem`)
    tky <- tky %>%
        mutate(pair = reorder(rownames(tky), `diff`),
               signifcante = (`p adj` <= 0.05),
               cor = ifelse(`p adj` <= 0.05, "red", "black"))
    p <- ggplot(tky, aes(colour = cut(`p adj`, c(0, 0.05, 1), 
                                 label = c("Significante", "Não significante")))) +
        geom_hline(yintercept=0, lty="11", colour="grey30") +
        geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.5, size=1) +
        geom_point(aes(pair, diff), size=3, shape=21) +
        scale_color_manual(values = c("red", "black"))+
        coord_flip() +
        theme_bw() +
        theme(axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10), 
              legend.position = "top") +
        labs(title = "Grupo taxonómico x Origem das espécies",
             subtitle = "Análise Tukey",
             colour = "", x = "Comparação de grupos", y = "\nDiferença entre as médias")
    return(p)
}

h4_teste_hipotese <- anovaTaxonomicos()
hipotese4 <- list(
    grafico = plotAnaliseTaxonomicosGenovart(),
    teste = h4_teste_hipotese$anova,
    grafico.tukey = plotAnaliseTukey(h4_teste_hipotese$tukey)
)    


########################################################################################################################################


print(hipotese1)
print(hipotese2)
print(hipotese3)
print(hipotese4$grafico)
print(hipotese4$teste)
print(hipotese4$grafico.tukey)

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################