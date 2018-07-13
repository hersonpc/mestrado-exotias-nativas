###
## Carregando bibliotecas
###
library(lubridate)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library("ggpubr")
library(gridExtra)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')

###
## Buscar os dados
###
url.ensino.medio <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRgHvi_hXGAeHNeAmH9u2M29kPCGQq9XJy8uJLEr7uihbNltlTuN3zUNV2V13hgL29hRKrAwDp0TCnE/pub?output=csv"
url.ensino.medio.q_desc <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTcqFIlnjBAECvc7K0ydn1Gz2MiG324JBS-JfBZ70a0emfnVkAxfBXHujoEf83q9zO3YJ5Y0pwer2rm/pub?output=csv"

jogo.dados.em <- read.csv(url.ensino.medio, stringsAsFactors = FALSE, encoding = "UTF-8")
names(jogo.dados.em) <- c('data', 'codigo', 'municipio', 'escola', 'modelo', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8', 'q9', 'q10', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10')
jogo.dados.em$modelo <- as.numeric(substr(jogo.dados.em$modelo, 8,8))
head(jogo.dados.em)
turmas <- c(rep(1, 14-0), rep(2, 32-14), rep(3, 50-32), rep(4, 68-50), rep(5, 84-68),
            rep(6, 111-84), rep(7, 132-111), rep(8, 159-132), rep(9, 182-159), 
            rep(10, 204-182), rep(11, 227-204), rep(12, 247-227), rep(13, 268-247),
            rep(14, 283-268), rep(15, 308-283), rep(16, 332-308), rep(17, 367-332))
jogo.dados.em$turmas <- turmas
#dim(jogo.dados.em)

questionario.descritivo <- read.csv(url.ensino.medio.q_desc, stringsAsFactors = FALSE, encoding = "UTF-8")
names(questionario.descritivo) <- c('data', 'codigo', 'cidade', 'tipo.escola', 
                                    'nome.escola', 'sexo', 'idade',  'bairro', 
                                    'area', 'temp.residencia', 'q1', 'q2', 
                                    'ex.exo.1', 'ex.exo.2', 'ex.exo.3', 'q3', 
                                    'q4', 'q5', 'ex.nat.1', 'ex.nat.2', 'ex.nat.3', 
                                    'frequencia', 'fez.aula', 'flona', 'flona.visitou', 
                                    'ex.prot.1', 'ex.prot.2', 'ex.prot.3', 
                                    'nota')
head(questionario.descritivo)
jogo.dados.em <- merge(jogo.dados.em, 
      questionario.descritivo %>% 
          select(codigo, area, frequencia, fez.aula, flona) %>%
          mutate(fez.aula = ifelse(substr(fez.aula, 1, 1) == "S", "Sim", "Não") ), 
      by = "codigo")
#dim(jogo.dados.em)
###
## Validando as respostas pelo gabarito
###
validar_questionario_no_gabarito <- function(data) {
    if(!exists("gabarito")) {
        gabarito <- read.csv(file = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQqltencC-1KU1X4nz-CJsANjcne8yAu_9Y4D0aBbr_AA-FX1Y627CpsFEpLdBObmAKpsrITYutPrWg/pub?gid=0&single=true&output=csv', stringsAsFactors = FALSE)
        names(gabarito) <- c('modelo', 'q', 'r', 's', 'grupo')
    }
    inc <- function(x) {eval.parent(substitute(x <- x + 1))}
    
    gabarito_do_modelo_quest <- (gabarito %>% filter(modelo == data$modelo))
    
    questoes <- data %>% select(q1:q10)
    questoesOrigem <- data %>% select(s1:s10)
    acertosNativas <- 0
    acertosExoticas <- 0
    acertosOrigemNativas <- 0
    acertosOrigemExoticas <- 0
    grupoTipo <- data.frame(tipo = sort(unique(gabarito$grupo)), exotica = c(0), nativa = c(0)) 
    
    for(i in 1:10) {
        gabarito_questao <- gabarito_do_modelo_quest %>% filter(q == i)
        if(questoes[i] == as.character(gabarito_questao$r)) {
            if(gabarito_questao$s == "Sim") {
                inc(acertosNativas)
                inc(grupoTipo[grupoTipo$tipo == gabarito_questao$grupo,]$nativa)
            } else {
                inc(acertosExoticas)
                inc(grupoTipo[grupoTipo$tipo == gabarito_questao$grupo,]$exotica)
            }
        }
        if(questoesOrigem[i] == as.character(gabarito_questao$s)) {
            if(gabarito_questao$s == "Sim") {
                inc(acertosOrigemNativas)
            } else {
                inc(acertosOrigemExoticas)
            }
        }
    }
    #print(paste0("Acertou #", acertosNativas, " nativas, #", acertosExoticas, " exoticas"))
    return (data.frame(
        exoticas = c(acertosExoticas), 
        nativas = c(acertosNativas), 
        origem.exoticas = c(acertosOrigemExoticas), 
        origem.nativas = c(acertosOrigemNativas), 
        grupos = c(grupoTipo)
        ))
}

# Aplicação do questionário no gabarito
## Processar o gabarito sobre cada uma das respostas dos alunos...
for(i in 1:nrow(jogo.dados.em)) {
    row <- jogo.dados.em[i,]
    # do stuff with row
    x <- validar_questionario_no_gabarito(row)
    jogo.dados.em[i,'exoticas'] <- x$exoticas[1]
    jogo.dados.em[i,'nativas'] <- x$nativas[1]

    jogo.dados.em[i,'origem_exoticas'] <- x$origem.exoticas[1]
    jogo.dados.em[i,'origem_nativas'] <- x$origem.nativas[1]
    
    jogo.dados.em[i,'exotica_aves'] <- x[x$grupos.tipo == "Ave",]$grupos.exotica[1]
    jogo.dados.em[i,'exotica_invertebrado'] <- x[x$grupos.tipo == "Invertebrado",]$grupos.exotica[1]
    jogo.dados.em[i,'exotica_mamifero'] <- x[x$grupos.tipo == "Mamifero",]$grupos.exotica[1]
    jogo.dados.em[i,'exotica_peixe'] <- x[x$grupos.tipo == "Peixe",]$grupos.exotica[1]
    jogo.dados.em[i,'exotica_reptil'] <- x[x$grupos.tipo == "Réptil",]$grupos.exotica[1]
    
    jogo.dados.em[i,'nativa_aves'] <- x[x$grupos.tipo == "Ave",]$grupos.nativa[1]
    jogo.dados.em[i,'nativa_invertebrado'] <- x[x$grupos.tipo == "Invertebrado",]$grupos.nativa[1]
    jogo.dados.em[i,'nativa_mamifero'] <- x[x$grupos.tipo == "Mamifero",]$grupos.nativa[1]
    jogo.dados.em[i,'nativa_peixe'] <- x[x$grupos.tipo == "Peixe",]$grupos.nativa[1]
    jogo.dados.em[i,'nativa_reptil'] <- x[x$grupos.tipo == "Réptil",]$grupos.nativa[1]
}

head(jogo.dados.em)
#jogo.dados.em.limpo <- jogo.dados.em %>% select(codigo:modelo, exoticas:peixes)
jogo.dados.em.limpo <- jogo.dados.em %>% select(-data, -q1:-s10)
head(jogo.dados.em.limpo)
jogo.dados.em.p <- jogo.dados.em.limpo %>% 
    group_by(turmas) %>%
    summarise(qtde = n(),
              total_acertos_esperado = qtde * 5,
              p_exoticas = sum(exoticas) / total_acertos_esperado,
              p_nativas = sum(nativas) / total_acertos_esperado,
              p_origem_exoticas = sum(origem_exoticas) / total_acertos_esperado,
              p_origem_nativas = sum(origem_nativas) / total_acertos_esperado) %>%
    ungroup()

# d.turmas <- d %>% group_by(turma) %>% 
#     summarise(total_acertos_esperado = n() *2
#               , aves = sum(aves) / total_acertos_esperado
#               , mamiferos = sum(mamiferos) / total_acertos_esperado
#               , repteis = sum(repteis) / total_acertos_esperado
#               , invertebrados = sum(invertebrados) / total_acertos_esperado
#               , peixes = sum(peixes) / total_acertos_esperado) 


write.csv2(file = "/Users/hersonmelo/Desktop/mestrado-exoticas-nativas/r/clean_data.csv", jogo.dados.em.limpo, row.names = FALSE, fileEncoding = "UTF-8")

tidy <- jogo.dados.em.limpo %>% select(-codigo:-modelo, -area:-flona, -exoticas:-nativas, -origem_exoticas:-origem_nativas) %>% melt(id = "turmas") 
tidy <- cbind(tidy, colsplit(tidy$variable, "_", c("tipo", "especie"))) %>% select(turmas, especie, tipo, value)
head(tidy)
write.csv2(file = "/Users/hersonmelo/Desktop/mestrado-exoticas-nativas/r/tidy_data.csv", tidy, row.names = FALSE, fileEncoding = "UTF-8")

read.csv2("/Users/hersonmelo/Desktop/mestrado-exoticas-nativas/r/tidy_data.csv") %>% View

dados <- read.csv2("https://raw.githubusercontent.com/hersonpc/mestrado-exoticas-nativas/master/r/tidy_data.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
str(dados)
###
## Testando a normalidade dos dados
###
verifica_normalidade <- function(dados, variavelAgrupamento = "municipio") {
    # Compute descriptive statistics by groups
    stable <- desc_statby(dados, measure.var = "p_nativas", grps = variavelAgrupamento)
    stable <- stable[, c(variavelAgrupamento, "length", "mean", "sd")]
    # Summary table plot, medium orange theme
    stable.nat <- ggtexttable(stable, rows = NULL, theme = ttheme("mOrange"))
    
    stable2 <- desc_statby(dados, measure.var = "p_exoticas", grps = variavelAgrupamento)
    stable2 <- stable2[, c(variavelAgrupamento, "length", "mean", "sd")]
    # Summary table plot, medium orange theme
    stable.exo <- ggtexttable(stable2, rows = NULL, theme = ttheme("mOrange"))
    
    # Draw text
    test <- shapiro.test(dados$p_nativas)
    text.nat <- ggparagraph(text = paste0(test$method, "\n\np-value: ", test$p.value), face = "bold", size = 11, color = "black")
    p1 <- ggtexttable(paste0(test$method, "\np-valor: ", test$p.value), rows = NULL, theme = ttheme("mOrange"))
    
    test <- shapiro.test(dados$p_exoticas)
    text.exo <- ggparagraph(text = paste0(test$method, "\n\np-value: ", test$p.value), face = "italic", size = 11, color = "black")
    p2 <- ggtexttable(paste0(test$method, "\np-valor: ", test$p.value), rows = NULL, theme = ttheme("mOrange"))
    
    # Draw all
    densNat <- 
        dados %>% 
        ggdensity("p_nativas", fill = "municipio",
                  main = "Grafico de densidade dos acertos de espécies Nativas",
                  xlab = "Número de acertos")
    densExo <- 
        dados %>% 
        ggdensity("p_exoticas", fill = "municipio",
                  main = "Grafico de densidade dos acertos de espécies Exóticas",
                  xlab = "Número de acertos")
    #par(old.par)
    
    d1 <- densNat + annotation_custom(ggplotGrob(p1), xmin = 2, ymin = 0.26)
    d2 <- densExo + annotation_custom(ggplotGrob(p2), xmin = -3, ymin = 0.32)
    grid.arrange(d1, d2, stable.nat, stable.exo, #text.nat, text.exo,
                 ncol = 4, nrow = 4, 
                 layout_matrix = rbind(c(1,1,2,2), c(1,1,2,2), c(1,1,2,2), c(3,3,4,4))) #, c(5,3,6,4)
    
}

verifica_normalidade(jogo.dados.em.p, "turmas")

shapiro.test(jogo.dados.em.p$p_nativas)
shapiro.test(jogo.dados.em.p$p_exoticas)
shapiro.test(jogo.dados.em.p$p_origem_exoticas)
shapiro.test(jogo.dados.em.p$p_origem_nativas)
# shapiro.test(jogo.dados.em.p$p_nativas + jogo.dados.em.p$p_origem_nativas)
# shapiro.test(jogo.dados.em.p$p_exoticas + jogo.dados.em.p$p_origem_exoticas)
# shapiro.test(jogo.dados.em.p$p_nativas + jogo.dados.em.p$p_origem_nativas + jogo.dados.em.p$p_exoticas + jogo.dados.em.p$p_origem_exoticas)

rbind(
    data.frame(variavel = c("exotica"), valor = jogo.dados.em.limpo$p_exoticas),
    data.frame(variavel = c("nativa"), valor = jogo.dados.em.limpo$p_nativas),
    data.frame(variavel = c("origem_exotica"), valor = jogo.dados.em.limpo$p_origem_exoticas),
    data.frame(variavel = c("origem_nativa"), valor = jogo.dados.em.limpo$p_origem_nativas)
) %>%
    ggplot(aes(variavel, valor)) +
    geom_boxplot() +
    theme_bw() +
    labs(title = "Boxplot da distribuição dos acertos de espécies exoticas e nativas",
         x = "", y = "Número de acertos no questionário")
    
## Hipotese 1
var.test(jogo.dados.em$nativas, jogo.dados.em$exoticas, alternative = "two.sided")
t.test(jogo.dados.em$nativas, jogo.dados.em$exoticas, alternative = "two.sided", paired = F, var.equal = T)    
