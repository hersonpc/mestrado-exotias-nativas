###
## Carregando bibliotecas
###
library(lubridate)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(gridExtra)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')

importar_dados_ensino_medio <- function() {
    url.questionario <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRgHvi_hXGAeHNeAmH9u2M29kPCGQq9XJy8uJLEr7uihbNltlTuN3zUNV2V13hgL29hRKrAwDp0TCnE/pub?output=csv"
    url.questionario.descritivo <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTcqFIlnjBAECvc7K0ydn1Gz2MiG324JBS-JfBZ70a0emfnVkAxfBXHujoEf83q9zO3YJ5Y0pwer2rm/pub?output=csv"
    
    dados <- read.csv(url.questionario, stringsAsFactors = FALSE, encoding = "UTF-8")
    names(dados) <- c('data', 'codigo', 'municipio', 'escola', 'modelo', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8', 'q9', 'q10', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10')
    dados <- dados %>%
        tbl_df() %>%
        mutate(modelo = as.numeric(substr(modelo, 8,8)),
               turmas = c(rep(1, 14-0), rep(2, 32-14), rep(3, 50-32), rep(4, 68-50), rep(5, 84-68),
                          rep(6, 111-84), rep(7, 132-111), rep(8, 159-132), rep(9, 182-159), 
                          rep(10, 204-182), rep(11, 227-204), rep(12, 247-227), rep(13, 268-247),
                          rep(14, 283-268), rep(15, 308-283), rep(16, 332-308), rep(17, 367-332)))
    
    questionario.descritivo <- read.csv(url.questionario.descritivo, stringsAsFactors = FALSE, encoding = "UTF-8") %>% tbl_df()
    names(questionario.descritivo) <- c('data', 'codigo', 'cidade', 'tipo.escola', 
                                        'nome.escola', 'sexo', 'idade',  'bairro', 
                                        'area', 'temp.residencia', 'q1', 'q2', 
                                        'ex.exo.1', 'ex.exo.2', 'ex.exo.3', 'q3', 
                                        'q4', 'q5', 'ex.nat.1', 'ex.nat.2', 'ex.nat.3', 
                                        'frequencia', 'fez.aula', 'flona', 'flona.visitou', 
                                        'ex.prot.1', 'ex.prot.2', 'ex.prot.3', 
                                        'nota')
    resultado <- merge(dados, 
                       questionario.descritivo %>% 
                           select(codigo, sexo, idade, temp.residencia, area, bairro, nome.escola, frequencia, fez.aula, flona) %>%
                           mutate(fez.aula = ifelse(substr(fez.aula, 1, 1) == "S", "Sim", "Não") ), 
                       by = "codigo") %>%
        tbl_df()
    
    return(tbl_df(resultado))
}

validar_questionario_no_gabarito <- function(data) {
    if(!exists("gabarito")) {
        message("* Baixando o gabarito...                         ")
        url.gabarito <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQqltencC-1KU1X4nz-CJsANjcne8yAu_9Y4D0aBbr_AA-FX1Y627CpsFEpLdBObmAKpsrITYutPrWg/pub?gid=0&single=true&output=csv'
        gabarito <<- read.csv(file = url.gabarito, stringsAsFactors = FALSE, encoding = "UTF-8")
        gabarito <<- gabarito %>% mutate(nativo_sn = substr(nativo_sn, 1, 1)) %>% tbl_df()
    }
    inc <- function(x) {eval.parent(substitute(x <- x + 1))}
    
    gabarito_do_modelo_quest <- (gabarito %>% filter(modelo == data$modelo))
    
    respostaAluno <- 
        list(
            nome = data %>% select(q1:q10),
            origem = data %>% select(s1:s10) %>% mutate_all(funs(substr(., 1, 1)))
        )
    resumo <- 
        data.frame(
            nome_nativas = 0,
            nome_exoticas = 0,
            origem_nativas = 0,
            origem_exoticas = 0,
            nome_origem_nativas = 0,
            nome_origem_exoticas = 0
        )
    grupoTipo <- data.frame(tipo = sort(unique(gabarito$grupo)), exotica = c(0), nativa = c(0)) 
    grupoEspecies <- data.frame(especie = sort(unique(gabarito$especie)), acertos = c(0)) 
    
    # Iterando cada uma das questões...
    for(i in 1:10) {
        # Determiando gabarito para a questao...
        gabarito_questao <- gabarito_do_modelo_quest %>% filter(questao == i)
        animalNativo <- (gabarito_questao$nativo_sn == "S")
        
        # Determinando se aluno acertou...
        acertouNomeEspecie <- (respostaAluno$nome[[i]] == gabarito_questao$resposta)
        acertouOrigemEspecie <- (respostaAluno$origem[[i]] == gabarito_questao$nativo_sn)
        
        # Realizando as contabilizações dos acertos...
        if(acertouNomeEspecie) {
            if(animalNativo) {
                inc(resumo$nome_nativas)
                inc(grupoTipo[grupoTipo$tipo == gabarito_questao$grupo,]$nativa)
            } else {
                inc(resumo$nome_exoticas)
                inc(grupoTipo[grupoTipo$tipo == gabarito_questao$grupo,]$exotica)
            }
        }
        if(acertouOrigemEspecie) {
            if(animalNativo) {
                inc(resumo$origem_nativas)
            } else {
                inc(resumo$origem_exoticas)
            }
        }
        if(acertouNomeEspecie && acertouOrigemEspecie) {
            if(animalNativo) {
                inc(resumo$nome_origem_nativas)
            } else {
                inc(resumo$nome_origem_exoticas)
            }
            inc(grupoEspecies[grupoEspecies$especie == gabarito_questao$especie,]$acertos)
        }
    }
    
    resultados <- 
        list(
            resultados = data.frame(
                nativas = c(resumo$nome_nativas), 
                exoticas = c(resumo$nome_exoticas), 
                origem.nativas = c(resumo$origem_nativas), 
                origem.exoticas = c(resumo$origem_exoticas), 
                indice.nativas = c(resumo$nome_origem_nativas), 
                indice.exoticas = c(resumo$nome_origem_exoticas)
            ),
            grupos = grupoTipo,
            especies = grupoEspecies
        )
    return(resultados)
}

aplicar_gabarito_no_questionario <- function(dados) {
    message(paste0("Processando o(s) ", nrow(dados), " registro(s)."))
    tabela_especies <- data.frame(especie = c(), acertos = c(), codigo = c())
    for(i in 1:nrow(dados)) {
        row <- dados[i,]
        
        message(paste0("\t\tLendo registro ", i, " de ", nrow(dados), " - ", round(i * 100 / nrow(dados), 2), "%"),"\r", appendLF = FALSE)
        flush.console()
        x <- validar_questionario_no_gabarito(row)
        dados[i,'exoticas'] <- x$resultados$exoticas
        dados[i,'nativas'] <- x$resultados$nativas
        
        dados[i,'origem_exoticas'] <- x$resultados$origem.exoticas
        dados[i,'origem_nativas'] <- x$resultados$origem.nativas
        
        dados[i,'indice_exoticas'] <- x$resultados$indice.exoticas
        dados[i,'indice_nativas'] <- x$resultados$indice.nativas
        
        dados[i,'exotica_aves'] <- x$grupos[x$grupos$tipo == "Ave",]$exotica
        dados[i,'exotica_invertebrado'] <- x$grupos[x$grupos$tipo == "Invertebrado",]$exotica
        dados[i,'exotica_mamifero'] <- x$grupos[x$grupos$tipo == "Mamifero",]$exotica
        dados[i,'exotica_peixe'] <- x$grupos[x$grupos$tipo == "Peixe",]$exotica
        dados[i,'exotica_reptil'] <- x$grupos[x$grupos$tipo == "Réptil",]$exotica
        
        dados[i,'nativa_aves'] <- x$grupos[x$grupos$tipo == "Ave",]$nativa[1]
        dados[i,'nativa_invertebrado'] <- x$grupos[x$grupos$tipo == "Invertebrado",]$nativa[1]
        dados[i,'nativa_mamifero'] <- x$grupos[x$grupos$tipo == "Mamifero",]$nativa[1]
        dados[i,'nativa_peixe'] <- x$grupos[x$grupos$tipo == "Peixe",]$nativa[1]
        dados[i,'nativa_reptil'] <- x$grupos[x$grupos$tipo == "Réptil",]$nativa[1]
        
        tabela_especies <- rbind(tabela_especies, 
                                 x$especies  %>% 
                                     filter(acertos > 0) %>% 
                                     mutate(codigo = row$codigo))
    }
    message(paste0("\tProcesso finalizado                  "))
    flush.console()
    resultado <- list(
        dados = tbl_df(
            dados %>% 
                select(-data, -q1:-s10)
        ),
        especies = tbl_df(tabela_especies)
    )
    
    return(resultado)
}

computar_lista_especies <- function(lista_pos_gabarito) {
    # calculando total de cada modelos de provas
    total_dos_modelos_prova <- lista_pos_gabarito$dados %>%
        group_by(modelo) %>%
        summarise(totalModelo = n())
    
    tbl_especies <-
        merge(
            lista_pos_gabarito$especie %>%
                group_by(especie) %>%
                summarise(acertos = sum(acertos)), 
            gabarito %>% 
                select(modelo, especie, grupo, nativo_sn) %>%
                unique(),
            by = "especie")
    tbl_especies <-
        merge(
            tbl_especies,
            total_dos_modelos_prova,
            by = "modelo"
        ) %>%
        mutate(
            fr = acertos / totalModelo,
            especie = factor(especie, levels = especie[order(-fr)]),
            especieDesc = factor(especie, levels = especie[order(fr)]),
            origem = ifelse(nativo_sn == "S", "Nativo", "Exótico")
        ) %>%
        select(origem, grupo, especie, especieDesc, modelo, tot.modelo = totalModelo, qtde.acertos = acertos, fr) %>%
        arrange(desc(fr))
    
    return(tbl_df(tbl_especies))
}

gravar_arquivo <- function(dados, nomeArquivo) {
    filename <- nomeArquivo
    message(paste0("\tGravando o arquivo: ", filename))
    write.csv2(file = filename, 
               dados, row.names = FALSE, fileEncoding = "UTF-8")
}

computar_proporcoes <- function(lista_pos_gabarito) {
    grp_turma <- 
        lista_pos_gabarito$dados %>% 
        group_by(turmas) %>%
        summarise(qtde = n(),
                  total_acertos_esperado = qtde * 5,
                  p_exoticas = sum(exoticas) / total_acertos_esperado,
                  p_nativas = sum(nativas) / total_acertos_esperado,
                  p_origem_exoticas = sum(origem_exoticas) / total_acertos_esperado,
                  p_origem_nativas = sum(origem_nativas) / total_acertos_esperado,
                  p_indice_exoticas = sum(indice_exoticas) / total_acertos_esperado,
                  p_indice_nativas = sum(indice_nativas) / total_acertos_esperado) %>%
        ungroup() %>%
        tbl_df()
    
    grp_turma_sexo <- 
        lista_pos_gabarito$dados %>%
        group_by(turmas, sexo) %>%
        summarise(qtde = n(),
                  total_acertos_esperado = qtde * 5,
                  p_exoticas = sum(exoticas) / total_acertos_esperado,
                  p_nativas = sum(nativas) / total_acertos_esperado,
                  p_origem_exoticas = sum(origem_exoticas) / total_acertos_esperado,
                  p_origem_nativas = sum(origem_nativas) / total_acertos_esperado,
                  p_indice_exoticas = sum(indice_exoticas) / total_acertos_esperado,
                  p_indice_nativas = sum(indice_nativas) / total_acertos_esperado) %>%
        ungroup() %>%
        tbl_df()
    
    grp_turma_area <- 
        lista_pos_gabarito$dados %>%
        group_by(turmas, area) %>%
        summarise(qtde = n(),
                  total_acertos_esperado = qtde * 5,
                  p_exoticas = sum(exoticas) / total_acertos_esperado,
                  p_nativas = sum(nativas) / total_acertos_esperado,
                  p_origem_exoticas = sum(origem_exoticas) / total_acertos_esperado,
                  p_origem_nativas = sum(origem_nativas) / total_acertos_esperado,
                  p_indice_exoticas = sum(indice_exoticas) / total_acertos_esperado,
                  p_indice_nativas = sum(indice_nativas) / total_acertos_esperado) %>%
        ungroup() %>%
        tbl_df()
    
    resultado <- 
        list(
            por_turma = grp_turma,
            por_turma_sexo = grp_turma_sexo,
            por_turma_area = grp_turma_area
        )
    
    return(resultado)
}

computar_grupos_taxonomicos <- function(lista_pos_gabarito) {
    tbl_grupos <- 
        lista_pos_gabarito$dados %>% 
        group_by(turmas) %>%
        summarise(qtde = n(),
                  total_acertos_esperado = qtde,
                  exotica_aves = sum(exotica_aves) / total_acertos_esperado,
                  exotica_invertebrado = sum(exotica_invertebrado) / total_acertos_esperado,
                  exotica_mamifero = sum(exotica_mamifero) / total_acertos_esperado,
                  exotica_peixe = sum(exotica_peixe) / total_acertos_esperado,
                  exotica_reptil = sum(exotica_reptil) / total_acertos_esperado,
                  nativa_aves = sum(nativa_aves) / total_acertos_esperado,
                  nativa_invertebrado = sum(nativa_invertebrado) / total_acertos_esperado,
                  nativa_mamifero = sum(nativa_mamifero) / total_acertos_esperado,
                  nativa_peixe = sum(nativa_peixe) / total_acertos_esperado,
                  nativa_reptil = sum(nativa_reptil) / total_acertos_esperado) %>%
        ungroup() %>%
        select(-qtde, -total_acertos_esperado)
    
    tbl_grupos <- 
        tbl_grupos %>%
        melt(id = "turmas") 
    
    resultado <-
        tbl_df(
            cbind(tbl_grupos, colsplit(tbl_grupos$variable, "_", c("origem", "grupo"))) %>% 
                select(turmas, grupo, origem, valor = value)
        )
    
    return(resultado)
}

# Aplicação do questionário no gabarito
## Processar o gabarito sobre cada uma das respostas dos alunos...
if(!exists("pos_gabarito")) {
    dados.importados <- importar_dados_ensino_medio()
    pos_gabarito <- aplicar_gabarito_no_questionario(dados.importados)
    pos_gabarito$especies <- computar_lista_especies(pos_gabarito)
    pos_gabarito$proporcoes <- computar_proporcoes(pos_gabarito)
    pos_gabarito$taxonomicos <- computar_grupos_taxonomicos(pos_gabarito)
    
    # setwd("F:/gdrive/code/mestrados/mestrado-exoticas-nativas/r")
    # setwd("/Users/hersonmelo/Desktop/mestrado-exoticas-nativas/r")
    save(pos_gabarito, file = "dados.RData")
    # para recuperar variavel do arquivo *.RData use: load("dados.RData")
    
    gravar_arquivo(pos_gabarito$dados, "dados.brutos.csv")
    gravar_arquivo(pos_gabarito$especie_comp, "dados.animais.csv")
    gravar_arquivo(pos_gabarito$proporcoes$por_turma, "dados.clean.csv")
    gravar_arquivo(pos_gabarito$taxonomicos, "dados.taxonomicos.csv")
}

# 
# pos_gabarito$dados %>%
#     group_by(turmas, sexo) %>%
#     summarise(qtde = n(),
#               total_acertos_esperado = qtde * 5,
#               p_exoticas = sum(exoticas) / total_acertos_esperado,
#               p_nativas = sum(nativas) / total_acertos_esperado,
#               p_origem_exoticas = sum(origem_exoticas) / total_acertos_esperado,
#               p_origem_nativas = sum(origem_nativas) / total_acertos_esperado,
#               p_indice_exoticas = sum(indice_exoticas) / total_acertos_esperado,
#               p_indice_nativas = sum(indice_nativas) / total_acertos_esperado) %>%
#     ungroup()

