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
                                        'x', 'nota')
    animais_exoticos = trimws(c(questionario.descritivo$ex.exo.1, 
                                questionario.descritivo$ex.exo.2, 
                                questionario.descritivo$ex.exo.3)) %>% 
                        as.data.frame() %>% 
                        filter(. != "") %>%
                        tbl_df()
    names(animais_exoticos) <- c("animais")
    animais_exoticos <- 
        animais_exoticos %>% 
        group_by(animais) %>% 
        summarise(freq = n()) %>% 
        arrange(desc(freq)) %>%
        mutate(animais = factor(animais, levels = animais))

    animais_nativos = trimws(c(questionario.descritivo$ex.nat.1, 
                               questionario.descritivo$ex.nat.2, 
                               questionario.descritivo$ex.nat.3)) %>% 
                        as.data.frame() %>% 
                        filter(. != "") %>%
                        tbl_df()
    names(animais_nativos) <- c("animais")
    animais_nativos <- 
        animais_nativos %>%
        group_by(animais) %>%
        summarise(freq = n()) %>%
        arrange(desc(freq)) %>%
        mutate(animais = factor(animais, levels = animais))

    animais_protegidos = trimws(c(questionario.descritivo$ex.prot.1, 
                                questionario.descritivo$ex.prot.2, 
                                questionario.descritivo$ex.prot.3)) %>% 
        as.data.frame() %>% 
        filter(. != "") %>%
        tbl_df()
    names(animais_protegidos) <- c("animais")
    animais_protegidos <- 
        animais_protegidos %>% 
        group_by(animais) %>% 
        summarise(freq = n()) %>% 
        arrange(desc(freq)) %>%
        mutate(animais = factor(animais, levels = animais))
    
    resultado <- merge(dados, 
                    questionario.descritivo %>% 
                       select(codigo, sexo, idade, temp.residencia, area, 
                              bairro, nome.escola, frequencia, fez.aula, flona, nota) %>%
                       mutate(aula = ifelse(substr(fez.aula, 1, 1) == "S", "Sim", "Não"),
                              nota = as.numeric(str_replace_all(nota, ",", "."))), 
                    by = "codigo") %>%
                    tbl_df()
    
    return(list(
        respostas = resultado,
        raking_animais = list(
            nativos = animais_nativos,
            exoticos = animais_exoticos,
            protegidos = animais_protegidos
        )))
}

gabarito_questionario <- function() {
    if(!exists("gabarito")) {
        message("* Baixando o gabarito...                         ")
        url.gabarito <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQqltencC-1KU1X4nz-CJsANjcne8yAu_9Y4D0aBbr_AA-FX1Y627CpsFEpLdBObmAKpsrITYutPrWg/pub?gid=0&single=true&output=csv'
        gabarito <<- read.csv(file = url.gabarito, stringsAsFactors = FALSE, encoding = "UTF-8")
        gabarito <<- gabarito %>% mutate(nativo_sn = substr(nativo_sn, 1, 1)) %>% tbl_df()
    }
    return(gabarito)    
}

validar_questionario_no_gabarito <- function(data) {
    gabarito <- gabarito_questionario()
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
        percentual_acerto = 
            dados %>%
            mutate(total_acertos_esperado = 5,
                   total_acertos_esperado_grupos_taxonomicos = 1,
                   p_nome_exoticas = (exoticas) / total_acertos_esperado,
                   p_nome_nativas = (nativas) / total_acertos_esperado,
                   p_origem_exoticas = (origem_exoticas) / total_acertos_esperado,
                   p_origem_nativas = (origem_nativas) / total_acertos_esperado,
                   p_indice_exoticas = (indice_exoticas) / total_acertos_esperado,
                   p_indice_nativas = (indice_nativas) / total_acertos_esperado,
                   p_exotica_aves = exotica_aves / total_acertos_esperado_grupos_taxonomicos,
                   p_exotica_invertebrado = exotica_invertebrado / total_acertos_esperado_grupos_taxonomicos,
                   p_exotica_mamifero = exotica_mamifero / total_acertos_esperado_grupos_taxonomicos,
                   p_exotica_peixe = exotica_peixe / total_acertos_esperado_grupos_taxonomicos,
                   p_exotica_reptil = exotica_reptil / total_acertos_esperado_grupos_taxonomicos,
                   p_nativa_aves = nativa_aves / total_acertos_esperado_grupos_taxonomicos,
                   p_nativa_invertebrado = nativa_invertebrado / total_acertos_esperado_grupos_taxonomicos,
                   p_nativa_mamifero = nativa_mamifero / total_acertos_esperado_grupos_taxonomicos,
                   p_nativa_peixe = nativa_peixe / total_acertos_esperado_grupos_taxonomicos,
                   p_nativa_reptil = nativa_reptil / total_acertos_esperado_grupos_taxonomicos) %>%
            select(codigo, municipio, turmas, area, sexo, frequencia, 
                   fez.aula, flona, nota, p_nome_exoticas:p_nativa_reptil) %>%
            tbl_df(),
        especies = tbl_df(tabela_especies)
    )
    
    return(resultado)
}

computar_lista_especies <- function(lista_pos_gabarito) {
    # calculando total de cada modelos de provas
    total_dos_modelos_prova <- lista_pos_gabarito$dados %>%
        group_by(modelo) %>%
        summarise(totalModelo = n())
    
    # tbl_especies1 <-
    #     merge(
    #         lista_pos_gabarito$especies, 
    #         gabarito_questionario() %>% 
    #             select(modelo, especie, grupo, nativo_sn) %>%
    #             unique(),
    #         by = "especie")
    tbl_especies <-
        merge(
            lista_pos_gabarito$especies %>%
                group_by(especie) %>%
                summarise(acertos = sum(acertos)), 
            gabarito_questionario() %>% 
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
    # dadosGeral <- 
        # lista_pos_gabarito$dados %>%
        # mutate(total_acertos_esperado = 5,
        #         p_nome_exoticas = (exoticas) / total_acertos_esperado,
        #         p_nome_nativas = (nativas) / total_acertos_esperado,
        #         p_origem_exoticas = (origem_exoticas) / total_acertos_esperado,
        #         p_origem_nativas = (origem_nativas) / total_acertos_esperado,
        #         p_indice_exoticas = (indice_exoticas) / total_acertos_esperado,
        #         p_indice_nativas = (indice_nativas) / total_acertos_esperado) %>%
        # select(codigo, municipio, turmas, area, sexo, frequencia, fez.aula, p_nome_exoticas:p_indice_nativas) %>%
        # tbl_df() %>%
        # View()
    
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
    
    grp_turma_municipio <- 
        lista_pos_gabarito$dados %>%
        group_by(turmas, municipio) %>%
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
        group_by(turmas, municipio, area) %>%
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

    grp_turma_freq_contato <- 
        lista_pos_gabarito$dados %>%
        group_by(turmas, frequencia) %>%
        summarise(qtde = n(),
                  total_acertos_esperado = qtde * 5,
                  p_exoticas = sum(exoticas) / total_acertos_esperado,
                  p_nativas = sum(nativas) / total_acertos_esperado,
                  p_origem_exoticas = sum(origem_exoticas) / total_acertos_esperado,
                  p_origem_nativas = sum(origem_nativas) / total_acertos_esperado,
                  p_indice_exoticas = sum(indice_exoticas) / total_acertos_esperado,
                  p_indice_nativas = sum(indice_nativas) / total_acertos_esperado) %>%
        ungroup() %>%
        mutate(
            frequencia = factor(frequencia, levels = c("Nunca", "Raramente", "Ás vezes", "Frequentemente", "Sempre"))
        ) %>%
        tbl_df()
    
    grp_turma_aula <- 
        lista_pos_gabarito$dados %>%
        group_by(turmas, aula) %>%
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
    
    grp_turma_fez_aula <- 
        lista_pos_gabarito$dados %>%
        group_by(turmas, fez.aula) %>%
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
    
    grp_turma_flona <- 
        lista_pos_gabarito$dados %>%
        filter(municipio == "Silvânia") %>% # Filtrando apenas Silvania
        group_by(turmas, flona) %>%
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
            por_municipio = grp_turma_municipio,
            por_turma_area = grp_turma_area,
            por_turma_freq_contato = grp_turma_freq_contato,
            por_turma_aula = grp_turma_aula,
            por_turma_fez_aula = grp_turma_fez_aula,
            por_turma_flona = grp_turma_flona
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
                select(turmas, grupo, origem, value)
        )
    
    return(resultado)
}

# Aplicação do questionário no gabarito
## Processar o gabarito sobre cada uma das respostas dos alunos...
if(!exists("pos_gabarito")) {
    dados_importados <- importar_dados_ensino_medio()
    pos_gabarito <- aplicar_gabarito_no_questionario(dados_importados$respostas)
    pos_gabarito_bkp <- pos_gabarito
    pos_gabarito$especiesBruto <- pos_gabarito$especies
    pos_gabarito$especies <- computar_lista_especies(pos_gabarito)
    pos_gabarito$proporcoes <- computar_proporcoes(pos_gabarito)
    pos_gabarito$taxonomicos <- computar_grupos_taxonomicos(pos_gabarito)
    pos_gabarito$raking_animais <- dados_importados$raking_animais
    
    # setwd("F:/gdrive/code/mestrados/mestrado-exoticas-nativas/r")
    setwd("/Users/hersonmelo/Desktop/mestrado-exoticas-nativas/r")
    # knit("analise.ensino.medio.Rmd", output = "README.md")
    save(pos_gabarito, file = "dados.RData")
    # para recuperar variavel do arquivo *.RData use: load("dados.RData")
    
    gravar_arquivo(pos_gabarito$dados, "dados.brutos.csv")
    gravar_arquivo(pos_gabarito$percentual_acerto, "dados.acertos.csv")
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


# 
# grid.arrange(
#     as.data.frame(table(pos_gabarito$dados$exoticas)) %>% 
#         ggplot(aes(Var1, Freq)) + geom_col() + 
#         labs(title = "Acerto de nomes exóticas", 
#              x = "Numero de acertos", 
#              y = "Quantidade de acertos"), 
#     as.data.frame(table(pos_gabarito$dados$nativas)) %>% 
#         ggplot(aes(Var1, Freq)) + geom_col() + 
#         labs(title = "Acerto de nomes nativas", 
#              x = "Numero de acertos", 
#              y = "Quantidade de acertos"),
#     as.data.frame(table(pos_gabarito$dados$origem_exoticas)) %>% 
#         ggplot(aes(Var1, Freq)) + geom_col() + 
#         labs(title = "Acerto de origem exóticas", 
#              x = "Numero de acertos", 
#              y = "Quantidade de acertos"), 
#     as.data.frame(table(pos_gabarito$dados$origem_nativas)) %>% 
#         ggplot(aes(Var1, Freq)) + geom_col() + 
#         labs(title = "Acerto de origem nativas", 
#              x = "Numero de acertos", 
#              y = "Quantidade de acertos")
# )
# 
# 
# dados <- read.csv2("https://raw.githubusercontent.com/hersonpc/mestrado-exoticas-nativas/master/r/dados.brutos.csv",
#                    stringsAsFactors = F, encoding = 'UTF-8') %>% tbl_df()
# dados
# 
# library(dplyr)
# dados2 <- 
#     dados %>% 
#     tbl_df() %>% 
#     select(codigo, municipio, nome.escola, turmas, area, flona,
#                  nome_exoticas = exoticas, nome_nativas = nativas,
#                  origem_exoticas, origem_nativas, indice_exoticas, indice_nativas)
