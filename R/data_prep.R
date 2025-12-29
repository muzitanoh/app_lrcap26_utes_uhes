# Funções de carga e preparação do dataset

get_produto_cols <- function(df) {
  names(df)[stringr::str_detect(names(df), "^20\\d{2}$")]
}

load_projetos <- function(path = "data/dados_tratados_lrcap26_ute_uhe_input_analistas.xlsx") {
  
  df <- readxl::read_xlsx(path)
  
  # Garante colunas dimensionais usadas nos filtros
  ensure_col <- function(df, col) {
    if (!col %in% names(df)) df[[col]] <- NA
    df
  }
  
  for (col in c("uf_do_ponto_de_conexao","submercado","classificacao","te",
                "nom_leilao","empreendimento","desconsiderar_projeto",
                "ceg","processo")) {
    df <- ensure_col(df, col)
  }
  
  # Limpa texto base
  df <- df %>%
    dplyr::mutate(
      uf_do_ponto_de_conexao = toupper(trimws(as.character(uf_do_ponto_de_conexao))),
      submercado             = trimws(as.character(submercado)),
      classificacao          = trimws(as.character(classificacao)),
      te                     = trimws(as.character(te)),
      nom_leilao             = trimws(as.character(nom_leilao)),
      empreendimento          = trimws(as.character(empreendimento)),
      desconsiderar_projeto  = trimws(as.character(desconsiderar_projeto))
    )
  
  produto_cols <- get_produto_cols(df)
  if (length(produto_cols) == 0) {
    stop("Nenhuma coluna de produto (ano) encontrada. Esperado algo como 2026, 2027, ...")
  }
  
  # Garante texto nas colunas de produto
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(produto_cols), ~ ifelse(is.na(.x), "", trimws(as.character(.x)))))
  
  # ID estável
  df <- df %>%
    dplyr::mutate(
      id_projeto = dplyr::case_when(
        !is.na(ceg) & trimws(as.character(ceg)) != "" ~ trimws(as.character(ceg)),
        TRUE ~ trimws(as.character(processo))
      )
    )
  
  # Long para achar anos marcados (X/x)
  df_long <- df %>%
    tidyr::pivot_longer(dplyr::all_of(produto_cols), names_to = "produto", values_to = "flag") %>%
    dplyr::mutate(
      flag = toupper(trimws(as.character(flag))),
      produto = suppressWarnings(as.integer(produto))
    ) %>%
    dplyr::filter(flag == "X", !is.na(produto))
  
  # Pode existir projeto sem nenhum X — nesse caso, ano_inicio/produtos ficam NA
  ano_inicio_df <- df_long %>%
    dplyr::group_by(id_projeto) %>%
    dplyr::summarise(
      ano_inicio = min(produto, na.rm = TRUE),
      produtos   = paste(sort(unique(produto)), collapse = ", "),
      .groups = "drop"
    )
  
  df_final <- df %>%
    dplyr::left_join(ano_inicio_df, by = "id_projeto")
  
  df_final
}

filter_projetos <- function(df,
                            uf = NULL,
                            produtos = NULL,
                            subsistema = NULL,
                            tipo_rede = NULL,
                            tipo_usina = NULL,
                            leilao = NULL,
                            empreendimento = NULL,
                            excluir_desconsiderados = TRUE) {
  
  is_all <- function(x) is.null(x) || length(x) == 0
  
  # Excluir desconsiderados (aceita NA / vazio como "considerado")
  if (isTRUE(excluir_desconsiderados) && "desconsiderar_projeto" %in% names(df)) {
    df <- df %>%
      dplyr::filter(is.na(desconsiderar_projeto) | trimws(as.character(desconsiderar_projeto)) == "")
  }
  
  # UF
  if (!is_all(uf) && "uf_do_ponto_de_conexao" %in% names(df)) {
    df <- df %>% dplyr::filter(uf_do_ponto_de_conexao %in% uf)
  }
  
  # Subsistema
  if (!is_all(subsistema) && "submercado" %in% names(df)) {
    df <- df %>% dplyr::filter(submercado %in% subsistema)
  }
  
  # Tipo de rede
  if (!is_all(tipo_rede) && "classificacao" %in% names(df)) {
    df <- df %>% dplyr::filter(classificacao %in% tipo_rede)
  }
  
  # Tipo de usina (TE)
  if (!is_all(tipo_usina) && "te" %in% names(df)) {
    df <- df %>% dplyr::filter(te %in% tipo_usina)
  }
  
  # Leilão
  if (!is_all(leilao) && "nom_leilao" %in% names(df)) {
    df <- df %>% dplyr::filter(nom_leilao %in% leilao)
  }
  
  # Empreendimento
  if (!is_all(empreendimento) && "empreendimento" %in% names(df)) {
    df <- df %>% dplyr::filter(empreendimento %in% empreendimento)
  }
  
  # Produtos (anos): filtra pelos campos "2026", "2027", ... com "X"
  if (!is_all(produtos)) {
    produto_cols <- get_produto_cols(df)
    prod_sel <- as.character(produtos)
    prod_sel <- prod_sel[prod_sel %in% produto_cols]
    
    if (length(prod_sel) > 0) {
      df <- df %>%
        dplyr::filter(dplyr::if_any(dplyr::all_of(prod_sel), ~ toupper(as.character(.x)) == "X"))
    }
  }
  
  df
}

# Dados agregados por UF
agg_por_uf <- function(df) {
  df_uf <- df %>%
    group_by(uf_do_ponto_de_conexao) %>%
    summarise(
      n_projetos = n_distinct(id_projeto),
      margem_total_mw = sum(margem_solicitada_mw, na.rm = TRUE),
      pot_inst_total_mw = sum(potencia_final_instalada_k_w, na.rm = TRUE)/1000,
      .groups = "drop"
    )
}

agg_por_ano_inicio <- function(df) {
  df_ano_inicio <- df %>%
    filter(!is.na(ano_inicio)) %>%
    group_by(ano_inicio) %>%
    summarise(
      n_projetos = n_distinct(id_projeto),
      margem_total_mw = sum(margem_solicitada_mw, na.rm = TRUE),
      pot_inst_total_mw = sum(potencia_final_instalada_k_w, na.rm = TRUE)/1000,
      .groups = "drop"
    ) %>%
    arrange(ano_inicio)
}

expand_margem_por_ano <- function(df, ano_fim = 2031) {
  df_expand_ano <- df %>%
    filter(!is.na(ano_inicio)) %>%
    mutate(
      ano_fim = pmax(ano_inicio, ano_fim),
      ano = purrr::map2(ano_inicio, ano_fim, ~ seq(.x, .y))
    ) %>%
    tidyr::unnest(ano)
}

agg_por_uf_ano <- function(df, ano_fim = 2031) {
  df_uf_ano <- expand_margem_por_ano(df, ano_fim = ano_fim) %>%
    group_by(uf_do_ponto_de_conexao, ano) %>%
    summarise(
      n_projetos = n_distinct(id_projeto),
      margem_total_mw = sum(margem_solicitada_mw, na.rm = TRUE),
      pot_inst_total_mw = sum(potencia_final_instalada_k_w, na.rm = TRUE)/1000,
      .groups = "drop"
    ) %>%
    arrange(uf_do_ponto_de_conexao, ano)
}

agg_por_ano <- function(df, ano_fim = 2031) {
  df_ano <- expand_margem_por_ano(df, ano_fim = ano_fim) %>%
    group_by(ano) %>%
    summarise(
      n_projetos = n_distinct(id_projeto),
      margem_total_mw = sum(margem_solicitada_mw, na.rm = TRUE),
      pot_inst_total_mw = sum(potencia_final_instalada_k_w, na.rm = TRUE)/1000,
      .groups = "drop"
    ) %>%
    arrange(ano)
}

