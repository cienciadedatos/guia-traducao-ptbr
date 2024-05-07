# Importar ----
traducao_r4ds <- readr::read_csv("traducao-r4ds.csv")

nomes <- readr::read_csv("equipe-traducao-r4ds.csv")

# tratamento e join ------
traducao_tratado <- traducao_r4ds |>
  tidyr::pivot_longer(
    cols = -c(secao, ordem, capitulo, n_issue, n_pr),
    values_to = "github",
    names_to = "contribuicao"
  ) |>
  dplyr::left_join(nomes) |>
  dplyr::mutate(
    contribuicao = dplyr::case_when(
      contribuicao == "traducao" ~ "Tradução",
      stringr::str_starts(contribuicao, pattern = "revisao") ~ "Revisão"
    ),
    url_issue = paste0(
      "https://github.com/cienciadedatos/pt-r4ds/issues/",
      n_issue
    ),
    url_pr = paste0("https://github.com/cienciadedatos/pt-r4ds/pull/", n_pr)
  )


# Equipe sumarizado ----
equipe_sumarizado <- traducao_tratado |>
  dplyr::count(contribuicao, github) |>
  tidyr::pivot_wider(values_from = n, names_from = contribuicao)

# Equipe incompleto ---
traducao_tratado |> 
  dplyr::filter(github != "???", is.na(nome)) |> 
  dplyr::distinct(github) |> 
  dplyr::pull(github) |> 
  writeLines()

# Equipe - Falta o email
traducao_tratado |> 
  dplyr::filter(github != "???", is.na(email) | email == "???") |> 
  dplyr::distinct(github) |> 
  dplyr::mutate(url_github = paste0("https://github.com/", github)) |> 
  dplyr::pull(url_github) |> 
  writeLines()


# Precisamos de ajuda! ----
help_wanted <- traducao_tratado |>
  dplyr::filter(github == "???") 



