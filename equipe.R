traducao_r4ds <- readr::read_csv("traducao-r4ds.csv")

nomes <- readr::read_csv("equipe-traducao-r4ds.csv")

traducao_tratado <- traducao_r4ds |> 
  tidyr::pivot_longer(cols = -c(secao, ordem, capitulo, n_issue, n_pr), 
                      values_to = "github",
                      names_to = "contribuicao") |>
  dplyr::mutate(
    contribuicao = dplyr::case_when(contribuicao == "traducao" ~ "Tradução",
                                     stringr::str_starts(contribuicao, pattern = "revisao") ~ "Revisão"),
    url_issue = paste0("https://github.com/cienciadedatos/pt-r4ds/issues/", n_issue),
    url_pr = paste0("https://github.com/cienciadedatos/pt-r4ds/pull/", n_pr )
  )

traducao_tratado |> 
  dplyr::filter(is.na(n_pr)) |> 
  dplyr::distinct(url_issue) |> 
  dplyr::arrange(url_issue)


equipe_sumarizado <- traducao_tratado |> 
  dplyr::count(contribuicao, github) |> 
  tidyr::pivot_wider(values_from = n, names_from = contribuicao)
# quantas linhas já foram traduzidas/revisadas?
# 


help_wanted <- traducao_tratado |> 
  dplyr::filter(github == "???") |> 
  dplyr::count(ordem, capitulo, secao, n_issue, n_pr, url_issue, url_pr)
