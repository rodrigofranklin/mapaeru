#' Atualiza os dados de um SpatialPolygonsDataFrame com base em uma expressão fornecida
#'
#' Esta função permite que os usuários atualizem os dados associados a um \code{SpatialPolygonsDataFrame} usando uma expressão que referencia tabelas e variáveis específicas. A função utiliza a expressão fornecida para obter os novos dados e atualiza o \code{SpatialPolygonsDataFrame} com esses dados.
#' Atualizar Dados em um SpatialPolygonsDataFrame
#'
#' Esta função atualiza a coluna 'dados' de um `SpatialPolygonsDataFrame` com base em uma expressão fornecida.
#' A expressão é avaliada usando a função `obter_dados` e os resultados são mapeados para o `SpatialPolygonsDataFrame` com base na coluna 'Name'.
#'
#' @param sp_df Um objeto `SpatialPolygonsDataFrame` que será atualizado.
#' @param expressao Uma expressão que será avaliada usando a função `obter_dados`.
#'
#' @details
#' As variáveis que podem ser referenciadas na expressão, bem como detalhes sobre elas,
#' estão disponíveis no data.frame "dicionario" incluído no pacote. Este data.frame fornece
#' uma descrição detalhada de cada variável disponível.
#'
#' @return Um `SpatialPolygonsDataFrame` atualizado com os dados resultantes da expressão fornecida.
#'
#' @examples
#' # Suponha que sp_df seja um SpatialPolygonsDataFrame e expressao uma expressão válida:
#' # atualizar_dados(sp_df, expressao)
#'
#' @encoding UTF-8
#' @export
atualizar_dados <- function(sp_df, expressao) {
  # Obter os resultados da expressão usando a função obter_dados
  resultados <- obter_dados(expressao)

  # Se a coluna 'dados' não existir no sp_df, crie-a
  if (!"dados" %in% names(sp_df)) {
    sp_df$dados <- NA
  }

  # Atualizar a coluna 'dados' no SpatialPolygonsDataFrame
  sp_df$dados <- resultados$dado[match(sp_df$Name, resultados$Cod_setor)]

  return(sp_df)
}
