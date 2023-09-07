#' Gera um mapa da cidade especificada
#'
#' Esta função baixa e carrega a malha dos setores censitários da cidade especificada
#' para um ano de censo fornecido.
#'
#' @param cidade Nome da cidade para a qual o mapa deve ser gerado. O padrão é "VITÓRIA".
#' @param dados Dados opcionais para serem usados no mapa (não implementado na função atual).
#' @param ano Ano do censo. Pode ser 2010 ou 2000. O padrão é 2010.
#'
#' @return Retorna um objeto espacial contendo a malha dos setores censitários da cidade especificada.
#' @importFrom rgdal readOGR
#' @export
#'
#' @examples
#' \dontrun{
#' mapa_vitoria <- mapa_da_cidade()
#' }
#'
mapa_da_cidade <- function(cidade = "VITÓRIA", dados = NULL, ano = 2010) {
  cidade <- toupper(cidade)

  if (ano == 2010) {
    setor_url <-
      "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_kmz/32-ES.kmz"
  } else if (ano == 2000) {

  }

  destino <- file.path(tempdir(), basename(setor_url))
  download.file(setor_url, destino, mode = "wb")
  unzip(destino, exdir = file.path(tempdir(),"coleta"))

  # carrega a malha dos setores da cidade
  setores <- readOGR("coleta/temp/doc.kml", layer = CIDADE)
}
