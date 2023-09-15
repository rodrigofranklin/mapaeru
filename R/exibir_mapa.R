#' Exibir Mapa com Dados do Censo
#'
#' Esta função exibe um mapa interativo com base nos dados do censo fornecidos.
#' Se a coluna 'dados' estiver presente no mapa, os polígonos serão coloridos de acordo com esses dados.
#' Caso contrário, os polígonos serão exibidos com rótulos baseados na coluna 'Description'.
#'
#' @param mapa Um objeto SpatialPolygonsDataFrame contendo os dados do censo a serem exibidos.
#'
#' @return Um objeto leaflet para visualização interativa.
#'
#' @import leaflet
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLegend fitBounds leafletOptions colorBin providers providerTileOptions
#' @importFrom sp bbox
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#'
#' @examples
#' # Suponha que 'mapa' seja um SpatialPolygonsDataFrame válido:
#' # exibir_mapa(mapa)
#'
#' @encoding UTF-8
#' @export
exibir_mapa <- function(mapa) {

  # Verificar se o mapa é nulo
  if (is.null(mapa)) return()

  # Inicializar o mapa com opções básicas
  mapa_leaflet <- leaflet(data = mapa, options = leafletOptions(
    zoomSnap = 0.1,
    zoomDelta = 0.1,
    boxZoom = TRUE,
    doubleClickZoom = FALSE,
    worldCopyJump = FALSE
  ))

  # Adicionar mapa de fundo
  mapa_leaflet <- mapa_leaflet %>%
    addProviderTiles(providers$CartoDB, options = providerTileOptions(opacity = 0.5))

  # Verificar se a coluna 'dados' existe
  if ("dados" %in% names(mapa)) {
    # Definir paleta de cores com intervalos fixos
    pal <- colorNumeric("YlOrRd", domain = mapa$dados, na.color = "transparent")

    # Adicionar polígonos com cores baseadas nos dados e rótulos mostrando os valores
    mapa_leaflet <- mapa_leaflet %>%
      addPolygons(
        fillColor = ~pal(dados),
        fillOpacity = 0.6,
        color = "#666",
        weight = 1,
        label = ~paste0("Dado: ", dados),
        highlightOptions = highlightOptions(
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      # Adicionar legenda com intervalos fixos
      addLegend(pal = pal, values = ~dados, title = "Dados")

  } else {
    # Adicionar polígonos com rótulos baseados na coluna 'Description'
    mapa_leaflet <- mapa_leaflet %>%
      addPolygons(
        fillOpacity = 0.6,
        color = "#666",
        weight = 1,
        label = ~Description |> lapply(HTML),
        highlightOptions = highlightOptions(
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      )
  }

  # Definir zoom inicial
  mapa_leaflet <- mapa_leaflet %>%
    fitBounds(sp::bbox(mapa)[1,1], sp::bbox(mapa)[2,1], sp::bbox(mapa)[1,2], sp::bbox(mapa)[2,2])

  return(mapa_leaflet)
}
