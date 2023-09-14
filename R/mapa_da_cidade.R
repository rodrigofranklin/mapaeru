#' Obter Mapa da Cidade com Dados do Censo
#'
#' Esta função retorna um mapa da cidade especificada com base nos dados do censo do IBGE.
#' O usuário pode optar por eliminar regiões inabitadas, massas de água e áreas sobrepostas.
#'
#' @param cidade Nome da cidade para a qual o mapa deve ser obtido. O padrão é "VITÓRIA".
#' @param dados Expressão ou coluna de dados a serem adicionados ao mapa.
#' @param eliminar_inabitadas Se TRUE, elimina regiões inabitadas do mapa.
#' @param eliminar_agua Se TRUE, elimina massas de água do mapa.
#' @param eliminar_sobrepostas Se TRUE, elimina áreas sobrepostas no mapa.
#'
#' @return Um objeto SpatialPolygonsDataFrame contendo o mapa da cidade com os dados do censo.
#'
#' @importFrom sp bbox
#' @importFrom methods as
#' @importFrom utils download.file flush.console
#' @importFrom zip unzip
#' @importFrom sf st_read st_zm st_bbox st_simplify
#' @importFrom osmdata opq add_osm_features osmdata_sf
#' @importFrom rgeos gArea gIntersects union
#' @importFrom raster erase
#' @importFrom magrittr %>%
#'
#' @seealso Para obter detalhes sobre as variáveis disponíveis para uso na expressão de dados, consulte o data.frame \code{dicionario}.
#'
#' @examples
#' # Suponha que 'dados' seja uma expressão válida:
#' expressao_dados <- "2.V002/1.V001" # Exemplo de expressão: média de moradores por domicílio
#' mapa <- mapa_da_cidade(cidade = "VITÓRIA", dados = expressao_dados)
#'
#' @encoding UTF-8
#' @export
mapa_da_cidade <- function(cidade = "VITÓRIA", dados = NULL,
                           eliminar_inabitadas = FALSE,
                           eliminar_agua = FALSE,
                           eliminar_sobrepostas = FALSE) {

  cidade <- toupper(cidade)

  # Definição da URL do Setor
  setor_url <- "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_kmz/32-ES.kmz"

  destino <- file.path(tempdir(), basename(setor_url))

  # Verificar se o arquivo já existe
  if (!file.exists(destino)) {
    print("Obtendo malha dos setores censitários...")
    download.file(setor_url, destino, mode = "wb")
    zip::unzip(destino, exdir = file.path(tempdir(),"coleta"))
  }

  # Carregar a malha dos setores da cidade
  mapa <- file.path(tempdir(),"coleta/doc.kml") |>
    st_read(layer = cidade) |>
    st_zm(drop=TRUE) |>
    as("Spatial")

  # Se "dados" for fornecido, atualize o mapa
  if (!is.null(dados)) {
    mapa <- atualizar_dados(mapa, dados)
  }

  # Eliminação de regiões inabitadas
  if (eliminar_inabitadas) {
    print("Obtendo dados dos domicílios...")
    censo_url <- "http://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/ES_20171016.zip"
    destino_censo <- file.path(tempdir(), basename(censo_url))

    if (!file.exists(destino_censo)) {
      download.file(censo_url, destino_censo, mode = "wb")
      zip::unzip(destino_censo, exdir = file.path(tempdir(),"coleta"), junkpaths = TRUE)
    }

    domicilio_basico_censo2010 <- read.csv2(file.path(tempdir(),"coleta/Basico_ES.csv"))
    mapa <- mapa[mapa$Name %in% domicilio_basico_censo2010$Cod_setor,]
  }

  # Eliminação de áreas sobrepostas
  if (eliminar_sobrepostas) {
    ## Alguns setores censitários apresentam áreas sobrepostas. Vamos eliminá-las para facilitar a visualização
    # Calcule as áreas para cada polígono em 'mapa', suprimindo avisos
    areas <- suppressWarnings(sapply(1:nrow(mapa), function(i) gArea(mapa[i, ])))
    # Ordene os polígonos do menor para o maior com base nas áreas
    ordem_ascendente <- order(areas)
    for (i in 2:length(ordem_ascendente)) {
      poligono_atual <- mapa[ordem_ascendente[i], ]

      # Identifique quais polígonos menores têm sobreposição com poligono_atual
      tem_sobreposicao <- sapply(1:(i-1), function(j) gIntersects(poligono_atual, mapa[ordem_ascendente[j], ], byid=TRUE)) |> suppressWarnings()

      if (any(tem_sobreposicao,is.na(tem_sobreposicao))) {
        # Combine apenas os polígonos menores que têm sobreposição usando union
        poligonos_menores <- union(mapa[ordem_ascendente[which(tem_sobreposicao)], ])
        # Realize uma única operação de recorte
        mapa_tmp <- erase(poligono_atual, poligonos_menores)

        # Atualizar a geometria
        slot(mapa, "polygons")[[ordem_ascendente[i]]] <- slot(mapa_tmp, "polygons")[[1]]
      }
      cat("\rEliminando sobreposição de setores censitários: ",
          round(i/length(ordem_ascendente)*100,0),"%            ", sep = "")
      flush.console()
    }
    cat("\n")
  }

  # Eliminação de massas de água
  if (eliminar_agua) {
    print("Eliminando massas de água...")
    massa_de_agua <- mapa |>
      st_bbox() |>
      opq() |>
      add_osm_features(c(
        "\"natural\"=\"strait\"",
        "\"natural\"=\"bay\"",
        "\"natural\"=\"river\"",
        "\"natural\"=\"water\""
      )) |>
      osmdata_sf(quiet = FALSE)
    massa_de_agua <- massa_de_agua$osm_multipolygons |>
      st_simplify(dTolerance = 0.01) |>
      as("Spatial")


    mapa <- raster::erase(mapa, massa_de_agua)
  }

  return(mapa)
}
