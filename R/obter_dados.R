#' Obter Dados com Base em uma Expressão
#'
#' Esta função avalia uma expressão fornecida e retorna um `data.frame` com os resultados.
#' A expressão deve referenciar variáveis disponíveis nos dados do censo, usando a notação "tabela.variavel".
#'
#' @param expressao Uma expressão que referencia variáveis nos dados do censo usando a notação "tabela.variavel".
#'
#' @details
#' As variáveis que podem ser referenciadas na expressão, bem como detalhes sobre elas,
#' estão disponíveis no data.frame "dicionario" incluído no pacote. Este data.frame fornece
#' uma descrição detalhada de cada variável disponível.
#'
#' @return Um `data.frame` contendo os resultados da expressão fornecida.
#'
#' @importFrom utils download.file read.csv2
#' @importFrom zip unzip
#'
#' @examples
#' # Suponha que expressao seja uma expressão válida que referencia variáveis nos dados do censo:
#' # obter_dados(expressao)
#'
#' @encoding UTF-8
#' @export
obter_dados <- function(expressao) {

  # Função auxiliar para obter o dataframe com base na tabela e variável
  get_data <- function(tabela, variavel) {
    arquivo <- file.path(tempdir(), "coleta", tabelas$Nome[tabelas$tabela == tabela])

    if (!file.exists(arquivo)) {
      print("Obtendo dados dos domicílios...")
      censo_url <- "http://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/ES_20171016.zip"
      destino_censo <- file.path(tempdir(), basename(censo_url))
      download.file(censo_url, destino_censo, mode = "wb")
      zip::unzip(destino_censo, exdir = file.path(tempdir(),"coleta"), junkpaths = TRUE)
    }

    df <- read.csv2(arquivo, stringsAsFactors = FALSE)

    # Converter Cod_setor para texto
    df$Cod_setor <- as.character(df$Cod_setor)

    # Verificar se a variável e 'Cod_setor' existem no dataframe
    if (!variavel %in% names(df) || !"Cod_setor" %in% names(df)) {
      stop(paste("A variável", variavel, "ou 'Cod_setor' não foram encontradas na tabela", tabela))
    }

    # Ordenar o dataframe por 'Cod_setor'
    df <- df[order(df$Cod_setor), ]

    # Se a coluna não for numérica, tentar convertê-la
    if (!is.numeric(df[[variavel]])) {
      suppressWarnings(df[[variavel]] <- as.numeric(as.character(df[[variavel]])))
    }

    return(df[, c("Cod_setor", variavel)])
  }

  # Use a função gsub para substituir cada referência de tabela.variavel pelo seu valor correspondente
  expressao_substituida <- gsub("([0-9]+)\\.([A-Za-z0-9_]+)",
                                "get_data('\\1', '\\2')$\\2",
                                expressao)

  # Avalie a expressão substituída
  resultado_dado <- eval(parse(text = expressao_substituida))
  resultado_dado[is.infinite(resultado_dado)] <- NA
  resultado_dado[is.nan(resultado_dado)] <- NA

  # Criar dataframe de resultado
  resultado_df <- data.frame(Cod_setor = get_data("1", "V001")$Cod_setor,
                             dado = resultado_dado)

  return(resultado_df)
}

