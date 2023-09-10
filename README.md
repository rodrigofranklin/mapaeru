# mapaeru

`mapaeru` é um pacote R desenvolvido para facilitar a visualização e análise de dados geográficos relacionados a setores censitários de cidades. Ele oferece funções para carregar, atualizar e visualizar mapas com dados específicos.

## Instalação

Você pode instalar a versão mais recente do pacote `mapaeru` diretamente do GitHub usando o pacote `devtools`:

``` r
# Se você ainda não tem o devtools, instale-o primeiro:
install.packages('devtools')

# Instale o pacote mapaeru:
devtools::install_github('rodrigofranklin/mapaeru')
```

## Uso Básico

Aqui está um exemplo rápido de como usar o pacote:

``` r
library(mapaeru)

# Carregar o mapa da cidade de Vitória
mapa <- mapa_da_cidade(cidade = 'VITÓRIA')

# Exibir o mapa
exibir_mapa(mapa)
```

## Funções Principais

-   `mapa_da_cidade()`: Carrega a malha dos setores censitários de uma cidade específica.
-   `exibir_mapa()`: Exibe um mapa com opções de visualização personalizadas.
-   `obter_dados()`: Obtém dados específicos para serem incorporados ao mapa.
-   `atualizar_dados()`: Atualiza um mapa existente com novos dados.

## Dados

O pacote inclui dois conjuntos de dados principais:

-   `dicionario`: Descrição das variáveis utilizadas no conjunto de dados.
-   `tabelas`: Nomes das tabelas de dados disponíveis.

## Contribuições

Feedback, sugestões e contribuições são bem-vindos! Por favor, abra uma 'issue' ou 'pull request' no [repositório GitHub do pacote](https://github.com/seu_nome_de_usuario_no_github/mapaeru).

## Licença

[GPL-3](LICENSE.md)
