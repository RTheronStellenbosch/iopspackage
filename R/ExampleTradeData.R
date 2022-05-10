#' BACI HS17 2020 data.
#'
#' A dataset containing the trade data for country codes 4 and 710.
#'
#' @format A data frame with 137178 rows and 6 variables:
#' \describe{
#'   \item{t}{Year}
#'   \item{k}{Product category, HS 6-digit code}
#'   \item{i}{Exporter, ISO 3-digit country code}
#'   \item{j}{Importer, ISO 3-digit country code}
#'   \item{v}{Value of the trade flow, in thousands current USD}
#'   \item{q}{Quantity, in metric tons}
#' }
#' @source \url{http://www.cepii.fr/cepii/en/bdd_modele/presentation.asp?id=37}
"ExampleTradeData"
