#' Population
#'
#' A tibble with `r ncol(population)` columns and `r nrow(population)` rows.
#'
#' @format ## `population`
#' @source https://www.abs.gov.au/statistics/people/population/regional-population/latest-release

#' \describe{
#'   \item{lga_code}{ABS Local government area code}
#'   \item{lga_name}{Local government area name}
#'   \item{year}{Year ending 30 June data was estimated.}
#'   \item{population}{Population estimate, persons}
#'   \item{financial_year}{Financial year}
#'   ...
#' }
"population"


#' Rents
#'
#' A tibble with `r ncol(lgv_rents)` columns and `r nrow(lgv_rents)` rows.
#'
#' @format ## `population`
#' @source https://www.dffh.vic.gov.au/publications/rental-report

#' \describe{
#' rental_type
#'   \item{rental_type}{The type of rental property, 1 bedroom, 2 bedroom, All properties}
#'   \item{lga_name}{Local government area name}
#'   \item{rental_date}{End of quarter that data is for, eg 30 June 2021}
#'   \item{value}{A numeric value.}
#'   \item{measure}{The type of data either the median rental value or count of rentals}
#'   ...
#' }
"lgv_rents"

#' Revenue
#'
#' A tibble with `r ncol(revenue)` columns and `r nrow(revenue)` rows.
#'
#' @format ## `revenue`

#' \describe{
#'   \item{lga_name}{Local government area name}
#'   \item{financial_year}{Financial year}
#'   \item{measure}{Measure description}
#'   \item{value}{Value}
#'   ...
#' }
"revenue"

#' Expenditure
#'
#' A tibble with `r ncol(expenditure)` columns and `r nrow(expenditure)` rows.
#'
#' @format ## `expenditure`

#' \describe{
#'   \item{lga_name}{Local government area name}
#'   \item{financial_year}{Financial year}
#'   \item{measure}{Measure description}
#'   \item{value}{Value}
#'   ...
#' }
"expenditure"

#' KPIs
#'
#' A tibble with `r ncol(kpis)` columns and `r nrow(kpis)` rows.
#'
#' @format ## `kpis`

#' \describe{
#'   \item{lga_name}{Local government area name}
#'   \item{financial_year}{Financial year}
#'   \item{measure}{Measure description}
#'   \item{value}{Value}
#'   \item{source}{Source of data}
#'   ...
#' }
"kpis"

#' Facts
#'
#' A tibble with `r ncol(facts)` columns and `r nrow(facts)` rows.
#'
#' @format ## `facts`

#' \describe{
#'   \item{lga_name}{Local government area name}
#'   \item{financial_year}{Financial year}
#'   \item{measure}{Measure description}
#'   \item{value}{Value}
#'   ...
#' }
"facts"

#' Population
#'
#' A tibble with `r ncol(population)` columns and `r nrow(population)` rows.
#'
#' @format ## `population`

#' \describe{
#'   \item{lga_code}{ABS Local government area code}
#'   \item{lga_name}{Local government area name}
#'   \item{year}{Year ending 30 June data was estimated.}
#'   \item{population}{Population estimate, persons}
#'   \item{financial_year}{Financial year}
#'   ...
#' }
"population"


