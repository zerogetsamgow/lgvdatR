
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lgvdatR

<!-- badges: start -->
<!-- badges: end -->

The goal of {lgvdatR} is to create easily accessible datasets related to
Local Governments in Victoria.

{lgvdatR} compiles data from the

[Essential Service Commission’s Local Council Outcomes
Report](https://www.esc.vic.gov.au/local-government/rate-capping-outcomes-reports/local-council-outcomes-report-2023)

[Victorian Audit-General’s Office’s Results of Local Government Audits
Report](https://www.audit.vic.gov.au/report/results-2022-23-audits-local-government)

[Australian Bureau of Statistic’s Regional
Population](https://www.abs.gov.au/statistics/people/population/regional-population/latest-release)

Data from these sources is saved in 7 datasets, category.rda,
expenditure.rda, facts.rda, kpis.rda, population.rda, revenue.rda,
services.rda

{lgvdatR} includes one function `clean_lga()` that provides for cleaning
of local government names. It borrows heavily from
`strayr::clean_state()`

## Installation

You can install the development version of {lgvdatR} from
[GitHub](www.github.com/zerogetsamgow) with:

``` r
# install.packages("devtools")
devtools::install_github("zerogetsamgow/lgvdatR")
```
