## code to prepare `get_data` dataset goes here

library(tidyverse)
library(rvest)


get_population_data = function(link) {

  stub = "https://www.abs.gov.au"

  link |>
    read_html() |>
    html_elements('div [href$="xlsx"]') |>
    html_attr("href") |>
    as_tibble_col("url") |>
    filter(str_detect(url,"32180DS0004")) |>
    mutate(url = str_c(stub, url)) |>
    mutate(
      download = tempfile(fileext = "xlsx")) |>
    mutate(x =
             pmap(
               list(url,download),
               function(a,b) if(!file.exists(b)) download.file(a,b,mode="wb"))
    )  |>
    select(-x) |>
    mutate(sheets=map(download,readxl::excel_sheets)) |>
    unnest(sheets) |>
    filter(str_detect(sheets,"Table 1")) |>
    mutate(skip =5) |>
    mutate(
      data=pmap(list(download,sheets,"skip"=skip), readxl::read_excel, col_types = "text"))  |>
    unnest(data) |>
    pivot_longer(contains("20"), names_to = "year", values_to = "population") |>
    select("lga_code" = ...1,
           "lga_name" = ...2,
           year,
           population) |>
    mutate(year = as.numeric(year),
           population = as.numeric(population)) |>
    filter(!is.na(population)) |>
    mutate(financial_year = str_c(year-1,"-",str_sub(year,3,4))) |>
    filter(str_sub(lga_code,1,1) == "2")

}

pop.tbl =
  get_population_data(
    "https://www.abs.gov.au/statistics/people/population/regional-population/latest-release"
    )


get_esc_data = function(link) {
  stub = "https://www.esc.vic.gov.au/"

  link |>
    read_html() |>
    html_elements('div [href$="xlsx"]') |>
    html_attr("href") |>
    as_tibble_col("url") |>
    mutate(
      url = str_c(stub ,url),
      download = tempfile(fileext = "xlsx")) |>
   mutate(x =
           pmap(
             list(url,download),
             function(a,b) if(!file.exists(b)) download.file(a,b,mode="wb"))
   )  |>
   select(-x) |>
   mutate(sheets=map(download,readxl::excel_sheets)) |>
   unnest(sheets) |>
   filter(!str_detect(sheets,"Cover|contents")) |>
   mutate(skip =5) |>
   mutate(
      data=pmap(list(download,sheets,"skip"=skip), readxl::read_excel, col_types = "text"))  |>
   unnest(data) |>
   janitor::clean_names() |>
   mutate(value = coalesce(value,value_in_nominal_terms),
          financial_year = str_replace(financial_year,"-|–","-"),
          financial_year = str_extract(financial_year,"[0-9]{4}-[0-9]{2}")) |>
   rename("lga_name" = council,
           "council_category" = group) |>
   select(sheets, lga_name:value,contains("actual"))

}

esc.tbl =
  get_esc_data(
  "https://www.esc.vic.gov.au/local-government/rate-capping-outcomes-reports/local-council-outcomes-report-2023"
  )


get_vago_data = function(link) {
  link |>
    read_html() |>
    html_elements('div [href$="csv"]') |>
    html_attr("href") |>
    as_tibble_col("url") |>
    mutate(url = str_c("https://www.audit.vic.gov.au/",url)) |>
    mutate(
      data=pmap(list(url), read_csv))  |>
    unnest(data) |>
    janitor::clean_names() |>
    mutate(lga_name = coalesce(council, council_and_benchmark_averages)) |>
    mutate(measure = coalesce(sub_category, attribute, attribute_2),
           value = coalesce(value,indicator_value),
           value = as.character(value)) |>
    rename("sheets" = composition_selection,
           "financial_year"=year) |>
    group_by(lga_name) |>
    fill(council_category) |>
    select(lga_name, measure, council_category, financial_year, sheets, sub_category, value)


}

vago.tbl = get_vago_data("https://www.audit.vic.gov.au/report/results-2022-23-audits-local-government")



get_key_facts = function (df) {
  df |>
    filter(sheets == "Key facts",
           !str_detect(measure,"Population")) |>
    select(lga_name,financial_year,measure,value) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value))

}

get_category = function (df) {
  df |>
    filter(!is.na(council_category)) |>
    select(lga_name,council_category) |>
    unique()

}


clean_lga_name = function(name) {
  list = c("City","Shire","Rural","Council","Borough","of","\\(.*\\)")
  str_remove_all(name, str_flatten(list,"|")) |>
    str_trim()
}


get_revenue = function (df1, df2) {

  bind_rows(
    df1 |> mutate(source = "df1"),
    df2 |> mutate(source = "df2")
    ) |>
    filter(sheets == "Revenue",  !str_detect(measure, "per person")) |>
    select(source, lga_name, financial_year, measure, value) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value)) |>
    mutate(value = as.numeric(value),
           lga_name = clean_lga_name(lga_name),
           measure = str_replace(measure,"Contributions.*","Contributions"),
           measure = str_replace(measure,".*(G|g)rants.*","Grants"),
           measure = str_replace(measure,"Other.*","Other revenue"),
           measure = str_replace(measure,".*fees.*","User fees and statutory fees")) |>
    group_by(source, lga_name, financial_year, measure) |>
    summarise(value = round(sum(value)/1000)*1000) |>
    ungroup() |>
    select(-source) |>
    unique() |>
    group_by(lga_name, financial_year, measure) |>
    slice(1)

}

get_expenditure = function (df1, df2) {

  bind_rows(
    df1 |> mutate(source = "df1"),
    df2 |> mutate(source = "df2")
  ) |>
    filter(str_detect(sheets, "Expense|Expenditure"),  !str_detect(measure, "per person|Operating")) |>
    select(source, lga_name, financial_year, measure, value) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value)) |>
    mutate(value = as.numeric(value),
           lga_name = clean_lga_name(lga_name),
           measure = str_replace(measure,"Employee.*","Employee benefits"),
           measure = str_replace(measure,".*(G|g)rants.*","Grants"),
           measure = str_replace(measure,"Other.*","Other revenue"),
           measure = str_replace(measure,".*fees.*","User fees and statutory fees")) |>
    group_by(source, lga_name, financial_year, measure) |>
    summarise(value = round(sum(value)/1000)*1000) |>
    ungroup() |>
    select(-source) |>
    unique() |>
    group_by(lga_name, financial_year, measure) |>
    slice(1) |>
    ungroup()

}

get_kpis = function(df1, df2) {

    bind_rows(
      df1,
      df2
    ) |>
      filter(
        (
         str_detect(source, "PRF") |
         str_detect(measure, "ratio|\\%\\)")
        )
        ) |>
      select(lga_name, financial_year, measure, value, source) |>
      mutate(value = as.numeric(value)) |>
      filter(!is.na(value))


}


get_services = function(df1, df2) {

  bind_rows(
    df1,
    df2
  ) |>
    filter(
      (
        str_detect(sheets, "Services")
      )
    ) |>
    select(lga_name, financial_year, measure, value, source) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(value))


}

services = get_services(esc.tbl,vago.tbl)
kpis = get_kpis(esc.tbl,vago.tbl)
revenue  = get_revenue(esc.tbl,vago.tbl)
expenditure = get_expenditure(esc.tbl,vago.tbl)
category = get_category(vago.tbl)
facts = get_key_facts(esc.tbl)


mutate(value = as.numeric(value)) |>
  filter(!is.na(value)) |>
  mutate(value = as.numeric(value),
         lga_name = clean_lga_name(lga_name),
         measure = str_replace(measure,"Employee.*","Employee benefits"),
         measure = str_replace(measure,".*(G|g)rants.*","Grants"),
         measure = str_replace(measure,"Other.*","Other revenue"),
         measure = str_replace(measure,".*fees.*","User fees and statutory fees")) |>
  group_by(source, lga_name, financial_year, measure) |>
  summarise(value = round(sum(value)/1000)*1000) |>
  ungroup() |>
  select(-source) |>
  unique() |>
  group_by(lga_name, financial_year, measure) |>
  slice(1) |>
  ungroup()
