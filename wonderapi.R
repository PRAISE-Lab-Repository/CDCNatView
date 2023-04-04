library("XML")
library("xml2")
library("methods")
library(httr)


get_long_data <- function(field_name, code_name) {
  x <- read_xml("long_base_request.xml")
  
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_2")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name
  
  
  write_xml(x, "new_long_request.xml")
  xx <- xmlParse("new_long_request.xml")
  
  query <- XML::saveXML(xx,
                        indent=FALSE,
                        prefix='<?xml version="1.0" encoding="utf-8"?>\n')
  
  url <- "https://wonder.cdc.gov/controller/datarequest/D149"
  r <- POST(url, body = list(request_xml = query, accept_datause_restrictions = "true"), encode = "form")
  r2 <- content(r, as="text", encoding = "utf-8")

  k <- xml2::read_xml(r2) %>% make_query_table()
  save_path <- sprintf("data/database1/long_tables/%s.rds", field_name)
  saveRDS(k, save_path)
}

get_education_data <- function(field_name, code_name) {
  x <- read_xml("education_base_request.xml")
  
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_1")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name
  
  write_xml(x, "new_education_request.xml")
  xx <- xmlParse("new_education_request.xml")
  
  query <- XML::saveXML(xx,
                        indent=FALSE,
                        prefix='<?xml version="1.0" encoding="utf-8"?>\n')
  
  url <- "https://wonder.cdc.gov/controller/datarequest/D149"
  r <- POST(url, body = list(request_xml = query, accept_datause_restrictions = "true"), encode = "form")
  r2 <- content(r, as="text", encoding = "utf-8") 
  k <- xml2::read_xml(r2) %>% make_query_table()
  
  save_path <- sprintf("data/database1/education_tables/%s.rds", field_name)
  saveRDS(k, save_path)
}


# get_education_data <- function(field_name, code_name) {
#   x <- read_xml("education_base_request.xml")
#   
#   lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_1")
#   to_mod <- xml_find_all(x, lookup)
#   xml_text(to_mod) <- code_name
#   
#   write_xml(x, "new_education_request.xml")
#   xx <- xmlParse("new_education_request.xml")
#   
#   query <- XML::saveXML(xx,
#                         indent=FALSE,
#                         prefix='<?xml version="1.0" encoding="utf-8"?>\n')
#   
#   url <- "https://wonder.cdc.gov/controller/datarequest/D149"
#   r <- POST(url, body = list(request_xml = query, accept_datause_restrictions = "true"), encode = "form")
#   r2 <- content(r, as="text", encoding = "utf-8") 
#   k <- xml2::read_xml(r2) %>% make_query_table()
#   
#   save_path <- sprintf("data/database1/education_tables/%s.rds", field_name)
#   saveRDS(k, save_path)
# }



changeFieldValue <- function(param, text, xml_file) {

  
}

replaceNAs <- function (x) {
  for (i in seq_along(rownames(x))) {
    for (j in seq_along(colnames(x))) {
      if (is.na(x[i,j])) x[i,j] <- x[i-1, j]
    }
  }
  return(x)
}



make_query_table <- function(query_result) {
  allrows <- query_result %>%
    xml2::xml_find_all("//r")
  
  # remove total rows
  dt <- vector()
  for (i in seq_along(allrows)) {
    ifelse (allrows[i] %>% rvest::html_elements("c") %>%
              xml2::xml_has_attr("dt") %>% sum() > 0, dt[i] <- TRUE,
            dt[i] <- FALSE)
  }
  allrows <- allrows[!dt]
  
  firstrow <- allrows[1] %>% rvest::html_elements("c")
  numcol <-  length(firstrow) +
    length(firstrow %>% xml2::xml_children()) # standard deviation
  # measures are children
  numl <- firstrow %>% xml2::xml_attr("l") %>%
    stats::na.omit() %>% length()
  
  querytable <- do.call(rbind, purrr::map(allrows, getrows,
                                          numcol)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    replaceNAs() %>%
    purrr::map_df(conditional_as.numeric)
  
  dbcode <- query_result %>% rvest::html_element("dataset") %>%
    xml2::xml_attr("code")
  
  # create lookup table from query request (needed for column names)
  variablecodes <- query_result %>% rvest::html_element("dataset") %>%
    xml2::xml_find_all("variable[@code] | variable/hier-level[@code]") %>%
    xml2::xml_attr("code")
  
  variablelabels <- query_result %>% rvest::html_element("dataset") %>%
    xml2::xml_find_all("variable[@code] | variable/hier-level[@code]") %>%
    xml2::xml_attr("label")
  
  measurecodes <- query_result %>% rvest::html_element("dataset") %>%
    rvest::html_elements("measure") %>% xml2::xml_attr("code")
  
  measurelabels <- query_result %>% rvest::html_element("dataset") %>%
    rvest::html_elements("measure") %>% xml2::xml_attr("label")
  
  lookup <- data.frame(code = c(variablecodes, measurecodes),
                       label = c(variablelabels, measurelabels),
                       stringsAsFactors = FALSE)
  
  # get column names (byvariables, then measures)
  
  byvariables <- query_result %>%
    rvest::html_element("byvariables") %>%
    rvest::html_elements("variable") %>%
    xml2::xml_attr("code")
  
  measures <- query_result %>%
    rvest::html_element("measure-selections") %>%
    rvest::html_elements("measure") %>%
    xml2::xml_attr("code")
  
  index <- c(byvariables, measures) %>%
    purrr::map_int(~which(.x == lookup$code)[1])
  table_col <- lookup$label[index]
  colnames(querytable) <- table_col
  return(querytable)
}

conditional_as.numeric <- function(.x) {
  if(sum(nchar(stringr::str_replace_all(.x, "[0-9|.|,]", ""))) == 0) {
    readr::parse_number(.x)
  } else {
    .x
  }
}

getrows <- function(thisrow, numcol) {
  cells <- thisrow %>% rvest::html_elements("c")
  # assuming all the labels ("l") are to the left of all
  # the values ("v")
  l <- cells %>% xml2::xml_attr("l") %>% stats::na.omit()
  v <- vector()
  for (i in seq_along(cells)) {
    v <- c(v, cells[i] %>% xml2::xml_attr("v"))
    if (xml2::xml_length(cells[i]) > 0) {
      v <- c(v, cells[i] %>% xml2::xml_child() %>%
               xml2::xml_attr("v"))
    }
  }
  
  v <- v %>% stats::na.omit()
  if (length(v) == 0) stop("length(v) = 0")
  # deal with percents -- need to take out % and divide by 100
  v[grepl("\\%", v)] <- as.numeric(gsub("\\%", "", v[grepl("\\%", v)]))/100
  len <- length(c(l, v))
  return(c(rep(NA, numcol - len), l, v))
}

