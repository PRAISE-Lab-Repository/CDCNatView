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

get_table_data <- function(field_name, code_name, field_name1, code_name1) {
  x <- read_xml("education_base_request.xml")
  
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_1")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name
  
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_3")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name1
  
  write_xml(x, "new_education_request.xml")
  xx <- xmlParse("new_education_request.xml")
  
  query <- XML::saveXML(xx,
                        indent=FALSE,
                        prefix='<?xml version="1.0" encoding="utf-8"?>\n')
  
  url <- "https://wonder.cdc.gov/controller/datarequest/D149"
  r <- POST(url, body = list(request_xml = query, accept_datause_restrictions = "true"), encode = "form")
  r2 <- content(r, as="text", encoding = "utf-8") 
  k <- xml2::read_xml(r2) %>% make_query_table()
  
  save_path <- sprintf("data/database1/%s_tables/%s.rds", field_name1, field_name)
  saveRDS(k, save_path)
}


get_table_data_v2 <- function(database, field_name, code_name, field_name1, code_name1) {
  database_map = list("D149", "D66", "D27", "D10")
  
  
  x <- read_xml(sprintf("%s_base_request.xml", database)) 
  
  # B_1 should e the condition that we are looking for
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_1")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name
  
  
  # B_3 is for the characterlistics such as race, bmi 
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_3")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name1
  
  # save the request
  write_xml(x, "new_request.xml")
  xx <- xmlParse("new_request.xml")
  
  query <- XML::saveXML(xx,
                        indent=FALSE,
                        prefix='<?xml version="1.0" encoding="utf-8"?>\n')
  
  # send the post request 
  url <- sprintf("https://wonder.cdc.gov/controller/datarequest/%s", database_map[[database]])
  r <- POST(url, body = list(request_xml = query, accept_datause_restrictions = "true"), encode = "form")
  r2 <- content(r, as="text", encoding = "utf-8") 
  k <- xml2::read_xml(r2) %>% make_query_table()
  
  table_directory <- sprintf("data/database%s/%s_tables/", database, field_name1)
  if (!dir.exists(table_directory)){
    dir.create(table_directory)
  }
  save_path <- sprintf("data/database%s/%s_tables/%s.rds", database, field_name1, field_name)
  saveRDS(k, save_path)
}

get_long_data_v2 <- function(database, field_name, code_name) {
  # usage get_long_data_v2(3, "anemia", "D27.V11")
  
  database_map = list("D149", "D66", "D27", "D10")
  
  x <- read_xml(sprintf("%s_long_base_request.xml", database))
  
  # B_1 is year and B_2 is the condition that we are looking for 
  lookup <- sprintf("//name[text() ='%s']/parent::parameter//value", "B_2")
  to_mod <- xml_find_all(x, lookup)
  xml_text(to_mod) <- code_name
  
  
  write_xml(x, "new_long_request.xml")
  xx <- xmlParse("new_long_request.xml")
  
  query <- XML::saveXML(xx,
                        indent=FALSE,
                        prefix='<?xml version="1.0" encoding="utf-8"?>\n')
  
  # send the post request 
  url <- sprintf("https://wonder.cdc.gov/controller/datarequest/%s", database_map[[database]])
  r <- POST(url, body = list(request_xml = query, accept_datause_restrictions = "true"), encode = "form")
  r2 <- content(r, as="text", encoding = "utf-8")
  
  k <- xml2::read_xml(r2) %>% make_query_table()
  
  save_path <- sprintf("data/database%s/long_tables/%s.rds", database, field_name)
  saveRDS(k, save_path)
}


# for (x in names(code_map_2)){
#   for (y in names(demo_map_2)){
#     get_table_data_v2(2, x, code_map_2[x], y, demo_map_2[y])
#   }
# }
# 
# for (x in names(code_map_3)){
#   for (y in names(demo_map_3)){
#     get_table_data_v2(3, x, code_map_3[x], y, demo_map_3[y])
#   }
# }
# 
# for (x in names(code_map_4)){
#   for (y in names(demo_map_4)){
#     get_table_data_v2(4, x, code_map_4[x], y, demo_map_4[y])
#   }
# }


# for (x in names(code_map_2)) {
#   get_long_data_v2(2, x, code_map_2[x])
# }
# 
# for (x in names(code_map_3)) {
#   get_long_data_v2(3, x, code_map_3[x])
# }
# 
# for (x in names(code_map_4)) {
#   get_long_data_v2(4, x, code_map_4[x])
# }


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

