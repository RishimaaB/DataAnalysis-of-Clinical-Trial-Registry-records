#***************************************Script - 1***************************************************************************
#Downloading the records from the REPEC registry
#Script-1a (Downloading the English version - 1861 records)

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:1948)
for (page_result in seq(from=1, to= 195)) {
  url <- url(paste0("https://www.ins.gob.pe/ensayosclinicos/rpec/listado.asp?flgB=1&nroPag=",page_result,"&estado=&anio=%&especialidad=%&fbusqueda=&val=1&fp=1"))
  page <- read_html(url)
  links <- page %>% html_nodes(".TdNegroCuerposPq td:nth-child(2)") %>% html_text()
  links <- paste0("https://www.ins.gob.pe/ensayosclinicos/rpec/recuperarECPBNuevo.asp?numEC=",links,"&ver=EN")
  links <- data.frame(links)
  write.table(links, "peruvian_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("peruvian_links.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter))
}

peru_page_links <- data.frame()
peru_page_links <- read.csv("peruvian_links.csv")

for (i in seq_along(ids)){
  official_url = peru_page_links[ids[i],"links"]
  output_file=paste0("peru_page",ids[i],".html")
  err <- try(download.file(official_url, destfile = output_file, quiet = TRUE,
                           mode = "wb"))
  if (class(err) == "try-error") next
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_peru.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_peru.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}

##Script-1b (Downloading the Spanish version of records from the REPEC registry)


ids1 <- c(6,7,24,39,59,101,113,126,139,145,152,164,172,174,183,213,216,218,270,313,
          330,339,375,379,390,395,397,403,429,434,439,460,463,469,473,474,488,496,497,
          516,517,555,567,582,587,601,612,626,634,635,673,721,737,749,767,778,790,792,795,
          809,862,870,910,917,934,948,952,961,989,996,1032,1039,1071,1110,1125,1135,1143,1154,
          1159,1182,1183,1205,1225,1249,1270,1301,1486)

j <- 1
peru_page_links <- data.frame()
peru_page_links <- read.csv("87_links_no_eng.csv")

for (i in seq_along(ids)){
  r=ids1[j]
  j=j+1
  
  official_url = peru_page_links[ids[i],"links"] 
  output_file=paste0("peru_page",r,".html")
  err <- try(download.file(official_url, destfile = output_file, quiet = TRUE,
                           mode = "wb"))
  if (class(err) == "try-error") next
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(r),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_peru_87.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_peru_87.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}


#****************************************Script - 2************************************************************

#Web-scraped all the downloaded records for 'India' or 'CTRI' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:1948)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("peru_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    peru_page <- read_html(myfile)
    
    if(is_empty(peru_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    Serial_no <- ids[i]
    Reg_num <- peru_page %>% html_nodes("td td td td tr:nth-child(2) tr:nth-child(2) .TdNegroCuerposPq") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Reg_num <- new_function(Reg_num)
    Reg_date <- peru_page %>% html_nodes("tr:nth-child(4) tr:nth-child(7) .TdNegroCuerposPq:nth-child(1)") %>% .[[1]] %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Reg_date <- new_function(Reg_date)
    Updt_date <- peru_page %>% html_nodes("tr:nth-child(7) .TdNegroCuerposPq+ .TdNegroCuerposPq") %>% .[[1]] %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Updt_date <- new_function(Updt_date)
    Scientific_title <- peru_page %>% html_nodes("tr:nth-child(3) .TdNegroCuerposPq") %>% html_text() %>% .[[1]] %>% str_remove_all("\n") %>% str_remove_all('\"') %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Scientific_title <- new_function(Scientific_title)
    Public_title <- peru_page %>% html_nodes("tr~ tr+ tr tr:nth-child(5) .TdNegroCuerposPq") %>% html_text() %>% .[[1]] %>% str_remove_all('\"') %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Public_title <- new_function(Public_title)
    WP_india<- peru_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- peru_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri<- peru_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- peru_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    India_ctri_peru <- data.frame(Serial_no,Reg_num,Reg_date,Updt_date,Scientific_title,Public_title,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    
    write.table(India_ctri_peru,"Scraped_fr_srching_ind_ctri_peru.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_peru.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}

#*********************************************Script - 3****************************************************************

#Web scraped the field 'Countries where the enrollment is conducted' from the records which have 'India' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(2:1691)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("peru_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    peru_page <- read_html(myfile)
    
    if(is_empty(peru_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "NA"
      } else if (a == "") {
        a <- "NA"
      } else {
        return(a)
      }
    }
    
    Serial_no <- ids[i]
    Reg_num <- peru_page %>% html_nodes("td td td td tr:nth-child(2) tr:nth-child(2) .TdNegroCuerposPq") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Reg_num <- new_function(Reg_num)
    Reg_date <- peru_page %>% html_nodes("tr:nth-child(4) tr:nth-child(7) .TdNegroCuerposPq:nth-child(1)") %>% .[[1]] %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Reg_date <- new_function(Reg_date)
    Updt_date <- peru_page %>% html_nodes("tr:nth-child(7) .TdNegroCuerposPq+ .TdNegroCuerposPq") %>% .[[1]] %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Updt_date <- new_function(Updt_date)
    Scientific_title <- peru_page %>% html_nodes("tr:nth-child(3) .TdNegroCuerposPq") %>% html_text() %>% .[[1]] %>% str_remove_all("\n") %>% str_remove_all('\"') %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Scientific_title <- new_function(Scientific_title)
    Public_title <- peru_page %>% html_nodes("tr~ tr+ tr tr:nth-child(5) .TdNegroCuerposPq") %>% html_text() %>% .[[1]] %>% str_remove_all('\"') %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.) %>% str_trim() %>% str_squish()
    Public_title <- new_function(Public_title)
    Countries_of_recruitment <- peru_page %>% html_nodes("td td td td td td .TdNegroCuerposPq tr .TdNegroCuerposPq") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all('\"') %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all("-") %>% toString(.) %>% str_trim() %>% str_squish()
    Countries_of_recruitment <- new_function(Countries_of_recruitment)
    Secondary_ID <- peru_page %>% html_nodes("tr:nth-child(40) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(39) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(30) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(38) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(37) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(36) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(35) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(34) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(33) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(31) .TdNegroCuerposPq+ .TdNegroCuerposPq, tr:nth-child(32) .TdNegroCuerposPq+ .TdNegroCuerposPq") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all('\"') %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all("-") %>% toString(.) %>% str_trim() %>% str_squish()
    Secondary_ID <- new_function(Secondary_ID)
    Phase <- peru_page %>% html_nodes(".TdNegroCuerposPq+ tr .TdNegroCuerposPq:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all('\"') %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all("-") %>% toString(.) %>% str_trim() %>% str_squish()
    Phase <- new_function(Phase)
    WP_ctri<- peru_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    
    
    Countries_secondary_ID_319 <- data.frame(Serial_no,Reg_num,Reg_date,Updt_date,Scientific_title,Public_title,Countries_of_recruitment,Secondary_ID,Phase,WP_ctri)
    
    write.table(Countries_secondary_ID_319,"Scraped_fr_cor_ind_ctri_peru.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_cor_ind_ctri_peru.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}




