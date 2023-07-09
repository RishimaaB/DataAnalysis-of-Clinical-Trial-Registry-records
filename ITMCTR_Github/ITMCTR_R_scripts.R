#*************************************Script 1**********************************************
#Downloaded all the records from the ITMCTR registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:4181)
for (page_result in seq(from=0, to= 278)) {
  url <- url(paste0("http://itmctr.ccebtcm.org.cn/en-US/Home/TrialSearch?pageNo=",page_result))
  page <- read_html(url)
  links <- page %>% html_nodes("td+ td a") %>% html_attr("href")
  links <- paste0("http://itmctr.ccebtcm.org.cn",links)
  registration_number <- page %>% html_nodes("td:nth-child(2)") %>% html_text() %>% str_squish() %>% str_trim()
  links <- data.frame(page_result,links,registration_number)
  write.table(links, "ITMCTR_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("ITMCTR_links.csv"), append = T)
  
}

ITMCTR_page_links <- data.frame()
ITMCTR_page_links <- read.csv("ITMCTR_links.csv")
for (i in seq_along(ids)) {
  official_url = ITMCTR_page_links[ids[i],"links"] 
  output_file=paste0("ITMCTR_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_ITMCTR.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_ITMCTR.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}



#**************************Script - 2*******************************************************************************
#Web-scraped all the records for the 'India' or 'CTRI' keyword (key-insensitive)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:4181)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("ITMCTR_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    itmctr_page <- read_html(myfile)
    
    if(is_empty(itmctr_page)) {
      
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
    
    Trial_ID <- ids[i]
    Registration_number <- itmctr_page %>% html_nodes(".form-group:nth-child(1) tr:nth-child(1) .col-lg-3+ td") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number <- new_function(Registration_number)
    Registration_date <- itmctr_page %>% html_nodes(".form-group:nth-child(1) tr:nth-child(3) .col-lg-3+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Registration_date <- new_function(Registration_date)
    Last_edited <- itmctr_page %>% html_nodes(".form-group:nth-child(1) tr:nth-child(2) .col-lg-3+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Last_edited <- new_function(Last_edited)
    Public_title <- itmctr_page %>% html_nodes(".form-group:nth-child(1) .en:nth-child(6) .col-lg-3+ td") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_squish() %>% str_trim() %>% toString()
    Public_title <- new_function(Public_title)
    Scientific_title <- itmctr_page %>% html_nodes(".form-group:nth-child(1) .en:nth-child(10) .col-lg-3+ td") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_squish() %>% str_trim() %>% toString()
    Scientific_title <- new_function(Scientific_title)
    Study_phase <- itmctr_page %>% html_nodes(".form-group:nth-child(5) tr:nth-child(4) .col-lg-3+ td") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_squish() %>% str_trim() %>% toString()
    Study_phase <- new_function(Study_phase)
    Study_type <- itmctr_page %>% html_nodes(".form-group:nth-child(5) tr:nth-child(3) td:nth-child(2)") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_squish() %>% str_trim() %>% toString()
    Study_type <- new_function(Study_type)
    WP_india<- itmctr_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- itmctr_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri<- itmctr_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- itmctr_page %>% html_nodes("td") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    
    India_ctri_itmctr<- data.frame(Trial_ID,Registration_number,Registration_date,Last_edited,Public_title,Scientific_title,Study_phase,Study_type,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    write.table(India_ctri_itmctr,"Scraped_fr_srching_ind_ctri_itmctr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_itmctr.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}


