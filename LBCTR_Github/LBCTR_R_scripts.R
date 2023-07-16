#*******************************************************************Script-1**************************************************************************************************
#Downloading all the records from the LBCTR registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:148)
for (page_result in seq(from=1, to= 15)) {
  url <- url(paste0("https://lbctr.moph.gov.lb/LBCTR/Trials/View?Grid-sort=&Grid-page=",page_result,"&Grid-pageSize=10&Grid-group=&Grid-filter="))
  page <- read_html(url)
  links <- page %>% html_nodes(".mbold a") %>% html_attr("href")
  links <- paste0("https://lbctr.moph.gov.lb",links)
  links <- data.frame(links)
  write.table(links, "LBCTR_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("LBCTR_links.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter))
}

lbctr_page_links <- data.frame()
lbctr_page_links <- read.csv("LBCTR_links.csv")
for (i in seq_along(ids)) {
  official_url = lbctr_page_links[ids[i],"links"] 
  output_file=paste0("lbctr_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_lbctr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_lbctr.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}



#****************************************************************Script - 2***************************************************************************************************
#Web scraped all the downloaded records for the 'India' or 'CTRI' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:148)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("lbctr_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    lbctr_page <- read_html(myfile)
    
    if(is_empty(lbctr_page)) {
      
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
    Reg_num <- lbctr_page %>% html_nodes(".form-group:nth-child(1) .col-md-6:nth-child(1) .data-label") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Reg_num <- new_function(Reg_num)
    WP_india<- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri <- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    India_ctri_lbctr <- data.frame(Serial_no,Reg_num,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    
    write.table(India_ctri_lbctr,"Scraped_fr_srching_ind_ctri_lbctr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_lbctr.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}

#*******************************************************************Script-3**************************************************************************************************
#Web scraped the field 'Countries of recruitment' from the records which have the keyword 'India'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(5,17,21,22,27,29,31,42,46,87,90,91,92,105,108,109,110,112,114,118,131,134,136,139,141,145)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("lbctr_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    lbctr_page <- read_html(myfile)
    
    if(is_empty(lbctr_page)) {
      
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
    Reg_num <- lbctr_page %>% html_nodes(".form-group:nth-child(1) .col-md-6:nth-child(1) .data-label") %>% html_text() %>% .[1] %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all('\"') %>% str_squish() %>% str_trim() %>% toString(.)
    Reg_num <- new_function(Reg_num)
    MOH_registration_number <- lbctr_page %>% html_nodes(".form-group:nth-child(2) .col-md-6:nth-child(1) .data-label") %>% html_text() %>% .[1] %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all('\"') %>% str_squish() %>% str_trim() %>% toString(.)
    MOH_registration_number <- new_function(MOH_registration_number)
    Date_of_registration <- lbctr_page %>% html_nodes(".data-label:nth-child(5)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Date_of_registration <- new_function(Date_of_registration)
    Date_last_updated <- lbctr_page %>% html_nodes(".data-label:nth-child(7)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Date_last_updated <- new_function(Date_last_updated)
    Date_of_registration_nra <- lbctr_page %>% html_nodes(".form-group:nth-child(5) .data-label") %>% html_text() %>% .[1] %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Date_of_registration_nra <- new_function(Date_of_registration_nra)
    Public_title <- lbctr_page %>% html_nodes(".form-group:nth-child(7) .col-md-6:nth-child(1) .data-label") %>% html_text() %>% .[1] %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all('\"') %>% str_squish() %>% str_trim() %>% toString(.)
    Public_title <- new_function(Public_title)
    Scientific_title <- lbctr_page %>% html_nodes(".form-group:nth-child(8) .col-md-6:nth-child(1) .data-label") %>% html_text() %>% .[1] %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all('\"') %>% str_squish() %>% str_trim() %>% toString(.)
    Scientific_title <- new_function(Scientific_title)
    Brief_summary <- lbctr_page %>% html_nodes(".form-group:nth-child(9) .col-md-12 .no-padding .data-label") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Brief_summary <- new_function(Brief_summary)
    Type_of_study <- lbctr_page %>% html_nodes("br+ .form-group .no-padding") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_trim() %>% str_squish() %>% toString(.)
    Type_of_study <- new_function(Type_of_study)
    Phase <- lbctr_page %>% html_nodes(".typeOfStudy_Interventional .form-group:nth-child(4) .col-md-6+ .col-md-6 .no-padding") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_trim() %>% str_squish() %>% toString(.)
    Phase <- new_function(Phase)
    Countries <- lbctr_page %>% html_nodes("#tbodyCountriesOfRecruitment label") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% str_replace_all('\"',",") %>% str_squish() %>% str_trim() %>% toString()
    Countries <- new_function(Countries)
    WP_india<- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri <- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- lbctr_page %>% html_nodes("label") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    India_lbctr_countries <- data.frame(Serial_no,Reg_num,MOH_registration_number,Date_of_registration,Date_last_updated,Date_of_registration_nra,
                                        Public_title,Scientific_title,Brief_summary,Type_of_study,Phase,Countries,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    
    write.table(India_lbctr_countries,"Scraped_fr_srching_cor_ind_lbctr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_cor_ind_lbctr.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}





