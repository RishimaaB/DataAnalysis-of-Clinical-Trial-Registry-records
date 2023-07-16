#***************************************************************Script - 1***************************************************************************************************
#Downloaded all the records from the IRCT registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:34697)
for (page_result in seq(from=1, to= 1388)) {
  url <- url(paste0("https://www.irct.ir/search/result?query=%28phase%3A%220%22%20OR%20phase%3A%221%22%20OR%20phase%3A%222%22%20OR%20phase%3A%223%22%20OR%20phase%3A%224%22%20OR%20phase%3A%221-2%22%20OR%20phase%3A%222-3%22%20OR%20phase%3A%22beq%22%20OR%20phase%3A%22na%22%29&filters=%5B%5D&page=",page_result))
  page <- read_html(url)
  links <- page %>% html_nodes(".result-title a") %>% html_attr("href")
  links <- paste0("https://www.irct.ir",links)
  links <- data.frame(links)
  write.table(links, "irct_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("irct_links.csv"), append = T)
}
irct_page_links <- data.frame()
irct_page_links <- read.csv("irct_links.csv")
for (i in seq_along(ids)) {
  official_url = irct_page_links[ids[i],"links"] 
  output_file=paste0("irct_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_irct.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_irct.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}


#*************************************************************************Script - 2*******************************************************************************************
#Web-scraped all the records for the keyword 'India' or 'CTRI' (case-insensitive)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:34697)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("irct_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    irct_page <- read_html(myfile)
    
    if(is_empty(irct_page)) {
      
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
    Reg_num <- irct_page %>% html_nodes(".field div div:nth-child(1) span+ strong") %>% html_text() %>% toString(.)
    Reg_num <- new_function(Reg_num)
    WP <- irct_page %>% html_nodes(".field") %>% html_text() %>% str_squish() %>% str_trim()
    WP_india <- toString(WP) %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE))
    WP_india_no_bound <- toString(WP) %>% str_extract(., regex("india", ignore_case = TRUE)) 
    WP_ctri <- toString(WP) %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE))
    
    India_ctri_irct <- data.frame(Serial_no,Reg_num,WP_india,WP_india_no_bound,WP_ctri)
    
    write.table(India_ctri_irct,"Scraped_fr_srching_ind_ctri_irct.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_irct.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}


#************************************************************************Script - 3**************************************************************************************************
#Script - 3 and Script - 4 was utilized to scrape only those fields which have the 'India' or 'CTRI' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(300:35000)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("irct_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    irct_page <- read_html(myfile)
    
    if(is_empty(irct_page)) {
      
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
    ##Scraping  India from protocol field
    Protocol_fields <- irct_page %>% html_nodes("#trial-summary+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Protocol_india <- toString(Protocol_fields) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from general information
    General_information <- irct_page %>% html_nodes("#trial-trial+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    General_information_india <- toString(General_information) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from secondary ID
    Secondary_identifiers <- irct_page %>% html_nodes("#trial-secondary_id+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Secondary_identifiers_india <- toString(Secondary_identifiers) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from ethics committee
    Ethics_committee <- irct_page %>% html_nodes("#trial-ethical_center+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Ethics_india <- toString(Ethics_committee) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from Health conditions studied
    Health_conditions <- irct_page %>% html_nodes("#trial-health_condition+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Health_india <- toString(Health_conditions) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from primary outcome
    Primary_outcome <- irct_page %>% html_nodes("#trial-primary_outcome+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Primary_outcome_india <- toString(Primary_outcome) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from secondary outcome
    Secondary_outcome <- irct_page %>% html_nodes("#trial-secondary_outcome+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Secondary_outcome_india <- toString(Secondary_outcome) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from intervention groups
    Intervention_groups <- irct_page %>% html_nodes("#trial-intervention+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Intervention_groups_india <- toString(Intervention_groups) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from recruitment centers
    Recruitment_centres <- irct_page %>% html_nodes("#trial-recruitment_center+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Recruitment_centres_india <- toString(Recruitment_centres) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from sponsors
    Sponsors <- irct_page %>% html_nodes("#trial-sponsor+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Sponsors_india <- toString(Sponsors) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from person responsible for general inquiries
    Person_gen_inq <- irct_page %>% html_nodes("#trial-public_queries_contact+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Person_gen_inq_india <- toString(Person_gen_inq) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from person responsible for scientific inquiries
    Person_scien_inq <- irct_page %>% html_nodes("#trial-scientific_queries_contact+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Person_scien_inq_india <- toString(Person_scien_inq) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from person responsible for updating data
    Person_updt_data <- irct_page %>% html_nodes("#trial-data_updater_contact+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Person_updt_data_india <- toString(Person_updt_data) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    ##Scraping India from sharing plan
    Sharing_plan <- irct_page %>% html_nodes("#trial-sharing_plan+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Sharing_plan_india <- toString(Sharing_plan) %>% str_extract(., regex("india", ignore_case = TRUE))
    
    fields <- data.frame(Serial_no,Protocol_india,General_information_india,Secondary_identifiers_india,Ethics_india,Health_india,
                         Primary_outcome_india,Secondary_outcome_india,Intervention_groups_india,Recruitment_centres_india,
                         Sponsors_india,Person_gen_inq_india,Person_scien_inq_india,Person_updt_data_india,
                         Sharing_plan_india)
    
    write.table(fields,"scrp_india_95_container.csv", sep = ",",row.names = FALSE, col.names = !file.exists("scrp_india_95_container.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
  }
}


#*****************************************************************************Script - 4*********************************************************************************************

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(300:35000)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("irct_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    irct_page <- read_html(myfile)
    
    if(is_empty(irct_page)) {
      
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
    Protocol_fields <- irct_page %>% html_nodes("#trial-summary+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Protocol_fields <- new_function(Protocol_fields)
    Reg_num <- irct_page %>% html_nodes(".field div div:nth-child(1) span+ strong") %>% html_text() %>% toString(.)
    Reg_num <- new_function(Reg_num)
    Reg_date <- irct_page %>% html_nodes(".field div:nth-child(2) span+ strong") %>% html_text()
    Reg_date <- new_function(Reg_date)
    Last_updt_date <- irct_page %>% html_nodes("div:nth-child(5) span+ strong") %>% html_text() 
    Last_updt_date <- new_function(Last_updt_date)
    Phase <- irct_page %>% html_nodes(".dl-subsection:nth-child(8) .field:nth-child(2)") %>% html_text() %>% toString(.) %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all("\n")
    Phase <- new_function(Phase)
    General_information <- irct_page %>% html_nodes("#trial-trial+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    General_information <- new_function(General_information)
    Secondary_identifiers <- irct_page %>% html_nodes("#trial-secondary_id+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Secondary_identifiers <- new_function(Secondary_identifiers)
    Primary_outcome <- irct_page %>% html_nodes("#trial-primary_outcome+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Primary_outcome <- new_function(Primary_outcome)
    Secondary_outcome <- irct_page %>% html_nodes("#trial-secondary_outcome+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Secondary_outcome <- new_function(Secondary_outcome)
    Intervention_groups <- irct_page %>% html_nodes("#trial-intervention+ .section-inner .field") %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
    Intervention_groups <- new_function(Intervention_groups)
    
    fields_india <- data.frame(Serial_no,Reg_num,Reg_date,Last_updt_date,Secondary_identifiers,
                               Phase,Protocol_fields,General_information,Primary_outcome,Secondary_outcome,
                               Intervention_groups)
    
    write.table(fields_india,"scrp_india_95_rcrds_info_detail.csv", sep = ",",row.names = FALSE, col.names = !file.exists("scrp_india_95_rcrds_info_detail.csv"), append = T)
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
  }}


