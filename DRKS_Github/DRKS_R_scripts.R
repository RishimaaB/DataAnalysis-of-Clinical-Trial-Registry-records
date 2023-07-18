#*******************************************************************Script 1*************************************************************************************************
#Downloaded all the records present in the DRKS registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:32000)
ids = str_pad(ids, width = 8, side = "left", pad = 0)
counter=0
for (i in seq_along(ids)) {
  myurl = paste0("https://drks.de/search/en/trial/DRKS", ids[i]) 
  url = url(paste0("https://drks.de/search/en/trial/DRKS",ids[i]))
  drks_page = read_html(url)
  keyword = drks_page %>% html_nodes(".modal-title") %>% html_text() %>% str_extract("Error!")
  keyword = toString(keyword)
  if (keyword == "")
  {
    keyword <- "NA"
  }
  if (keyword != "Error!") { 
    myfile = paste0("drks_page",ids[i],".html")
    download.file(myurl, destfile = myfile, quiet = TRUE)
    time_of_download = as.character(timestamp())
    
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            downloaded_time = time_of_download,
                            URL = as.character(myurl))
    write.table(time_stamp, "time_stamp_DRKS.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_DRKS.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
  }
  else {
    print("page is invalid one")
  }
}


#**********************************************************************Script - 2********************************************************************************************
#Web scraped the downloaded records for the 'India' or 'CTRI' keyword (case-insensitive)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:31282)
ids = str_pad(ids, width = 8, side = "left", pad = 0)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("drks_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    drks_page <- read_html(myfile)
    
    if(is_empty(drks_page)) {
      
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
    Registration_number <- drks_page %>% html_nodes(".trial-details-float dd:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Registration_number <- new_function(Reg_num)
    WP_india<- drks_page %>% html_nodes("h3 , dd , p , .withLineBreak") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- drks_page %>% html_nodes("h3 , dd , p , .withLineBreak") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri <- drks_page %>% html_nodes("h3 , dd , p , .withLineBreak") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    
    India_ctri_drks <- data.frame(Serial_no,Registration_number,WP_india,WP_india_no_bound,WP_ctri)
    
    write.table(India_ctri_drks,"Scraped_fr_srching_ind_ctri_DRKS.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_DRKS.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
  }
}


#**************************************************************************Script - 3*****************************************************************************************
#Web-scraped the field 'Countries of Recruitment' from the records which have the keyword 'India'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(99:30592)
ids = str_pad(ids, width = 8, side = "left", pad = 0)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("drks_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    drks_page <- read_html(myfile)
    
    if(is_empty(drks_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "--"
      } else if (a == "") {
        a <- "--"
      } else {
        return(a)
      }
    }
    
    Serial_no <- ids[i]
    Reg_num <- drks_page %>% html_nodes(".trial-details-float dd:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Reg_num <- new_function(Reg_num)
    title <- drks_page %>% html_nodes("#j_idt76+ h3") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    title <- new_function(title)
    Date_of_registration <- drks_page %>% html_nodes(".trial-details-float dd:nth-child(6)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Date_of_registration <-new_function(Date_of_registration)
    Last_updt_date <- drks_page %>% html_nodes(".trial-details-float dd:nth-child(8) .withLineBreak") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Last_updt_date <- new_function(Last_updt_date)
    Type_of_study <- drks_page %>% html_nodes(".col-md-4:nth-child(2) dd:nth-child(2) .withLineBreak") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Type_of_study <- new_function(Type_of_study)
    Countries <- drks_page %>% html_nodes(".col-md-6:nth-child(1) .card-body") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    Phase <- drks_page %>% html_nodes(".mb-0 dd:nth-child(8) .withLineBreak") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString(.)
    Phase <- new_function(Phase)
    Countries_all <- drks_page %>% html_nodes("dd:nth-child(2) .mb-0") %>% html_text() %>% str_replace_all("\n\t\t\t\t\t\t",",") %>% str_replace_all("\n\t\t\t\t\n\t\t",":") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% toString(.)
    Countries_all <- new_function(Countries_all)
    
    fields_drks <- data.frame(Serial_no,Reg_num,title,Date_of_registration,Last_updt_date,
                              Type_of_study,Countries,Countries_all,Phase)
    
    
    write.table(fields_drks,"Scraped_fr_srching_ind_ctri_COR.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_COR.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}


#*****************************************************************Script - 4*************************************************************************************************
#Storing all the files in a local SQLite database 

libraries = c("tidyverse", "stringr",  "rvest", "XML", "purrr", "data.table", "DBI", "httr")
lapply(libraries, require, character.only = TRUE)

mydb <- dbConnect(RSQLite::SQLite(), "DRKS.sqlite")
file <- read.csv(file.choose())
dbWriteTable(mydb, "file", file, append = TRUE)

#*******************************************************************The End*****************************************************************************************************
