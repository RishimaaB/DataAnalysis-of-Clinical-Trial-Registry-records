#*****************************************************************Script-1****************************************************************************************************
#This script is used to extract the links of all the records present in the ChiCTR registry)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0

for (page_result in seq(from=1,to=6882)) {
  link <- paste0("https://www.chictr.org.cn/searchprojEN.html?page=",page_result,"&title=&officialname=&subjectid=&regstatus=&regno=&secondaryid=&applier=&studyleader=&createyear=&sponsor=&secsponsor=&sourceofspends=&studyailment=&studyailmentcode=&studytype=&studystage=&studydesign=&recruitmentstatus=&gender=&agreetosign=&measure=&country=&province=&city=&institution=&institutionlevel=&intercode=&ethicalcommitteesanction=&whetherpublic=&minstudyexecutetime=&maxstudyexecutetime=&btngo=btn")
  page <- curl::curl(link) %>% read_html(link)
  record_links <- page %>% html_nodes(".wto") %>% html_attr("href")
  record_links <- paste0("http://www.chictr.org.cn/",record_links)
  registration_number <- page %>% html_nodes("td:nth-child(2)") %>% html_text() %>% str_squish() %>% str_trim()
  time <- as.character(timestamp())
  chinese_page_links <- data.frame(page_result,record_links,registration_number,time)
  write.table(chinese_page_links, "ChiCTR_page_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("ChiCTR_page_links.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
  
}



#************************************************************************Script-2**********************************************************************************************
#Web scraped the downloaded records for the 'India' and the 'CTRI' keyword (case-insensitive) 

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:66773)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0(ids[i],"_english_chictr.html")
  
  if (file.exists(myfile)) {
    
    chictr_page <- read_html(myfile)
    
    if(is_empty(chictr_page)) {
      
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
    
    
    Page_number <- ids[i]
    Registration_number= try(chictr_page %>% html_nodes(xpath="/html/body/div[1]/div/div[3]/div[3]/table/tbody/tr[1]/td[2]") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString())
    if (class(Registration_number) == "try-error") next
    Registration_number = new_function(Registration_number)
    Registered_date = chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(3) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% map_chr(1) %>% str_squish() %>% str_trim() 
    Registered_date = toString(Registered_date) 
    Registered_date = new_function(Registered_date)
    Last_updated_date = chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(2) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() 
    Last_updated_date = toString(Last_updated_date)
    Last_updated_date = new_function(Last_updated_date)
    Registration_number_partner_registry = chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(13) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number_partner_registry = new_function(Registration_number_partner_registry)
    WP_india = chictr_page %>% html_nodes("td , p") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound = chictr_page %>% html_nodes("td , p") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri = chictr_page %>% html_nodes("td , p") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound = chictr_page %>% html_nodes("td , p") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    
    
    India_ctri_cris<- data.frame(Page_number,Registration_number,Registered_date,Last_updated_date,Registration_number_partner_registry,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    write.table(India_ctri_cris,"Scraped_fr_srching_ind_ctri_ChiCTR.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_ChiCTR.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
    
  }}


#************************************************************Script - 3********************************************************************************************************
#Web scraped the field 'Countries of recruitment and research settings' in all the records with the keyword 'India'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:63)


counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0(ids[i],"_english_chictr.html")
  
  if (file.exists(myfile)) {
    
    chictr_page <- read_html(myfile)
    
    if(is_empty(chictr_page)) {
      
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
    
    
    
    
    Page_number = ids[i]
    Registration_number= chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(1) .left_title+ td p") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number = new_function(Registration_number)
    Registered_date = chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(3) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% map_chr(1) %>% str_squish() %>% str_trim() 
    Registered_date = toString(Registered_date) 
    Registered_date = new_function(Registered_date)
    Last_updated_date = chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(2) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() 
    Last_updated_date = toString(Last_updated_date)
    Last_updated_date = new_function(Last_updated_date)
    Public_title = chictr_page %>% html_nodes(".project-tit+ .project-ms .en:nth-child(7) .left_title+ td .en") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Public_title = new_function(Public_title)
    Scientific_title = chictr_page %>% html_nodes(".project-tit+ .project-ms .en:nth-child(11) .left_title+ td .en") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Scientific_title = new_function(Scientific_title)
    Phase = chictr_page %>% html_nodes(".en:nth-child(13) .left_title+ td .en") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Phase = new_function(Phase)
    Study_type = chictr_page %>% html_nodes(".project-ms~ .project-ms+ .project-ms .en:nth-child(11) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Study_type = new_function(Study_type)
    Registration_number_partner_registry = chictr_page %>% html_nodes(".project-tit+ .project-ms tr:nth-child(13) .left_title+ td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number_partner_registry = new_function(Registration_number_partner_registry)
    Country_of_recruitment = chictr_page %>% html_nodes("table.noComma .en:nth-child(2) td:nth-child(2) p") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Country_of_recruitment = new_function(Country_of_recruitment)
    Institution = chictr_page %>% html_nodes(".en .sub:nth-child(2) p") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Institution = new_function(Institution)
    City = chictr_page %>% html_nodes(".en td:nth-child(6)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    City = new_function(City)
    
    
    
    file <- data.frame(Page_number,Registration_number,Registered_date,Last_updated_date,Registration_number_partner_registry,Public_title,Scientific_title,Phase,Study_type,Country_of_recruitment,Institution,City)
    
    write.table(file,"Country_of_recruitment_ChiCTR.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Country_of_recruitment_ChiCTR.csv"), append = T)
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    }
}

#*****************************************************************Script - 4*************************************************************************************************
#Storing all the files in a local SQLite database 

libraries = c("tidyverse", "stringr",  "rvest", "XML", "purrr", "data.table", "DBI", "httr")
lapply(libraries, require, character.only = TRUE)

mydb <- dbConnect(RSQLite::SQLite(), "ChiCTR.sqlite")
file <- read.csv(file.choose())
dbWriteTable(mydb, "file", file, append = TRUE)

#*******************************************************************The End*****************************************************************************************************
