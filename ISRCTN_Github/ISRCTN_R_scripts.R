#******************************************************************Script - 1*************************************************************************************************
#Downloaded all the records from the ISRCTN registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:23215)

for (page_result in seq(from=1,to=2322)) {
  link <- url(paste0("https://www.isrctn.com/search?q=&page=",page_result))
  page <- read_html(link)
  record_links <- page %>% html_nodes(".ResultsList_item_title a") %>% html_attr("href")
  record_links <- paste0("https://www.isrctn.com",record_links)
  isrctn_page_links <- data.frame(record_links)
  write.table(isrctn_page_links, "isrctn_page_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("isrctn_page_links.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
  
}
isrctn_page_links <- data.frame()
isrctn_page_links <- read.csv("isrctn_page_links.csv")
View(isrctn_page_links)
for (i in seq_along(ids)) {
  official_url = isrctn_page_links[ids[i],"record_links"] 
  output_file=paste0("isrctn_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_isrctn.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_isrctn.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}



#**********************************************************************Script - 2**********************************************************************************************
#Web-scraped all the downloaded records for the 'India' or 'CTRI' keyword (case-insensitive)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:23215)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("isrctn_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    isrctn_page <- read_html(myfile)
    
    if(is_empty(isrctn_page)) {
      
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
    Registration_number <- isrctn_page %>% html_nodes(".ComplexTitle_primary") %>% html_text() %>% toString()
    Registration_number <- new_function(Registration_number)
    Registration_date <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(4)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Registration_date <- new_function(Registration_date)
    Last_edited <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(6)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Last_edited <- new_function(Last_edited)
    Recruitment_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(2)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Recruitment_status <- new_function(Recruitment_status)
    Overall_trial_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(4)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Overall_trial_status <- new_function(Overall_trial_status)
    WP_india<- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri<- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    
    India_ctri_isrctn<- data.frame(Trial_ID,Registration_number,Registration_date,Last_edited,Recruitment_status,Overall_trial_status,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    write.table(India_ctri_isrctn,"Scraped_fr_srching_ind_ctri_isrctn.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_isrctn.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}




#*************************************************************************Script - 3*************************************************************************************************

#Web-scraped the field 'Countries of recruitment' from the records which have 'India' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(1:23114)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("isrctn_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    isrctn_page <- read_html(myfile)
    
    if(is_empty(isrctn_page)) {
      
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
    Registration_number <- isrctn_page %>% html_nodes(".ComplexTitle_primary") %>% html_text() %>% toString()
    Registration_number <- new_function(Registration_number)
    Registration_date <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(4)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Registration_date <- new_function(Registration_date)
    Last_edited <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(6)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Last_edited <- new_function(Last_edited)
    Scientific_title <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Scientific_title <- new_function(Scientific_title)
    Recruitment_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(2)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Recruitment_status <- new_function(Recruitment_status)
    Overall_trial_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(4)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Overall_trial_status <- new_function(Overall_trial_status)
    WP_india<- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri<- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- isrctn_page %>% html_nodes(".PageStickyFooter_row--expanded .l-Container") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    Recruitment_country <- isrctn_page %>% html_nodes(".\\]:nth-child(6) p:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% toString() %>% str_squish() %>% str_trim()
    Recruitment_country <- new_function(Recruitment_country)
    Country_india <- isrctn_page %>% html_nodes(".\\]:nth-child(6) p:nth-child(2)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Recruitment_centres <- isrctn_page %>% html_nodes(".\\]:nth-child(6) p") %>% html_text() %>% str_remove_all("\n") %>% toString() %>% str_squish() %>% str_trim()
    Primary_study_design  <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(12)") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Primary_study_design <- new_function(Primary_study_design)
    Phase  <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(28)") %>% html_text() %>% toString() %>% str_replace_all('\"',",")  %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Phase <- new_function(Phase)
    
    India_ctri_isrctn<- data.frame(Trial_ID,Registration_number,Registration_date,Last_edited,Scientific_title,Recruitment_status,Overall_trial_status,
                                   WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound,Recruitment_country,Country_india,Recruitment_centres,Primary_study_design,Phase)
    write.table(India_ctri_isrctn,"Scraped_fr_srching_ind_cor_isrctn_fnl.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_cor_isrctn_fnl.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}



#************************************************************************Script - 4**************************************************************************************************

#Scraping the remaining records which does not have 'India' in the field 'Countries of recruitment' but in other fields. 

##Script 4a

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter = 0
ids = c(277,535,563,580,581,736,774,968,1047,1102,1179,1306,1340,1377,1494,1679,1762,1796,2068,2126,2428,2429,2443,2510,2532,2599,2654,2761,2774,2838,2901,3022,3158,3230,3231,3343,3446,3454,3484,3620,3625,3635,3664,3790,3898,4463,4562,4815,5042,5281,5306,5581,5763,6010,6072,6113,6219,6293,6294,6296,6451,6552,6880,7058,7167,7198,7227,7238,7390,7614,7824,7836,7884,7898,7955,8941,9041,9120,9561,9676,9724,9773,10318,10409,10499,10554,10647,10833,10942,11016,11157,11356,11418,11691,11699,11924,11986,12402,12433,12546,13041,13149,13342,13368,13815,13858,14020,14057,14483,14810,15222,15820,16191,16426,16859,16887,17242,17285,17718,17891,18467,18507,18756,18805,18907,19112,19376,19480,20288,20479,20487,21808,21852)
for (i in seq_along(ids)) {
  
  myfile = paste0("isrctn_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    isrctn_page <- read_html(myfile)
    
    if(is_empty(isrctn_page)) {
      
      next 
    }
    
    
    
    Trial_ID <- ids[i]
    Registration_number <- isrctn_page %>% html_nodes(".ComplexTitle_primary") %>% html_text() %>% toString()
    Registration_number <- new_function(Registration_number)
    Registration_date <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(4)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Registration_date <- new_function(Registration_date)
    Last_edited <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(6)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Last_edited <- new_function(Last_edited)
    Scientific_title <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Scientific_title <- new_function(Scientific_title)
    Recruitment_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(2)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Recruitment_status <- new_function(Recruitment_status)
    Overall_trial_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(4)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Overall_trial_status <- new_function(Overall_trial_status)
    Country_india <- isrctn_page %>% html_nodes(".\\]:nth-child(6) p:nth-child(2)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    First_para_india <- isrctn_page %>% html_nodes(".Info_aside") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Plain_english_summary <- isrctn_page %>% html_nodes(".Section--default:nth-child(1)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Contact_details <- isrctn_page %>% html_nodes(".\\]:nth-child(2)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Additional_identifiers <- isrctn_page %>% html_nodes(".\\]:nth-child(3)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Study_information <- isrctn_page %>% html_nodes(".\\]:nth-child(4)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Eligibility <- isrctn_page %>% html_nodes(".\\]:nth-child(5) .Info_main") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Location <- isrctn_page %>% html_nodes(".\\]:nth-child(6) .Info_main") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Sponsor <- isrctn_page %>% html_nodes(".\\]:nth-child(7) .Info_main") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Funders <- isrctn_page %>% html_nodes(".\\]:nth-child(8)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Results_and_Publications <- isrctn_page %>% html_nodes(".\\]:nth-child(9) .Info_main") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Additional_files <- isrctn_page %>% html_nodes(".\\]:nth-child(10)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Editorial_notes <- isrctn_page %>% html_nodes(".\\]:nth-child(11)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    
    
    file <- data.frame(Trial_ID,Registration_number,Registration_date,Last_edited,
                       Scientific_title,Recruitment_status,Overall_trial_status,
                       Country_india,First_para_india,Plain_english_summary,Contact_details,
                       Additional_identifiers,Study_information,Eligibility,
                       Location,Sponsor,Funders,Results_and_Publications,Additional_files,Editorial_notes)
    
    write.table(file,"scrap_133_container_india2.csv", sep = ",",row.names = FALSE, col.names = !file.exists("scrap_133_container_india2.csv"), append = T)
    
    
    
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
  }}

##Script 4b


libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter = 0
ids = c(277,535,563,580,581,736,774,968,1047,1102,1179,1306,1340,1377,1494,1679,1762,1796,2068,2126,2428,2429,2443,2510,2532,2599,2654,2761,2774,2838,2901,3022,3158,3230,3231,3343,3446,3454,3484,3620,3625,3635,3664,3790,3898,4463,4562,4815,5042,5281,5306,5581,5763,6010,6072,6113,6219,6293,6294,6296,6451,6552,6880,7058,7167,7198,7227,7238,7390,7614,7824,7836,7884,7898,7955,8941,9041,9120,9561,9676,9724,9773,10318,10409,10499,10554,10647,10833,10942,11016,11157,11356,11418,11691,11699,11924,11986,12402,12433,12546,13041,13149,13342,13368,13815,13858,14020,14057,14483,14810,15222,15820,16191,16426,16859,16887,17242,17285,17718,17891,18467,18507,18756,18805,18907,19112,19376,19480,20288,20479,20487,21808,21852)
for (i in seq_along(ids)) {
  
  myfile = paste0("isrctn_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    isrctn_page <- read_html(myfile)
    
    if(is_empty(isrctn_page)) {
      
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
    Registration_number <- isrctn_page %>% html_nodes(".ComplexTitle_primary") %>% html_text() %>% toString()
    Registration_number <- new_function(Registration_number)
    Registration_date <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(4)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Registration_date <- new_function(Registration_date)
    Last_edited <- isrctn_page %>% html_nodes(".Meta:nth-child(1) .Meta_value:nth-child(6)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Last_edited <- new_function(Last_edited)
    Scientific_title <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(2)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Scientific_title <- new_function(Scientific_title)
    Recruitment_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(2)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Recruitment_status <- new_function(Recruitment_status)
    Overall_trial_status <- isrctn_page %>% html_nodes(".Meta+ .Meta .Meta_value:nth-child(4)") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_trim() %>% str_squish()
    Overall_trial_status <- new_function(Overall_trial_status)
    Country_india <- isrctn_page %>% html_nodes(".\\]:nth-child(6) p:nth-child(2)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Primary_study_design  <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(12)") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Primary_study_design <- new_function(Primary_study_design)
    Phase  <- isrctn_page %>% html_nodes(".\\]:nth-child(4) p:nth-child(28)") %>% html_text() %>% toString() %>% str_replace_all('\"',",")  %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Phase <- new_function(Phase)
    Plain_english_summary  <- isrctn_page %>% html_nodes(".Section--default:nth-child(1)") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Plain_english_summary <- new_function(Plain_english_summary)
    Contact  <- isrctn_page %>% html_nodes(".\\]:nth-child(2) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Contact <- new_function(Contact)
    Study_information  <- isrctn_page %>% html_nodes(".\\]:nth-child(4) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Study_information <- new_function(Study_information)
    Eligibility  <- isrctn_page %>% html_nodes(".\\]:nth-child(5) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Eligibility <- new_function(Eligibility)
    Locations  <- isrctn_page %>% html_nodes(".\\]:nth-child(6) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Locations <- new_function(Locations)
    Sponsor_information <- isrctn_page %>% html_nodes(".\\]:nth-child(7) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Sponsor_information <- new_function(Sponsor_information)
    Funders <- isrctn_page %>% html_nodes(".\\]:nth-child(8) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",")%>%  str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Funders <- new_function(Funders)
    Results_and_publication <- isrctn_page %>% html_nodes(".\\]:nth-child(9) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",")  %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Results_and_publication <- new_function(Results_and_publication)
    Editorial_notes <- isrctn_page %>% html_nodes(".\\]:nth-child(11) .Info_main") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Editorial_notes <- new_function(Editorial_notes)
    
    file <- data.frame(Trial_ID,Registration_number,Registration_date,Last_edited,Scientific_title,Recruitment_status,Overall_trial_status,
                       Country_india,Primary_study_design,Phase,Plain_english_summary,Contact,Study_information,
                       Eligibility,Locations,Sponsor_information,Funders,Results_and_publication,Editorial_notes)
    
    write.table(file,"scrap_133_detail_india.csv", sep = ",",row.names = FALSE, col.names = !file.exists("scrap_133_detail_india.csv"), append = T)
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
  }
}

#*****************************************************************Script - 5*************************************************************************************************
#Storing all the files in a local SQLite database 

libraries = c("tidyverse", "stringr",  "rvest", "XML", "purrr", "data.table", "DBI", "httr")
lapply(libraries, require, character.only = TRUE)

mydb <- dbConnect(RSQLite::SQLite(), "ISRCTN.sqlite")
file <- read.csv(file.choose())
dbWriteTable(mydb, "file", file, append = TRUE)

#*******************************************************************The End*****************************************************************************************************



