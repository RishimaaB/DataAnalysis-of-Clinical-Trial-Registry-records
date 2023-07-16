#***********************************************************************Script-1*********************************************************************************************
#Downloading the records from the CRIS registry

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids = c(850:40000)
counter = 0

new_function <- function(a) {
  
  if (length(a) == 0) {
    a <- "NA"
  } else if (a == "") {
    a <- "NA"
  } else {
    return(a)
  }
}



for (i in seq_along(ids)) {
  official_url = paste0("https://cris.nih.go.kr/cris/search/detailSearch.do?seq=",ids[i])
  url = url(paste0("https://cris.nih.go.kr/cris/search/detailSearch.do?seq=",ids[i]))
  output_file=paste0("cris_page",ids[i],".html")
  myfile = read_html(url) 
  output1 = myfile %>% html_nodes("#step1 tr:nth-child(2) .text-nowrap") %>% html_text()
  output2 = myfile %>% html_nodes(".dtl_tit_n") %>% html_text() 
  
  if ((length(output1) != 0) | (length(output2) != 0)) {   
    Registration_number=myfile %>% html_nodes("#step1 tr:nth-child(2) td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number = new_function(Registration_number)
    Registered_date =myfile %>% html_nodes("span:nth-child(4)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% map_chr(1) %>% str_squish() %>% str_trim() 
    Registered_date = toString(Registered_date) 
    Registered_date = new_function(Registered_date)
    Last_updated_date =myfile %>% html_nodes("#wrapper span~ span+ span") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() 
    Last_updated_date = toString(Last_updated_date)
    Last_updated_date = new_function(Last_updated_date)
    
    download.file(official_url, destfile = output_file, quiet = TRUE)
    time_of_download = as.character(timestamp())
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            Downloaded_time = time_of_download,
                            URL=as.character(official_url),Registration_number,Registered_date,Last_updated_date)
    
    write.table(time_stamp, "time_stamp_CRIS.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_CRIS.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
  } else {
    print(counter)
    print("This record is not present")
  }
    
  
}



#*********************************************************************Script-2***********************************************************************************************
#Web scraped the downloaded records for the keyword 'India' or 'CTRI' (case-insensitive)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(859:24709)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("cris_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    cris_page <- read_html(myfile)
    
    if(is_empty(cris_page)) {
      
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
    Registration_number= cris_page %>% html_nodes("#step1 tr:nth-child(2) td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number = new_function(Registration_number)
    Registered_date = cris_page %>% html_nodes("span:nth-child(4)") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% map_chr(1) %>% str_squish() %>% str_trim() 
    Registered_date = toString(Registered_date) 
    Registered_date = new_function(Registered_date)
    Last_updated_date = cris_page %>% html_nodes("#wrapper span~ span+ span") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() 
    Last_updated_date = toString(Last_updated_date)
    Last_updated_date = new_function(Last_updated_date)
    WP_india = cris_page %>% html_nodes("#printDiv") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound = cris_page %>% html_nodes("#printDiv") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri = cris_page %>% html_nodes("#printDiv") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound = cris_page %>% html_nodes("#printDiv") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    
    
    India_ctri_cris<- data.frame(Trial_ID,Registration_number,Registered_date,Last_updated_date,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    write.table(India_ctri_cris,"Scraped_fr_srching_ind_ctri_CRIS.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_CRIS.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}

#***************************************************************Script - 3****************************************************************************************************

#Script-3a (This script was used for web scraping the field 'Study site' to retrieve the number of recruitment centers as written in that field)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(859:24709)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("cris_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    cris_page <- read_html(myfile)
    
    if(is_empty(cris_page)) {
      
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
    Registration_number= cris_page %>% html_nodes("#step1 tr:nth-child(2) td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Registration_number= new_function(Registration_number)
    Number_of_study_sites= cris_page %>% html_nodes("#step4 tr:nth-child(2) td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
    Number_of_study_sites= new_function(Number_of_study_sites)
    
    
    Studysites<- data.frame(Trial_ID,Registration_number,Number_of_study_sites)
    write.table(Studysites,"Scraped_fr_studysites_cris_14k.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_studysites_cris_14k.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}

##Script - 3b (This script was used for confirming the number of recruitment sites for each records)


libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- data.frame()
ids <- read.csv("20385_Eng_URLs.csv")
counter=1



new_function <- function(a) {
  
  if (length(a) == 0) {
    a <- "NA"
  } else if (a == "") {
    a <- "NA"
  } else {
    return(a)
  }
}


for (row in 1:nrow(ids)) {
  URL = ids[row,1]
  cris_page = read_html(URL)
  Registration_number = cris_page %>% html_nodes("#step1 tr:nth-child(2) td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
  Registration_number = new_function(Registration_number)
  Content_labels = cris_page %>% html_nodes(".dtl_sec_tit") %>% html_text() %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_squish() %>% str_trim()
  Content_labels = t(Content_labels)
  length_of_all_content_labels = data.frame(Registration_number,URL,Content_labels)
  write.table(length_of_all_content_labels,"length_of_content_labels_CRIS.csv", sep = ",",row.names = FALSE, col.names = !file.exists("length_of_content_labels_CRIS.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter))
  
  
}


#Script - 3c (This script was used for scraping the recruitment centres)

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(859:24709)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("cris_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    cris_page <- read_html(myfile)
    
    if(is_empty(cris_page)) {
      
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
    
    
    
    ids2 <- seq(10,360,5) ##71 max number of sites for 14k pages
    #ids2 <- seq(10,170,5) ##33 max number of sites for 549 pages
    #ids2 <- seq(10,310,5) ##61 max number of sites for 5614 pages
    
    for (j in seq_along(ids2)) {
      Trial_ID <- ids[i]
      Registration_number= cris_page %>% html_nodes("#step1 tr:nth-child(2) td") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() %>% toString()
      Registration_number = new_function(Registration_number)
      Recruitment_centres = cris_page %>% html_nodes(paste0("#step4 tr:nth-child(",ids2[j],") td")) %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim() 
      Recruitment_centres = new_function(Recruitment_centres)
      
      file <- data.frame(Trial_ID,Registration_number,Recruitment_centres)
      write.table(file, "Recruit_details_CRIS.csv", sep = ",",row.names = TRUE, col.names = !file.exists("Recruit_details_CRIS.csv"), append = T)
      print(j)
      
    }
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
  }
}




