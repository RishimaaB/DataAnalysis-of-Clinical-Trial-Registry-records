#***************************************************************Script - 1****************************************************************************************************
#Downloading all the records from the jRCT registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:5152)

for (page_result in seq(from=1,to=104)) {
  link <- url(paste0("https://jrct.niph.go.jp/search?language=en&page=",page_result))
  page <- read_html(link)
  record_links <- page %>% html_nodes("td:nth-child(1)") %>% html_text()
  record_links <- paste0("https://jrct.niph.go.jp/en-latest-detail/",record_links)
  japan_page_links <- data.frame(record_links)
  write.table(japan_page_links, "Japan_page_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Japan_page_links.csv"), append = T)
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
  
}

japan_page_links <- data.frame()
japan_page_links <- read.csv("Japan_page_links.csv")
for (i in seq_along(ids)) {
  official_url = japan_page_links[ids[i],"record_links"] 
  print(official_url)
  output_file=paste0("japan_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_japan.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_japan.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}


#**********************************************************************Script - 2*********************************************************************************************
#Web-scraped all the downloaded records for the keyword 'India' or 'CTRI'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids = c(1:5152)
counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("japan_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    japan_page <- read_html(myfile)
    
    if(is_empty(japan_page)) {
      
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
    

    Trial_ID <- japan_page %>% html_nodes("p+ .table-bordered tr~ tr+ tr .col-md-8") %>% html_text() %>% str_squish() %>% str_trim()
    Trial_ID <- new_function(Trial_ID)
    Date_of_registration <- japan_page %>% html_nodes("p+ .table-bordered tr:nth-child(1) .col-md-8") %>% html_text() %>% str_squish() %>% str_trim()
    Date_of_registration <- new_function(Date_of_registration)
    Last_modified_on <- japan_page %>% html_nodes("p+ .table-bordered tr:nth-child(2) .col-md-8") %>% html_text() %>% str_squish() %>% str_trim()
    Last_modified_on <- new_function(Last_modified_on)
    Scientific_title <- japan_page %>% html_nodes(".table-bordered:nth-child(4) tr:nth-child(1) .space") %>% html_text() %>% str_squish() %>% str_trim()
    Scientific_title <- new_function(Scientific_title)
    Public_title <- japan_page %>% html_nodes(".table-bordered:nth-child(4) tr+ tr .space") %>% html_text() %>% .[1] %>% str_squish() %>% str_trim()
    Public_title <- new_function(Public_title)
    india_keyword <- japan_page %>% html_nodes(".container") %>% html_text() %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE)) ##to be case-insensitive
    india_keyword <- toString(india_keyword)
    ctri_keyword <- japan_page %>% html_nodes(".container") %>% html_text() %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE)) ##to be case-insensitive
    ctri_keyword <- toString(ctri_keyword)
    
    
    Information_japan_registry <- data.frame(Trial_ID,Date_of_registration,Last_modified_on,Scientific_title,Public_title,india_keyword,ctri_keyword)
    write.table(Information_japan_registry,"Information_japan_registry_file1.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Information_japan_registry_file1.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}



#****************************************************************Script - 3****************************************************************************************************
#Web scraped the field 'Countries of recruitment' from the records which have the 'India' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids = c(23,120,175,177,245,252,262,263,313,332,348,370,398,403,434,508,547,622,653,719,729,771,1017,1105,1140,1192,1194,1209,1263,1268,1299,1397,1428,1433,1439,1495,1498,1514,1520,1594,1596,1597,1621,1634,1700,1702,1748,1753,1775,1780,1795,1811,1934,1957,2041,2081,2134,2146,2195,2261,2270,2274,2286,2292,2339,2369,2434,2475,2563,2658,2690,2743,2744,2754,2775,2776,2832,2850,2854,2859,2876,2877,2899,2933,2957,2993,3248,3637,4271,4349,4391)
counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("japan_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    japan_page <- read_html(myfile)
    
    if(is_empty(japan_page)) {
      
      next 
    }
    
    new_function <- function(a) {
      
      if (length(a) == 0) {
        a <- "--"
      } else if (a == "") {
        a <- "--"
      } else if (a == "NA") {
        a <- "--"
      } else {
        return(a)
      }
    }
    
    new_function2 <- function(b) {
      date_of_registration <- japan_page %>% html_nodes("p+ .table-bordered tr:nth-child(1) .col-md-8") %>% html_text() %>% str_squish() %>% str_trim()
      date_of_registration <- new_function(date_of_registration)
      last_modified_on <- japan_page %>% html_nodes("p+ .table-bordered tr:nth-child(2) .col-md-8") %>% html_text() %>% str_squish() %>% str_trim()
      last_modified_on <- new_function(last_modified_on)
      scientific_title <- japan_page %>% html_nodes(".table-bordered:nth-child(4) tr:nth-child(1) .space") %>% html_text() %>% str_remove_all(.,'"') %>% str_squish() %>% str_trim()
      scientific_title <- new_function(scientific_title)
      public_title <- japan_page %>% html_nodes(".table-bordered:nth-child(4) tr+ tr .space") %>% html_text() %>% .[1] %>% str_remove_all(.,'"') %>% str_squish() %>% str_trim()
      public_title <- new_function(public_title)
      india_keyword <- japan_page %>% html_nodes(".container") %>% html_text() %>% str_extract(., regex("\\bindia\\b", ignore_case = TRUE)) ##to be case-insensitive
      india_keyword <- toString(india_keyword)
      country_of_recruitment <- japan_page %>% html_nodes(b) %>% html_text() %>% str_replace_all("/",",") %>% str_squish() %>% str_trim()
      country_of_recruitment <- toString(country_of_recruitment)
      country_of_recruitment <- new_function(country_of_recruitment)
      study_type_interventional <- japan_page %>% html_nodes(".container") %>% html_text() %>% str_extract(., regex("interventional", ignore_case = TRUE))  ##to be case-insensitive
      study_type_interventional <- toString(study_type_interventional)
      study_type_observentional <- japan_page %>% html_nodes(".container") %>% html_text() %>% str_extract(., regex("observational", ignore_case = TRUE)) ##to be case-insensitive
      study_type_observentional <- toString(study_type_observentional)
      ctri_keyword <- japan_page %>% html_nodes(".container") %>% html_text() %>% str_extract(., regex("\\bctri\\b", ignore_case = TRUE)) ##to be case-insensitive
      ctri_keyword <- toString(ctri_keyword)
      
      
      file <- data.frame(trial_ID,date_of_registration,last_modified_on,scientific_title,public_title,india_keyword,country_of_recruitment,study_type_interventional,study_type_observentional,ctri_keyword)
      write.table(file,"India_91.csv", sep = ",",row.names = FALSE, col.names = !file.exists("India_91.csv"), append = T)
      
      counter = counter + 1
      print(paste("Count = ", counter,"ID = ",ids[i]))
      
      
    }
    
    
    trial_ID <- japan_page %>% html_nodes("p+ .table-bordered tr~ tr+ tr .col-md-8") %>% html_text() %>% str_squish() %>% str_trim()
    trial_ID <- new_function(trial_ID)
    
    #29th-column
    Country_of_recruitment1 <- japan_page %>% html_nodes(".table-bordered:nth-child(6) tr:nth-child(10) .col-md-2") %>% html_text() %>% str_squish() %>% str_trim() %>% str_extract("Countries")
    Country_of_recruitment1 <- toString(Country_of_recruitment1)
    Country_of_recruitment1 <- new_function(Country_of_recruitment1)
    #59th-column
    Country_of_recruitment2 <- japan_page %>% html_nodes(".table-bordered:nth-child(14) .col-md-4") %>% html_text() %>% str_squish() %>% str_trim() %>% str_extract("Countries")
    Country_of_recruitment2 <- toString(Country_of_recruitment2)
    Country_of_recruitment2 <- new_function(Country_of_recruitment2)
    #61st-column
    Country_of_recruitment3 <- japan_page %>% html_nodes(".table-bordered:nth-child(15) .col-md-4") %>% html_text() %>% str_squish() %>% str_trim() %>% str_extract("Countries")
    Country_of_recruitment3 <- toString(Country_of_recruitment3)
    Country_of_recruitment3 <- new_function(Country_of_recruitment3)
    #62nd-column
    Country_of_recruitment4 <- japan_page %>% html_nodes(".table-bordered:nth-child(16) .col-md-4") %>% html_text() %>% str_squish() %>% str_trim() %>% str_extract("Countries")
    Country_of_recruitment4 <- toString(Country_of_recruitment4)
    Country_of_recruitment4 <- new_function(Country_of_recruitment4)
    #72nd-column
    Country_of_recruitment5 <- japan_page %>% html_nodes(".table-bordered:nth-child(23) .col-md-4") %>% html_text() %>% str_squish() %>% str_trim() %>% str_extract("Countries")
    Country_of_recruitment5 <- toString(Country_of_recruitment5)
    Country_of_recruitment5 <- new_function(Country_of_recruitment5)
    #42nd-column
    Country_of_recruitment6 <- japan_page %>% html_nodes(".table-bordered:nth-child(8) tr:nth-child(10) .col-md-2") %>% html_text() %>% str_squish() %>% str_trim() %>% str_extract("Countries")
    Country_of_recruitment6 <- toString(Country_of_recruitment6)
    Country_of_recruitment6 <- new_function(Country_of_recruitment6)
    
    Contact_for_scientific_queries <- japan_page %>% html_nodes(".table-bordered:nth-child(5) tr:nth-child(1) .col-md-2:nth-child(1) label") %>% html_text() %>% str_squish() %>% str_trim()
    
    if (length(Contact_for_scientific_queries)==1) {
      
      if(length(Country_of_recruitment1)==1){
        if (Country_of_recruitment1 == "Countries") {
          file <- new_function2(".table-bordered:nth-child(6) tr:nth-child(10) .space")
        }
      }
      
      if(length(Country_of_recruitment2)==1){
        if (Country_of_recruitment2 == "Countries") {
          file <- new_function2(".table-bordered:nth-child(14) .space")
        }
      }
      
      if(length(Country_of_recruitment3)==1){
        if (Country_of_recruitment3 == "Countries") {
          file <- new_function2(".table-bordered:nth-child(15) .space")
        }
      }
      
      if(length(Country_of_recruitment4)==1){
        if (Country_of_recruitment4 == "Countries") {
          file <- new_function2(".table-bordered:nth-child(16) .space")
        }
      }
      
      if(length(Country_of_recruitment5)==1){
        if (Country_of_recruitment5 == "Countries") {
          file <- new_function2(".table-bordered:nth-child(23) .space")
        }
      }
      
      
    } else {
      
      if(length(Country_of_recruitment6)==1){
        if (Country_of_recruitment6 == "Countries") {
          file <- new_function2(".table-bordered:nth-child(8) tr:nth-child(10) .space")
        }
      }
    }
  }
}
