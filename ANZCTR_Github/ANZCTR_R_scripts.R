#********************************************************************Script-1 ***********************************************************************************************
  #Downloading the records from the ANZCTR registry

  libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
  lapply(libraries, require, character.only = TRUE)
  ids <- data.frame()
  ids <- read.csv("ids.csv")
  colnames(ids) <- "ids"
  counter=0
  
  
  for (row in 1:nrow(ids)) {
    i <- ids[row,1]
    myurl = paste0("https://www.anzctr.org.au/Trial/Registration/TrialReview.aspx?id=",i,"&isReview=true") 
    url = url(paste0("https://www.anzctr.org.au/Trial/Registration/TrialReview.aspx?id=",i,"&isReview=true"))
    anzctr_page = read_html(url)
    keyword=anzctr_page %>% html_nodes(".page-title") %>% html_text() %>% str_remove_all("\r") %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    keyword = toString(keyword)
    if (keyword == "")
    {
      keyword <- "NA"
    }
    if (keyword != "Restricted Area") { 
      myfile = paste0("anzctr_page",i,".html")
      download.file(myurl, destfile = myfile, quiet = TRUE)
      time_of_download = as.character(timestamp())
      
      time_stamp = data.frame(Trial_ID = as.character(i),
                              downloaded_time = time_of_download,
                              URL = as.character(myurl))
      write.table(time_stamp, "time_stamp_anzctr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_anzctr.csv"), append = T)
      
      counter = counter + 1
      print(paste("Count = ", counter,"ID = ",i))
    }
    else {
      print(paste("Count = ", counter,"ID = ",i," IS INVALID "))
      
    }
  }
  
  
  
#*********************************************************************Script - 2**********************************************************************************************
#Web-scraped all the downloaded records for 'India' or 'CTRI' keyword (case-insensitive)
  
  
libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- data.frame()
ids <- read.csv("ids.csv")
colnames(ids) <- "ids"
counter=0
  
for (row in 1:nrow(ids)) {
    i <- ids[row,1]
    myfile = paste0("anzctr_page",i,".html")
    
    if (file.exists(myfile)) {
      
      anzctr_page <- read_html(myfile)
      
      if(is_empty(anzctr_page)) {
        
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
      
      
      
      Serial_no <- i
      Registration_number <- anzctr_page %>% html_nodes("#ctl00_body_CXACTRNUMBER") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString()
      Registration_number <- new_function(Reg_num)
      Whole_page_india<- anzctr_page %>% html_nodes(".review-element-content") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
      Whole_page_india_bound <- anzctr_page %>% html_nodes(".review-element-content") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
      Whole_page_CTRI <- anzctr_page %>% html_nodes(".review-element-content") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
      Whole_page_CTRI_no_bound <- anzctr_page %>% html_nodes(".review-element-content") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
      
      
      India_ctri_anzctr <- data.frame(Serial_no,Registration_number,Whole_page_india,Whole_page_india_bound,Whole_page_CTRI,Whole_page_CTRI_no_bound)
      write.table(India_ctri_anzctr,"Scraped_fr_srching_ind_ctri_anzctr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_anzctr.csv"), append = T)
      
      counter = counter + 1
      print(paste("Count = ", counter,"ID = ",i))
      
      
      
    }
  }
  
  
  
#*************************************************************Script-3*********************************************************************************************************
#After execution of the 'Script-2', we came to know that 282 records have the 'India' keyword.
#Scraped the 282 records for the field 'Recruitment outside Australia' in order to know whether 'India' is mentioned as a Country of recruitment.

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- data.frame()
ids <- read.csv("ids_282.csv")
colnames(ids) <- "ids"
counter=0

for (row in 1:nrow(ids)) {
  i <- ids[row,1]
  myfile = paste0("anzctr_page",i,".html")
  
  if (file.exists(myfile)) {
    
    anzctr_page <- read_html(myfile)
    
    if(is_empty(anzctr_page)) {
      
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
    
    Serial_no <- i
    Reg_num <- anzctr_page %>% html_nodes("#ctl00_body_CXACTRNUMBER") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString()
    Reg_num <- new_function(Reg_num)
    Date_of_registration <- anzctr_page %>% html_nodes("#ctl00_body_CXAPPROVALDATE")%>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r")
    Date_of_registration <- new_function(Date_of_registration)
    Date_of_last_update <- anzctr_page %>% html_nodes("#ctl00_body_CXUPDATEDATE")%>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") 
    Date_of_last_update <- new_function(Date_of_last_update)
    Public_title <- anzctr_page %>% html_nodes("#ctl00_body_CXSTUDYTITLE") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString()
    Public_title <- new_function(Public_title)
    Scientific_title <- anzctr_page %>% html_nodes("#ctl00_body_CXSCIENTIFICTITLE") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString()
    Scientific_title <- new_function(Scientific_title)
    Study_type <- anzctr_page %>% html_nodes("#ctl00_body_CXSTUDYTYPE") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString()
    Study_type <- new_function(Study_type)
    Phase <- anzctr_page %>% html_nodes("#ctl00_body_CXPHASE") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% toString()
    Phase <- new_function(Phase)
    RecruitmentStates_australia <- anzctr_page %>% html_nodes("#ctl00_body_RecruitmentState .review-element-content") %>% html_text() %>% str_remove_all("\n") %>% str_remove_all("\t") %>% str_remove_all("\r") %>% str_squish() %>% str_trim() %>% toString() %>% str_remove_all("Query!")
    RecruitmentStates_australia <- new_function(RecruitmentStates_australia)
    df1 <- data.frame()
    for (j in 1:99) {
      num <- toString(j)
      num <-  str_pad(num, width = 2, side = "left", pad = 0)
      roa <- anzctr_page %>% html_nodes(paste0("#ctl00_body_repeater_TXCOUNTRYOUTSIDEAUSTRALIA_ctl",num,"_CXCOUNTRY")) %>% html_text() %>% str_squish() %>% str_trim() %>% toString()
      roa <- new_function(roa)
      df1 <- rbind(df1,data.frame(roa))
    }
    file1 <- data.frame(Serial_no,Reg_num,Date_of_registration,Date_of_last_update,Public_title,Scientific_title,
                        Study_type,Phase,RecruitmentStates_australia)
    Recruitment_outside_australia <- t(df1) %>% toString()
    
    file2 <- cbind(file1,Recruitment_outside_australia)
    file <- data.frame(file2)
    
    write.table(file,"Scraped_fr_srching_cor_282_anzctr_finals.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_cor_282_anzctr_finals.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",i))
    
    
    
  }
}


  
