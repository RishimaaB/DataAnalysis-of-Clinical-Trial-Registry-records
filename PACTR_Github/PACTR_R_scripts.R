#*****************************************************************Script 1******************************************************************************************************
#Downloaded all the records from the PACTR registry

libraries = c( "XML","robotstxt", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:30000)


for (i in seq_along(ids)) {
  url = url(paste0("https://pactr.samrc.ac.za/TrialDisplay.aspx?TrialID=", ids[i]),"rb") 
  
  myurl = paste0("https://pactr.samrc.ac.za/TrialDisplay.aspx?TrialID=", ids[i]) 
  page = read_html(url)
  query = page %>% html_nodes("h2") %>% html_text()
  if (length(query)!=0) {
    myfile = paste0("pactr_page",ids[i],".html")
    download.file(myurl, destfile = myfile, quiet = TRUE)
    time_of_download = as.character(timestamp())
    
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            downloaded_time = time_of_download,
                            URL = as.character(myurl))
    
    write.table(time_stamp, "time_stamp_pactr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_pactr.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
  } else {
    print(paste0("Record is not present with the ID number = ",ids[i]))
  }
}


#********************************************************************Script 2****************************************************************************************************
#Web-scraped all the downloaded records for the keyword 'India' or 'CTRI' keyword (case-insensitive)

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(85:24388)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("pactr_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    pactr_page <- read_html(myfile)
    
    if(is_empty(pactr_page)) {
      
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
    Reg_num <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr:nth-child(1) .info:nth-child(2)") %>% html_text() %>% toString()
    Reg_num <- new_function(Reg_num)
    WP_india<- pactr_page %>% html_nodes(".info") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    WP_india_no_bound <- pactr_page %>% html_nodes(".info") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    WP_ctri<- pactr_page %>% html_nodes(".info") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bctri\\b",ignore_case = TRUE))
    WP_ctri_no_bound <- pactr_page %>% html_nodes(".info") %>% html_text() %>% toString() %>% str_extract(.,regex("CTRI",ignore_case = FALSE))
    
    
    India_ctri_pactr<- data.frame(Trial_ID,Reg_num,WP_india,WP_india_no_bound,WP_ctri,WP_ctri_no_bound)
    write.table(India_ctri_pactr,"Scraped_fr_srching_ind_ctri_pactr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_ctri_pactr.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}

#**********************************************************************Script - 3***************************************************************************************************
#Web scraped the field 'Countries of Recruitment' from all the records which had the 'India' keyword

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(89:24110)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("pactr_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    pactr_page <- read_html(myfile)
    
    if(is_empty(pactr_page)) {
      
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
    Reg_num <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr:nth-child(1) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Reg_num <- new_function(Reg_num)
    Date_of_approval <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo .info~ .info") %>% html_text() %>% toString()
    Date_of_approval <- new_function(Date_of_approval)
    Trial_status <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr+ tr .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Trial_status <- new_function(Trial_status)
    Public_title <- pactr_page %>% html_nodes("tr:nth-child(3) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Public_title <- new_function(Public_title)
    Scientific_title <- pactr_page %>% html_nodes("tr:nth-child(4) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Scientific_title <- new_function(Scientific_title)
    Type_of_trial <- pactr_page %>% html_nodes("tr:nth-child(6) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Type_of_trial <- new_function(Type_of_trial)
    Recruitment_country <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_remove_all("\n") %>% str_squish() %>% str_trim()
    Recruitment_country <- new_function(Recruitment_country)
    Country_india <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    
    file <- data.frame(Trial_ID,Reg_num,Date_of_approval,Trial_status,Public_title,Scientific_title,Type_of_trial,Recruitment_country,Country_india)
    write.table(file,"Scraped_fr_srching_ind_cor_pactr.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_cor_pactr.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
}


#***********************************************************************Script - 4***************************************************************************************************
#Scraped the remaining records where 'India' was not mentioned as a recruitment country, but the keyword 'India' was present in other fields

##Script 4a

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(328,354,508,1043,1057,1143,1703,1930,2053,2222,2239,2287,2969,2970,3096,3240,3336,3454,3584,4692,5796,5822,6050,6085,8186,8230,9330,9541,9651,9672,9690,9708,9776,9814,9842,10860,10908,10926,11011,11051,11101,12124,12326,12329,14648,15691,15734,15906,15911,15996,16031,17164,19304,19308,20409,23665,23668,23682,23716,24002,24058,24069,24082,24086,24110)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("pactr_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    pactr_page <- read_html(myfile)
    
    if(is_empty(pactr_page)) {
      
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
    Reg_num <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr:nth-child(1) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Reg_num <- new_function(Reg_num)
    Date_of_approval <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo .info~ .info") %>% html_text() %>% toString()
    Date_of_approval <- new_function(Date_of_approval)
    Trial_status <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr+ tr .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Trial_status <- new_function(Trial_status)
    Public_title <- pactr_page %>% html_nodes("tr:nth-child(3) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Public_title <- new_function(Public_title)
    Scientific_title <- pactr_page %>% html_nodes("tr:nth-child(4) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Scientific_title <- new_function(Scientific_title)
    Type_of_trial <- pactr_page %>% html_nodes("tr:nth-child(6) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Type_of_trial <- new_function(Type_of_trial)
    Recruitment_country <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .tableTrialInfo") %>% html_text() %>% toString() %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_replace_all("\n",",") %>% str_squish() %>% str_trim()
    Country_india <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    Trial_description <- pactr_page %>% html_nodes(".header+ .divtrialInformation > .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Secondary_identifiers <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(3) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Study_design <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(4) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Interventions <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Eligibility_criteria <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Ethics_approval <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(7) .info") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Outcomes <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(8) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Funding_sources <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(10) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Sponsors <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Collaborators <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(12) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Contact_people <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(13) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Reporting <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(14) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Changes_to_trial_information <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(15) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    
    
    file <- data.frame(Trial_ID,Reg_num,Date_of_approval,Trial_status,Public_title,Scientific_title,Type_of_trial,Recruitment_country,Country_india,Trial_description,Secondary_identifiers,
                       Study_design,Interventions,Eligibility_criteria,Ethics_approval,Outcomes,Funding_sources,Sponsors,Collaborators,Contact_people,
                       Reporting,Changes_to_trial_information)
    write.table(file,"Scraped_fr_srching_ind_pactr_65.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_fr_srching_ind_pactr_65.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
  }
} 



##Script 4b

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids <- c(328,354,508,1043,1057,1143,1703,1930,2053,2222,2239,2287,2969,2970,3096,3240,3336,3454,3584,4692,5796,5822,6050,6085,8186,8230,9330,9541,9651,9672,9690,9708,9776,9814,9842,10860,10908,10926,11011,11051,11101,12124,12326,12329,14648,15691,15734,15906,15911,15996,16031,17164,19304,19308,20409,23665,23668,23682,23716,24002,24058,24069,24082,24086,24110)

counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("pactr_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    pactr_page <- read_html(myfile)
    
    if(is_empty(pactr_page)) {
      
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
    Reg_num <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr:nth-child(1) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Reg_num <- new_function(Reg_num)
    Date_of_approval <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo .info~ .info") %>% html_text() %>% toString()
    Date_of_approval <- new_function(Date_of_approval)
    Trial_status <- pactr_page %>% html_nodes(".tableTrialInfo .tableTrialInfo tr+ tr .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Trial_status <- new_function(Trial_status)
    Public_title <- pactr_page %>% html_nodes("tr:nth-child(3) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Public_title <- new_function(Public_title)
    Scientific_title <- pactr_page %>% html_nodes("tr:nth-child(4) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Scientific_title <- new_function(Scientific_title)
    Type_of_trial <- pactr_page %>% html_nodes("tr:nth-child(6) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all("\n") %>% str_remove_all("\r") %>% str_remove_all("\t") %>% str_squish() %>% str_trim()
    Type_of_trial <- new_function(Type_of_trial)
    Country_india <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .tableTrialInfo") %>% html_text() %>% toString() %>% str_extract(.,regex("\\bindia\\b",ignore_case = TRUE))
    
    ##Within the super field Trial Description
    Brief_description <- pactr_page %>% html_nodes("tr:nth-child(5) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Brief_description <- new_function(Brief_description)
    Disease_studied <- pactr_page %>% html_nodes("tr:nth-child(8) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all('\"') %>% str_squish() %>% str_trim()
    Disease_studied <- new_function(Disease_studied)
    Sub_disease <- pactr_page %>% html_nodes("tr:nth-child(9) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all('\"') %>% str_squish() %>% str_trim()
    Sub_disease <- new_function(Sub_disease)
    Purpose_of_the_trial <- pactr_page %>% html_nodes("tr:nth-child(10) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all('\"') %>% str_squish() %>% str_trim()
    Purpose_of_the_trial <- new_function(Purpose_of_the_trial)
    Recruitment_status <- pactr_page %>% html_nodes("tr:nth-child(17) .captiondescription+ .info") %>% html_text() %>% toString() %>% str_remove_all('\"') %>% str_squish() %>% str_trim()
    Recruitment_status <- new_function(Recruitment_status)
    
    ##Secondary identifiers
    Secondary_identifiers <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(3) .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Secondary_identifiers <- new_function(Secondary_identifiers)
    
    ##Interventions
    Intervention_type <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Intervention_type <- new_function(Intervention_type)
    Intervention_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Intervention_name <- new_function(Intervention_name)
    Dose <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .info:nth-child(3)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Dose <- new_function(Dose)
    Intervention_description <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_squish() %>% str_trim()
    Intervention_description <- new_function(Intervention_description)
    Group_size <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .info:nth-child(6)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Group_size <- new_function(Group_size)
    Nature_of_control <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(5) .info:nth-child(7)") %>% html_text() %>% toString() %>% str_replace_all('\"',",") %>% str_squish() %>% str_trim()
    Nature_of_control <- new_function(Nature_of_control)
    
    ##Eligibility criteria
    List_inclusion_criteria <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    List_inclusion_criteria <- new_function(List_inclusion_criteria)
    List_exclusion_criteria <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    List_exclusion_criteria <- new_function(List_exclusion_criteria)
    Age_category <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .info:nth-child(3)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Age_category <- new_function(Age_category)
    Min_age<- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .info:nth-child(4)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Min_age<- new_function(Min_age)
    Max_age<- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Max_age<- new_function(Max_age)
    Gender<- pactr_page %>% html_nodes(".divtrialInformation:nth-child(6) .info:nth-child(6)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Gender<- new_function(Gender)
    
    ##Ethics-approval
    Ethics_committee_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(7) .info:nth-child(4)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Ethics_committee_name <- new_function(Ethics_committee_name)
    Ethics_committee_address <- pactr_page %>% html_nodes(".tableTrialInfoInner .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Ethics_committee_address <- new_function(Ethics_committee_address)
    Ethics_committee_city <- pactr_page %>% html_nodes(".tableTrialInfoInner .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Ethics_committee_city <- new_function(Ethics_committee_city)
    Ethics_committee_country <- pactr_page %>% html_nodes(".tableTrialInfoInner .info:nth-child(4)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Ethics_committee_country <- new_function(Ethics_committee_country)
    
    
    ##Recruitment-centres
    
    Recruitment_centre_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Recruitment_centre_name <- new_function(Recruitment_centre_name)
    Recruitment_centre_address <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Recruitment_centre_address <- new_function(Recruitment_centre_address)
    Recruitment_centre_city <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .info:nth-child(3)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Recruitment_centre_city <- new_function(Recruitment_centre_city)
    Recruitment_centre_country <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(9) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Recruitment_centre_country <- new_function(Recruitment_centre_country)
    
  
    ##Funding sources
    Funding_source_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(10) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Funding_source_name <- new_function(Funding_source_name)
    Funding_source_address <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(10) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Funding_source_address <- new_function(Funding_source_address)
    Funding_source_city <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(10) .info:nth-child(3)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Funding_source_city <- new_function(Funding_source_city)
    Funding_source_country <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(10) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Funding_source_country <- new_function(Funding_source_country)
    
    
    ##Sponsors
    Sponsor_level <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Sponsor_level <- new_function(Sponsor_level)
    Sponsor_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Sponsor_name <- new_function(Sponsor_name)
    Sponsor_address <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .info:nth-child(3)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Sponsor_address <- new_function(Sponsor_address)
    Sponsor_city <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .info:nth-child(4)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Sponsor_city <- new_function(Sponsor_city)
    Sponsor_country <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .info:nth-child(6)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Sponsor_country <- new_function(Sponsor_country)
    Nature_of_sponsor <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(11) .info:nth-child(7)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Nature_of_sponsor <- new_function(Nature_of_sponsor)
    
    
    ##Collaborators
    Collaborator_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(12) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Collaborator_name <- new_function(Collaborator_name)
    Collaborator_address <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(12) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Collaborator_address <- new_function(Collaborator_address)
    Collaborator_city <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(12) .info:nth-child(3)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Collaborator_city <- new_function(Collaborator_city)
    Collaborator_country <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(12) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Collaborator_country <- new_function(Collaborator_country)
    
    ##Contact people
    Contact_info <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(13) .info") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Contact_info <- new_function(Contact_info)
    
    
    ##Changes to trial information
    Section_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(15) .info:nth-child(1)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Section_name <- new_function(Section_name)
    Field_name <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(15) .info:nth-child(2)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Field_name <- new_function(Field_name)
    Reason <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(15) .info:nth-child(4)") %>% html_text() %>% toString() %>% str_squish() %>% str_trim()
    Reason <- new_function(Reason)
    Old_value_india <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(15) .info:nth-child(5)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    Updated_value_india <- pactr_page %>% html_nodes(".divtrialInformation:nth-child(15) .info:nth-child(6)") %>% html_text() %>% toString() %>% str_extract(.,regex("india",ignore_case = TRUE))
    
    
    
    
    file <- data.frame(Trial_ID,Reg_num,Date_of_approval,Trial_status,Public_title,
                       Scientific_title,Type_of_trial,Country_india,
                       Brief_description,Disease_studied,Sub_disease,Purpose_of_the_trial,Recruitment_status,
                       Secondary_identifiers,Intervention_type,Intervention_name,
                       Dose,Intervention_description,Group_size,Nature_of_control,
                       List_inclusion_criteria,List_exclusion_criteria,Age_category,
                       Min_age,Max_age,Gender,Ethics_committee_name,Ethics_committee_address,
                       Ethics_committee_city,Ethics_committee_country,Recruitment_centre_name,Recruitment_centre_address,Recruitment_centre_city,
                       Recruitment_centre_country,Funding_source_name,
                       Funding_source_address,Funding_source_city,Funding_source_country,
                       Sponsor_level,Sponsor_name,Sponsor_address,Sponsor_city,
                       Sponsor_country,Nature_of_sponsor,Collaborator_name,Collaborator_address,
                       Collaborator_city,Collaborator_country,Contact_info,Section_name,
                       Field_name,Reason,Old_value_india,Updated_value_india)
    
    
    
    write.table(file,"Scraped_ind_pactr_65_detail.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Scraped_ind_pactr_65_detail.csv"), append = T)
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    
    
    
    
  }
}


