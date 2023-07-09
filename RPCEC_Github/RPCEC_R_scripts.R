#****************************Script - 1**************************************************
#Downloading all the records from the RPCEC registry

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
mydb <- dbConnect(RSQLite::SQLite(), "rishima_rpcec_cuban.sqlite")

ids = c(1:410)
counter = 0

for (i in seq_along(ids)) {
  if (ids[i] < 10)
  {
    official_url = paste0("https://rpcec.sld.cu/en/trials/RPCEC0000000",ids[i],"-En") 
    output_file=paste0("cuban_page",ids[i],".html")
    download.file(official_url, destfile = output_file, quiet = TRUE)
    time_of_download = as.character(timestamp())
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            downloaded_time = time_of_download,
                            URL = as.character(official_url))
    dbWriteTable(mydb, "time_stamp_cuban", time_stamp, append = TRUE)
    write.table(time_stamp, "time_stamp_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_cuban.csv"), append = T)
    
  }
  
  else if (ids[i]>=10  && ids[i]<100) {
    official_url = paste0("https://rpcec.sld.cu/en/trials/RPCEC000000",ids[i],"-En") 
    output_file=paste0("cuban_page",ids[i],".html")
    download.file(official_url, destfile = output_file, quiet = TRUE)
    time_of_download = as.character(timestamp())
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            downloaded_time = time_of_download,
                            URL = as.character(official_url))
    dbWriteTable(mydb, "time_stamp_cuban", time_stamp, append = TRUE)
    write.table(time_stamp, "time_stamp_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_cuban.csv"), append = T)
    
  } else {
    
    official_url = paste0("https://rpcec.sld.cu/en/trials/RPCEC00000",ids[i],"-En") 
    output_file=paste0("cuban_page",ids[i],".html")
    download.file(official_url, destfile = output_file, quiet = TRUE)
    time_of_download = as.character(timestamp())
    time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                            downloaded_time = time_of_download,
                            URL = as.character(official_url))
    dbWriteTable(mydb, "time_stamp_cuban", time_stamp, append = TRUE)
    write.table(time_stamp, "time_stamp_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_cuban.csv"), append = T)
    
  }
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}

dbDisconnect(mydb)


#**********************************************Script - 2***************************************************************
#Web-scraped all the fields from the downloaded records to look for the keyword 'India' or 'CTRI'


libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)

ids = c(1:410)
counter = 0

mydb <- dbConnect(RSQLite::SQLite(), "cubandb1.sqlite")


for (i in seq_along(ids)) { 
  
  myfile = paste0("cuban_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    print(myfile)
    cuban_page <- read_html(myfile)
    
    if(is_empty(cuban_page)) {
      
      next 
    }
    
    
    
    
    CommonTable = NULL
    blank <- "blank"
    
    #Table1: General Information
    
    CommonTable$Unique_id_number <- cuban_page %>% html_nodes(".field-field-id-number .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    CommonTable$Unique_id_number_short <- cuban_page %>% html_nodes(".field-field-id-number .field-item") %>% html_text() %>% str_remove_all("\\t") %>% str_remove_all("\\r") %>% str_remove_all("\\n") %>% str_remove_all("RPCEC") %>% as.numeric() ##This was done in order to arrange it from ascending to descending order, the trial records( such that we confirm that the required trial records are present based on the registration number)
    
    new_function <- function(a,b,c) {
      
      c <- paste0(a,b)
      if (c == "blank") {
        c <- NA
      } else {
        c <- a
      }
    }  
    
    Acronym1 <- cuban_page %>% html_nodes(".field-field-public-acronym .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Acronym1 <- toString(Acronym1)
    CommonTable$Acronym_of_public_title <- new_function(Acronym1,blank,Acronym_of_public_title)
    
    Scientific_title1 <- cuban_page %>% html_nodes(".field-field-scientific-title .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Scientific_title1 <- toString(Scientific_title1)
    CommonTable$Scientific_title <- new_function(Scientific_title1,blank,Scientific_title)
    
    
    Acronym3 <- cuban_page %>% html_nodes(".field-field-scientific-acronym .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Acronym3 <- toString(Acronym3)
    CommonTable$Acronym_of_scientific_title <- new_function(Acronym3,blank,Acronym_of_scientific_title)
    
    
    Secondary_identification_numbers1 <- cuban_page %>% html_nodes(".field-field-identifying .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Secondary_identification_numbers1 <- toString(Secondary_identification_numbers1)
    CommonTable$Secondary_identification_numbers <- new_function(Secondary_identification_numbers1,blank,Secondary_identification_numbers) 
    
    
    
    Issuing_authority_of_secondary_identification_numbers1 <- cuban_page %>% html_nodes(".field-field-authority-identifying .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Issuing_authority_of_secondary_identification_numbers1 <- toString(Issuing_authority_of_secondary_identification_numbers1)
    CommonTable$Issuing_authority_of_secondary_identification_numbers <- new_function(Issuing_authority_of_secondary_identification_numbers1,blank,Issuing_authority_of_secondary_identification_numbers)
    
    
    Primary_sponsor1 <- cuban_page %>% html_nodes(".field-field-sponsor .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Primary_sponsor1 <- toString(Primary_sponsor1)
    CommonTable$Primary_sponsor<- new_function(Primary_sponsor1,blank,Primary_sponsor)
    
    Secondary_sponsor1 <- cuban_page %>% html_nodes(".field-field-sec-sponsor .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Secondary_sponsor1 <- toString(Secondary_sponsor1)
    CommonTable$Secondary_sponsor <- new_function(Secondary_sponsor1,blank,Secondary_sponsor)
    
    Source_of_monetary_or_material_support1 <- cuban_page %>% html_nodes(".field-field-source-monetary .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Source_of_monetary_or_material_support1 <- toString(Source_of_monetary_or_material_support1)
    CommonTable$Source_of_monetary_or_material_support <- new_function(Source_of_monetary_or_material_support1,blank,Source_of_monetary_or_material_support)
    
    General_information <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                      Unique_id_number_short=CommonTable$Unique_id_number_short,
                                      Acronym_of_public_title=CommonTable$Acronym_of_public_title,
                                      Scientific_title=CommonTable$Scientific_title,
                                      Acronym_of_scientific_title=CommonTable$Acronym_of_scientific_title,
                                      Secondary_identification_numbers=CommonTable$Secondary_identification_numbers,
                                      Issuing_authority_of_secondary_identification_numbers=CommonTable$Issuing_authority_of_secondary_identification_numbers,
                                      Primary_sponsor=CommonTable$Primary_sponsor,
                                      Secondary_sponsor=CommonTable$Secondary_sponsor,
                                      Source_of_monetary_or_material_support=CommonTable$Source_of_monetary_or_material_support)
    
    dbWriteTable(mydb, "General_information", General_information , append = TRUE )
    write.table(General_information,"General_information.csv", sep = ",",row.names = FALSE, col.names = !file.exists("General_information.csv"), append = T)
    
    
    
    
    
    # Table2: Authorization-for-beginning
    
    Regulatory_instance_to_authorize_initiation1 <- cuban_page %>% html_nodes(".field-field-reg-instance .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Regulatory_instance_to_authorize_initiation1 <- toString(Regulatory_instance_to_authorize_initiation1)
    CommonTable$Regulatory_instance_to_authorize_initiation <- new_function(Regulatory_instance_to_authorize_initiation1,blank,Regulatory_instance_to_authorize_initiation)
    
    Regulatory_instance1 <- cuban_page %>% html_nodes(".field-field-regulatiry-inst .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Regulatory_instance1 <- toString(Regulatory_instance1)
    CommonTable$Regulatory_instance <- new_function(Regulatory_instance1,blank,Regulatory_instance)
    
    
    Other_Regulatory_instances1 <- cuban_page %>% html_nodes(".field-field-other-instances .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Other_Regulatory_instances1 <- toString(Other_Regulatory_instances1)
    CommonTable$Other_Regulatory_instances <- new_function(Other_Regulatory_instances1,blank,Other_Regulatory_instances)
    
    
    Notification_date1 <- cuban_page %>% html_nodes(".field-field-date-notification .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Notification_date1 <- toString(Notification_date1)
    CommonTable$Notification_date <- new_function(Notification_date1,blank,Notification_date)
    
    Reference_number1 <- cuban_page %>% html_nodes(".field-field-ref-number .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Reference_number1 <- toString(Reference_number1)
    CommonTable$Reference_number <- new_function(Reference_number1,blank,Reference_number)   
    
    
    Authorization_from_beginning <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                               Unique_id_number_short=CommonTable$Unique_id_number_short,
                                               Regulatory_instance_to_authorize_initiation=CommonTable$Regulatory_instance_to_authorize_initiation,
                                               Regulatory_instance=CommonTable$Regulatory_instance,
                                               Other_Regulatory_instances=CommonTable$Other_Regulatory_instances,
                                               Notification_date=CommonTable$Notification_date,
                                               Reference_number=CommonTable$Reference_number)
    
    dbWriteTable(mydb, "Authorization_from_beginning", Authorization_from_beginning , append = TRUE )
    write.table(Authorization_from_beginning,"Authorization_from_beginning_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Authorization_from_beginning_cuban.csv"), append = T)
    
    
    #Table3: Principal Investigator Details
    
    First_name1 <- cuban_page %>% html_nodes(".field-field-first-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    First_name1 <- toString(First_name1)
    CommonTable$First_name <- new_function(First_name1,blank,First_name)
    
    Middle_name1 <- cuban_page %>% html_nodes(".field-field-midle-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Middle_name1 <- toString(Middle_name1)
    CommonTable$Middle_name <- new_function(Middle_name1,blank,Middle_name)
    
    Last_name1 <- cuban_page %>% html_nodes(".field-field-last-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Last_name1 <- toString(Last_name1)
    CommonTable$Last_name <- new_function(Last_name1,blank,Last_name)
    
    Medical_speciality1 <- cuban_page %>% html_nodes(".field-field-spec-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim() %>% str_remove_all('\\"')
    Medical_speciality1 <- toString(Medical_speciality1)
    CommonTable$Medical_speciality <- new_function(Medical_speciality1,blank,Medical_speciality)
    
    Affiliation1 <- cuban_page %>% html_nodes(".field-field-affiliation .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim() %>% str_remove_all('\\"')
    Affiliation1 <- toString(Affiliation1)
    CommonTable$Affiliation <- new_function(Affiliation1,blank,Affiliation)
    
    Postal_address1 <- cuban_page %>% html_nodes(".field-field-address-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim() %>% str_remove_all('\\"')
    Postal_address1 <- toString(Postal_address1)
    CommonTable$Postal_address <- new_function(Postal_address1,blank,Postal_address)
    
    City1 <- cuban_page %>% html_nodes(".field-field-city-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    City1 <- toString(City1)
    CommonTable$City <- new_function(City1,blank,City)
    
    Country1 <- cuban_page %>% html_nodes(".field-field-country-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Country1 <- toString(Country1)
    CommonTable$Country <- new_function(Country1,blank,Country)
    
    Zip_code1 <- cuban_page %>% html_nodes(".field-field-code-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Zip_code1 <- toString(Zip_code1)
    CommonTable$Zip_code <- new_function(Zip_code1,blank,Zip_code)
    
    Telephone_number1 <- cuban_page %>% html_nodes(".field-field-investigator-phone .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim() %>% str_remove_all("\\+")
    Telephone_number1 <- toString(Telephone_number1)
    CommonTable$Telephone_number <- new_function(Telephone_number1,blank,Telephone_number)
    
    Email_address1 <- cuban_page %>% html_nodes(".field-field-email-investigator .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Email_address1 <- toString(Email_address1)
    CommonTable$Email_address <- new_function(Email_address1,blank,Email_address)
    
    Principal_investigator_details <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                                 Unique_id_number_short=CommonTable$Unique_id_number_short,
                                                 First_name=CommonTable$First_name,
                                                 Middle_name=CommonTable$Middle_name,
                                                 Last_name=CommonTable$Last_name,
                                                 Medical_speciality=CommonTable$Medical_speciality,
                                                 Affiliation=CommonTable$Affiliation,
                                                 Postal_address=CommonTable$Postal_address,
                                                 City=CommonTable$City,
                                                 Country=CommonTable$Country,
                                                 Zip_code=CommonTable$Zip_code,
                                                 Telephone_number=CommonTable$Telephone_number,
                                                 Email_address=CommonTable$Email_address)
    
    dbWriteTable(mydb, "Principal_investigator_details", Principal_investigator_details , append = TRUE )
    write.table(Principal_investigator_details,"Principal_investigator_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Principal_investigator_details.csv"), append = T)
    
    
    
    
    #Table4: Clinical Sites to participate
    
    Countries_of_recruitment1 <- cuban_page %>% html_nodes(".field-field-countries .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Countries_of_recruitment1 <- toString(Countries_of_recruitment1)
    CommonTable$Countries_of_recruitment <- new_function(Countries_of_recruitment1,blank,Countries_of_recruitment)
    
    
    Clinical_sites1 <- cuban_page %>% html_nodes(".field-field-participating-sc .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Clinical_sites1 <- toString(Clinical_sites1)   
    CommonTable$Clinical_sites <- new_function(Clinical_sites1,blank,Clinical_sites) 
    
    Research_ethics_committee1<- cuban_page %>% html_nodes(".field-field-ethics-committes .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Research_ethics_committee1<- toString(Research_ethics_committee1)
    CommonTable$Research_ethics_committee <- new_function(Research_ethics_committee1,blank,Research_ethics_committee)
    
    
    Clinical_sites <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                 Unique_id_number_short=CommonTable$Unique_id_number_short,
                                 Countries_of_recruitment=CommonTable$Countries_of_recruitment,
                                 Clinical_sites=CommonTable$Clinical_sites,
                                 Research_ethics_committee=CommonTable$Research_ethics_committee)
    
    dbWriteTable(mydb, "Clinical_sites", Clinical_sites , append = TRUE )
    write.table(Clinical_sites,"Clinical_sites.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Clinical_sites.csv"), append = T)
    
    
    
    #Table 5: Recruitment-Status
    
    Recruitment_status1 <- cuban_page %>% html_nodes(".field-field-recruitment-status .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Recruitment_status1 <- toString(Recruitment_status1)
    CommonTable$Recruitment_status <- new_function(Recruitment_status1,blank,Recruitment_status)
    
    Date_of_first_enrollment1 <- cuban_page %>% html_nodes(".field-field-date-first-enrollment .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Date_of_first_enrollment1 <- toString(Date_of_first_enrollment1)
    CommonTable$Date_of_first_enrollment <- new_function(Date_of_first_enrollment1,blank,Date_of_first_enrollment)
    
    Recruitment_status <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                     Unique_id_number_short=CommonTable$Unique_id_number_short,
                                     Recruitment_status=CommonTable$Recruitment_status,
                                     Date_of_first_enrollment=CommonTable$Date_of_first_enrollment)
    
    
    dbWriteTable(mydb, "Recruitment_status", Recruitment_status , append = TRUE )
    write.table(Recruitment_status,"Recruitment_status.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Recruitment_status.csv"), append = T)
    
    
    #Table 6: Health Condition and Intervention
    
    Health_condition1 <- cuban_page %>% html_nodes(".field-field-health-condition .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Health_condition1 <- toString(Health_condition1)  
    CommonTable$Health_condition <- new_function(Health_condition1,blank,Health_condition)
    
    Health_condition_code1 <- cuban_page %>% html_nodes(".field-field-hc-code .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Health_condition_code1 <- toString(Health_condition_code1)
    CommonTable$Health_condition_code <- new_function(Health_condition_code1,blank,Health_condition_code)
    
    Health_condition_keyword1 <- cuban_page %>% html_nodes(".field-field-hc-keyword .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Health_condition_keyword1 <- toString(Health_condition_keyword1)
    CommonTable$Health_condition_keyword <- new_function(Health_condition_keyword1,blank,Health_condition_keyword)
    
    
    Intervention1 <- cuban_page %>% html_nodes(".field-field-intervention .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Intervention1 <- toString(Intervention1)   
    CommonTable$Intervention <- new_function(Intervention1,blank,Intervention)
    
    Intervention_code1 <- cuban_page %>% html_nodes(".field-field-inter-code .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Intervention_code1 <- toString(Intervention_code1)
    CommonTable$Intervention_code <- new_function(Intervention_code1,blank,Intervention_code)
    
    Intervention_keyword1 <- cuban_page %>% html_nodes(".field-field-i-keyword .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Intervention_keyword1 <- toString(Intervention_keyword1)
    CommonTable$Intervention_keyword <- new_function(Intervention_keyword1,blank,Intervention_keyword)
    
    
    
    Health_condition_intervention <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                                Unique_id_number_short=CommonTable$Unique_id_number_short,
                                                Health_condition=CommonTable$Health_condition,
                                                Health_condition_code=CommonTable$Health_condition_code,
                                                Health_condition_keyword=CommonTable$Health_condition_keyword,
                                                Intervention=CommonTable$Intervention,
                                                Intervention_code=CommonTable$Intervention_code,
                                                Intervention_keyword=CommonTable$Intervention_keyword)
    
    dbWriteTable(mydb, "Health_condition_intervention", Health_condition_intervention , append = TRUE )
    write.table(Health_condition_intervention,"Health_condition_intervention.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Health_condition_intervention.csv"), append = T)
    
    
    #Table 7: Outcomes and Timepoint
    
    
    Primary_outcome1 <- cuban_page %>% html_nodes(".field-field-primary-outcomes .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Primary_outcome1 <- toString(Primary_outcome1)
    Primary_outcome <- new_function(Primary_outcome1,blank,Primary_outcome)
    
    
    Secondary_outcome1 <- cuban_page %>% html_nodes(".field-field-secondary-outcomes .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Secondary_outcome1 <- toString(Secondary_outcome1)
    Secondary_outcome <- new_function(Secondary_outcome1,blank,Secondary_outcome)
    
    Outcomes_and_timepoint <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                         Unique_id_number_short=CommonTable$Unique_id_number_short,
                                         Primary_outcome=CommonTable$Primary_outcome,
                                         Secondary_outcome=CommonTable$Secondary_outcome)
    
    dbWriteTable(mydb, "Outcomes_and_timepoint", Outcomes_and_timepoint , append = TRUE )
    write.table(Outcomes_and_timepoint,"Outcomes_and_timepoint.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Outcomes_and_timepoint.csv"), append = T)
    
    
    #Table 8: Selection-criteria
    
    Gender1 <- cuban_page %>% html_nodes(".field-field-gender .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Gender1 <- toString(Gender1)
    CommonTable$Gender <- new_function(Gender1,blank,Gender)
    
    Minimum_age1 <- cuban_page %>% html_nodes(".field-field-minimum-age .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Minimum_age1 <- toString(Minimum_age1)
    CommonTable$Minimum_age <- new_function(Minimum_age1,blank,Minimum_age)
    
    Maximum_age1 <- cuban_page %>% html_nodes(".field-field-maximum-age .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Maximum_age1 <- toString(Maximum_age1)
    CommonTable$Maximum_age  <- new_function(Maximum_age1,blank,Maximum_age)
    
    Inclusion_criteria1 <- cuban_page %>% html_nodes(".field-field-inclusion-criteria .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Inclusion_criteria1 <- toString(Inclusion_criteria1)
    CommonTable$Inclusion_criteria <- new_function(Inclusion_criteria1,blank,Inclusion_criteria)
    
    Exclusion_criteria1 <- cuban_page %>% html_nodes(".field-field-exclusion-criteria .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Exclusion_criteria1 <- toString(Exclusion_criteria1)
    CommonTable$Exclusion_criteria <- new_function(Exclusion_criteria1,blank,Exclusion_criteria)
    
    Type_of_population1 <- cuban_page %>% html_nodes(".field-field-type-population .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Type_of_population1 <- toString(Type_of_population1)
    CommonTable$Type_of_population <- new_function(Type_of_population1,blank,Type_of_population)
    
    Type_of_participant1 <- cuban_page %>% html_nodes(".field-field-type-participant .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Type_of_participant1 <- toString(Type_of_participant1)
    CommonTable$Type_of_participant <- new_function(Type_of_participant1,blank,Type_of_participant)
    
    
    
    Selection_criteria <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                     Unique_id_number_short=CommonTable$Unique_id_number_short,
                                     Gender=CommonTable$Gender,
                                     Minimum_age=CommonTable$Minimum_age,
                                     Maximum_age=CommonTable$Maximum_age,
                                     Inclusion_criteria=CommonTable$Inclusion_criteria,
                                     Exclusion_criteria=CommonTable$Exclusion_criteria,
                                     Type_of_population=CommonTable$Type_of_population,
                                     Type_of_participant=CommonTable$Type_of_participant)
    
    dbWriteTable(mydb, "Selection_criteria", Selection_criteria , append = TRUE )
    write.table(Selection_criteria,"Selection_criteria.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Selection_criteria.csv"), append = T)
    
    
    
    #Table9: Study Design
    
    
    Type_study1 <- cuban_page %>% html_nodes(".field-field-type-study .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Type_study1 <- toString(Type_study1)
    CommonTable$Type_study <- new_function(Type_study1,blank,Type_study)
    
    Purpose1 <- cuban_page %>% html_nodes(".field-field-purpose .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Purpose1 <- toString(Purpose1)
    CommonTable$Purpose <- new_function(Purpose1,blank,Purpose)
    
    Other_purpose1 <- cuban_page %>% html_nodes(".field-field-other-purpose .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Other_purpose1 <- toString(Other_purpose1)
    CommonTable$Other_purpose <- new_function(Other_purpose1,blank,Other_purpose)
    
    Allocation1 <- cuban_page %>% html_nodes(".field-field-allocation .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Allocation1 <- toString(Allocation1)
    CommonTable$Allocation <- new_function(Allocation1,blank,Allocation)
    
    Masking1 <- cuban_page %>% html_nodes(".field-field-masking .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Masking1 <- toString(Masking1)
    CommonTable$Masking <- new_function(Masking1,blank,Masking)
    
    Control_group1 <- cuban_page %>% html_nodes(".field-field-control-group .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Control_group1 <- toString(Control_group1)
    CommonTable$Control_group <- new_function(Control_group1,blank,Control_group)
    
    Study_design1 <- cuban_page %>% html_nodes(".field-field-study-design .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Study_design1 <- toString(Study_design1)
    CommonTable$Study_design <- new_function(Study_design1,blank,Study_design)
    
    Other_design1 <- cuban_page %>% html_nodes(".field-field-other-design .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Other_design1 <- toString(Other_design1)
    CommonTable$Other_design <- new_function(Other_design1,blank,Other_design)
    
    Phase1 <- cuban_page %>% html_nodes(".field-field-phase .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Phase1 <- toString(Phase1)
    CommonTable$Phase <- new_function(Phase1,blank,Phase)
    
    Target_sample_size1 <- cuban_page %>% html_nodes(".field-field-target-sample-size .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Target_sample_size1 <- toString(Target_sample_size1)
    CommonTable$Target_sample_size <- new_function(Target_sample_size1,blank,Target_sample_size)
    
    
    Study_design <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                               Unique_id_number_short=CommonTable$Unique_id_number_short,
                               Type_study=CommonTable$Type_study,
                               Purpose=CommonTable$Purpose,
                               Other_purpose=CommonTable$Other_purpose,
                               Allocation=CommonTable$Allocation,
                               Masking=CommonTable$Masking,
                               Control_group=CommonTable$Control_group,
                               Study_design=CommonTable$Study_design,
                               Other_design=CommonTable$Other_design,
                               Phase=CommonTable$Phase,
                               Target_sample_size=CommonTable$Target_sample_size)
    
    dbWriteTable(mydb, "Study_design", Study_design , append = TRUE )
    write.table(Study_design,"Study_design_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Study_design_cuban.csv"), append = T)
    
    
    
    #Table 10: Contact for Public Queries
    
    First_name1 <- cuban_page %>% html_nodes(".field-field-firstname-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    First_name1 <- toString(First_name1)
    CommonTable$First_name <- new_function(First_name1,blank,First_name)
    
    
    Middle_name1 <- cuban_page %>% html_nodes(".field-field-midlename-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Middle_name1 <- toString(Middle_name1)
    CommonTable$Middle_name <- new_function(Middle_name1,blank,Middle_name)
    
    
    Last_name1 <- cuban_page %>% html_nodes(".field-field-lastname-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Last_name1 <- toString(Last_name1)
    CommonTable$Last_name <- new_function(Last_name1,blank,Last_name)
    
    Specialty1 <- cuban_page %>% html_nodes(".field-field-specialty-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Specialty1 <- toString(Specialty1)
    CommonTable$Specialty <- new_function(Specialty1,blank,Specialty)
    
    Affiliation1 <- cuban_page %>% html_nodes(".field-field-affiliation-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Affiliation1 <- toString(Affiliation1)
    CommonTable$Affiliation <- new_function(Affiliation1,blank,Affiliation)
    
    Postal_address1 <- cuban_page %>% html_nodes(".field-field-address-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Postal_address1 <- toString(Postal_address1)
    CommonTable$Postal_address <- new_function(Postal_address1,blank,Postal_address)
    
    City1 <- cuban_page %>% html_nodes(".field-field-city-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    City1 <- toString(City1)
    CommonTable$City <- new_function(City1,blank,City)
    
    Country1 <- cuban_page %>% html_nodes(".field-field-country-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Country1 <- toString(Country1)
    CommonTable$Country <- new_function(Country1,blank,Country)
    
    Zip_code1 <- cuban_page %>% html_nodes(".field-field-zipcode-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Zip_code1 <- toString(Zip_code1)
    CommonTable$Zip_code <- new_function(Zip_code1,blank,Zip_code)
    
    Telephone1 <- cuban_page %>% html_nodes(".field-field-tel-contact-gen .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim() %>% str_remove_all("\\+")
    Telephone1 <- toString(Telephone1)
    CommonTable$Telephone<- new_function(Telephone1,blank,Telephone)
    
    Email1 <- cuban_page %>% html_nodes(".field-field-email-public .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Email1 <- toString(Email1)
    CommonTable$Email <- new_function(Email1,blank,Email)
    
    
    
    Contact_for_public_queries <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                             Unique_id_number_short=CommonTable$Unique_id_number_short,
                                             First_name=CommonTable$First_name,
                                             Middle_name=CommonTable$Middle_name,
                                             Last_name=CommonTable$Last_name,
                                             Specialty=CommonTable$Specialty,
                                             Affiliation=CommonTable$Affiliation,
                                             Postal_address=CommonTable$Postal_address,
                                             City=CommonTable$City,
                                             Country=CommonTable$Country,
                                             Zip_code=CommonTable$Zip_code,
                                             Telephone=CommonTable$Telephone,
                                             Email=CommonTable$Email)
    
    dbWriteTable(mydb, "Contact_for_public_queries", Contact_for_public_queries , append = TRUE )
    write.table(Contact_for_public_queries,"Contact_for_public_queries.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Contact_for_public_queries.csv"), append = T)
    
    
    
    
    #Table 11: Contact for Scientific Queries
    
    First_name1 <- cuban_page %>% html_nodes(".field-field-firstname-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    First_name1 <- toString(First_name1)
    CommonTable$First_name <- new_function(First_name1,blank,First_name)
    
    Middle_name1 <- cuban_page %>% html_nodes(".field-field-midlename-scientfic .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Middle_name1 <- toString(Middle_name1)
    CommonTable$Middle_name <- new_function(Middle_name1,blank,Middle_name)
    
    Last_name1 <- cuban_page %>% html_nodes(".field-field-lastname-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Last_name1 <- toString(Last_name1)
    CommonTable$Last_name <- new_function(Last_name1,blank,Last_name)
    
    Specialty1 <- cuban_page %>% html_nodes(".field-field-specialty-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Specialty1 <- toString(Specialty1)
    CommonTable$Specialty <- new_function(Specialty1,blank,Specialty)
    
    Affiliation1 <- cuban_page %>% html_nodes(".field-field-affiliation-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Affiliation1 <- toString(Affiliation1)
    CommonTable$Affiliation <- new_function(Affiliation1,blank,Affiliation)
    
    Postal_address1 <- cuban_page %>% html_nodes(".field-field-address-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Postal_address1 <- toString(Postal_address1)
    CommonTable$Postal_address <- new_function(Postal_address1,blank,Postal_address)
    
    
    City1 <- cuban_page %>% html_nodes(".field-field-city-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    City1 <- toString(City1)
    CommonTable$City <- new_function(City1,blank,City)
    
    Country1 <- cuban_page %>% html_nodes(".field-field-country-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Country1 <- toString(Country1)
    CommonTable$Country <- new_function(Country1,blank,Country)
    
    
    Zip_code1 <- cuban_page %>% html_nodes(".field-field-zipcode-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Zip_code1 <- toString(Zip_code1)
    CommonTable$Zip_code <- new_function(Zip_code1,blank,Zip_code)
    
    
    Telephone1 <- cuban_page %>% html_nodes(".field-field-tel-contact-cient .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim() %>% str_remove_all("\\+")
    Telephone1 <- toString(Telephone1)
    CommonTable$Telephone <- new_function(Telephone1,blank,Telephone)
    
    Email1 <- cuban_page %>% html_nodes(".field-field-email-scientific .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Email1 <- toString(Email1)
    CommonTable$Email <- new_function(Email1,blank,Email)
    
    
    Contact_for_scientific_queries <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                                 Unique_id_number_short=CommonTable$Unique_id_number_short,
                                                 First_name=CommonTable$First_name,
                                                 Middle_name=CommonTable$Middle_name,
                                                 Last_name=CommonTable$Last_name,
                                                 Specialty=CommonTable$Specialty,
                                                 Affiliation=CommonTable$Affiliation,
                                                 Postal_address=CommonTable$Postal_address,
                                                 City=CommonTable$City,
                                                 Country=CommonTable$Country,
                                                 Zip_code=CommonTable$Zip_code,
                                                 Telephone=CommonTable$Telephone,
                                                 Email=CommonTable$Email)
    
    dbWriteTable(mydb, "Contact_for_scientific_queries", Contact_for_scientific_queries , append = TRUE )
    write.table(Contact_for_scientific_queries,"Contact_for_scientific_queries.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Contact_for_scientific_queries.csv"), append = T)
    
    
    
    #Table 12: Data Sharing plan
    
    Data_sharing_plan1 <- cuban_page %>% html_nodes(".field-field-data-plan .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Data_sharing_plan1 <- toString(Data_sharing_plan1)
    CommonTable$Data_sharing_plan <- new_function(Data_sharing_plan1,blank,Data_sharing_plan)
    
    Description_of_data_sharing_plan1 <- cuban_page %>% html_nodes(".field-field-des-plan .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Description_of_data_sharing_plan1 <- toString(Description_of_data_sharing_plan1)
    CommonTable$Description_of_data_sharing_plan <- new_function(Description_of_data_sharing_plan1,blank,Description_of_data_sharing_plan)
    
    Additional_information_to_share1 <- cuban_page %>% html_nodes(".field-field-aditional-documents .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Additional_information_to_share1 <- toString(Additional_information_to_share1)
    CommonTable$Additional_information_to_share <- new_function(Additional_information_to_share1,blank,Additional_information_to_share)
    
    URL_for_additional_information1 <- cuban_page %>% html_nodes(".field-field-url-documents .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    URL_for_additional_information1 <- toString(URL_for_additional_information1)
    CommonTable$URL_for_additional_information <- new_function(URL_for_additional_information1,blank,URL_for_additional_information)
    
    Data_sharing_plan <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                    Unique_id_number_short=CommonTable$Unique_id_number_short,
                                    Data_sharing_plan=CommonTable$Data_sharing_plan,
                                    Description_of_data_sharing_plan=CommonTable$Description_of_data_sharing_plan,
                                    Additional_information_to_share=CommonTable$Additional_information_to_share,
                                    URL_for_additional_information=CommonTable$URL_for_additional_information)
    
    
    dbWriteTable(mydb, "Data_sharing_plan", Data_sharing_plan , append = TRUE )
    write.table(Data_sharing_plan,"Data_sharing_plan_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Data_sharing_plan_cuban.csv"), append = T)
    
    
    
    
    #Table 13: Research-ethics-committee
    
    Name_of_research_ethics_committee1 <- cuban_page %>% html_nodes(".field-field-research-committee .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Name_of_research_ethics_committee1 <- toString(Name_of_research_ethics_committee1)
    CommonTable$Name_of_research_ethics_committee  <- new_function(Name_of_research_ethics_committee1,blank,Name_of_research_ethics_committee)
    
    
    Status_of_evaluation1 <- cuban_page %>% html_nodes(".field-field-evaluation-status .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Status_of_evaluation1 <- toString(Status_of_evaluation1)
    CommonTable$Status_of_evaluation<- new_function(Status_of_evaluation1,blank,Status_of_evaluation) 
    
    
    
    Status_of_evaluation_date1 <- cuban_page %>% html_nodes(".field-field-date-committee .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Status_of_evaluation_date1 <- toString(Status_of_evaluation_date1)
    CommonTable$Status_of_evaluation_date<- new_function(Status_of_evaluation_date1,blank,Status_of_evaluation_date) 
    
    
    
    Postal_address_of_ethics_committee1 <- cuban_page %>% html_nodes(".field-field-zip-committee .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Postal_address_of_ethics_committee1 <- toString(Postal_address_of_ethics_committee1)
    CommonTable$Postal_address_of_ethics_committee <- new_function(Postal_address_of_ethics_committee1,blank,Postal_address_of_ethics_committee)
    
    
    
    Telephone1 <- cuban_page %>% html_nodes(".field-field-commmitte-fhone .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim() %>% str_remove_all("\\+")
    Telephone1 <- toString(Telephone1)
    CommonTable$Telephone <- new_function(Telephone1,blank,Telephone) 
    
    
    
    Email1 <- cuban_page %>% html_nodes(".field-field-commmitte-mail .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Email1 <- toString(Email1)
    CommonTable$Email <- new_function(Email1,blank,Email)
    
    Research_ethics_committee <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                            Unique_id_number_short=CommonTable$Unique_id_number_short,
                                            Name_of_research_ethics_committee=CommonTable$Name_of_research_ethics_committee,
                                            Status_of_evaluation=CommonTable$Status_of_evaluation,
                                            Status_of_evaluation_date=CommonTable$Status_of_evaluation_date,
                                            Postal_address_of_ethics_committee=CommonTable$Postal_address_of_ethics_committee,
                                            Telephone=CommonTable$Telephone,
                                            Email=CommonTable$Email)
    
    dbWriteTable(mydb, "Research_ethics_committee", Research_ethics_committee, append = TRUE )
    write.table(Research_ethics_committee, "Research_ethics_committee.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Research_ethics_committee.csv"), append = T)
    
    
    
    
    
    
    
    #Table 14: Study Completion Date
    
    
    Final_enrollment_number1 <- cuban_page %>% html_nodes(".field-field-actual-enroment .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Final_enrollment_number1 <- toString(Final_enrollment_number1)
    CommonTable$Final_enrollment_number <- new_function(Final_enrollment_number1,blank,Final_enrollment_number)
    
    Study_completion_date1 <- cuban_page %>% html_nodes(".field-field-study-date .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Study_completion_date1 <- toString(Study_completion_date1)
    CommonTable$Study_completion_date <- new_function(Study_completion_date1,blank,Study_completion_date)
    
    
    Date_of_available_results1 <- cuban_page %>% html_nodes(".field-field-results-date .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Date_of_available_results1 <- toString(Date_of_available_results1)
    CommonTable$Date_of_available_results  <- new_function(Date_of_available_results1,blank,Date_of_available_results)
    
    
    Date_of_first_publication1 <- cuban_page %>% html_nodes(".field-field-first-publication .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Date_of_first_publication1 <- toString(Date_of_first_publication1)
    CommonTable$Date_of_first_publication <- new_function(Date_of_first_publication1,blank,Date_of_first_publication)
    
    
    
    Study_completion_date <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                        Unique_id_number_short=CommonTable$Unique_id_number_short,
                                        Final_enrollment_number=CommonTable$Final_enrollment_number,
                                        Study_completion_date=CommonTable$Study_completion_date,
                                        Date_of_available_results=CommonTable$Date_of_available_results,
                                        Date_of_first_publication=CommonTable$Date_of_first_publication)
    
    dbWriteTable(mydb, "Study_completion_date", Study_completion_date, append = TRUE )
    write.table(Study_completion_date, "Study_completion_date.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Study_completion_date.csv"), append = T)
    
    
    
    #Table 15: Results_Study
    
    Participant_flow1 <- cuban_page %>% html_nodes(".field-field-par-flow .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Participant_flow1 <- toString(Participant_flow1)
    CommonTable$Participant_flow <- new_function(Participant_flow1,blank,Participant_flow)
    
    Baseline_characteristics1 <- cuban_page %>% html_nodes(".field-field-base-charat .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Baseline_characteristics1 <- toString(Baseline_characteristics1)
    CommonTable$Baseline_characteristics <- new_function(Baseline_characteristics1,blank,Baseline_characteristics)
    
    Outcome_measures1 <- cuban_page %>% html_nodes(".field-field-out-measures .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Outcome_measures1 <- toString(Outcome_measures1)
    CommonTable$Outcome_measures <- new_function(Outcome_measures1,blank,Outcome_measures)
    
    Adverse_events1 <- cuban_page %>% html_nodes(".field-field-adv-events .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Adverse_events1 <- toString(Adverse_events1)
    CommonTable$Adverse_events <- new_function(Adverse_events1,blank,Adverse_events)
    
    Summary_studies1 <- cuban_page %>% html_nodes(".field-field-summary .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Summary_studies1 <- toString(Summary_studies1)
    CommonTable$Summary_studies <- new_function(Summary_studies1,blank,Summary_studies)
    
    Results_file1 <- cuban_page %>% html_nodes(".filefield-file a") %>%  html_attr("href") %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Results_file1 <- toString(Results_file1)
    CommonTable$Results_file <- new_function(Results_file1,blank,Results_file)
    
    Url_for_results_file1 <- cuban_page %>% html_nodes(".field-field-results-url-link .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_squish() %>% str_trim()
    Url_for_results_file1 <- toString(Url_for_results_file1)
    CommonTable$Url_for_results_file <- new_function(Url_for_results_file1,blank,Url_for_results_file) 
    
    Results_study <- data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                Unique_id_number_short=CommonTable$Unique_id_number_short,
                                Participant_flow=CommonTable$Participant_flow,
                                Baseline_characteristics=CommonTable$Baseline_characteristics,
                                Outcome_measures=CommonTable$Outcome_measures,
                                Adverse_events=CommonTable$Adverse_events,
                                Summary_studies=CommonTable$Summary_studies,
                                Results_file=CommonTable$Results_file,
                                Url_for_results_file=CommonTable$Url_for_results_file)
    
    dbWriteTable(mydb, "Results_study", Results_study , append = TRUE )
    write.table(Results_study,"Results_study_cuban.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Results_study_cuban.csv"), append = T)
    
    
    #Table 16: Registration and Update
    
    Primary_registry1 <- cuban_page %>% html_nodes(".field-field-primary-registry .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Primary_registry1 <- toString(Primary_registry1)
    CommonTable$Primary_registry <- new_function(Primary_registry1,blank,Primary_registry)
    
    
    Date_of_registration_in_primary_registry1 <- cuban_page %>% html_nodes(".field-field-date-register .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Date_of_registration_in_primary_registry1 <- toString(Date_of_registration_in_primary_registry1)
    CommonTable$Date_of_registration_in_primary_registry <- new_function(Date_of_registration_in_primary_registry1,blank,Date_of_registration_in_primary_registry)
    
    
    Record_verification_date1 <- cuban_page %>% html_nodes(".field-field-record-verification .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Record_verification_date1 <- toString(Record_verification_date1)
    CommonTable$Record_verification_date <- new_function(Record_verification_date1,blank,Record_verification_date)
    
    
    Next_update_date1 <- cuban_page %>% html_nodes(".field-field-update-date .field-item") %>%  html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\r") %>% str_remove_all("\\t") %>% str_remove_all('\\"') %>% str_squish() %>% str_trim()
    Next_update_date1 <- toString(Next_update_date1)
    CommonTable$Next_update_date <- new_function(Next_update_date1,blank,Next_update_date)
    
    Registration_and_update=data.frame(Unique_id_number=CommonTable$Unique_id_number,
                                       Unique_id_number_short=CommonTable$Unique_id_number_short,
                                       Primary_registry=CommonTable$Primary_registry,
                                       Date_of_registration_in_primary_registry=CommonTable$Date_of_registration_in_primary_registry,
                                       Record_verification_date=CommonTable$Record_verification_date,
                                       Next_update_date=CommonTable$Next_update_date)
    
    dbWriteTable(mydb, "Registration_and_update", Registration_and_update, append = TRUE )
    write.table(Registration_and_update, "Registration_and_update.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Registration_and_update.csv"), append = T)
    
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
  }
}
dbDisconnect(mydb)


