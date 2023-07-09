#**********************************************Script - 1**************************************************************************
#Downloading records from the REBEC registry

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids=c(1:5820)
mydb <- dbConnect(RSQLite::SQLite(), "rishima_brazil.sqlite")
counter=0
for (page_result in seq(from =1,to=291)) {
  link <- url(paste0("https://ensaiosclinicos.gov.br/list/",page_result),"rb")
  print(link)
  page <- read_html(link)
  link_of_all_pages <-  page %>% html_nodes(".text-justify .title_link") %>% html_attr("href") 
  brazil_page_links <- data.frame(link_of_all_pages)
  write.table(brazil_page_links, "brazil_page_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("brazil_page_links.csv"), append = T)
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
} 



brazil_page_links <- data.frame()
brazil_page_links <- read.csv("brazil_page_links.csv")
for (i in seq_along(ids)) {
  official_url = brazil_page_links[ids[i],"link_of_all_pages"] 
  output_file=paste0("brazil_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  dbWriteTable(mydb, "time_stamp_brazil", time_stamp, append = TRUE)
  write.table(time_stamp, "time_stamp_brazil.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_brazil.csv"), append = T)
  
  
  
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}

dbDisconnect(mydb)




#****************************************************Script - 2*************************************************************************************
#Web-scraped the fields from all the downloaded records to look for the keyword 'India' or 'CTRI'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)

ids = c(1:5820)
counter = 0

mydb <- dbConnect(RSQLite::SQLite(), "brazildb1.sqlite")


for (i in seq_along(ids)) { 
  
  myfile = paste0("brazil_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    brazil_page <- read_html(myfile)
    
    if(is_empty(brazil_page)) {
      
      next 
    }
    
    CommonTable <- NULL
    
    blank <- "blank"
    
    new_function <- function(a,b,c) {
      
      c <- paste0(a,b)
      if (c == "blank") {
        c <- "---"
      } else {
        c <- a
      }
    }  
    UTN_code1 <- brazil_page %>% html_nodes("h3+ ul:nth-child(15) .label+ .value") %>% html_text() %>% str_squish() %>% str_trim()
    
    CommonTable$UTN_code <- new_function(UTN_code1,blank,UTN_code)
    CommonTable$ID_with_title <- brazil_page %>% html_nodes(".mainContent > h2") %>% html_text() %>% str_squish() %>% str_trim() %>% str_split(" ") %>% map_chr(1)
    
    
    #******************************************Table1: Dates of registration and update**************************************************************
    
    Date_of_registration1 <- brazil_page %>% html_nodes(".label+ .value:nth-child(3)") %>% html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\(mm/dd/yyyy\\)") %>% str_squish() %>% str_trim()
    Date_of_registration1 <- as.Date(Date_of_registration1,format= "%m/%d/%Y")
    CommonTable$Date_of_registration <- new_function(Date_of_registration1,blank,Date_of_registration)
    
    Last_approval_date1 <- brazil_page %>% html_nodes("br~ .label+ .value") %>% html_text() %>% str_remove_all("\\n") %>% str_remove_all("\\(mm/dd/yyyy\\)") %>% str_squish() %>% str_trim()
    Last_approval_date1 <- as.Date(Last_approval_date1,format ="%m/%d/%Y")
    CommonTable$Last_approval_date <- new_function(Last_approval_date1,blank,Last_approval_date)
    
    
    Dates_registration_and_update <- data.frame(ID_with_title=CommonTable$ID_with_title,
                                                UTN_code=CommonTable$UTN_code,
                                                Date_of_registration=CommonTable$Date_of_registration,
                                                Last_approval_date=CommonTable$Last_approval_date)
    
    
    dbWriteTable(mydb, "Dates_registration_and_update", Dates_registration_and_update , append = TRUE )
    write.table(Dates_registration_and_update,"Dates_registration_and_update.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Dates_registration_and_update.csv"), append = T)
    
    
    #********************************************Table2: Study type and Scientific title*************************************************************
    
    Study_type1 <- brazil_page %>% html_nodes("h3+ p") %>% html_text() %>% str_squish() %>% str_trim()
    CommonTable$Study_type <- new_function(Study_type1,blank,Study_type)
    Scientific_title1 <- brazil_page %>% html_nodes("h3+ .row .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Scientific_title1 <- toString(Scientific_title1)
    CommonTable$Scientific_title <- new_function(Scientific_title1,blank,Scientific_title)
    
    Study_type_and_scientific_title  <- data.frame(ID_with_title=CommonTable$ID_with_title,
                                                   UTN_code=CommonTable$UTN_code,
                                                   Study_type=CommonTable$Study_type,
                                                   Scientific_title=CommonTable$Scientific_title)
    
    
    dbWriteTable(mydb, "Study_type_and_scientific_title", Study_type_and_scientific_title , append = TRUE )
    write.table(Study_type_and_scientific_title,"Study_type_and_scientific_title.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Study_type_and_scientific_title.csv"), append = T)
    
    
    
    
    
    #*********************************************Table3: Trial Identification*********************************************************************
    
    
    Public_title1 <- brazil_page %>% html_nodes("ul:nth-child(15) li+ li .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Public_title1 <- toString(Public_title1)
    CommonTable$Public_title <- new_function(Public_title1,blank,Public_title)
    
    Scientific_acronym1 <- brazil_page %>% html_nodes(xpath="/html/body/main/div[2]/section/div/div[1]/section/div[1]/div[2]/div/ul[1]/li[3]/div/div[1]") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Scientific_acronym1 <- toString(Scientific_acronym1)
    CommonTable$Scientific_acronym <- new_function(Scientific_acronym1,blank,Scientific_acronym)
    
    
    Public_acronym1 <- brazil_page %>% html_nodes(xpath="/html/body/main/div[2]/section/div/div[1]/section/div[1]/div[2]/div/ul[1]/li[4]/div/div[1]") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Public_acronym1 <- toString(Public_acronym1)
    CommonTable$Public_acronym <- new_function(Public_acronym1,blank,Public_acronym)
    
    
    Secondary_identifiers1 <- brazil_page %>% html_nodes(".spacer+ .subset") %>% html_text() %>% str_remove_all("Secondaries identifiers:") %>% str_remove_all("Issuing authority") %>% str_squish() %>% str_trim() 
    Secondary_identifiers1 <- toString(Secondary_identifiers1)
    CommonTable$Secondary_identifiers <- new_function(Secondary_identifiers1,blank,Secondary_identifiers)
    
    Trial_identification <- data.frame(ID_with_title=CommonTable$ID_with_title,
                                       UTN_code=CommonTable$UTN_code,
                                       Public_title=CommonTable$Public_title,
                                       Scientific_acronym=CommonTable$Scientific_acronym,
                                       Public_acronym=CommonTable$Public_acronym,
                                       Secondary_identifiers=CommonTable$Secondary_identifiers)
    
    dbWriteTable(mydb, "Trial_identification", Trial_identification , append = TRUE )
    write.table(Trial_identification,"Trial_identification.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_identification.csv"), append = T)
    
    
    
    #***************************************************Table4: Sponsors*************************************************************************
    
    Primary_sponsor1 <- brazil_page %>% html_nodes("ul:nth-child(17) > li > .value") %>% html_text() %>% str_squish() %>% str_trim()
    Primary_sponsor1 <- toString(Primary_sponsor1)
    CommonTable$Primary_sponsor <- new_function(Primary_sponsor1,blank,Primary_sponsor)
    
    Secondary_sponsor1 <- brazil_page %>% html_nodes("ul:nth-child(17) .subset:nth-child(2)") %>% html_text() %>% str_replace_all("Institution:",",Institution:") %>% str_remove(",") %>% str_remove_all("Secondary sponsor:") %>% str_squish() %>% str_trim()
    Secondary_sponsor1 <- toString(Secondary_sponsor1)
    CommonTable$Secondary_sponsor <- new_function(Secondary_sponsor1,blank,Secondary_sponsor)
    
    Supporting_source1 <- brazil_page %>% html_nodes("ul:nth-child(17) .subset+ .subset") %>% html_text() %>% str_replace_all("Institution:",",Institution:") %>% str_remove(",") %>% str_remove_all("Supporting source:") %>% str_squish() %>% str_trim()
    Supporting_source1 <- toString(Supporting_source1)
    CommonTable$Supporting_source <- new_function(Supporting_source1,blank,Supporting_source)
    
    Sponsors <- data.frame(ID_with_title=CommonTable$ID_with_title,
                           UTN_code=CommonTable$UTN_code,
                           Primary_sponsor=CommonTable$Primary_sponsor,
                           Secondary_sponsor=CommonTable$Secondary_sponsor,
                           Supporting_source=CommonTable$Supporting_source)
    
    dbWriteTable(mydb, "Sponsors", Sponsors , append = TRUE )
    write.table(Sponsors,"Sponsors.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Sponsors.csv"), append = T)
    
    #*************************************************Table5: Heath-conditions**************************************************************
    
    Health_conditions1 <- brazil_page %>% html_nodes("ul:nth-child(19) li:nth-child(1) .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Health_conditions1 <- toString(Health_conditions1)
    CommonTable$Health_conditions <- new_function(Health_conditions1,blank,Health_conditions)
    
    
    Gend_health_conditions1 <- brazil_page %>% html_nodes("ul:nth-child(19) .spacer+ li .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Gend_health_conditions1 <- toString(Gend_health_conditions1)
    CommonTable$Gend_health_conditions <- new_function(Gend_health_conditions1,blank,Gend_health_conditions)
    
    
    Specific_descriptors1 <- brazil_page %>% html_nodes(".spacer~ li+ li .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Specific_descriptors1 <- toString(Specific_descriptors1)
    CommonTable$Specific_descriptors <- new_function(Specific_descriptors1,blank,Specific_descriptors)
    
    
    Health_conditions = data.frame(ID_with_title=CommonTable$ID_with_title,
                                   UTN_code=CommonTable$UTN_code,
                                   Health_conditions=CommonTable$Health_conditions,
                                   General_descriptors=CommonTable$Gend_health_conditions,
                                   Specific_descriptors=CommonTable$Specific_descriptors)
    
    
    dbWriteTable(mydb, "Health_conditions", Health_conditions , append = TRUE )
    write.table(Health_conditions,"Health_conditions.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Health_conditions.csv"), append = T)
    
    
    #*********************************************Table 6: Interventions************************************************************************
    Interventions1 <- brazil_page %>% html_nodes("ul:nth-child(21) li:nth-child(1) .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Interventions1 <- toString(Interventions1)
    CommonTable$Interventions <- new_function(Interventions1,blank,Interventions)
    
    Intervention_descriptors1 <- brazil_page %>% html_nodes("ul:nth-child(21) .spacer+ li .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Intervention_descriptors1 <- toString(Intervention_descriptors1)
    CommonTable$Intervention_descriptors <- new_function(Intervention_descriptors1,blank,Intervention_descriptors) 
    
    
    Interventions = data.frame(ID_with_title=CommonTable$ID_with_title,
                               UTN_code=CommonTable$UTN_code,
                               Interventions=CommonTable$Interventions,
                               Intervention_descriptors=CommonTable$Intervention_descriptors)
    
    dbWriteTable(mydb, "Interventions", Interventions , append = TRUE )
    write.table(Interventions,"Interventions.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Interventions.csv"), append = T)
    
    
    #*********************************************Table 7: Recruitment***************************************************************************
    
    Study_status1 <- brazil_page %>% html_nodes("ul:nth-child(23) li:nth-child(1) .value") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Study_status1 <- toString(Study_status1)
    CommonTable$Study_status <- new_function(Study_status1,blank,Study_status)
    
    Countries1 <- brazil_page %>% html_nodes("ul:nth-child(23) .subset li") %>% html_text() %>% str_remove_all("en") %>% str_remove_all("Countries") %>% str_squish() %>% str_trim()
    Countries1 <- toString(Countries1)
    CommonTable$Countries <- new_function(Countries1,blank,Countries)
    
    Date_of_first_enrollment1 <- brazil_page %>% html_nodes(".subset+ li > .value") %>% html_text() %>% str_remove_all("\\(mm/dd/yyyy\\)") %>% str_squish() %>% str_trim()
    Date_of_first_enrollment1 <- as.Date(Date_of_first_enrollment1,format= "%m/%d/%Y")
    CommonTable$Date_of_first_enrollment <- new_function(Date_of_first_enrollment1,blank,Date_of_first_enrollment)
    
    
    Date_last1 <- brazil_page %>% html_nodes(".subset~ li+ li .value") %>% html_text() %>% str_remove_all("\\(mm/dd/yyyy\\)") %>% str_squish() %>% str_trim()
    Date_last1 <- as.Date(Date_last1,format= "%m/%d/%Y")
    CommonTable$Date_of_last_enrollment <- new_function(Date_last1,blank,Date_of_last_enrollment)
    
    
    Target_sample_size1 <- brazil_page %>% html_nodes("li+ li tr :nth-child(1)")  %>% html_text %>% str_remove_all("Target sample size:") %>% str_squish() %>% str_trim()
    Target_sample_size1 <- parse_number(Target_sample_size1)
    Target_sample_size1 <- Target_sample_size1 [!is.na(Target_sample_size1)]
    CommonTable$Target_sample_size <- new_function(Target_sample_size1,blank,Target_sample_size)
    
    
    Gender1 <- brazil_page %>% html_nodes("li+ li tr :nth-child(2)") %>% html_text() %>% str_remove_all("Gender:") %>% str_squish() %>% str_trim()
    Gender1 <- parse_character(Gender1)
    Gender1 <- Gender1 [!is.na(Gender1)]
    CommonTable$Gender <- new_function(Gender1,blank,Gender)
    
    
    Min_age1 <- brazil_page %>% html_nodes("li+ li .dataTable :nth-child(3)") %>% html_text() %>% str_remove_all("Minimum age:") %>% str_squish() %>% str_trim()
    Min_age1 <- parse_character(Min_age1)
    Min_age1 <- Min_age1 [!is.na(Min_age1)]
    CommonTable$Min_age <- new_function(Min_age1,blank,Min_age)
    
    Max_age1 <- brazil_page %>% html_nodes("li+ li .dataTable :nth-child(4)") %>% html_text() %>% str_remove_all("Maximum age:") %>% str_squish() %>% str_trim()
    Max_age1 <- parse_character(Max_age1)
    Max_age1 <- Max_age1 [!is.na(Max_age1)]
    CommonTable$Max_age <- new_function(Max_age1,blank,Max_age)
    
    Inclusion_criteria1 <- brazil_page %>% html_nodes(xpath="/html/body/main/div[2]/section/div/div[1]/section/div[1]/div[2]/div/ul[5]/li[5]/div/div[1]")  %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Inclusion_criteria1 <- toString(Inclusion_criteria1)
    CommonTable$Inclusion_criteria <- new_function(Inclusion_criteria1,blank,Inclusion_criteria)
    
    if (CommonTable$Inclusion_criteria == "---") {
      Inclusion_criteria2 <- brazil_page %>% html_nodes(xpath="/html/body/main/div[2]/section/div/div[1]/section/div[1]/div[2]/div/ul[5]/li[6]/div/div[1]")  %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
      Inclusion_criteria2 <- toString(Inclusion_criteria2)
      CommonTable$Inclusion_criteria <- new_function(Inclusion_criteria2,blank,Inclusion_criteria)
      
      Exclusion_criteria2 <- brazil_page %>% html_nodes(xpath="/html/body/main/div[2]/section/div/div[1]/section/div[1]/div[2]/div/ul[5]/li[7]/div/div[1]")  %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
      Exclusion_criteria2 <- toString(Exclusion_criteria2)
      CommonTable$Exclusion_criteria <- new_function(Exclusion_criteria2,blank,Exclusion_criteria)
    } else {
      Exclusion_criteria3 <- brazil_page %>% html_nodes(xpath="/html/body/main/div[2]/section/div/div[1]/section/div[1]/div[2]/div/ul[5]/li[6]/div/div[1]") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
      Exclusion_criteria3 <- toString(Exclusion_criteria3)
      CommonTable$Exclusion_criteria <- new_function(Exclusion_criteria3,blank,Exclusion_criteria)
    }
    
    
    Recruitment <- data.frame(ID_with_title=CommonTable$ID_with_title,
                              UTN_code=CommonTable$UTN_code,
                              Study_status=CommonTable$Study_status,
                              Countries=CommonTable$Countries,
                              Date_of_first_enrollment=CommonTable$Date_of_first_enrollment,
                              Date_of_last_enrollment=CommonTable$Date_of_last_enrollment,
                              Target_sample_size=CommonTable$Target_sample_size,
                              Gender=CommonTable$Gender,
                              Min_age=CommonTable$Min_age,
                              Max_age=CommonTable$Max_age,
                              Inclusion_criteria=CommonTable$Inclusion_criteria,
                              Exclusion_criteria=CommonTable$Exclusion_criteria)
    
    
    dbWriteTable(mydb, "Recruitment", Recruitment , append = TRUE )
    write.table(Recruitment,"Recruitment.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Recruitment.csv"), append = T)
    
    
    
    #****************************************************Table 8: Study type***********************************************************************
    
    Study_design1 <- brazil_page %>% html_nodes("ul:nth-child(25) .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Study_design1 <- toString(Study_design1)
    CommonTable$Study_design <- new_function(Study_design1,blank,Study_design)
    
    Expanded_excess_program1 <- brazil_page %>% html_nodes("li:nth-child(3) tr > :nth-child(1)") %>% html_text() %>% str_remove_all("Expanded access program") %>% str_squish() %>% str_trim()
    Expanded_excess_program1 <- parse_number(Expanded_excess_program1)
    Expanded_excess_program1 <- Expanded_excess_program1 [!is.na(Expanded_excess_program1)]
    CommonTable$Expanded_excess_program <- new_function(Expanded_excess_program1,blank,Expanded_excess_program)
    
    Purpose1 <- brazil_page %>% html_nodes("thead+ tbody td:nth-child(2) , th:nth-child(2) .label") %>%   html_text() %>% str_remove_all("Purpose") %>% str_squish() %>% str_trim()
    Purpose1 <- parse_character(Purpose1)
    Purpose1 <- Purpose1 [!is.na(Purpose1)]
    CommonTable$Purpose <- new_function(Purpose1,blank,Purpose)
    
    Intervention_assignment1 <- brazil_page %>% html_nodes("li:nth-child(3) .dataTable :nth-child(3)") %>%   html_text() %>% str_remove_all("Intervention assignment") %>% str_squish() %>% str_trim()
    Intervention_assignment1 <- parse_character(Intervention_assignment1)
    Intervention_assignment1 <- Intervention_assignment1 [!is.na(Intervention_assignment1)]
    CommonTable$Intervention_assignment <- new_function(Intervention_assignment1,blank,Intervention_assignment)
    
    Number_of_arms1 <- brazil_page %>% html_nodes("th:nth-child(4) .label , thead+ tbody td:nth-child(4)") %>%   html_text() %>% str_remove_all("Number of arms") %>% str_squish() %>% str_trim()
    Number_of_arms1 <- parse_character(Number_of_arms1)
    Number_of_arms1 <- Number_of_arms1 [!is.na(Number_of_arms1)]
    CommonTable$Number_of_arms <- new_function(Number_of_arms1,blank,Number_of_arms)
    
    Masking_type1 <- brazil_page %>% html_nodes(".dataTable :nth-child(5) span") %>% html_text() %>% str_remove_all("Masking type") %>% str_squish() %>% str_trim()
    Masking_type1 <- parse_character(Masking_type1)
    Masking_type1 <- Masking_type1 [!is.na(Masking_type1)]
    CommonTable$Masking_type <- new_function(Masking_type1,blank,Masking_type)
    
    Allocation1 <- brazil_page %>% html_nodes("td:nth-child(6) .value , th:nth-child(6)") %>% html_text() %>% str_remove_all("Allocation") %>% str_squish() %>% str_trim()
    Allocation1 <- parse_character(Allocation1)
    Allocation1 <- Allocation1 [!is.na(Allocation1)]
    CommonTable$Allocation <- new_function(Allocation1,blank,Allocation)
    
    Study_phase1 <- brazil_page %>% html_nodes("td:nth-child(7) , th:nth-child(7) .label") %>% html_text() %>% str_remove_all("Study phase") %>% str_squish() %>% str_trim()
    Study_phase1 <- parse_character(Study_phase1)
    Study_phase1 <- Study_phase1 [!is.na(Study_phase1)]
    CommonTable$Study_phase <- new_function(Study_phase1,blank,Study_phase)
    
    
    Study_type = data.frame(ID_with_title=CommonTable$ID_with_title,
                            UTN_code=CommonTable$UTN_code,
                            Study_design=CommonTable$Study_design,
                            Expanded_excess_program=CommonTable$Expanded_excess_program,
                            Purpose=CommonTable$Purpose,
                            Intervention_assignment=CommonTable$Intervention_assignment,
                            Number_of_arms=CommonTable$Number_of_arms,
                            Masking_type=CommonTable$Masking_type,
                            Allocation=CommonTable$Allocation,
                            Study_phase=CommonTable$Study_phase)
    
    dbWriteTable(mydb, "Study_type", Study_type , append = TRUE )
    write.table(Study_type,"Study_type.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Study_type.csv"), append = T)
    
    
    #*************************************************Table 9: Outcomes****************************************************************************
    
    Primary_outcomes1 <- brazil_page %>% html_nodes("ul:nth-child(27) li:nth-child(1) .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Primary_outcomes1 <- paste(Primary_outcomes1, collapse = ".(end) ,")
    CommonTable$Primary_outcomes <- new_function(Primary_outcomes1,blank,Primary_outcomes)
    
    Secondary_outcomes1 <- brazil_page %>% html_nodes("ul~ ul li:nth-child(2) .balloon:nth-child(1)") %>% html_text() %>% str_remove_all("en") %>% str_squish() %>% str_trim()
    Secondary_outcomes1 <- paste(Secondary_outcomes1, collapse = ".(end) ,")
    CommonTable$Secondary_outcomes <- new_function(Secondary_outcomes1,blank,Secondary_outcomes)
    
    
    Outcomes <- data.frame(ID_with_title=CommonTable$ID_with_title,
                           UTN_code=CommonTable$UTN_code,
                           Primary_outcomes=CommonTable$Primary_outcomes,
                           Secondary_outcomes=CommonTable$Secondary_outcomes)
    
    dbWriteTable(mydb, "Outcomes", Outcomes , append = TRUE )
    write.table(Outcomes,"Outcomes.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Outcomes.csv"), append = T)
    
    
    #************************************************Table 10: Contact Details*********************************************************************
    
    Public_contact1 <- brazil_page %>% html_nodes(".subset:nth-child(1)") %>% html_text() %>% str_remove_all("Public contact") %>% str_replace_all("Full name","  ;  Full name") %>%str_replace("  ;  Full name","Full name")  %>% str_replace_all("Address",",Address") %>% str_replace_all("City",",City") %>% str_replace_all("Zip code",",Zip code") %>% str_replace_all("Phone",",Phone") %>% str_replace_all("Email",",Email") %>% str_replace_all("Affiliation",",Affiliation") %>% str_squish() %>% str_trim()
    CommonTable$Public_contact <- new_function(Public_contact1,blank,Public_contact)
    
    Scientific_contact1 <- brazil_page %>% html_nodes(".subset+ .subset:nth-child(2)") %>% html_text() %>% str_remove_all("Scientific contact") %>% str_replace_all("Full name","  ;  Full name") %>%str_replace("  ;  Full name","Full name")  %>% str_replace_all("Address",",Address") %>% str_replace_all("City",",City") %>% str_replace_all("Zip code",",Zip code") %>% str_replace_all("Phone",",Phone") %>% str_replace_all("Email",",Email") %>% str_replace_all("Affiliation",",Affiliation") %>% str_squish() %>% str_trim()
    CommonTable$Scientific_contact <- new_function(Scientific_contact1,blank,Scientific_contact)
    
    Site_contact1 <- brazil_page %>% html_nodes(".subset~ .subset+ .subset") %>% html_text() %>% str_remove_all("Site contact") %>% str_replace_all("Full name","  ;  Full name") %>%str_replace("  ;  Full name","Full name")  %>% str_replace_all("Address",",Address") %>% str_replace_all("City",",City") %>% str_replace_all("Zip code",",Zip code") %>% str_replace_all("Phone",",Phone") %>% str_replace_all("Email",",Email") %>% str_replace_all("Affiliation",",Affiliation") %>% str_squish() %>% str_trim()
    CommonTable$Site_contact <- new_function(Site_contact1,blank,Site_contact)
    
    Contact_details <- data.frame(ID_with_title=CommonTable$ID_with_title,
                                  UTN_code=CommonTable$UTN_code,
                                  Public_contact=CommonTable$Public_contact,
                                  Scientific_contact=CommonTable$Scientific_contact,
                                  Site_contact=CommonTable$Site_contact)
    
    dbWriteTable(mydb, "Contact_details", Contact_details , append = TRUE )
    write.table(Contact_details,"Contact_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Contact_details.csv"), append = T)
    
    
    
    
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
  }
}

dbDisconnect(mydb)




