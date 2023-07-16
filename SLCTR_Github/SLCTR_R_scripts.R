#*******************************************************************Script - 1************************************************************************************************
#Downloaded all the records from the SLCTR registry

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
counter=0
ids <- c(1:417)
for (page_result in seq(from =1,to=42)) {
  srilanka_page_links <- data.frame()
  link <- url(paste0("https://slctr.lk/trials?page=",page_result))
  print(link)
  page <- read_html(link)
  if (page_result < 42) {
    for (i in seq(from=1,to=10)) {
      link_of_all_pages <-  page %>% html_nodes(xpath=paste0("/html/body/div[2]/table/tbody/tr[",i,"]/td[1]/strong/a")) %>% html_attr("href")
      link_of_all_pages <- paste0("https://slctr.lk",link_of_all_pages)
      srilanka_page_links <- rbind(srilanka_page_links, link_of_all_pages)
    }
  } else {
    for (i in seq(from=1,to=7)) {
      link_of_all_pages <-  page %>% html_nodes(xpath=paste0("/html/body/div[2]/table/tbody/tr[",i,"]/td[1]/strong/a")) %>% html_attr("href")
      link_of_all_pages <- paste0("https://slctr.lk",link_of_all_pages)
      srilanka_page_links <- rbind(srilanka_page_links, link_of_all_pages)
    }
  }
  
  write.table(srilanka_page_links, "srilanka_page_links.csv", sep = ",",row.names = FALSE, col.names = !file.exists("srilanka_page_links.csv"), append = T)
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",page_result))
} 

srilanka_page_links <- data.frame()
srilanka_page_links <- read.csv("srilanka_page_links.csv")
for (i in seq_along(ids)) {
  official_url = srilanka_page_links[ids[i],"links_of_all_pages"] 
  print(official_url)
  output_file=paste0("srilanka_page",ids[i],".html")
  download.file(official_url, destfile = output_file, quiet = TRUE)
  time_of_download = as.character(timestamp())
  time_stamp = data.frame(Trial_ID = as.character(ids[i]),
                          downloaded_time = time_of_download,
                          URL = as.character(official_url))
  write.table(time_stamp, "time_stamp_srilanka.csv", sep = ",",row.names = FALSE, col.names = !file.exists("time_stamp_srilanka.csv"), append = T)
  counter = counter + 1
  print(paste("Count = ", counter,"ID = ",ids[i]))
  
}



#*******************************************************************Script - 2*****************************************************************************************************
#Web-scraped all the fields from the downloaded records to look for the keyword 'India' or 'CTRI'

libraries = c( "XML", "tidyft","data.table", "DBI", "httr", "RSQLite","tidyverse","rvest","stringr","robotstxt","selectr","xml2","dplyr","forcats","magrittr","tidyr","ggplot2","lubridate","tibble","purrr","googleLanguageR","cld2")
lapply(libraries, require, character.only = TRUE)
ids = c(1:417)
counter = 0
for (i in seq_along(ids)) { 
  
  myfile = paste0("srilanka_page",ids[i],".html")
  
  if (file.exists(myfile)) {
    
    srilanka_page <- read_html(myfile)
    
    if(is_empty(srilanka_page)) {
      
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
   
   
    labels <- srilanka_page %>% html_nodes(".light") %>% html_text()
    length <- length(labels)
    
##***********************************************Table1: General information******************************************************************

    SLCTR_registration_number <- srilanka_page %>% html_nodes(".row:nth-child(1) .span6 p") %>% html_text() %>% str_squish() %>% str_trim()
    SLCTR_registration_number <- new_function(SLCTR_registration_number)
    Date_of_registration <- srilanka_page %>% html_nodes("#preview .row:nth-child(3) .span6") %>% html_text() %>% str_squish() %>% str_trim()
    Date_of_registration <- new_function(Date_of_registration)
    Date_of_last_modification <- srilanka_page %>% html_nodes(".row:nth-child(5) .span6 p") %>% html_text() %>% str_squish() %>% str_trim()
    Date_of_last_modification <- new_function(Date_of_last_modification)
    Original_TRDS <- srilanka_page %>% html_nodes(".span6 a") %>% html_attr("href")
    if (length(Original_TRDS) == 1) {
      Original_TRDS <- paste0("https://slctr.lk",Original_TRDS)
    } else {
      Original_TRDS <- toString(Original_TRDS)
    }
    Original_TRDS <- new_function(Original_TRDS)
    Trial_status <- srilanka_page %>% html_nodes("img") 
    length_trial_status=length(Trial_status)
    if (length_trial_status == 1) {
      Trial_status = "NA"
    } else if (length_trial_status ==2 ) {
      Trial_status <- srilanka_page %>% html_nodes("img") %>% .[2] %>% html_attr("title")
    } else if (length_trial_status ==3 ){
      
      Trial_status1 <- srilanka_page %>% html_nodes("img") %>% .[2] %>% html_attr("title")
      Trial_status2 <- srilanka_page %>% html_nodes("img") %>% .[3] %>% html_attr("title")
      Trial_status <- paste(Trial_status1,Trial_status2,sep=",")
      
    } else {
      
      Trial_status1 <- srilanka_page %>% html_nodes("img") %>% .[2] %>% html_attr("title")
      Trial_status2 <- srilanka_page %>% html_nodes("img") %>% .[3] %>% html_attr("title")
      Trial_status3 <- srilanka_page %>% html_nodes("img") %>% .[4] %>% html_attr("title")
      
      Trial_status <- paste(Trial_status1,Trial_status2,Trial_status3,sep=",")
    }
    
    
    Trial_status <- new_function(Trial_status)
    
    General_information <- data.frame(SLCTR_registration_number,Date_of_registration,Date_of_last_modification,Original_TRDS, length_trial_status,Trial_status)
    write.table(General_information,"General_information.csv", sep = ",",row.names = FALSE, col.names = !file.exists("General_information.csv"), append = T)
    
    
    if (length == 58) {
    
##*************************************************Table2: Application summary*****************************************************************************
    
    Scientific_title <- srilanka_page %>% html_nodes(".row:nth-child(10)") %>% html_text() %>% str_remove_all("Scientific Title of Trial") %>% str_squish() %>% str_trim()
    Scientific_title <- new_function(Scientific_title)
    Public_title <- srilanka_page %>% html_nodes(".row:nth-child(12)") %>% html_text() %>% str_remove_all("Public Title of Trial") %>% str_squish() %>% str_trim()
    Public_title <- new_function(Public_title)
    Disease_studied <- srilanka_page %>% html_nodes(".row:nth-child(14)") %>% html_text() %>% str_remove_all("Disease or Health Condition\\(s\\) Studied") %>% str_squish() %>% str_trim()
    Disease_studied <- new_function(Disease_studied)
    Scientific_acronym <- srilanka_page %>% html_nodes(".row:nth-child(16)") %>% html_text() %>% str_remove_all("Scientific Acronym") %>% str_squish() %>% str_trim()
    Scientific_acronym <- new_function(Scientific_acronym)
    Public_acronym <- srilanka_page %>% html_nodes(".row:nth-child(18)") %>% html_text() %>% str_remove_all("Public Acronym") %>% str_squish() %>% str_trim()
    Public_acronym <- new_function(Public_acronym)
    Brief_title <- srilanka_page %>% html_nodes(".row:nth-child(20)") %>% html_text() %>% str_remove_all("Brief title") %>% str_squish() %>% str_trim()
    Brief_title <- new_function(Brief_title)
    UTN_number <- srilanka_page %>% html_nodes(".row:nth-child(22)") %>% html_text() %>% str_remove_all("Universal Trial Number") %>% str_squish() %>% str_trim()
    UTN_number <- new_function(UTN_number)
    Any_numbers_assigned_by_authority <- srilanka_page %>% html_nodes(".row:nth-child(24)") %>% html_text() %>% str_remove_all("Any other number\\(s\\) assigned to the trial and issuing authority") %>% str_squish() %>% str_trim()
    Any_numbers_assigned_by_authority <- new_function(Any_numbers_assigned_by_authority)
    
    
    Application_summary <- data.frame(SLCTR_registration_number,Scientific_title,Public_title,Disease_studied,Scientific_acronym,Public_acronym,Brief_title,UTN_number,Any_numbers_assigned_by_authority)
    write.table(Application_summary,"Application_summary.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Application_summary.csv"), append = T)
    
    
    
##**********************************************************Table3 : Trial Details*******************************************************************
     
   Research_question_addressed <- srilanka_page %>% html_nodes(".row:nth-child(28)") %>% html_text() %>% str_remove_all("What is the research question being addressed\\?") %>% str_squish() %>% str_trim()
   Research_question_addressed <- new_function(Research_question_addressed)
   Type_of_study <- srilanka_page %>% html_nodes(".row:nth-child(30)") %>% html_text() %>% str_remove_all("Type of study") %>% str_squish() %>% str_trim()
   Type_of_study <- new_function(Type_of_study)
   Allocation  <- srilanka_page %>% html_nodes(".light+ .row .span3+ .span6") %>% html_text() %>% str_squish() %>% str_trim()
   Allocation <- new_function(Allocation)
   Masking  <- srilanka_page %>% html_nodes(".row:nth-child(35)") %>% html_text() %>% str_remove_all("Masking") %>% str_squish() %>% str_trim()
   Masking <- new_function(Masking)
   Control  <- srilanka_page %>% html_nodes(".row:nth-child(37)") %>% html_text() %>% str_remove_all("Control") %>% str_squish() %>% str_trim()
   Control <- new_function(Control)
   Assignment  <- srilanka_page %>% html_nodes(".row:nth-child(39)") %>% html_text() %>% str_remove_all("Assignment") %>% str_squish() %>% str_trim()
   Assignment <- new_function(Assignment)
   Purpose  <- srilanka_page %>% html_nodes(".row:nth-child(41)") %>% html_text() %>% str_remove_all("Purpose") %>% str_squish() %>% str_trim()
   Purpose <- new_function(Purpose)
   Study_phase  <- srilanka_page %>% html_nodes(".row:nth-child(43)") %>% html_text() %>% str_remove_all("Study Phase") %>% str_squish() %>% str_trim()
   Study_phase <- new_function(Study_phase)
   Interventions_planned  <- srilanka_page %>% html_nodes(".row:nth-child(45) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
   Interventions_planned <- new_function(Interventions_planned)
   Inclusion_criteria <- srilanka_page %>% html_nodes(".row:nth-child(47) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
   Inclusion_criteria <- new_function(Inclusion_criteria)
   Exclusion_criteria <- srilanka_page %>% html_nodes(".row:nth-child(49) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
   Exclusion_criteria <- new_function(Exclusion_criteria)
   Primary_outcome <- srilanka_page %>% html_nodes(".row:nth-child(52)") %>% html_text() %>% str_remove_all("Primary outcome\\(s\\)") %>% str_squish() %>% str_trim()
   Primary_outcome <- new_function(Primary_outcome)
   Secondary_outcome <- srilanka_page %>% html_nodes(".row:nth-child(54)") %>% html_text() %>% str_remove_all("Secondary outcome\\(s\\)") %>% str_squish() %>% str_trim()
   Secondary_outcome <- new_function(Secondary_outcome)
   
   Trial_details <- data.frame(SLCTR_registration_number,Research_question_addressed,Type_of_study,Allocation,Masking,Control,Assignment,Purpose,Study_phase,Interventions_planned,Inclusion_criteria,Exclusion_criteria,Primary_outcome,Secondary_outcome)
   write.table(Trial_details,"Trial_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_details.csv"), append = T)
   
   
##*********************************************************Table 4: Recruitment-table********************************************************************
   Target_number <- srilanka_page %>% html_nodes(".row:nth-child(56)")  %>% html_text() %>% str_remove_all("Target number/sample size") %>% str_squish() %>% str_trim()
   Target_number <- new_function(Target_number)
   Countries_of_recruitment <- srilanka_page %>% html_nodes(".row:nth-child(58)")  %>% html_text() %>% str_remove_all("Countries of recruitment") %>% str_squish() %>% str_trim()
   Countries_of_recruitment <- new_function(Countries_of_recruitment)
   Anticipated_start_date <- srilanka_page %>% html_nodes(".row:nth-child(60)")  %>% html_text() %>% str_remove_all("Anticipated start date") %>% str_squish() %>% str_trim()
   Anticipated_start_date <- new_function(Anticipated_start_date)
   Anticipated_end_date <- srilanka_page %>% html_nodes(".row:nth-child(62)")  %>% html_text() %>% str_remove_all("Anticipated end date") %>% str_squish() %>% str_trim()
   Anticipated_end_date <- new_function(Anticipated_end_date)
   Date_of_first_enrollment <- srilanka_page %>% html_nodes(".row:nth-child(64)")  %>% html_text() %>% str_remove_all("Date of first enrollment") %>% str_squish() %>% str_trim()
   Date_of_first_enrollment <- new_function(Date_of_first_enrollment)
   Date_of_study_completion <- srilanka_page %>% html_nodes(".row:nth-child(66)")  %>% html_text() %>% str_remove_all("Date of study completion") %>%  str_squish() %>% str_trim()
   Date_of_study_completion <- new_function(Date_of_study_completion)
   Recruitment_status <- srilanka_page %>% html_nodes(".row:nth-child(68)")  %>% html_text() %>% str_remove_all("Recruitment status") %>% str_squish() %>% str_trim()
   Recruitment_status <- new_function(Recruitment_status)
   Funding_source <- srilanka_page %>% html_nodes(".row:nth-child(70)")  %>% html_text() %>% str_remove_all("Funding source") %>% str_squish() %>% str_trim()
   Funding_source <- new_function(Funding_source)
   Regulatory_approvals <- srilanka_page %>% html_nodes(".row:nth-child(72)")  %>% html_text() %>% str_remove_all("Regulatory approvals") %>% str_squish() %>% str_trim()
   Regulatory_approvals <- new_function(Regulatory_approvals)
    
    
   Recruitment_details <- data.frame(SLCTR_registration_number,Target_number,Countries_of_recruitment,Anticipated_start_date,Anticipated_end_date,Date_of_first_enrollment,Date_of_study_completion,Recruitment_status,Funding_source,Regulatory_approvals)
   write.table(Recruitment_details,"Recruitment_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Recruitment_details.csv"), append = T)
   
   
   
##**************************************************Table5: Ethics Review table*********************************************************************

   
   Status <- srilanka_page %>% html_nodes(".row:nth-child(77)")  %>% html_text() %>% str_remove_all("Status") %>% str_squish() %>% str_trim()
   Status <- new_function(Status)
   Date_of_approval <- srilanka_page %>% html_nodes(".row:nth-child(79)")  %>% html_text() %>% str_remove_all("Date of Approval") %>% str_squish() %>% str_trim()
   Date_of_approval <- new_function(Date_of_approval)
   Approval_number <- srilanka_page %>% html_nodes(".row:nth-child(81)")  %>% html_text() %>% str_remove_all("Approval number") %>% str_squish() %>% str_trim()
   Approval_number <- new_function(Approval_number)
   Name <- srilanka_page %>% html_nodes(".span7 tr:nth-child(1) td+ td")  %>% html_text()  %>% str_squish() %>% str_trim()
   Name <- new_function(Name)
   Institutional_address <- srilanka_page %>% html_nodes(".span7 tr:nth-child(2) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
   Institutional_address <- new_function(Institutional_address)
   Telephone <- srilanka_page %>% html_nodes(".span7 tr:nth-child(3) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
   Telephone <- new_function(Telephone)
   Email <- srilanka_page %>% html_nodes(".span7 tr:nth-child(4) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
   Email <- new_function(Email)
   
   
   Ethics_review <- data.frame(SLCTR_registration_number,Status,Date_of_approval,Approval_number,Name,Institutional_address,Telephone,Email)
   write.table(Ethics_review,"Ethics_review.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Ethics_review.csv"), append = T)
   
   
   #********************************************Table 6:Contact and Sponsor Information*************************************************************
   
     Principal_investigator_details <- srilanka_page %>% html_nodes(".row:nth-child(88) .span5")  %>% html_text() %>% str_remove_all("Contact person for Scientific Queries/Principal Investigator") %>% str_squish() %>% str_trim()
     Principal_investigator_details <- new_function(Principal_investigator_details)
     Contact_person_for_public_queries <- srilanka_page %>% html_nodes(".row:nth-child(88) .span4")  %>% html_text() %>% str_remove_all("Contact Person for Public Queries") %>% str_squish() %>% str_trim()
     Contact_person_for_public_queries <- new_function(Contact_person_for_public_queries)
     Primary_study_sponsor <- srilanka_page %>% html_nodes(".bdot+ .row .span5")  %>% html_text() %>% str_remove_all("Primary study sponsor/organization") %>% str_squish() %>% str_trim()
     Primary_study_sponsor <- new_function(Primary_study_sponsor)
     Secondary_study_sponsor <- srilanka_page %>% html_nodes(".bdot+ .row .span4")  %>% html_text() %>% str_remove_all("Secondary study sponsor \\(If any\\)") %>% str_squish() %>% str_trim()
     Secondary_study_sponsor <- new_function(Secondary_study_sponsor)
     
     Contact_and_sponsor_information <- data.frame(SLCTR_registration_number,Principal_investigator_details,Contact_person_for_public_queries,Primary_study_sponsor,Secondary_study_sponsor)
     write.table(Contact_and_sponsor_information,"Contact_and_sponsor_information.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Contact_and_sponsor_information.csv"), append = T)
     
     
     
#*******************************************Table7: Trial completion details*************************************************************************
   
    Plan_to_share_details <- srilanka_page %>% html_nodes(".row:nth-child(94)")  %>% html_text() %>% str_squish() %>% str_trim()
    Plan_to_share_details <- new_function(Plan_to_share_details)
    IPD_sharing_plan_description <- srilanka_page %>% html_nodes(".row:nth-child(96)")  %>% html_text() %>% str_remove_all("IPD sharing plan description") %>% str_squish() %>% str_trim()
    IPD_sharing_plan_description <- new_function(IPD_sharing_plan_description)
    Study_protocol_available <- srilanka_page %>% html_nodes(".row:nth-child(98)")  %>% html_text() %>% str_remove_all("Study protocol available") %>% str_squish() %>% str_trim()
    Study_protocol_available <- new_function(Study_protocol_available)
    Protocol_version_and_date <- srilanka_page %>% html_nodes(".row:nth-child(100)")  %>% html_text() %>% str_remove_all("Protocol version and date") %>% str_squish() %>% str_trim()
    Protocol_version_and_date <- new_function(Protocol_version_and_date)
    Protocol_URL <- srilanka_page %>% html_nodes(".row:nth-child(102)")  %>% html_text() %>% str_remove_all("Protocol URL") %>% str_squish() %>% str_trim()
    Protocol_URL <- new_function(Protocol_URL)
    Results_summary_available <- srilanka_page %>% html_nodes(".row:nth-child(104)")  %>% html_text() %>% str_remove_all("Results summary available") %>% str_squish() %>% str_trim()
    Results_summary_available <- new_function(Results_summary_available)
    Date_of_posting_results <- srilanka_page %>% html_nodes(".row:nth-child(106)")  %>% html_text() %>% str_remove_all("Date of posting results") %>% str_squish() %>% str_trim()
    Date_of_posting_results <- new_function(Date_of_posting_results)
    Date_of_study_completion <- srilanka_page %>% html_nodes(".row:nth-child(108)")  %>% html_text() %>% str_remove_all("Date of study completion") %>% str_squish() %>% str_trim()
    Date_of_study_completion <- new_function(Date_of_study_completion)
    Final_sample_size <- srilanka_page %>% html_nodes(".row:nth-child(110)")  %>% html_text() %>% str_remove_all("Final sample size") %>% str_squish() %>% str_trim()
    Final_sample_size <- new_function(Final_sample_size)
    Date_of_first_publication <- srilanka_page %>% html_nodes(".row:nth-child(112)")  %>% html_text() %>% str_remove_all("Date of first publication") %>% str_squish() %>% str_trim()
    Date_of_first_publication <- new_function(Date_of_first_publication)
    Link_to_results <- srilanka_page %>% html_nodes(".row:nth-child(114)")  %>% html_text() %>% str_remove_all("Link to results") %>% str_squish() %>% str_trim()
    Link_to_results <- new_function(Link_to_results)
    Brief_summary_of_results <- srilanka_page %>% html_nodes(".light:nth-child(116)")  %>% html_text() %>% str_remove_all("Brief summary of results") %>% str_squish() %>% str_trim()
    Brief_summary_of_results <- new_function(Brief_summary_of_results)
    
    
    
    Trial_completion_details <- data.frame(SLCTR_registration_number,Plan_to_share_details,IPD_sharing_plan_description,Study_protocol_available,Protocol_version_and_date,Protocol_URL,Results_summary_available,Date_of_posting_results,Date_of_study_completion,Final_sample_size,Date_of_first_publication,Link_to_results,Brief_summary_of_results)
    write.table(Trial_completion_details,"Trial_completion_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_completion_details.csv"), append = T)
    
    
    
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",ids[i]))
    
    } 
    
    
    
##When no. of labels is 59
    
    if (length==59) {

#***************************************************Table 2: Application-summary*************************************************************   
      
      Scientific_title <- srilanka_page %>% html_nodes(".row:nth-child(12)") %>% html_text() %>% str_remove_all("Scientific Title of Trial") %>% str_squish() %>% str_trim()
      Scientific_title <- new_function(Scientific_title)
      Public_title <- srilanka_page %>% html_nodes(".row:nth-child(14)") %>% html_text() %>% str_remove_all("Public Title of Trial") %>% str_squish() %>% str_trim()
      Public_title <- new_function(Public_title)
      Disease_studied <- srilanka_page %>% html_nodes(".row:nth-child(16)") %>% html_text() %>% str_remove_all("Disease or Health Condition\\(s\\) Studied") %>% str_squish() %>% str_trim()
      Disease_studied <- new_function(Disease_studied)
      Scientific_acronym <- srilanka_page %>% html_nodes(".row:nth-child(18)") %>% html_text() %>% str_remove_all("Scientific Acronym") %>% str_squish() %>% str_trim()
      Scientific_acronym <- new_function(Scientific_acronym)
      Public_acronym <- srilanka_page %>% html_nodes(".row:nth-child(20)") %>% html_text() %>% str_remove_all("Public Acronym") %>% str_squish() %>% str_trim()
      Public_acronym <- new_function(Public_acronym)
      Brief_title <- srilanka_page %>% html_nodes(".row:nth-child(22)") %>% html_text() %>% str_remove_all("Brief title") %>% str_squish() %>% str_trim()
      Brief_title <- new_function(Brief_title)
      UTN_number <- srilanka_page %>% html_nodes(".row:nth-child(24)") %>% html_text() %>% str_remove_all("Universal Trial Number") %>% str_squish() %>% str_trim()
      UTN_number <- new_function(UTN_number)
      Any_numbers_assigned_by_authority <- srilanka_page %>% html_nodes(".row:nth-child(26)") %>% html_text() %>% str_remove_all("Any other number\\(s\\) assigned to the trial and issuing authority") %>% str_squish() %>% str_trim()
      Any_numbers_assigned_by_authority <- new_function(Any_numbers_assigned_by_authority)
      
      
      Application_summary <- data.frame(SLCTR_registration_number,Scientific_title,Public_title,Disease_studied,Scientific_acronym,Public_acronym,Brief_title,UTN_number,Any_numbers_assigned_by_authority)
      write.table(Application_summary,"Application_summary.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Application_summary.csv"), append = T)
      
      
      
#*****************************************************Table3: Trial Details***********************************************************************

      Research_question_addressed <- srilanka_page %>% html_nodes(".row:nth-child(30)") %>% html_text() %>% str_remove_all("What is the research question being addressed\\?") %>% str_squish() %>% str_trim()
      Research_question_addressed <- new_function(Research_question_addressed)
      Type_of_study <- srilanka_page %>% html_nodes(".row:nth-child(32)") %>% html_text() %>% str_remove_all("Type of study") %>% str_squish() %>% str_trim()
      Type_of_study <- new_function(Type_of_study)
      Allocation  <- srilanka_page %>% html_nodes(".light+ .row .span3+ .span6") %>% html_text() %>% str_squish() %>% str_trim()
      Allocation <- new_function(Allocation)
      Masking  <- srilanka_page %>% html_nodes(".row:nth-child(37)") %>% html_text() %>% str_remove_all("Masking") %>% str_squish() %>% str_trim()
      Masking <- new_function(Masking)
      Control  <- srilanka_page %>% html_nodes(".row:nth-child(39)") %>% html_text() %>% str_remove_all("Control") %>% str_squish() %>% str_trim()
      Control <- new_function(Control)
      Assignment  <- srilanka_page %>% html_nodes(".row:nth-child(41)") %>% html_text() %>% str_remove_all("Assignment") %>% str_squish() %>% str_trim()
      Assignment <- new_function(Assignment)
      Purpose  <- srilanka_page %>% html_nodes(".row:nth-child(43)") %>% html_text() %>% str_remove_all("Purpose") %>% str_squish() %>% str_trim()
      Purpose <- new_function(Purpose)
      Study_phase  <- srilanka_page %>% html_nodes(".row:nth-child(45)") %>% html_text() %>% str_remove_all("Study Phase") %>% str_squish() %>% str_trim()
      Study_phase <- new_function(Study_phase)
      Interventions_planned  <- srilanka_page %>% html_nodes(".row:nth-child(47) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
      Interventions_planned <- new_function(Interventions_planned)
      Inclusion_criteria <- srilanka_page %>% html_nodes(".row:nth-child(49) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
      Inclusion_criteria <- new_function(Inclusion_criteria)
      Exclusion_criteria <- srilanka_page %>% html_nodes(".row:nth-child(51) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
      Exclusion_criteria <- new_function(Exclusion_criteria)
      Primary_outcome <- srilanka_page %>% html_nodes(".row:nth-child(54)") %>% html_text() %>% str_remove_all("Primary outcome\\(s\\)") %>% str_squish() %>% str_trim()
      Primary_outcome <- new_function(Primary_outcome)
      Secondary_outcome <- srilanka_page %>% html_nodes(".row:nth-child(56)") %>% html_text() %>% str_remove_all("Secondary outcome\\(s\\)") %>% str_squish() %>% str_trim()
      Secondary_outcome <- new_function(Secondary_outcome)
      
      Trial_details <- data.frame(SLCTR_registration_number,Research_question_addressed,Type_of_study,Allocation,Masking,Control,Assignment,Purpose,Study_phase,Interventions_planned,Inclusion_criteria,Exclusion_criteria,Primary_outcome,Secondary_outcome)
      write.table(Trial_details,"Trial_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_details.csv"), append = T)
      
      
      
#***********************************************************Table4: Recruitment-table***************************************************************
      
      Target_number <- srilanka_page %>% html_nodes(".row:nth-child(58)")  %>% html_text() %>% str_remove_all("Target number/sample size") %>% str_squish() %>% str_trim()
      Target_number <- new_function(Target_number)
      Countries_of_recruitment <- srilanka_page %>% html_nodes(".row:nth-child(60)")  %>% html_text() %>% str_remove_all("Countries of recruitment") %>% str_squish() %>% str_trim()
      Countries_of_recruitment <- new_function(Countries_of_recruitment)
      Anticipated_start_date <- srilanka_page %>% html_nodes(".row:nth-child(62)")  %>% html_text() %>% str_remove_all("Anticipated start date") %>% str_squish() %>% str_trim()
      Anticipated_start_date <- new_function(Anticipated_start_date)
      Anticipated_end_date <- srilanka_page %>% html_nodes(".row:nth-child(64)")  %>% html_text() %>% str_remove_all("Anticipated end date") %>% str_squish() %>% str_trim()
      Anticipated_end_date <- new_function(Anticipated_end_date)
      Date_of_first_enrollment <- srilanka_page %>% html_nodes(".row:nth-child(66)")  %>% html_text() %>% str_remove_all("Date of first enrollment") %>% str_squish() %>% str_trim()
      Date_of_first_enrollment <- new_function(Date_of_first_enrollment)
      Date_of_study_completion <- srilanka_page %>% html_nodes(".row:nth-child(68)")  %>% html_text() %>% str_remove_all("Date of study completion") %>%  str_squish() %>% str_trim()
      Date_of_study_completion <- new_function(Date_of_study_completion)
      Recruitment_status <- srilanka_page %>% html_nodes(".row:nth-child(70)")  %>% html_text() %>% str_remove_all("Recruitment status") %>% str_squish() %>% str_trim()
      Recruitment_status <- new_function(Recruitment_status)
      Funding_source <- srilanka_page %>% html_nodes(".row:nth-child(72)")  %>% html_text() %>% str_remove_all("Funding source") %>% str_squish() %>% str_trim()
      Funding_source <- new_function(Funding_source)
      Regulatory_approvals <- srilanka_page %>% html_nodes(".row:nth-child(74)")  %>% html_text() %>% str_remove_all("Regulatory approvals") %>% str_squish() %>% str_trim()
      Regulatory_approvals <- new_function(Regulatory_approvals)
      
      
      Recruitment_details <- data.frame(SLCTR_registration_number,Target_number,Countries_of_recruitment,Anticipated_start_date,Anticipated_end_date,Date_of_first_enrollment,Date_of_study_completion,Recruitment_status,Funding_source,Regulatory_approvals)
      write.table(Recruitment_details,"Recruitment_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Recruitment_details.csv"), append = T)
      
      
      
      
#********************************************************Table 5: State of Ethics Review Approval*************************************************
      
      Status <- srilanka_page %>% html_nodes(".row:nth-child(79)")  %>% html_text() %>% str_remove_all("Status") %>% str_squish() %>% str_trim()
      Status <- new_function(Status)
      Date_of_approval <- srilanka_page %>% html_nodes(".row:nth-child(81)")  %>% html_text() %>% str_remove_all("Date of Approval") %>% str_squish() %>% str_trim()
      Date_of_approval <- new_function(Date_of_approval)
      Approval_number <- srilanka_page %>% html_nodes(".row:nth-child(83)")  %>% html_text() %>% str_remove_all("Approval number") %>% str_squish() %>% str_trim()
      Approval_number <- new_function(Approval_number)
      Name <- srilanka_page %>% html_nodes(".span7 tr:nth-child(1) td+ td")  %>% html_text()  %>% str_squish() %>% str_trim()
      Name <- new_function(Name)
      Institutional_address <- srilanka_page %>% html_nodes(".span7 tr:nth-child(2) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
      Institutional_address <- new_function(Institutional_address)
      Telephone <- srilanka_page %>% html_nodes(".span7 tr:nth-child(3) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
      Telephone <- new_function(Telephone)
      Email <- srilanka_page %>% html_nodes(".span7 tr:nth-child(4) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
      Email <- new_function(Email)
      
      
      Ethics_review <- data.frame(SLCTR_registration_number,Status,Date_of_approval,Approval_number,Name,Institutional_address,Telephone,Email)
      write.table(Ethics_review,"Ethics_review.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Ethics_review.csv"), append = T)
      
      
#****************************************************Table 6: Contact and sponsor information*******************************************************

      Principal_investigator_details <- srilanka_page %>% html_nodes(".row:nth-child(90) .span5")  %>% html_text() %>% str_remove_all("Contact person for Scientific Queries/Principal Investigator") %>% str_squish() %>% str_trim()
      Principal_investigator_details <- new_function(Principal_investigator_details)
      Contact_person_for_public_queries <- srilanka_page %>% html_nodes(".row:nth-child(90) .span4")  %>% html_text() %>% str_remove_all("Contact Person for Public Queries") %>% str_squish() %>% str_trim()
      Contact_person_for_public_queries <- new_function(Contact_person_for_public_queries)
      Primary_study_sponsor <- srilanka_page %>% html_nodes(".bdot+ .row .span5")  %>% html_text() %>% str_remove_all("Primary study sponsor/organization") %>% str_squish() %>% str_trim()
      Primary_study_sponsor <- new_function(Primary_study_sponsor)
      Secondary_study_sponsor <- srilanka_page %>% html_nodes(".bdot+ .row .span4")  %>% html_text() %>% str_remove_all("Secondary study sponsor \\(If any\\)") %>% str_squish() %>% str_trim()
      Secondary_study_sponsor <- new_function(Secondary_study_sponsor)
      
      Contact_and_sponsor_information <- data.frame(SLCTR_registration_number,Principal_investigator_details,Contact_person_for_public_queries,Primary_study_sponsor,Secondary_study_sponsor)
      write.table(Contact_and_sponsor_information,"Contact_and_sponsor_information.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Contact_and_sponsor_information.csv"), append = T)
      
      
      

#********************************************************Table7: Trial Completion Details******************************************************************      
      
      Plan_to_share_details <- srilanka_page %>% html_nodes(".row:nth-child(96)")  %>% html_text() %>% str_squish() %>% str_trim()
      Plan_to_share_details <- new_function(Plan_to_share_details)
      IPD_sharing_plan_description <- srilanka_page %>% html_nodes(".row:nth-child(98)")  %>% html_text() %>% str_remove_all("IPD sharing plan description") %>% str_squish() %>% str_trim()
      IPD_sharing_plan_description <- new_function(IPD_sharing_plan_description)
      Study_protocol_available <- srilanka_page %>% html_nodes(".row:nth-child(100)")  %>% html_text() %>% str_remove_all("Study protocol available") %>% str_squish() %>% str_trim()
      Study_protocol_available <- new_function(Study_protocol_available)
      Protocol_version_and_date <- srilanka_page %>% html_nodes(".row:nth-child(102)")  %>% html_text() %>% str_remove_all("Protocol version and date") %>% str_squish() %>% str_trim()
      Protocol_version_and_date <- new_function(Protocol_version_and_date)
      Protocol_URL <- srilanka_page %>% html_nodes(".row:nth-child(104)")  %>% html_text() %>% str_remove_all("Protocol URL") %>% str_squish() %>% str_trim()
      Protocol_URL <- new_function(Protocol_URL)
      Results_summary_available <- srilanka_page %>% html_nodes(".row:nth-child(106)")  %>% html_text() %>% str_remove_all("Results summary available") %>% str_squish() %>% str_trim()
      Results_summary_available <- new_function(Results_summary_available)
      Date_of_posting_results <- srilanka_page %>% html_nodes(".row:nth-child(108)")  %>% html_text() %>% str_remove_all("Date of posting results") %>% str_squish() %>% str_trim()
      Date_of_posting_results <- new_function(Date_of_posting_results)
      Date_of_study_completion <- srilanka_page %>% html_nodes(".row:nth-child(110)")  %>% html_text() %>% str_remove_all("Date of study completion") %>% str_squish() %>% str_trim()
      Date_of_study_completion <- new_function(Date_of_study_completion)
      Final_sample_size <- srilanka_page %>% html_nodes(".row:nth-child(112)")  %>% html_text() %>% str_remove_all("Final sample size") %>% str_squish() %>% str_trim()
      Final_sample_size <- new_function(Final_sample_size)
      Date_of_first_publication <- srilanka_page %>% html_nodes(".row:nth-child(114)")  %>% html_text() %>% str_remove_all("Date of first publication") %>% str_squish() %>% str_trim()
      Date_of_first_publication <- new_function(Date_of_first_publication)
      Link_to_results <- srilanka_page %>% html_nodes(".row:nth-child(116)")  %>% html_text() %>% str_remove_all("Link to results") %>% str_squish() %>% str_trim()
      Link_to_results <- new_function(Link_to_results)
      Brief_summary_of_results <- srilanka_page %>% html_nodes(".light:nth-child(118)")  %>% html_text() %>% str_remove_all("Brief summary of results") %>% str_squish() %>% str_trim()
      Brief_summary_of_results <- new_function(Brief_summary_of_results)
      
      
      
      Trial_completion_details <- data.frame(SLCTR_registration_number,Plan_to_share_details,IPD_sharing_plan_description,Study_protocol_available,Protocol_version_and_date,Protocol_URL,Results_summary_available,Date_of_posting_results,Date_of_study_completion,Final_sample_size,Date_of_first_publication,Link_to_results,Brief_summary_of_results)
      write.table(Trial_completion_details,"Trial_completion_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_completion_details.csv"), append = T)
      
      
      
      counter = counter + 1
      print(paste("Count = ", counter,"ID = ",ids[i]))
} 
    
    
#**if the number of labels is 60 in each page

    if (length == 60) {
#***************************************************Table 2: Application-summary********************************************************************* 
      
      Scientific_title <- srilanka_page %>% html_nodes(".row:nth-child(14)") %>% html_text() %>% str_remove_all("Scientific Title of Trial") %>% str_squish() %>% str_trim()
      Scientific_title <- new_function(Scientific_title)
      Public_title <- srilanka_page %>% html_nodes(".row:nth-child(16)") %>% html_text() %>% str_remove_all("Public Title of Trial") %>% str_squish() %>% str_trim()
      Public_title <- new_function(Public_title)
      Disease_studied <- srilanka_page %>% html_nodes(".row:nth-child(18)") %>% html_text() %>% str_remove_all("Disease or Health Condition\\(s\\) Studied") %>% str_squish() %>% str_trim()
      Disease_studied <- new_function(Disease_studied)
      Scientific_acronym <- srilanka_page %>% html_nodes(".row:nth-child(20)") %>% html_text() %>% str_remove_all("Scientific Acronym") %>% str_squish() %>% str_trim()
      Scientific_acronym <- new_function(Scientific_acronym)
      Public_acronym <- srilanka_page %>% html_nodes(".row:nth-child(22)") %>% html_text() %>% str_remove_all("Public Acronym") %>% str_squish() %>% str_trim()
      Public_acronym <- new_function(Public_acronym)
      Brief_title <- srilanka_page %>% html_nodes(".row:nth-child(24)") %>% html_text() %>% str_remove_all("Brief title") %>% str_squish() %>% str_trim()
      Brief_title <- new_function(Brief_title)
      UTN_number <- srilanka_page %>% html_nodes(".row:nth-child(26)") %>% html_text() %>% str_remove_all("Universal Trial Number") %>% str_squish() %>% str_trim()
      UTN_number <- new_function(UTN_number)
      Any_numbers_assigned_by_authority <- srilanka_page %>% html_nodes(".row:nth-child(28)") %>% html_text() %>% str_remove_all("Any other number\\(s\\) assigned to the trial and issuing authority") %>% str_squish() %>% str_trim()
      Any_numbers_assigned_by_authority <- new_function(Any_numbers_assigned_by_authority)
      
      
      Application_summary <- data.frame(SLCTR_registration_number,Scientific_title,Public_title,Disease_studied,Scientific_acronym,Public_acronym,Brief_title,UTN_number,Any_numbers_assigned_by_authority)
      write.table(Application_summary,"Application_summary.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Application_summary.csv"), append = T)
      
      
      
#*****************************************************Table3: Trial Details************************************************************************
      
      Research_question_addressed <- srilanka_page %>% html_nodes(".row:nth-child(32)") %>% html_text() %>% str_remove_all("What is the research question being addressed\\?") %>% str_squish() %>% str_trim()
      Research_question_addressed <- new_function(Research_question_addressed)
      Type_of_study <- srilanka_page %>% html_nodes(".row:nth-child(34)") %>% html_text() %>% str_remove_all("Type of study") %>% str_squish() %>% str_trim()
      Type_of_study <- new_function(Type_of_study)
      Allocation  <- srilanka_page %>% html_nodes(".light+ .row .span3+ .span6") %>% html_text() %>% str_squish() %>% str_trim()
      Allocation <- new_function(Allocation)
      Masking  <- srilanka_page %>% html_nodes(".row:nth-child(39)") %>% html_text() %>% str_remove_all("Masking") %>% str_squish() %>% str_trim()
      Masking <- new_function(Masking)
      Control  <- srilanka_page %>% html_nodes(".row:nth-child(41)") %>% html_text() %>% str_remove_all("Control") %>% str_squish() %>% str_trim()
      Control <- new_function(Control)
      Assignment  <- srilanka_page %>% html_nodes(".row:nth-child(43)") %>% html_text() %>% str_remove_all("Assignment") %>% str_squish() %>% str_trim()
      Assignment <- new_function(Assignment)
      Purpose  <- srilanka_page %>% html_nodes(".row:nth-child(45)") %>% html_text() %>% str_remove_all("Purpose") %>% str_squish() %>% str_trim()
      Purpose <- new_function(Purpose)
      Study_phase  <- srilanka_page %>% html_nodes(".row:nth-child(47)") %>% html_text() %>% str_remove_all("Study Phase") %>% str_squish() %>% str_trim()
      Study_phase <- new_function(Study_phase)
      Interventions_planned  <- srilanka_page %>% html_nodes(".row:nth-child(49) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
      Interventions_planned <- new_function(Interventions_planned)
      Inclusion_criteria <- srilanka_page %>% html_nodes(".row:nth-child(51) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
      Inclusion_criteria <- new_function(Inclusion_criteria)
      Exclusion_criteria <- srilanka_page %>% html_nodes(".row:nth-child(53) .span9 div") %>% html_text() %>% str_squish() %>% str_trim()
      Exclusion_criteria <- new_function(Exclusion_criteria)
      Primary_outcome <- srilanka_page %>% html_nodes(".row:nth-child(56)") %>% html_text() %>% str_remove_all("Primary outcome\\(s\\)") %>% str_squish() %>% str_trim()
      Primary_outcome <- new_function(Primary_outcome)
      Secondary_outcome <- srilanka_page %>% html_nodes(".row:nth-child(58)") %>% html_text() %>% str_remove_all("Secondary outcome\\(s\\)") %>% str_squish() %>% str_trim()
      Secondary_outcome <- new_function(Secondary_outcome)
      
      Trial_details <- data.frame(SLCTR_registration_number,Research_question_addressed,Type_of_study,Allocation,Masking,Control,Assignment,Purpose,Study_phase,Interventions_planned,Inclusion_criteria,Exclusion_criteria,Primary_outcome,Secondary_outcome)
      write.table(Trial_details,"Trial_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_details.csv"), append = T)
      
      
#***********************************************************Table4: Recruitment-table*********************************************************************
      
      Target_number <- srilanka_page %>% html_nodes(".row:nth-child(60)")  %>% html_text() %>% str_remove_all("Target number/sample size") %>% str_squish() %>% str_trim()
      Target_number <- new_function(Target_number)
      Countries_of_recruitment <- srilanka_page %>% html_nodes(".row:nth-child(62)")  %>% html_text() %>% str_remove_all("Countries of recruitment") %>% str_squish() %>% str_trim()
      Countries_of_recruitment <- new_function(Countries_of_recruitment)
      Anticipated_start_date <- srilanka_page %>% html_nodes(".row:nth-child(64)")  %>% html_text() %>% str_remove_all("Anticipated start date") %>% str_squish() %>% str_trim()
      Anticipated_start_date <- new_function(Anticipated_start_date)
      Anticipated_end_date <- srilanka_page %>% html_nodes(".row:nth-child(66)")  %>% html_text() %>% str_remove_all("Anticipated end date") %>% str_squish() %>% str_trim()
      Anticipated_end_date <- new_function(Anticipated_end_date)
      Date_of_first_enrollment <- srilanka_page %>% html_nodes(".row:nth-child(68)")  %>% html_text() %>% str_remove_all("Date of first enrollment") %>% str_squish() %>% str_trim()
      Date_of_first_enrollment <- new_function(Date_of_first_enrollment)
      Date_of_study_completion <- srilanka_page %>% html_nodes(".row:nth-child(70)")  %>% html_text() %>% str_remove_all("Date of study completion") %>%  str_squish() %>% str_trim()
      Date_of_study_completion <- new_function(Date_of_study_completion)
      Recruitment_status <- srilanka_page %>% html_nodes(".row:nth-child(72)")  %>% html_text() %>% str_remove_all("Recruitment status") %>% str_squish() %>% str_trim()
      Recruitment_status <- new_function(Recruitment_status)
      Funding_source <- srilanka_page %>% html_nodes(".row:nth-child(74)")  %>% html_text() %>% str_remove_all("Funding source") %>% str_squish() %>% str_trim()
      Funding_source <- new_function(Funding_source)
      Regulatory_approvals <- srilanka_page %>% html_nodes(".row:nth-child(76)")  %>% html_text() %>% str_remove_all("Regulatory approvals") %>% str_squish() %>% str_trim()
      Regulatory_approvals <- new_function(Regulatory_approvals)
      
      
      Recruitment_details <- data.frame(SLCTR_registration_number,Target_number,Countries_of_recruitment,Anticipated_start_date,Anticipated_end_date,Date_of_first_enrollment,Date_of_study_completion,Recruitment_status,Funding_source,Regulatory_approvals)
      write.table(Recruitment_details,"Recruitment_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Recruitment_details.csv"), append = T)
      

#********************************************************Table 5: State of Ethics Review Approval**************************************************
      
      Status <- srilanka_page %>% html_nodes(".row:nth-child(81)")  %>% html_text() %>% str_remove_all("Status") %>% str_squish() %>% str_trim()
      Status <- new_function(Status)
      Date_of_approval <- srilanka_page %>% html_nodes(".row:nth-child(83)")  %>% html_text() %>% str_remove_all("Date of Approval") %>% str_squish() %>% str_trim()
      Date_of_approval <- new_function(Date_of_approval)
      Approval_number <- srilanka_page %>% html_nodes(".row:nth-child(85)")  %>% html_text() %>% str_remove_all("Approval number") %>% str_squish() %>% str_trim()
      Approval_number <- new_function(Approval_number)
      Name <- srilanka_page %>% html_nodes(".span7 tr:nth-child(1) td+ td")  %>% html_text()  %>% str_squish() %>% str_trim()
      Name <- new_function(Name)
      Institutional_address <- srilanka_page %>% html_nodes(".span7 tr:nth-child(2) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
      Institutional_address <- new_function(Institutional_address)
      Telephone <- srilanka_page %>% html_nodes(".span7 tr:nth-child(3) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
      Telephone <- new_function(Telephone)
      Email <- srilanka_page %>% html_nodes(".span7 tr:nth-child(4) td+ td span")  %>% html_text()  %>% str_squish() %>% str_trim()
      Email <- new_function(Email)
      
      
      Ethics_review <- data.frame(SLCTR_registration_number,Status,Date_of_approval,Approval_number,Name,Institutional_address,Telephone,Email)
      write.table(Ethics_review,"Ethics_review.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Ethics_review.csv"), append = T)
      

#****************************************************Table 6: Contact and sponsor information**********************************************************
      
      Principal_investigator_details <- srilanka_page %>% html_nodes(".row:nth-child(92) .span5")  %>% html_text() %>% str_remove_all("Contact person for Scientific Queries/Principal Investigator") %>% str_squish() %>% str_trim()
      Principal_investigator_details <- new_function(Principal_investigator_details)
      Contact_person_for_public_queries <- srilanka_page %>% html_nodes(".row:nth-child(92) .span4")  %>% html_text() %>% str_remove_all("Contact Person for Public Queries") %>% str_squish() %>% str_trim()
      Contact_person_for_public_queries <- new_function(Contact_person_for_public_queries)
      Primary_study_sponsor <- srilanka_page %>% html_nodes(".bdot+ .row .span5")  %>% html_text() %>% str_remove_all("Primary study sponsor/organization") %>% str_squish() %>% str_trim()
      Primary_study_sponsor <- new_function(Primary_study_sponsor)
      Secondary_study_sponsor <- srilanka_page %>% html_nodes(".bdot+ .row .span4")  %>% html_text() %>% str_remove_all("Secondary study sponsor \\(If any\\)") %>% str_squish() %>% str_trim()
      Secondary_study_sponsor <- new_function(Secondary_study_sponsor)
      
      Contact_and_sponsor_information <- data.frame(SLCTR_registration_number,Principal_investigator_details,Contact_person_for_public_queries,Primary_study_sponsor,Secondary_study_sponsor)
      write.table(Contact_and_sponsor_information,"Contact_and_sponsor_information.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Contact_and_sponsor_information.csv"), append = T)
      

#********************************************************Table7: Trial Completion Details***************************************************************     
      
      Plan_to_share_details <- srilanka_page %>% html_nodes(".row:nth-child(98)")  %>% html_text() %>% str_squish() %>% str_trim()
      Plan_to_share_details <- new_function(Plan_to_share_details)
      IPD_sharing_plan_description <- srilanka_page %>% html_nodes(".row:nth-child(100)")  %>% html_text() %>% str_remove_all("IPD sharing plan description") %>% str_squish() %>% str_trim()
      IPD_sharing_plan_description <- new_function(IPD_sharing_plan_description)
      Study_protocol_available <- srilanka_page %>% html_nodes(".row:nth-child(102)")  %>% html_text() %>% str_remove_all("Study protocol available") %>% str_squish() %>% str_trim()
      Study_protocol_available <- new_function(Study_protocol_available)
      Protocol_version_and_date <- srilanka_page %>% html_nodes(".row:nth-child(104)")  %>% html_text() %>% str_remove_all("Protocol version and date") %>% str_squish() %>% str_trim()
      Protocol_version_and_date <- new_function(Protocol_version_and_date)
      Protocol_URL <- srilanka_page %>% html_nodes(".row:nth-child(106)")  %>% html_text() %>% str_remove_all("Protocol URL") %>% str_squish() %>% str_trim()
      Protocol_URL <- new_function(Protocol_URL)
      Results_summary_available <- srilanka_page %>% html_nodes(".row:nth-child(108)")  %>% html_text() %>% str_remove_all("Results summary available") %>% str_squish() %>% str_trim()
      Results_summary_available <- new_function(Results_summary_available)
      Date_of_posting_results <- srilanka_page %>% html_nodes(".row:nth-child(110)")  %>% html_text() %>% str_remove_all("Date of posting results") %>% str_squish() %>% str_trim()
      Date_of_posting_results <- new_function(Date_of_posting_results)
      Date_of_study_completion <- srilanka_page %>% html_nodes(".row:nth-child(112)")  %>% html_text() %>% str_remove_all("Date of study completion") %>% str_squish() %>% str_trim()
      Date_of_study_completion <- new_function(Date_of_study_completion)
      Final_sample_size <- srilanka_page %>% html_nodes(".row:nth-child(114)")  %>% html_text() %>% str_remove_all("Final sample size") %>% str_squish() %>% str_trim()
      Final_sample_size <- new_function(Final_sample_size)
      Date_of_first_publication <- srilanka_page %>% html_nodes(".row:nth-child(116)")  %>% html_text() %>% str_remove_all("Date of first publication") %>% str_squish() %>% str_trim()
      Date_of_first_publication <- new_function(Date_of_first_publication)
      Link_to_results <- srilanka_page %>% html_nodes(".row:nth-child(118)")  %>% html_text() %>% str_remove_all("Link to results") %>% str_squish() %>% str_trim()
      Link_to_results <- new_function(Link_to_results)
      Brief_summary_of_results <- srilanka_page %>% html_nodes(".light:nth-child(120)")  %>% html_text() %>% str_remove_all("Brief summary of results") %>% str_squish() %>% str_trim()
      Brief_summary_of_results <- new_function(Brief_summary_of_results)
      
      
      
      Trial_completion_details <- data.frame(SLCTR_registration_number,Plan_to_share_details,IPD_sharing_plan_description,Study_protocol_available,Protocol_version_and_date,Protocol_URL,Results_summary_available,Date_of_posting_results,Date_of_study_completion,Final_sample_size,Date_of_first_publication,Link_to_results,Brief_summary_of_results)
      write.table(Trial_completion_details,"Trial_completion_details.csv", sep = ",",row.names = FALSE, col.names = !file.exists("Trial_completion_details.csv"), append = T)
      
     
      counter = counter + 1
      print(paste("Count = ", counter,"ID = ",ids[i]))
      

      
      
    }
  }
}
    
      
      

    
    
    
    
    
    
    
   













   

