Trial registries have evolved into an essential component of the global biomedical research infrastructure, allowing numerous stakeholders to monitor registered studies on a variety of levels. Therefore, maintaining clinical trial transparency is crucial.

In this effort, we sought to establish the procedures that must be followed in order to identify each study conducted in India that has been listed in any of the (non-Indian) WHO-recognized registry. Where possible, we used two techniques for each registry.

Methodology A involved downloading all of the registry's records, web-scraping for the necessary data, and saving the retrieved data in a local SQLite database for further querying. Since studies registered with the Indian registry have registration numbers that begin with CTRI and a foreign registry might cross-reference a trial filed in India using its CTRI number, we searched for "India" and "CTRI" in the database. Then, we kept note of every instance of these keywords to find those that suggested the study had been conducted in India. A search on the registry website was conducted using the keyword "India" as part of Methodology B. This should have been a "Advanced search" that looked in the "Country of recruitment" field. However, if that option wasn't available, we just did a simple basic search. 

We found inconsistencies in the outcomes by using both Methodology A and Methodology B. Although there weren't many discrepant records, it's nevertheless necessary to be aware of the issue and to take it into consideration when conducting meta-studies on trials in the future.

The following are included in this file: 
a) R scripts for downloading the records and web-scraping the required data.
b) Data analysis scripts written in Python.

To filter the data and providing the desired results, shell scripting and MS-Excel functions were used.
