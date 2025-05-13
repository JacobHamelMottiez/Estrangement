#  This function is from here : https://github.com/christopherBelter/scopusAPI 
# I added the parameter "insttoken" to be able to fetch the data. 


install.packages(c("httr", "XML"))
require(httr)
require(XML)


source("scopusAPI.R")
insttoken <- "d7e05f3ac9d63a6d51b9170681078e96"
insttoken <- inst_token_header(insttoken)

query <- rscopus::scopus_search('DOI(10.1257/aer.20161923)', 
                                view = "COMPLETE", 
                                headers = insttoken,
                                cursor = TRUE
                                )
raw <- gen_entries_to_df(query$entries)





myQuery <- "KEY(\"cryoelectron microscopy\") AND PUBYEAR > 2005 AND PUBYEAR < 2016"


theXML <- searchByString(string = myQuery, content = "complete", outfile = "testdata.xml")


theData <- extractXML(theXML)




