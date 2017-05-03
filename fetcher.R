suppressWarnings(library(httr))
suppressWarnings(library(xml2))
r <- read_html(GET("https://seekingalpha.com/all-articles"))
articles_hrefs <- xml_attr(xml_find_all(r, "/html/body/div/div/div[2]/ul/li/div/a"), "href")