library(rvest)
library(data.table)
library(pbapply)
library(purrr)

links <- read.csv("data/notelections_majorit_parsed_links.csv")

#links <- links[1:100,]



extract_table_from_elections <- function(base_link){
  i <<- 1
  #print(base_link)
  #base_link <- 'http://notelections.online/region/izbirkom?action=show&vrn=4144016173755&region=14&prver=0&pronetvd=null'
  html_table_link <- read_html(base_link, encoding = 'windows-1251') %>%
    html_node('#standard-reports a') %>% 
    #html_node('#220-rep-dir-link') %>% 
    html_attr('href') 
  if (is.na(html_table_link)) {
    return(data.frame(base_link = base_link))
  }
  
  html_table_link <- html_table_link %>% 
    paste0('http://notelections.online',.)
  
  extract_table_per_page <- function(html_table_link_n, page_number = 1){
    #html_table_link_n = html_table_link
    html_table_link_n_base <- html_table_link_n
    if (page_number != 1) { 
      html_table_link_n <- paste0(html_table_link_n,paste0("&number=",page_number))
    }
    
    #print(html_table_link_n)
    html_table_page <- read_html(html_table_link_n, encoding = 'windows-1251') 
    
    elections_table <- html_table_page %>%
      html_node('.table-responsive')
    
    if (is.na(elections_table)) {
      elections_table <- data.frame(tik = NA)
      tik <- html_table_page %>% html_nodes('li:nth-child(2)') %>% html_text()
      elections_table$tik <- tik[1]
      #elections_table$date <- html_table_page %>% html_nodes('.table-borderless tr:nth-child(1) td') %>% html_text()
      elections_table$base_link <- base_link
      elections_table$table_link <- html_table_link_n
      return(elections_table)
    }
    
    elections_table <-  html_table(elections_table,header=F)
    
    
    elections_table <- elections_table[!is.na(elections_table$X3),]
    column_names <- trimws(elections_table[2,])
    column_names[column_names == 'Субьект выдвижения'] <- "Субъект выдвижения"
    colnames(elections_table) <- column_names
    
    elections_table <- elections_table[-1,]
    elections_table <- elections_table[-1,]
    tik <- html_table_page %>% html_nodes('li:nth-child(2)') %>% html_text()
    elections_table$tik <- tik[1]
    elections_table$date<- html_table_page %>% html_nodes('.table-borderless tr:nth-child(1) td') %>% html_text()
    elections_table$base_link <- base_link
    elections_table$table_link <- html_table_link_n
    #print(elections_table)
    
    #print(nrow(elections_table))
    if (nrow(elections_table) == 20) {
      i <<- i + 1
      #print(i)
      #paginated_link <- paste0(html_table_link_n,paste0("&number=",i))
      #print(paginated_link)
      elections_table <- rbind(elections_table,
                               extract_table_per_page(html_table_link_n_base,page_number = i))
      
    }
    return(elections_table)
  }
  elections_table <- extract_table_per_page(html_table_link)
  
  return(elections_table)
}

elections <- pblapply(links$link, extract_table_from_elections)
saveRDS(elections,"data/elections_list.rds")

elections_df <- rbindlist(elections, fill = T)


