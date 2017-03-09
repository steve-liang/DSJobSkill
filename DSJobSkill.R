library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)

clean.text <- function(text)
{
  str_replace_all(text, regex('\r\n|\n|\t|\r|,|/|<|>|\\.'), ' ')
}

# Given running total dataframe and links to scrape skills and compute running total
ScrapeJobLinks <- function(running, job.links){
  for(i in 1:length(job.links)){
    job.url <- paste0(BASE_URL,job.links[i])
    
    Sys.sleep(1)
    cat(paste0('Reading job ', i, '\n'))
    
    tryCatch({
      html <- read_html(job.url)
      text <- html_text(html)
      text <- clean.text(text)
      df <- data.frame(skill = KEYWORDS, count = ifelse(str_detect(text, KEYWORDS), 1, 0))
      running$count <- running$count + df$count
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  return(running)
}

KEYWORDS <- c('Hadoop','Python','\\bSQL\\b', 'NoSQL','\\bR\\b', 'Spark', 'SAS', 'Excel', 'AWS', 'Azure', 'Java', 'Tableau')
KEYWORDS_DISPLAY <- c('Hadoop','Python','SQL', 'NoSQL','R', 'Spark', 'SAS', 'Excel', 'AWS', 'Azure', 'Java', 'Tableau')

# Indeed Search Words
# job_title <- "\"Data+Analyst\""
job_title <- "\"Data+Scientist\""
# job_title <- "\"Trader\""

location <- 'New+York%2C+NY'

# use advanced search to get 50 results in a page
BASE_URL <- 'https://www.indeed.com'
ADV_URL <- paste0('https://www.indeed.com/jobs?as_and=&as_not=&as_cmp=&jt=all&st=&salary=&sr=directhire&radius=25&fromage=any&limit=50&sort=date&psf=advsrch&as_any=&as_phr=&as_ttl=', job_title, '&l=', location)

cat(ADV_URL)

# Scrape the search result
start_page <- read_html(ADV_URL)

job_count <- unlist(strsplit(start_page %>% 
                               html_node("#searchCount") %>%
                               html_text(), split = ' ')) 
job_count <- as.numeric(job_count[length(job_count)])
cat('Total job count: ', job_count)

# Get start page job URLs
links <- start_page %>%
  html_nodes("h2 a") %>%
  html_attr('href')

# Get result page links
page.links <- start_page %>%
  html_nodes(xpath = '//div[contains(@class,"pagination")]//a') %>%
  html_attr('href')


# Create running total dataframe
running <- data.frame(skill = KEYWORDS_DISPLAY, count = rep(0, length(KEYWORDS_DISPLAY)))

if(job_count != 0){
  cat('Scraping jobs in Start Page\n')
  running <- ScrapeJobLinks(running, links)
}

for(p in 1:length(page.links)-1){
  
  cat('Moving to Next 50 jobs\n')
  
  # Navigate to next page
  new.page <- read_html(paste0(BASE_URL, page.links[p]))
  
  # Get new page job URLs
  links <- new.page %>%
    html_nodes("h2 a") %>%
    html_attr('href')
  
  # Scrap job links
  running <- ScrapeJobLinks(running, links)
  
}

# running total
print(arrange(running, -count))

# running total count as percentage
running$count<-running$count/job_count
jt <- str_replace_all(job_title, '\\+|\\\"', ' ')
loc <- str_replace_all(location, '\\%2C+|\\+',' ')
ggplot(running, aes(reorder(skill,-count), count)) + geom_bar(stat="identity") + labs(x = 'Skill', y = 'Count', title = paste0('Skill occurrences(%) for ', jt, ' in ', loc))
