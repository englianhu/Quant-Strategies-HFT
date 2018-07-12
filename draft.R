require('BBmisc')
require('plyr')
require('dplyr')
require('purrr')
require('stringr')
require('devtools')

## http://coin.wne.uw.edu.pl/pwojcik/hfd_en.html

if(!dir.exists('data')){
  dir.create('./data')
  
  url1 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_assess_rules_and_data.zip'
  url2 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_data_out.zip'
  url3 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_assess_sample_report_and_pres.zip'
  url4 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_sample_questions.zip'
  
  download.file(url1, destfile = './data/HFD_assess_rules_and_data.zip')
  download.file(url2, destfile = './data/HFD_data_out.zip')
  download.file(url3, destfile = './data/HFD_assess_sample_report_and_pres.zip')
  download.file(url4, destfile = './data/HFD_sample_questions.zip')
  rm(url1, url2, url3, url4)
}

## extract zipped dataset.
#'@ llply(dir('data'), function(x) unzip(paste0('data/', x), exdir = 'data'))
## 
## password protected zipped files unable to unzip through R.


## file:///C:/Users/Nijat/AppData/Local/Temp/Rar$EXb0.553/HDF_sections_assessment_201718.html
## http://coin.wne.uw.edu.pl/pwojcik/hfd_en.html
## password : #!hFd$Zo16%
## 
## This is password
## You will see
## 
## Research project: rules and in-sample data
## Research project: out-of-sample data
## Research project: template of presentation and final report
## 
## Sample exam questions
## I also sent my project that my teacher did not accept
## We should make it again
## I sent you all on gmail
## 

lnk1 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q1.RData'
lnk2 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q2.RData'
lnk3 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q3.RData'
lnk4 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q4.RData'
lnk5 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2012Q1.RData'
lnk <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_201112.zip'

download.file(lnk1, destfile = 'data/data_2011Q1.RData')
download.file(lnk2, destfile = 'data/data_2011Q2.RData')
download.file(lnk3, destfile = 'data/data_2011Q3.RData')
download.file(lnk4, destfile = 'data/data_2011Q4.RData')
download.file(lnk5, destfile = 'data/data_2012Q1.RData')
download.file(lnk, destfile = 'data/data_201112.zip')

unzip('data/data_201112.zip', exdir = 'data/data')

## load tick dataset
data.US <- llply(dir('data/', pattern = 'data'), function(x) {
  BBmisc::load2(paste0('data/', x)) %>% xts
})
data.US <- do.call(rbind, data.US)

## high frequency data week 26 to week 28.
unzip('data/W25.zip', exdir = 'data')

## https://www.r-bloggers.com/faster-files-in-r/
con <- file('data/W25.txt', open = "rb")
result <- readChar(con, file.info('data/W25.txt')$size, useBytes = TRUE)
close(con)

W25 <- data.table::fread('data/W25.txt')
W26 <- data.table::fread('data/W26.txt')
W27 <- data.table::fread('data/W27.txt')




