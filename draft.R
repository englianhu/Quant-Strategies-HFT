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


