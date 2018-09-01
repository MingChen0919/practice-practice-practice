# install.packages('rjson')
# install.packages('blsAPI')
library(blsAPI)
library(rjson)

# supply series identifier to pull data
layoffs_json = blsAPI('MLUMS00NN0001003')
layoffs = fromJSON(layoffs_json)
str(layoffs)


library(httr)
url = modify_url('http://treegenes.cam.uchc.edu/galaxy', path = '/api/version')

http_status(r)
