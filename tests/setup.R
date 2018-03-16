download.file('ftp://ftp.cdc.gov/pub/data/Brfss/CDBRFS84_XPT.zip',tempfile(),mode='wb')

library(RCurl)
getURL("ftp://ftp.cdc.gov/")
