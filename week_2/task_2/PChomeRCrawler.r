
library(httr)

url<-"https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=%E3%84%87%E5%9E%8B%E6%9E%B6&page=1&sort=rnk/dc"
res= GET(url)
res_json= content(res)
raw= data.frame(do.call(rbind,res_json$prods))
