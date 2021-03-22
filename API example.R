require(tidyverse)
require(httr)
require(jsonlite)
require(rlist)
require(stringr)
require(sqldf)
require(ggplot2)
require(beeswarm)
require(googleway)
require(highcharter)
require(quantmod)
require(romato)

client_id <- "oRXG-5EbOsC_XMiFbn-csA"
client_secret <- "MGB45IYKRpTVpbxDdfWccm0PpaLESo1BZMoI9EngKbxOJTfB041SwucvPlgGWWUnCyy22KQ-G1vIrfd5s4N_uyob1u0xQCpYuZYx5uYb3CyZLbO1l1UegYVnwsJSYHYx"
res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

token <- content(res)$access_token

yelp <- "https://api.yelp.com"
location <- "Traverse City, MI"
categories <- NULL
limit <- 50
radius <- 8800
term <- "Domino"
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit,
                               radius = radius))
res <- GET(url, add_headers('Authorization' = paste("bearer", client_secret)))

results <- content(res)

yelp_httr_parse <- function(x) {
  
  parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     distance = x$distance)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- tibble(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state, 
                   distance= parse_list$distance)
  df
}

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)

business_data <- do.call("rbind", results_list)

dominos_data <- business_data[grepl('\\Domino', business_data$name),]

#write.table(dominos_data, "/Users/Abs/Documents/GitHub/R Sample Codes/test2.csv",
#            append = TRUE,
#            sep = ",",
#            col.names = FALSE,
#            row.names = FALSE,
#            quote = FALSE)

df_new <- read.csv(file = '/Users/Abs/Documents/GitHub/R Sample Codes/test2.csv')

ggplot(data = df_new) + 
  geom_point(mapping = aes(x = review_count, y = rating, color = city))

hchart(df_new, "scatter", hcaes(x = review_count, y = rating, group = city))

ggplot(data = df_new, mapping = aes(x = review_count, y = rating)) + 
  geom_point(mapping = aes(color = city)) + 
  geom_smooth(data = filter(df_new, rating>=3), se = FALSE)

ggplot(data = df_new) + 
  geom_point(mapping = aes(x = review_count, y = rating)) +
  geom_smooth(mapping = aes(x = review_count, y = rating))

ggplot(data = business_data) + 
  geom_point(mapping = aes(x = review_count, y = rating, color = city))

ggplot(df_new, aes(x = city, y = review_count)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=review_count), position=position_dodge(width=0.9), vjust=-0.25)

p = ggplot(business_data, aes( x = city, y = review_count, fill = city))

p + geom_dotplot(binaxis = "y", binwidth = 1/6,
                 stackdir = "center", stackratio = 0.75,
                 aes(color = city))

p + geom_beeswarm(aes(color = city))+
  geom_text(aes(label=rating), position=position_dodge(width=0.9), vjust=-0.25)

p + geom_violin()+
  geom_text(aes(label=rating), position=position_dodge(width=0.9), vjust=-0.25)


key <- "AIzaSyALX7BwpASYTO-pTHtQpzeN99bwonIuTjY"

df <- data.frame(lat = df_new$latitude,
                 lon = df_new$longitude,
                 info = paste(df_new$name,df_new$rating),
                 city = df_new$city,
                 rating = df_new$rating,
                 review = df_new$review_count)

google_map(key = key, height = 600, search_box = T) %>%
  add_markers(data = df, info_window = "info")

x <- getSymbols("DPZ", auto.assign = FALSE)
y <- getSymbols("YUM", auto.assign = FALSE)
z <- getSymbols("PZZA", auto.assign = FALSE)

highchart(type = "stock") %>% 
  hc_add_series(x) %>% 
  hc_add_series(y) %>%
  hc_add_series(z)


library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(data=df, popup=paste(df_new$name,df_new$rating))

zmt <- zomato$new(api_key = "2bfbc4c07e842a726a76cb88d72429c4")

mugen <- zmt$search(query = "Pizza Delivery", lat = 12.972442, lon = 77.580643)
print(zmt$restaurant(res_id = mugen$id[1]))

leaflet(df) %>% addTiles() %>%
  addCircles(lng =df$lon, lat = df$lat, weight = 1,
             radius = df$review* 100, popup = df$city
  )