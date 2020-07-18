# NYC-flights-Data-R
NYC-Flights_DATA-R_assignment
---
title: "Nyc flights"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library ( nycflights13 )

library ( tidyverse )

library(ggplot2)

library(maps)

library(dplyr)
```

```{r}
## question -1
flights<-nycflights13::flights
airports<-nycflights13::airports
planes<-nycflights13::planes
weather<-nycflights13::weather
avgdelay<-group_by(flights,dest)%>%
  summarise(avg_delay=mean(arr_delay,na.rm=TRUE))
airports<-rename(airports,"dest"="faa")
join1<-airports%>%semi_join(avgdelay,by='dest')
airports%>%semi_join(avgdelay,by='dest')%>%ggplot(aes(lon,lat,colors="avg_delay"))+borders("state")+geom_point()+coord_quickmap()
```
```{r}
##2nd question
loc<-airports%>%select(dest,lat,lon)
flights_loc<-flights%>%left_join(loc,by=c('origin'='dest'))
flights_loc<-rename(flights_loc,"origin_lat"="lat","origin_lon"="lon")
flights_loc<-flights_loc%>%left_join(loc,by="dest")
flights_loc<-rename(flights_loc,"dest_lat"="lat","dest_lon"="lon")

```


```{r}
##3rd question
planes_age<-planes%>%select(tailnum,year)
planes_age<-arrange(planes_age,-desc(year))
flights_delay<-group_by(flights,tailnum)%>%
  summarise(delay=mean(arr_delay,na.rm=TRUE))
flights_age<-flights_delay%>%inner_join(planes_age,by="tailnum")
flights_age%>%drop_na()
```


```{r}
##4th question
by_day <-group_by ( flights , year , month , day )
flights_weather<-merge(weather,flights,by=c("origin",'year','month','day','hour'))
flights_weather<-flights_weather%>%drop_na()
ggplot(data=flights_weather,aes(x=dep_delay,y=temp))+geom_point()+geom_smooth()
ggplot(data=flights_weather,aes(x=dep_delay,y=dewp))+geom_point()+geom_smooth()
ggplot(data=flights_weather,aes(x=dep_delay,y=humid))+geom_point()+geom_smooth()
ggplot(data=flights_weather,aes(x=dep_delay,y=humid))+geom_point()+geom_smooth()

```
```{r}
##5th question
june13<-filter(flights,year==2013,month==6,day==13)
flights_weather<-merge(june13,weather,by=c('year','month','day','hour'))
ggplot(june13,aes(x=hour,y=dep_delay))+geom_point()+geom_smooth()
```

