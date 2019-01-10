## Packages
require("tidyverse")
require("forcats") ## For factor releveling etc.

## Function for extracting relevant lines from raw-data
grep.csv <- function(pattern,opts="-E", ...) {
    p <- pipe(sprintf("grep %s '%s' %s",opts,pattern,"../data/expt-04-raw-data"))
    read.csv(p,as.is=T,header=F, ...)
}

## Extracting information and merging it all into one data frame d
grep.csv(",code,")[c(1,2,8)] %>% rename(code=V8) -> code
grep.csv(",purpose,NULL,purpose")[c(1,2,9)] %>% rename(purpose=V9) -> purpose
grep.csv(",comments,NULL,comments")[c(1,2,9)] %>% rename(comments=V9) -> comments
grep.csv(",native,")[c(1,2,9)] %>% rename(native=V9) -> native
grep.csv(",AcceptabilityJudgment,.*,NULL,.*,NULL")[c(1,2,4,6,7,9,11)] -> r
grep.csv(",AcceptabilityJudgment,.*,<b>.*</b>") -> sen
r %>%
    inner_join(sen,by=c("V1","V2","V4","V6","V7")) %>%
    inner_join(code,by=c("V1","V2")) %>%
    inner_join(purpose,by=c("V1","V2")) %>%
    inner_join(comments,by=c("V1","V2")) %>%
    inner_join(native,by=c("V1","V2")) -> d

## Create unique sid.long variable (for each data set)
d %>%
    mutate(submission.time=as.POSIXct(V1,origin="1970-01-01")) %>%
    mutate(sid.long=paste(V1,V2,sep="_")) %>%
    select(-V1,-V2) -> d

## Create numbered sid variable
subs <- levels(as.factor(d$sid.long))
d %>%
    mutate(sid=sapply(sid.long,function(x) match(x,subs))) %>%
    mutate(sid=sprintf(paste0("%0",max(nchar(sid)),"d"),sid)) %>%
    mutate(sid=paste("Sub",sid)) -> d

## Renaming variables
d %>% rename(itemType=V6) %>%
    rename(response=V9) %>%
    rename(RT=V11) %>%
    rename(sen=V8) %>%
    rename(ibexNo=V4) %>%
    rename(itemNo=V7) %>%
    select(-V3,-V5) -> d


## Removing html characters from sentences
d %>% mutate(sen=gsub(x=sen,pattern="</?b>",replacement="")) %>%
    mutate(sen=gsub(x=sen,pattern="&quot",replacement="")) %>%
    mutate(sen=gsub(x=sen,pattern="%2C",replacement=",")) -> d

## Get 'ellipsis.type' and 'ellipsis' from 'expt-04-items.csv'
read_csv("../items/expt-04-items.csv") %>%
    rename("sen"=`New item`,
           "ellipsis"=`Ellipsis`,
           "ellipsis.type"=`Ellipsis Type`,
           "voice"=`Voice`) %>%
    fill(ellipsis.type,.direction = "down") %>%
    mutate(sen=trimws(sen,"both")) %>%
    select(sen,voice,ellipsis,ellipsis.type) %>%
    right_join(d) ->
    d


## Get 'filler.type' from 'fillers.csv'
read_csv("../items/fillers.csv") %>%
    rename("filler.type"=`Type`,
           "sen"="Sentence") %>%
    right_join(d) ->
    d

## Data exclusion:
d %>%
    mutate(excl=ifelse(native=="yes",F,T),
           excl=ifelse(RT<1000,T,excl)) ->
    d

## Export dataframe as RDS
write_rds(d %>% filter(excl==F), ## only not-to-be-excluded data
          path="../data/expt-04-clean-data.rds")
if (nrow(d %>% filter(excl==T)) > 0) { ## only if some data to be excluded
    write_rds(d %>% filter(excl==T), ## export to-be-excluded data
              path="../data/expt-04-clean-excl-data.rds")
}
