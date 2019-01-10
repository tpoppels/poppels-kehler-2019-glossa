## Packages
require("tidyverse")
require("forcats") ## For factor releveling etc.

## Function for extracting relevant lines from raw-data
grep.csv <- function(pattern,opts="-E", ...) {
    p <- pipe(sprintf("grep %s '%s' %s",opts,pattern,"../data/expt-03-raw-data"))
    read.csv(p,as.is=T,header=F, ...)
}

## Extracting information and merging it all into one data frame d
grep.csv(",code,")[c(1,2,8)] %>% rename(code=V8) -> code
grep.csv(",purpose,NULL,purpose")[c(1,2,9)] %>% rename(purpose=V9) -> purpose
grep.csv(",comments,NULL,comments")[c(1,2,9)] %>% rename(comments=V9) -> comments
grep.csv(",native,")[c(1,2,9)] %>% rename(native=V9) -> native
grep.csv(",AcceptabilityJudgment,.*,NULL")[c(1,2,4,6,7,9,11)] %>% # extracting judgments
    inner_join(grep.csv(",AcceptabilityJudgment,.*,<b>.*</b>"), # extracting sentences and merging them
               by=c("V1","V2","V4","V6","V7")) %>%
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

## Calculate item variants
## - first experimental item: d$ibexNo==7 (=min(d$ibexNo) with enough subjects)
## - labels:
##    *  7: '[A <- P]'   mismatch
##    *  8: '[P <- A]'   mismatch
##    *  9: '[A <- A]'   match
##    * 10: '[P <- P]'   match
d %>% mutate(condition=ifelse(itemType=="experimental",
                              c("[A <- P]",
                                "[P <- A]",
                                "[A <- A]",
                                "[P <- P]")[((ibexNo+1) %% 4) + 1],
                              NA)) -> d

## Filler item types
## - first filler item: d$ibexNo == 103
## - then 12 of each
##   * upper-bound elliptical
##   * lower-bound elliptical
##   * upper-bound non-elliptical
##   * lower-bound non-elliptical
d %>% mutate(fillerType = ifelse(ibexNo %in% c(103:114),"upper-bound elliptical",NA)) %>%
    mutate(fillerType = ifelse(ibexNo %in% c(115:126),"lower-bound elliptical",fillerType)) %>%
    mutate(fillerType = ifelse(ibexNo %in% c(127:138),"upper-bound non-elliptical",fillerType)) %>%
    mutate(fillerType = ifelse(ibexNo %in% c(139:150),"lower-bound non-elliptical",fillerType)) -> d

## Derive other useful variables
#### voice.cl1, voice.cl2, voice.ellipsis, mismatch, cataphoric (for expt 3)
d %>%
    separate(condition,remove=F,sep=" ",
             c("voice.cl1","cataphoric","voice.cl2")) %>%
    mutate(cataphoric=recode(cataphoric,`<-`=T,`->`=F)) %>%
    mutate(voice.cl1=recode(voice.cl1,
                            `[A`="Active",
                            `[P`="Passive")) %>%
    mutate(voice.cl2=recode(voice.cl2,
                            `A]`="Active",
                            `P]`="Passive")) %>%
    mutate(mismatch=ifelse(voice.cl1==voice.cl2,F,T)) %>%
    mutate(voice.ellipsis = ifelse(cataphoric==T,voice.cl1,voice.cl2)) ->
    d

## Data types
d %>%
    mutate(condition=factor(condition)) %>%
    mutate(condition=fct_relevel(condition,
                                 "[A <- A]",
                                 "[A <- P]",
                                 "[P <- P]",
                                 "[P <- A]")) -> d

## Data exclusion:
d %>%
    mutate(excl=ifelse(native=="yes",F,T)) ->
    d



## Export dataframe as RDS
write_rds(d %>% filter(excl==F), ## only not-to-be-excluded data
          path="../data/expt-03-clean-data.rds")
if (nrow(d %>% filter(excl==T)) > 0) { ## only if some data to be excluded
    write_rds(d %>% filter(excl==T), ## export to-be-excluded data
              path="../data/expt-03-clean-excl-data.rds")
}
