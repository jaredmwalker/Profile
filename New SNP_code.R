---
  title: "Untitled"
output: html_document
date: "`r Sys.Date()`"
---

  #```{r setup, include=FALSE}
  knitr::opts_chunk$set(echo = TRUE)
#```

#```{r}

#rm(list = ls(all.names = T)) #clear all objects from environment.
options(max.print=100) # Limit output.
start.time <- Sys.time()   # Log start time.
options(scipen=999) # Prohibit use of scientific notation
gc() # Free up memory and report the memory usage.

today <- as.numeric(format(Sys.Date(), "%Y%m%d")) # Format date as numeric

options(max.print=20000)
options(scipen=999)
library(lubridate)
library(janitor)
library(tidyverse)
library(openxlsx)
library(devtools)
library(naniar)
library(readr)
library(stringr)
library(data.table)
library(tidyselect)
library(reconhub)

# Set current-month date
curr_date  <- as.Date(format(Sys.Date(), "%Y-%m-01")) %>%
  print(curr_date) %>%
  str(curr_date)

mmm_yyyy <- NULL

mmm_yyyy <- paste(month.abb[as.numeric(
  format(curr_date %m+% months(-1), format="%m"))],
  format(curr_date %m+% months(-1), format="%Y"))
#```

# Claim Date Report

#```{r}
# Create import function
import <- function(x) {
  read_csv(x, col_types = cols("Claim Date" = col_date(format = "%m/%d/%Y"),
                               "Claim Submit Date" = col_date(format = "%m/%d/%Y"),
                               "Approval Date" = col_date(format = "%m/%d/%Y"),
                               "Create Date" = col_date(format = "%m/%d/%Y"),
                               "Last Modified Date" = col_date(format = "%m/%d/%Y"))) %>%
    clean_names %>%
    mutate(due_date = claim_date + 90,
           claim_submitted_on_time = fifelse(due_date < claim_submit_date, "LATE", "On time"),
           notes = fifelse(claim_submitted_on_time == "LATE" & claim_status =="Approved",
                           paste0("Claim submitted ", claim_submit_date, ". Due date ",
                                  due_date, ". Approved by the State Agency ", approval_date,
                                  ". Revision type = ", revision_type, "."),
                           as.character(NA)),
           notes = fifelse(claim_submitted_on_time == "LATE" & claim_status =="Pending Approval",
                           paste0("Claim submitted ", claim_submit_date, ". Due date ",
                                  due_date, ". Pending approval. Revision type = ", revision_type, "."),
                           as.character(notes)))
}
# Import data
d1145 <- import(Data1145) # NSLP claim dates
d1146 <- import(Data1146) # CACFP Clam Dates
d1147 <- import(Data1147) # SFSP Clam Dates

# Naming function
soil_names <- function(y){str_to_title(gsub("_", " ",colnames(y)))}


# Column names to upper case
colnames(d1145) <- soil_names(d1145)
colnames(d1146) <- soil_names(d1146)
colnames(d1147) <- soil_names(d1147)

#waldo::compare(d1146.1, d1146.2)
#arsenal::comparedf(d1146.1, d1146.2)

clmdte_report  <- createWorkbook()

# Write data object to particular sheet/table
construct <- function(a, b) {
  addWorksheet(clmdte_report, a)
  writeData(clmdte_report, sheet = a, x = b)
}

construct("NSLP", d1145)
construct("CACFP", d1146)
construct("SFSP", d1147)

mmm_yyyy <- paste(month.abb[as.numeric(
  format(curr_date %m+% months(-1), format="%m"))],
  format(curr_date %m+% months(-1), format="%Y"))

title.l <- paste0("/", mmm_yyyy, " Claim Date Report.xlsx")

rm(mmm_yyyy)

mmm_yyyy
mmm_yyyy <- NA

title.l
#```

# Claim Variance Report

#```{r}

# Site level data
data1197_s <- read_csv(Data1197, col_types = cols("Claim Date"= col_date(format = "%m/%d/%Y"))) %>%
  clean_names %>%
  replace_with_na(replace = list(total_reimbursement = 0.0)) %>% # $0 reimbursement == NA; no cliam submitted
  filter(claim_status != "Error") %>% # Filter on claim_status "Error"
  mutate(operating_sites = 1) # Crete count of operating sites

# Sponsor level data
data1197 <- data1197_s %>%
  select(-c(site_name:site_id, claim_status:brk_type, afterschool_snack_participation,
            asp_jul:sso_snk_participation, x80)) %>% # Remove Categorical Data
  aggregate(. ~ sponsor_number + sponsor_name + claim_date, FUN=sum) %>%
  as.data.frame()

# Pivot into crosstable
ct_s <- data1197_s %>%
  select(sponsor_name:site_number, claim_date, total_reimbursement) %>%
  filter(claim_date >= curr_date %m+% months(-14)) %>%
  arrange(claim_date) %>%
  pivot_wider(names_from = claim_date,
              values_from = total_reimbursement,
              values_fill = NA)

site_level <- data1197_s
sponsor_level <- data1197
ct_s.r <- ct_s

#pivoted_ct_s <- ct_s
#efresh_data1197_s <- data1197_s
#refresh_data1197 <- data1197

#```

### Outlier Calculations

# AAD = (Current-mean)/aad

# AAD = (Current-prior)/aad

#```{r}
ct_s <- ct_s.r





ct_s$prior_year <- unlist(ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])

ct_s$current <- unlist(ct_s[, grep(curr_date %m+% months(-2), colnames(ct_s))])

ct_s$mean_claim <- rowMeans(ct_s[, c(5:17)], na.rm= T)


ct_s$mean_claim <- rowMeans(ct_s[, c(grep(curr_date %m+% months(-14), colnames(ct_s))
                                     :grep(curr_date %m+% months(-2), colnames(ct_s)))], na.rm= T)

ct_s$counts <- rowSums(!is.na(ct_s[, c(grep(curr_date %m+% months(-14), colnames(ct_s))
                                       :grep(curr_date %m+% months(-2), colnames(ct_s)))]))

ct_s[, c(grep(curr_date %m+% months(-14), colnames(ct_s)))]
ct_s[, 18]

ct_s[, 6]

ct_s[, grep(curr_date %m+% months(-2), colnames(ct_s))]


m <- function(x) {
  grep(curr_date %m+% months(x), colnames(ct_s))
}

m(-13)

ct_s$e<- abs(ct_s[, 6] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])

e<- abs(ct_s[, 6] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])

e

grep(curr_date %m+% months(-13), colnames(ct_s))
grep(curr_date %m+% months(-12), colnames(ct_s))
grep(curr_date %m+% months(-11), colnames(ct_s))
grep(curr_date %m+% months(-10), colnames(ct_s))
grep(curr_date %m+% months(-9), colnames(ct_s))
grep(curr_date %m+% months(-8), colnames(ct_s))
grep(curr_date %m+% months(-9), colnames(ct_s))
grep(curr_date %m+% months(-8), colnames(ct_s))
grep(curr_date %m+% months(-7), colnames(ct_s))
grep(curr_date %m+% months(-6), colnames(ct_s))
grep(curr_date %m+% months(-5), colnames(ct_s))
grep(curr_date %m+% months(-4), colnames(ct_s))
grep(curr_date %m+% months(-3), colnames(ct_s))
grep(curr_date %m+% months(-2), colnames(ct_s))


grep(curr_date %m+% months(-12), colnames(ct_s))

ct_s %>%
  select(6)

ct_s[, m(-12)] - ct_s[, prior_year]
ct_s[, 6] - ct_s[, grep(curr_date %m+% months(-13), colnames(ct_s))]
#```

#```{r}
View(ct_s)

View(ct_s)
# Avg Deviation from Prior Year = 0.6745*(Current-Mean)/AAD
#ct_s$d<- abs(ct_s[, 5] - ct_s[, 18])
ct_s$e<- abs(ct_s[, 6] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$f<- abs(ct_s[, 7] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$g<- abs(ct_s[, 8] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$h<- abs(ct_s[, 9] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$i<- abs(ct_s[, 10] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$j<- abs(ct_s[, 11] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$k<- abs(ct_s[, 12] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$l<- abs(ct_s[, 13] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$m<- abs(ct_s[, 14] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$n<- abs(ct_s[, 15] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$o<- abs(ct_s[, 16] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
ct_s$p<- abs(ct_s[, 17] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])


grep(curr_date %m+% months(-14), colnames(ct_s))
#```

#```{r}
for (x in 6:17) {
  for (y in list(ct_s$e,ct_s$f,ct_s$g,ct_s$h,ct_s$i,ct_s$j,ct_s$k,ct_s$l,ct_s$m,ct_s$n,ct_s$o,ct_s$p)) {
    y <- abs(ct_s[, x] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
  }
}

df

ct_s$e

dice <- c(1, 2, 3, 4, 5, 6)

for (x in dice) {
  print(x)
}


y <- abs(ct_s[, x] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
#```

#```{r}
months <- list(-13, -12, -11, -10)
letters <- list("e", "f", "g", "h", "i")
for (y in letters) {
  for (x in months) {
    print(abs(ct_s[, grep(curr_date %m+% months(x), colnames(ct_s))]
              - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))]))
  }
}

print(abs(ct_s[, grep(curr_date %m+% months(-13), colnames(ct_s))]
          - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))]))

abs(ct_s[, grep(curr_date %m+% months(x), colnames(ct_s))]
    - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])



ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))]
#```

#```{r}
months <- list(-13:-10)
letters <- list(e, f, g, h)
for (y in letters) {
  for (x in months) {
    y <- abs(ct_s[, grep(curr_date %m+% months(x), colnames(ct_s))]
             - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
  }
}

e
abs(ct_s[, 6] - ct_s[, grep(curr_date %m+% months(-14), colnames(ct_s))])
#```

#```{r}
View(ct_s)

ct_s$sums_prior<-rowSums(ct_s[,22:33], na.rm=T )
ct_s$aad_yoy <- ct_s$sums_prior/ct_s$counts

# Z score
# Calculate dispersion mesure: z_score = (current claim - prior year claim)/AAD
#ct_s$z_yoy<-0.6745*(ct_s[, 17] - ct_s$prior_year)/ct_s$aad_yoy
ct_s$z_yoy<-(ct_s[, 17] - ct_s$prior_year)/ct_s$aad_yoy

ct_s$z_yoy <- ct_s$z_yoy[, ]

#```

#### a. z_score = (current claim - prior year claim)/AAD

#```{r}
ct_s$z_yoy <- fifelse((ct_s[, 5] == 0 | is.na(ct_s[, 5])),
                      as.character(NA),
                      as.character(ct_s$z_yoy))
ct_s$z_yoy <- fifelse((ct_s[, 17] == 0 | is.na(ct_s[, 17])),
                      as.character(NA),
                      as.character(ct_s$z_yoy))
ct_s$z_yoy <- as.numeric(ct_s$z_yoy)



# Clean z_yoy
# ct_s <- ct_s %>%
#   replace_with_na(replace = list(z_yoy = 0.000))
# ct_s <- ct_s %>%
#   replace_with_na(replace = list(z_yoy = 1.000))
# ct_s <- ct_s %>%
#   replace_with_na(replace = list(z_yoy = Inf))
# ct_s <- ct_s %>%
#   replace_with_na(replace = list(z_yoy = NaN))

# Avg Deviation from the Mean  = 0.6745*(Current-Prioir Year)/AAD
#ct_s$d<- abs(ct_s[, 5] - ct_s[, 19])
ct_s$e<- abs(ct_s[, 6] - ct_s[, 18])
ct_s$f<- abs(ct_s[, 7] - ct_s[, 18])
ct_s$g<- abs(ct_s[, 8] - ct_s[, 18])
ct_s$h<- abs(ct_s[, 9] - ct_s[, 18])
ct_s$i<- abs(ct_s[, 10] - ct_s[, 18])
ct_s$j<- abs(ct_s[, 11] - ct_s[, 18])
ct_s$k<- abs(ct_s[, 12] - ct_s[, 18])
ct_s$l<- abs(ct_s[, 13] - ct_s[, 18])
ct_s$m<- abs(ct_s[, 14] - ct_s[, 18])
ct_s$n<- abs(ct_s[, 15] - ct_s[, 18])
ct_s$o<- abs(ct_s[, 16] - ct_s[, 18])
ct_s$p<- abs(ct_s[, 17] - ct_s[, 18])



ct_s$sums_mean<-rowSums(ct_s[,22:33], na.rm=T )

# Avg Deviation
ct_s$aad_mean <- ct_s$sums_mean/ct_s$counts

# 0.6745*(Current-Mean)/AAD
#ct_s$z_mean <-0.6745*(ct_s[, 17] - ct_s$mean_claim)/ct_s$aad_mean
ct_s$z_mean <-(ct_s[, 17] - ct_s$mean_claim)/ct_s$aad_mean

ct_s$z_mean <-(ct_s$z_mean[, ])

ct_s$z_mean <- fifelse(ct_s[, 5] == 0 | is.na(ct_s[, 5]),
                       as.character(NA),
                       as.character(ct_s$z_mean))
ct_s$z_mean <- fifelse(ct_s[, 17] == 0 | is.na(ct_s[, 17]),
                       as.character(NA),
                       as.character(ct_s$z_mean))
ct_s$z_mean <- as.numeric(ct_s$z_mean)



#```

#```{r}
# Clean


# Wrangle
names_s <- ct_s %>%
  select(mean_claim, aad_mean, z_mean, prior_year, aad_yoy, z_yoy)
ct_s <- cbind(ct_s[, 1:17], names_s)


#ct_s <- ct_s %>%
#  mutate(abs_M = abs(z_mean),
#         abs_YOY = abs(z_yoy),
#         score = abs_M + abs_YOY) %>%
#  arrange(desc(score)) %>%
#  select(-c("abs_M", "abs_YOY", "score"))

site_var <- ct_s
site_var_ref <- site_var

#```

#### Top Ten Site Level Outliers

#```{r}
site_var <- site_var_ref

#site_var$abs_z_yoy <- abs(site_var$z_yoy)
top_yoy_s  <- site_var[with(site_var,order(-z_yoy)),]
top_yoy_s1  <- top_yoy_s[1:5,]
top_yoy_s  <- site_var[with(site_var,order(z_yoy)),]
top_yoy_s2  <- top_yoy_s[1:5,] %>%
  arrange(desc(z_yoy))
top_yoy_s <- rbind(top_yoy_s1, top_yoy_s2)


#top_yoy_s <- top_yoy_s %>%
#  arrange(desc(abs_YOY))


drop <- c("mean_claim", "z_mean", "aad_mean")
top_yoy_s  <- top_yoy_s[ , !(names(top_yoy_s) %in% drop)]

#site_var$abs_z_mean <- abs(site_var$z_mean)
top_dm_s  <- site_var[with(site_var,order(-z_mean)),]
top_dm_s1  <- top_dm_s[1:5,]
top_dm_s  <- site_var[with(site_var,order(z_mean)),]
top_dm_s2  <- top_dm_s[1:5,] %>%
  arrange(desc(z_mean))
top_dm_s <- rbind(top_dm_s1, top_dm_s2)


#top_dm_s <- top_dm_s %>%
#  mutate(abs_M = abs(z_mean)) %>%
#  arrange(desc(abs_M)) %>%
#  select(-abs_M)





top_yoy_s_ref <- top_yoy_s
top_dm_s_ref <- top_dm_s
site_var_ref2 <- site_var
#```

# Pivot Sponsor Claims by Date

#```{r}


p_reimb <- data1197 %>%
  select("sponsor_number", "sponsor_name", "claim_date", "total_reimbursement")
p_reimb  <- p_reimb %>%
  filter(claim_date >= curr_date %m+% months(-13))
p_reimb <- p_reimb[with(p_reimb,order(claim_date)),]

ct <- p_reimb %>%
  pivot_wider(names_from = claim_date, values_from = total_reimbursement, values_fill = NA)

ct_pivoted <- ct
#```

### Outlier Calculations

# AAD = (Current-mean)/aad

# AAD = (Current-prior)/aad

#```{r}
ct <- ct_pivoted


# Log Transformation
#col1_4<-ct[,1:4]
#ct <- log(ct[,3:15])
#ct <- cbind(col1_4, ct)

# Growth Rate Formula
ct$prior_year <- ct[,3]
ct[, 16] <- ct$prior_year

#ct$current <- ct[,17]



ct$mean_claim <- rowMeans(ct[, c(3:15)], na.rm= T)

#ct$year_over_year_change <- (ct[,17]-ct[,5])/ct[,5]
#ct$variance_from_the_mean <- (ct[,17]-ct[,20])/ct[,20]

ct$counts <- rowSums(!is.na(ct[,3:15]))

#ct$b<- abs(ct[, 3] - ct[, 16])
ct$c<- abs(ct[, 4] - ct[, 16])
ct$d<- abs(ct[, 5] - ct[, 16])
ct$e<- abs(ct[, 6] - ct[, 16])
ct$f<- abs(ct[, 7] - ct[, 16])
ct$g<- abs(ct[, 8] - ct[, 16])
ct$h<- abs(ct[, 9] - ct[, 16])
ct$i<- abs(ct[, 10] - ct[, 16])
ct$j<- abs(ct[, 11] - ct[, 16])
ct$k<- abs(ct[, 12] - ct[, 16])
ct$l<- abs(ct[, 13] - ct[, 16])
ct$m
ct$aad_yoy <- ct$sums_prior/ct$counts

<- abs(ct[, 14] - ct[, 16])
ct$n<- abs(ct[, 15] - ct[, 16])

ct$sums_prior<-rowSums(ct[,19:30], na.rm=T )
#ct$z_yoy <- 0.6745*(ct[, 15] - ct$prior_year)/ct$aad_yoy
ct$z_yoy <- (ct[, 15] - ct$prior_year)/ct$aad_yoy

ct$z_yoy <- ct$z_yoy[, ]

ct$z_yoy <- fifelse(ct[, 3] == 0 | is.na(ct[, 3]),
                    as.character(NA),
                    as.character(ct$z_yoy))
ct$z_yoy <- fifelse(ct[, 15] == 0 | is.na(ct[, 15]),
                    as.character(NA),
                    as.character(ct$z_yoy))
ct$z_yoy <- as.numeric(ct$z_yoy)


#sum(is.na(ct$z_yoy))

#ct$b<- abs(ct[, 3] - ct[, 17])
ct$c<- abs(ct[, 4] - ct[, 17])
ct$d<- abs(ct[, 5] - ct[, 17])
ct$e<- abs(ct[, 6] - ct[, 17])
ct$f<- abs(ct[, 7] - ct[, 17])
ct$g<- abs(ct[, 8] - ct[, 17])
ct$h<- abs(ct[, 9] - ct[, 17])
ct$i<- abs(ct[, 10] - ct[, 17])
ct$j<- abs(ct[, 11] - ct[, 17])
ct$k<- abs(ct[, 12] - ct[, 17])
ct$l<- abs(ct[, 13] - ct[, 17])
ct$m<- abs(ct[, 14] - ct[, 17])
ct$n<- abs(ct[, 15] - ct[, 17])



ct$sums_mean<-rowSums(ct[,19:30], na.rm=T )
ct$aad_mean <- ct$sums_mean/ct$counts

#ct$z_mean <-0.6745*(ct[, 15] - ct$mean_claim)/ct$aad_mean
ct$z_mean <-(ct[, 15] - ct$mean_claim)/ct$aad_mean

ct$z_mean <- ct$z_mean[, ]
#```

#```{r}


names <- ct %>%
  select("mean_claim", "aad_mean", "z_mean", "prior_year", "aad_yoy", "z_yoy")

ct <- cbind(ct[, 1:15], names)






ct$z_mean <- fifelse((ct[, 3] == 0 | is.na(ct[, 3])),
                     as.character(NA),
                     as.character(ct$z_mean))
ct$z_mean <- fifelse((ct[, 15] == 0 | is.na(ct[, 15])),
                     as.character(NA),
                     as.character(ct$z_mean))
ct$z_mean <- as.numeric(ct$z_mean)

#sum(is.na(ct$z_yoy))




# ct <- ct %>%
#   mutate(abs_M = abs(z_mean),
#          abs_YOY = abs(z_yoy),
#          score = abs_M + abs_YOY) %>%
#   arrange(desc(score)) %>%
#   select(-c("abs_M", "abs_YOY", "score"))





spon_var <- ct
spon_var_ref <- spon_var



#```

#### Top Ten Sponsor Level Outliers

#```{r}
#spon_var <- as.data.frame(spon_var_ref)

#library(tidyverse)

#spon_var <- do.call(data.frame, spon_var)

# Top Ten

#top_yoy <- spon_var %>%
##  arrange(desc(z_yoy))

#top_yoy <- spon_var %>%
#  top_n(z_yoy, 5) %>%
#  bottom_n(z_yoy, 5)


#as.numeric(spon_var$"z_yoy.2021-11-01")


#top_yoy <- spon_var %>%
#  slice_max(n = 5, spon_var$"z_yoy.2021-11-01") %>%
#  rbind(slice_min(n = 5, spon_var$"z_yoy.2021-11-01")))



#```

#```{r}
spon_var <- spon_var_ref

top_yoy  <- spon_var[with(spon_var,order(-z_yoy)),]
top_yoy1  <- top_yoy[1:5,]
top_yoy  <- spon_var[with(spon_var,order(z_yoy)),]
top_yoy2  <- top_yoy[1:5,] %>%
  arrange(desc(z_yoy))
top_yoy <- rbind(top_yoy1, top_yoy2)

#top_yoy <- top_yoy %>%
#  mutate(abs_YOY = abs(z_yoy)) %>%
#  arrange(desc(abs_YOY)) %>%
#  select(-abs_YOY)

#frp_pct_diff_2  = round(frp_pct_diff_2, digits=2)



# Top Ten
#spon_var$abs_z_mean <- abs(spon_var$z_mean)
top_dm  <- spon_var[with(spon_var,order(-z_mean)),]
top_dm1  <- top_dm[1:5,]
top_dm  <- spon_var[with(spon_var,order(z_mean)),]
top_dm2  <- top_dm[1:5,] %>%
  arrange(desc(z_mean))
top_dm <- rbind(top_dm1, top_dm2)

#top_dm <- top_dm %>%
#  mutate(abs_M = abs(z_mean)) %>%
#  arrange(desc(abs_M)) %>%
#  select(-abs_M)


names <- spon_var %>%
  select("mean_claim", "aad_mean", "z_mean", "prior_year", "aad_yoy", "z_yoy")

spon_var <- cbind(spon_var[, 1:15], names)

#spon_var <- spon_var %>%
#  mutate(abs_M = abs(z_mean),
#         abs_YOY = abs(z_yoy),
#         score = abs_M + abs_YOY) %>%
#  arrange(desc(score)) %>%
#  select(-c("abs_M", "abs_YOY", "score"))

View(top_yoy)
spon_var_ref <- spon_var
top_yoy_ref <- top_yoy
top_dm_ref <- top_dm

#```

#-------- \# 5 - Save Workbook

#```{r}
data1197_s <- refresh_data1197_s
data1197 <- refresh_data1197
top_yoy <- as_tibble(top_yoy_ref)
top_dm <- as_tibble(top_dm_ref)
top_yoy_s <- as_tibble(top_yoy_s_ref)
top_dm_s <- as_tibble(top_dm_s_ref)
spon_var <- spon_var_ref
site_var <- site_var_ref2


View(top_dm)
top_dm_y <- top_dm[, 2]
top_dm_y$date <- format(as.Date(curr_date %m+% months(-1), "%m/%d/%Y"))
top_dm_y$unique <- paste(top_dm_y$date, top_dm_y$sponsor_name )
top_dm_y$top_dm_y <- "Y"
drop <- c("sponsor_name", "date")
top_dm_y  <- top_dm_y[ , !(names(top_dm_y) %in% drop)]



top_yoy_y <- top_yoy[, 2]
top_yoy_y$date <- format(as.Date(curr_date %m+% months(-1), "%m/%d/%Y"))
top_yoy_y$unique <- paste(top_yoy_y$date, top_yoy_y$sponsor_name )
top_yoy_y$top_yoy_y <- "Y"
drop <- c("sponsor_name", "date")
top_yoy_y  <- top_yoy_y[ , !(names(top_yoy_y) %in% drop)]

View(top_yoy_y)
# Tableau sheet

refresh_1197 <- data1197


tab1  <- data1197 %>%
  select("sponsor_name", "sponsor_number", "claim_date", "total_eligible_students",
         "adp_brk", "adp_lunch", "adp_sso_brk", "adp_sso_lunch",'breakfast_operating_days',
         'lunch_operating_days', 'sso_operating_days', 'free_lunches', 'rdc_lunches',
         'paid_lunches', 'free_breakfasts', 'rdc_breakfasts', 'paid_breakfasts', 'sso_lunches',
         'sso_breakfasts', "total_reimbursement", "operating_sites")


tab1$adp <- tab1$adp_brk + tab1$adp_lunch + tab1$adp_sso_brk + tab1$adp_sso_lunch

tab1$days <- tab1$breakfast_operating_days + tab1$lunch_operating_days + tab1$sso_operating_days

tab1$meals <- tab1$free_lunches +  tab1$rdc_lunches + tab1$paid_lunches + tab1$free_breakfasts +  tab1$rdc_breakfasts +  tab1$paid_breakfasts + tab1$sso_lunches + tab1$sso_breakfasts

tab1$uniq <- paste0(tab1$sponsor_number, tab1$claim_date)


spon_var$current <- spon_var[, 15]

spon_var[, 22] <- spon_var$current

#```

#```{r}
spon_var$claim_date <-format(as.Date(curr_date %m+% months(-1), "%m/%d/%Y"))

spon_var$uniq <- paste0(spon_var$sponsor_number, spon_var$claim_date)


#
spon_var_mean <- spon_var[!is.na(spon_var$z_mean), ]
spon_var_mean <- spon_var_mean %>%
  rename(current_mean = current,
         sponsor_name_mean = sponsor_name)

spon_var_mean <- spon_var_mean %>%
  select("sponsor_number", "sponsor_name_mean", "current_mean", "mean_claim", "aad_mean", "z_mean", "uniq")


#
spon_var_yoy <- spon_var[!is.na(spon_var$z_yoy), ]
spon_var_yoy <- spon_var_yoy %>%
  rename(current_YOY = current,
         sponsor_name_YOY = sponsor_name)
spon_var_yoy <- spon_var_yoy%>%
  select("sponsor_number", "sponsor_name_YOY", "prior_year", "current_YOY", "aad_yoy", "z_yoy", "uniq")

names(spon_var_yoy)
#drop  <- c("current")
#spon_var  <- spon_var[ , !(names(spon_var) %in% drop)]



#tab_2$claim_date <- curr_date %m+% months(-1)

#tab_2
#tab_2$uniq <- paste0(tab_2$sponsor_number, tab_2$claim_date)


tab1 <- as.matrix(tab1, drop=F)
spon_var_yoy <- as.matrix(spon_var_yoy, drop=F)

tab2  <- merge(tab1, spon_var_yoy, by="uniq", all.x = T)

spon_var_mean <- as.matrix(spon_var_mean, drop=F)
tab3  <- merge(tab2,spon_var_mean, by=c("uniq"), all = T)


drop  <- c("uniq", "sponsor_number.y" , "claim_date.y")
tab3  <- tab3[ , !(names(tab3) %in% drop)]

drop  <- c("uniq", "sponsor_number.y" , "claim_date.y" )
spon_var  <- spon_var[ , !(names(spon_var) %in% drop)]


tab3$unique <- paste(tab3$claim_date, tab3$sponsor_name )

tab4 <- merge(tab3, top_dm_y, by= "unique", all.x = T)
tab5 <- merge(tab4, top_yoy_y, by= "unique", all.x = T)
#```

#```{r}

library(data.table)
tab5$top_20 <- NA
tab5$top_20 <- as.character(tab5$top_20)
tab5$top_20 <- fifelse(!is.na(tab5$top_dm_y), "Y", tab5$top_20)
tab5$top_20 <- fifelse(!is.na(tab5$top_yoy_y), "Y", tab5$top_20)

drop  <- c("unique")
tab5  <- tab5[ , !(names(tab5) %in% drop)]
#```

#```{r}
# Column Names

spon_var <- spon_var_ref

colnames(tab5) <- stringr::str_to_title(gsub("_", " ",colnames(tab5)))

colnames(data1197) <- stringr::str_to_title(gsub("_", " ",colnames(data1197)))

colnames(data1197_s) <- stringr::str_to_title(gsub("_", " ",colnames(data1197_s)))

colnames(top_dm_s) <- stringr::str_to_title(gsub("_", " ",colnames(top_dm_s)))

colnames(top_yoy_s) <- stringr::str_to_title(gsub("_", " ",colnames(top_yoy_s)))

colnames(site_var) <- stringr::str_to_title(gsub("_", " ",colnames(site_var)))

colnames(top_dm) <- stringr::str_to_title(gsub("_", " ",colnames(top_dm)))

colnames(top_yoy) <- stringr::str_to_title(gsub("_", " ",colnames(top_yoy)))

colnames(spon_var) <- stringr::str_to_title(gsub("_", " ",colnames(spon_var)))

# Save


#```

#```{r}

# data1197_s <- refresh_data1197_s
# data1197 <- refresh_data1197
# top_yoy <- top_yoy_ref
# top_dm <- top_dm_ref
# top_yoy_s <- top_yoy_s_ref
# top_dm_s <- top_dm_s_ref
# spon_var <- spon_var_ref
# site_var <- site_var_ref2


#writeDataTable(wb = excel_overview_file, x = income, sheet = excel_overview_income,


# withFilter = TRUE, keepNA = FALSE, na.string = NULL)




#```

#```{r}
# top_yoy <- top_yoy_ref

#apply(top_dm, c(1:2), is.nan)

# top_dm <- as.matrix(top_dm)
# top_yoy <- as.matrix(top_yoy)

# top_dm <- top_dm_ref
#top_dm <- as.matrix(top_dm)

# View(top_dm)

# names <- colnames(top_dm[, 1:17])

# test <- cbind(top_dm[, 1], apply(top_dm[, 2:17], 2, as.numeric))


# top_dm_num <- matrix(as.numeric(unlist(top_dm)),nrow=nrow(top_dm))

#top_dm_num <- top_dm_num[, -1]


# colnames(top_dm_num) <- names

# top_dm_num[, 1] <- as.character(top_dm_num[, 1])

# top_dm2 <- cbind(top_dm[, 1], top_dm_num)

# View(top_dm2)

# names(top_dm)

# View(top_yoy)
#```

#```{r}




SNP_list.1  <- list('DEVIATION FROM THE MEAN'= top_dm,
                    'DEVIATION FROM PRIOR YEAR'= top_yoy)
variances  <- createWorkbook()
addWorksheet(variances, 'Top Ten Sponsors')

curr_row  <- 3
for(i in seq_along(SNP_list.1)) {
  writeData(variances, 'Top Ten Sponsors', names(SNP_list.1)[i], startCol = 1, startRow = curr_row)
  writeData(variances, 'Top Ten Sponsors', SNP_list.1[[i]], startCol = 1, startRow = curr_row+1)
  curr_row  <- curr_row + nrow(SNP_list.1[[i]])+6
}

#top_dm_s <- as.data.frame(top_dm_s)
#top_yoy_s <- as.data.frame(top_yoy_s)

SNP_list.2  <- list('DEVIATION FROM THE MEAN'= top_dm_s,
                    'DEVIATION FROM PRIOR YEAR'= top_yoy_s)

addWorksheet(variances, "Top Ten Sites")

curr_row  <- 3
for(i in seq_along(SNP_list.2)) {
  writeData(variances, 'Top Ten Sites', names(SNP_list.2)[i], startCol = 1, startRow = curr_row)
  writeData(variances, 'Top Ten Sites', SNP_list.2[[i]], startCol = 1, startRow = curr_row+1)
  curr_row  <- curr_row + nrow(SNP_list.2[[i]])+5
}


#```

#```{r}


addWorksheet(variances, 'Sponsor Variances')
writeData(variances, sheet = 'Sponsor Variances', x = spon_var)


addWorksheet(variances, 'Site Variances')
writeData(variances, sheet = 'Site Variances', x = site_var)

# data1197 <- as.matrix(data1197, drop=F)
# addWorksheet(variances, 'Raw Data 1137 (Sponsor Level)')
# writeData(variances, sheet = 'Raw Data 1137 (Sponsor Level)', x = data1197)

# data1197 <- as.matrix(data1197, drop=F)
addWorksheet(variances, 'Data1137')
writeData(variances, sheet = 'Data1137', x = data1197_s)

# data1197_s <- as.matrix(data1197_s, drop=F)
# addWorksheet(variances, "Data1137")
# writeData(variances, sheet = "Data1137 ", x = data1197_s)


addWorksheet(variances, "Tableau")
writeData(variances, sheet = "Tableau", x = tab5)
#```

#```{r}
# Save Workbook

title.v <- paste0("/", mmm_yyyy, " SNP Reimbursement Variance Report.xlsx")
#month<-format(curr_date %m+% months(-1), format="%B")
#name <- paste(month, "SNP Reimbursement Variance Report.xlsx")

#saveWorkbook(wb, "TEST (10).xlsx", overwrite = T)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#```
