setwd("/Users/alisonzhang/Desktop/2018 Spring/STAT 686/Mini Project 2")
dat = read.csv("1617 Data.csv")
fff = read.csv('FamaFrench.csv')
  # Keep the year for dat and fff the same!!!!!

View(fff)

require(dplyr)

########## Data Preprocessing ##########
class(dat$RET)
dat1 = subset(dat, RET != 'B' & RET != 'C' & RET != "" & (!is.na(dat[,3])) &
                (!is.na(dat[,4])) & (!is.na(dat[,5])))
    # Filter out missing PRC, RET, and SHROUT
class(dat1$date)
dat1 = transform(dat1, date = as.Date(as.character(date), "%Y%m%d"))
    # Transform the format of date 


dat1$year = format(dat1$date, '%Y')
count_day = dat1 %>% group_by(year, PERMNO) %>% summarize(n = n())
med = count_day %>% group_by(year) %>% summarize(median = median(n))
    # Get the median number of trading days for stocks by year 


countt = 0
med$year = as.numeric(med$year)
count_day$year = as.numeric(count_day$year)
traded_comp = vector("list", length(med$year))
names(traded_comp) = med$year
for(i in med$year){
  countt = countt + 1
  subset_year = subset(count_day, year == i)
  med_year = as.numeric(med[countt, 2])
  for(j in 1:length(subset_year$PERMNO)){
    if(as.numeric(subset_year[j, 3]) == med_year){
      traded_comp[[countt]] = c(traded_comp[[countt]], as.integer(subset_year[j, 2]))
    }
  }
}
    # Create a dictionary (list) to store the year as key, and the identity of 
    # company with median number of trading days as value 


dat1$year = as.numeric(dat1$year)
dat2 = data.frame()
countt = 0
for(i in med$year){
  countt = countt + 1
  subset_year = subset(dat1, year == i)
  subset_year = subset_year[(subset_year$PERMNO %in% traded_comp[[countt]]), ]
  dat2 = rbind(dat2, subset_year)
}
  # Filter out companies with non-median trading days from dat1 by year
  # The new dataset is called dat2 
length(dat2$PERMNO)/length(dat1$PERMNO)
  # Coverage: 0.9294203


dat2$RET = as.numeric(as.character(dat2$RET))
dat2$market_cap = dat2$SHROUT * dat2$PRC
    # Calculate market_cap
dat2$month = as.numeric(format(dat2$date, '%m'))
dat2$day = as.numeric(format(dat2$date, '%d'))
jan = subset(dat2, month == 1)
ini_traded = jan %>% group_by(year) %>% summarise(min = min(day))
   # Get the initial trading day for each year 
init_traded_dat = data.frame()
countt = 0
for(i in ini_traded$year){
  countt = countt + 1
  subset_year = subset(jan, year == i)
  init_day_year = as.numeric(ini_traded[countt, 2])
  subset_year = subset(subset_year, day == init_day_year)
  init_traded_dat = rbind(init_traded_dat, subset_year)
}
   # Get all data for on the initial trading day for each year 
rank_comp =  init_traded_dat %>%
  group_by(year) %>%
  mutate(my_ranks = dense_rank(desc(market_cap)))
   # Rank the market cap by company by year (dense rank)



############################ Input N HERE ############################
select_N_comp = function(n){
  countt = 0
  comp_lst = vector("list", length(med$year))
  names(comp_lst) = med$year
  for(i in med$year){
    countt = countt + 1
    subset_year = subset(rank_comp, year == i)
    gett = subset_year[(subset_year$my_ranks %in% 1:n), ]
    comp_lst[[countt]] = gett$PERMNO
  }
  return(comp_lst)
}

###############################################
# Every time, re-run from here, if changing N # 
###############################################

select_comp = select_N_comp(5)

  # Generate year & company list for top N market-cap company 
  # in dictionary format (key:value)
######################################################################


dat3 = data.frame()
countt = 0
for(i in med$year){
  countt = countt + 1
  subset_year = subset(dat2, year == i) 
  subset_year = subset_year[subset_year$PERMNO %in% select_comp[[countt]], ]
  dat3 = rbind(dat3, subset_year)
}
  # Get all data for top N market-cap company
dat3$month = NULL
dat3$day = NULL  




require(tidyr)
########## Run the model ##########
paper_entry = function(n, yearr){
  w = runif(n, min = 0, max = 1)
  W = w/sum(w)  
  # Generate random weights w 
  
  dat4 = subset(dat3, year == yearr)
  dat4$date_ = as.integer(format(as.Date(dat4$date), "%Y%m%d"))
  get_lst = cbind(dat4$PERMNO, dat4$RET)
  get_lst = cbind(get_lst, dat4$date_)
  colnames(get_lst) = c("PERMNO", 'return', 'date')
  get_lst = as.data.frame(get_lst)

  R = get_lst %>% spread(PERMNO, return)
  R$date = NULL
  R = as.matrix(R)
  # Generate r the return of company j on day i: company: row; day: column
  N = nrow(R)
  # Let N be the number of trading days in the year 
  
  P = R %*% w
  # Return of the random stock portfolio on a day 
  mu = sum(P)/N
  # Average daily return of the random stock portfolio. Not returned 
  mu_year = sum(P)
  # Annualized random stock portfolio return 
  sigma = sqrt(N/(N-1) * sum((P-mu)^2))
  # Annualized standard deviation. 
  
  result = c(mu_year, sigma)
  return(result)
}




############################ Input N, Year HERE ############################

####### Keeo the N you enter here the same as the N you entered above
simulation = function(n1, year1){
  new_df = data.frame(matrix(ncol = 2, nrow = 0))
  colnames(new_df) = c('mu', 'sigma')
  for(i in 1:10000){
    result = paper_entry(n1, year1)
    new_df[i, ] = result
  }
  
  # Normalize: 
  # new_df$mu = new_df$mu/mean(new_df$mu)
  # new_df$sigma = new_df$sigma/mean(new_df$sigma)
  
  
  return(new_df)
}
result = simulation(5, 2016)
#############################################################################


######### Fama French risk free rate 
fff = transform(fff, date = as.Date(as.character(date), "%Y%m%d"))
fff$year = format(fff$date, '%Y')
min_max = fff %>% group_by(year) %>% summarise(low = min(rf), high = max(rf))
min_max$average = (min_max$low + min_max$high)/2 * med$median


require(ggplot2)
######### Data Visualization


