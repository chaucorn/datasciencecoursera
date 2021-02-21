best <- function(state, outcome){
  data <- read.csv(unz('C:/Users/chaud/OneDrive/2. Study/0. MOOC/[Coursera] DS with R/2. R programming/rprog_data_ProgAssignment3-data.zip', 'outcome-of-care-measures.csv'))
  dat <- data %>% replace_with_na_all(condition = ~.x == 'Not Available')
  if (state %in% data$State == FALSE) {
    stop(print('invalid state name'))
  }
  else if(outcome %in% c("heart attack", "heart failure",
                               "pneumonia") == FALSE){
    stop(print('invalid outcome'))
  }
  if (outcome == 'heart attack') {
    colnum <- 11
  }
  if (outcome == 'heart failure') {
    colnum <- 17
  }
  if (outcome == 'pneumonia') {
    colnum <- 23
  }
  dat1 <- subset(dat, State == state )
  minrow <- which(as.numeric(unlist(dat1[,colnum]), na.rm = TRUE) == min(as.numeric(unlist(dat1[,colnum])), na.rm = TRUE))
  hos <- dat1[minrow,2]
  hos <- sort(hos)
  return(hos)
  }