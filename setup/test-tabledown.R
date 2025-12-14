# library(tabledown)
library(MOTE)
library(dplyr)

# functions that call MOTE
apa <- function(value, decimals = 3, leading = TRUE) {

  if (missing(value)) {
    stop("Be sure to include the numeric values you wish to format.")
  }

  if (!is.numeric(value)){
    stop("The values you provided are not numeric.")
  }

  if (leading == T) {
    formnumber <- format(round(as.numeric(value), decimals), digits = decimals, nsmall = decimals)
  }
  if (leading == F) {
    formnumber <- sub("^(-?)0.", "\\1.", sprintf(paste("%.", decimals, "f", sep = ""), as.numeric(value)))
  }
  return(formnumber)
}

apa <- function(value, decimals = 3, leading = TRUE) {

  if (missing(value)) {
    stop("Be sure to include the numeric values you wish to format.")
  }

  # Track input type
  is_df  <- is.data.frame(value)
  is_vec <- is.numeric(value) && is.null(dim(value))

  # Validate value
  if (is_df) {
    if (!all(vapply(value, is.numeric, logical(1)))) {
      stop("All columns in 'value' must be numeric.")
    }
    value_mat <- as.matrix(value)
  } else if (is.numeric(value)) {
    value_mat <- as.matrix(value)
  } else {
    stop(
      "'value' must be numeric (vector or matrix) ",
      "or a data frame with all-numeric columns."
    )
  }

  # Validate decimals
  if (!is.numeric(decimals) || length(decimals) != 1 ||
      decimals < 0 || decimals != as.integer(decimals)) {
    stop("'decimals' must be a single non-negative integer.")
  }
  decimals <- as.integer(decimals)

  # Validate leading
  if (!is.logical(leading) || length(leading) != 1) {
    stop("'leading' must be TRUE or FALSE.")
  }

  # Format values
  out <- format(
    round(value_mat, decimals),
    nsmall = decimals,
    trim = FALSE,
    scientific = FALSE
  )

  if (!leading) {
    out <- sub("^(-?)0\\.", "\\1.", out)
  }

  # Restore original structure
  if (is_df) {
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    names(out) <- names(value)
  } else if (is_vec) {
    out <- as.vector(out)
    names(out) <- NULL
  }

  out
}

cfa.tab <- function(x, robust = FALSE ){
  ifelse(robust==TRUE,{
    Model <- lavaan::fitmeasures(x, c("chisq", "df", "pvalue", "gfi",  "nfi",
                                      "cfi.robust","tli.robust",
                                      "rmsea.robust", "rmsea.ci.upper.robust",
                                      "rmsea.ci.lower.robust","srmr"))

  },
  {Model <- lavaan::fitmeasures(x, c("chisq", "df", "pvalue", "gfi",  "nfi",
                                     "cfi","tli","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","srmr"))

  })

  Model <- apa(Model, 2, TRUE)
  Model <- as.data.frame(Model)
  Model <- (t(Model))
  colnames(Model) <- c("Chi-square", "df", "p", "GFI","NFI", "CFI", "TIL", "RMSEA", "RMSEA-Upper", "RMSEA-Lower", "SRMR")
  Model
}

cfa.tab.multi <- function(x,y,z=NULL, a=NULL, b=NULL, robust = FALSE){
  if(is.null(z) & is.null(a) & is.null(b)){
    ifelse(robust == TRUE,{
      table1 <- cfa.tab(x,robust = TRUE )
      table2 <- cfa.tab(y,robust = TRUE )

    },
    {
      table1 <- cfa.tab(x,robust = FALSE )
      table2 <- cfa.tab(y,robust = FALSE )

    })
    table <- rbind(table1, table2)
    rownames(table) <- c("Model1", "Model2")
  } else if (is.null(a) & is.null(b)){
    ifelse(robust == TRUE,{
      table1 <- cfa.tab(x,robust = TRUE )
      table2 <- cfa.tab(y,robust = TRUE )
      table3 <- cfa.tab(z,robust = TRUE )

    },
    {table1 <- cfa.tab(x,robust = FALSE )
    table2 <- cfa.tab(y,robust = FALSE )
    table3 <- cfa.tab(z,robust = FALSE )


    })
    table <- rbind(table1, table2, table3)
    rownames(table) <- c("Model1", "Model2", "Model3")

  }else if (is.null(b)){
    ifelse(robust == TRUE,{
      table1 <- cfa.tab(x,robust = TRUE )
      table2 <- cfa.tab(y,robust = TRUE )
      table3 <- cfa.tab(z,robust = TRUE )
      table4 <- cfa.tab(a,robust = TRUE )
    },
    {table1 <- cfa.tab(x,robust = FALSE )
    table2 <- cfa.tab(y,robust = FALSE )
    table3 <- cfa.tab(z,robust = FALSE )
    table4 <- cfa.tab(a,robust = FALSE )

    })
    table <- rbind(table1, table2, table3, table4)
    rownames(table) <- c("Model1", "Model2", "Model3", "Model4")
  } else{
    ifelse(robust == TRUE,{
      table1 <- cfa.tab(x,robust = TRUE )
      table2 <- cfa.tab(y,robust = TRUE )
      table3 <- cfa.tab(z,robust = TRUE )
      table4 <- cfa.tab(a,robust = TRUE )
      table5 <- cfa.tab(b,robust = TRUE )
    },
    {table1 <- cfa.tab(x,robust = FALSE )
    table2 <- cfa.tab(y,robust = FALSE )
    table3 <- cfa.tab(z,robust = FALSE )
    table4 <- cfa.tab(a,robust = FALSE )
    table5 <- cfa.tab(b,robust = FALSE )

    })
    table <- rbind(table1, table2, table3, table4, table5)
    rownames(table) <- c("Model1", "Model2", "Model3", "Model4", "Model5")
  }
  table
}


## testing function
dataframe <- tabledown::FFMQ.CFA[, c(9,10,12,14)]
recode_code <- c( "1" = "Never or very rarely true", "2" = "Rarely true",
                  "3"= "Sometimes true","4" = "Often true","5" = "Very often or always true")

gt_tab <- function(dataframe, recode_code){
  Items <- 0
  value <- 0
  longtab <- as.data.frame(tidyr::gather(dataframe, Items, value))
  longtab$value <- as.numeric(as.character(longtab$value))

  ##Summarizing and creating gt object

  summary_tab <- longtab %>%
    dplyr::group_by(Items) %>%
    # calculate summary stats & create data for the histogram and density plot
    dplyr::summarise(
      nr = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      med = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      hist_data = list(value),
      dens_data = list(value),
      .groups = "drop"
    )


  descriptive_tab <- tabledown::des.tab(dataframe)
  summary_tab_2 <- dplyr::inner_join(summary_tab, descriptive_tab, by = "Items")
  data_likert_1 <- dataframe
  data_likert_2 <-  dplyr::mutate(data_likert_1, dplyr::across(dplyr::starts_with(c("item")), ~unname(recode_code[.])))
  data_Factor_1 = as.data.frame(lapply(data_likert_2,factor, ordered = T))

  #get the items name
  items <- names(data_Factor_1)
  #Calculate percentage
  percentage_1 <- kutils::likert(data_Factor_1, vlist = items )

  percentage_2 <- percentage_1$table %>%
    as.data.frame(.)
  #data wrangling

  labels<- tibble::rownames_to_column(percentage_2, "Items")

  full_percentage_1<- as.data.frame(t(labels )) #transpose
  full_percentage_2 <- full_percentage_1[,-6] #removing 1st row and total column
  full_percentage_3 <- tibble::rownames_to_column(full_percentage_2, "Items")
  col_names <- full_percentage_3[1,]
  full_percentage_4 <-full_percentage_3[-1,]
  full_percentage_5 <- as.data.frame(full_percentage_4)

  colnames(full_percentage_5) <- (col_names)

  full.table <-  dplyr::inner_join( summary_tab_2, full_percentage_5, by = "Items")

  return(full.table )
}

## testing this function
df <- dataframe
reverse = FALSE
des.tab <- function(df, reverse = FALSE){

  Descriptives <- psych::describe(df)
  Mean <-apa(Descriptives$mean,2,TRUE)
  SD <-apa(Descriptives$sd,2,TRUE)
  Skew <-apa(Descriptives$skew,2,TRUE)
  Kurtosis <- apa(Descriptives$kurtosis,2,TRUE)
  ifelse(reverse==TRUE,{
    alpha <- psych::alpha(df,check.keys=TRUE)
  },
  {alpha <- psych::alpha(df,check.keys=FALSE)})
  Items <- rownames(alpha$item.stats)
  Corrected.item.total.correlation <- apa(alpha$item.stats$r.cor,2,TRUE)
  normality.test <- tabledown::normality.loop(df)
  statistics <-apa(normality.test$statistic,2,TRUE)
  sig <-(normality.test$significance)
  Normality <- paste(statistics, sig, sep = "" )
  des.tab <-as.data.frame((cbind(Items, Mean, SD, Skew,Kurtosis,  Normality,Corrected.item.total.correlation)))
  des.tab}


sample_tab <- gt_tab(data,recode_code)

SD <-apa(Descriptives$sd,2,TRUE)
SD2 <- apa(Descriptives$sd,2,TRUE)
