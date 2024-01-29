
remove_column <- function(df , column_name) {
  if (column_name  %in% colnames(df)) {
    df <- df[ , !names(df) %in% c(column_name)]
  }
  df
}
remove_columns <-function (df, column_names){
  for (column_name in column_names) {
    if(!column_name %inn% df){
        # message_debug( glue::glue(" { column_name}
        #                       not found passing ... \n\r
        #                       ") )
    }else{
        .blue(" removing ...{column_name}\n\r")
      try({
        df <- remove_column(df, column_name)
      })
    }
  }
  df
}
#' remove_na_safe
#'
#' @param df data.frame to remove na rows from the beginning and from the end
#' @param verbose give detailed info while removing NA values
#'
#' @return data.frame
#' @export
#' @examples
#'
#' df <- data.frame( a = c( NA, 2: 7 , NA ) ,
#'                   b = c( NA, NA, 5, NA, 12, NA, 8, 9 ) )
#' df2 <- remove_na_safe( df )
remove_na_safe <- function(df , verbose = F) {

  df <-   df_check_remove(df , verbose = verbose)
  invisible(df)
}
remove_any_na <- function(df) {
  df <- dplyr::filter(df, rowSums(is.na(df)) == 0)
}
first_row_that_ok <- function(df_ , reverse = F , except= null ) {
  if( !is.null(except) ){
    df_ <- df_ %>% dplyr::select(-c(except ))
  }
  numbers <- seq(from  = 1 , to = nrow(df_))
  if (reverse) {
    numbers <- seq(from  = nrow(df_) , to =  1)
  }
  for (num in  numbers) {
    a <- df_[num:num  , ]
    cond <-  all(!is.na(unlist(as.vector(a))))
    if (cond) {
      # print( as.vector(a)  )
      return (num)
    }
  }
  return (NA)
}
looks_like_template <- function(x) {
  is.character(x) && length(x) == 1  &&  grepl("\n" ,  x[[1]])
}
last_row_that_ok <- function(df_) {
  first_row_that_ok(df_ , reverse = T)
}
safe_remove_col<-function(df , colname){
  if( ! colname %in% colnames(df )) return (df)
  df %>% dplyr::select(-c(!!colname))
}
df_check_remove <- function(df , verbose = F) {
  first_row   <- first_row_that_ok(df)
  last_row   <- last_row_that_ok(df)
  if(any( purrr::map_vec( c(first_row ,last_row ) , is.na ) ) ) return (df)
  df2 <- df[ first_row : last_row , ]
  n_ <- nrow(df)
  fark <- nrow(df)  - nrow(df2)
  if (verbose) {
    .green(
      "
     first_row : {first_row }
     last_row : {last_row }
     {fark} rows removed ...
                  "
    )
  }
  return(df2)
}
# ..................................................   convert_df_numeric_helper
convert_df_numeric_helper <- function( df ){
  df <- remove_column( df , "Tarih")
  df <- remove_column( df , "YEARWEEK")
    if(!is.data.frame(df))
      return(df)
  return(
    try_or_default( {
                        df %>% dplyr::mutate_if(is.character, as.numeric)
                    } , .default = df  )  )
}
# ........................................................................ limit_ilk_yil
limit_start_date<-function(sonuc , start_date   =  default_start_date() ){
  if( ! is.data.frame( sonuc )  || is.null( start_date ) ) return (sonuc )

  if( "date" %in% colnames( sonuc ) )
     sonuc <-  sonuc |> dplyr::filter( date >= lubridate::ymd(  start_date  ))

  sonuc
}
