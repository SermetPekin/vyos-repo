
# ....................................................... create_evds_url
create_evds_url <- function(type = c( "subject" ,
                                      "datagroups" ,
                                      "info_api" )  ,
                            subject_num = 5 ,
                            table_name = "bie_altingr" ,
                            key = null ){
  domain = "https://evds2.tcmb.gov.tr/service/evds"
  type <- match.arg(type )
  api_key_evds = key
  if(is.null( key ))
    api_key_evds =  get_api_key("evds")
  g = glue::glue
  liste <- list(
     subject    =  g("{domain}/categories/key={api_key_evds}&type=json") ,
     datagroups =  g("{domain}/datagroups/key={api_key_evds}&mode=2&code={subject_num}&type=json"),
     info_api   =  g("{domain}/serieList/key={api_key_evds}&type=json&code={table_name}")
  )
  liste[[ type ]]
}
get_evds_table_names_with_konu_num <- function(subject_num = 5 ,
                                               cache = F   ){
  url = create_evds_url( "datagroups" , subject_num )
  gelen<-request_httr2_helper(url ,  cache )
  if( !  is_response(gelen))
    return (false )
  contentList <- gelen %>% httr2::resp_body_json()
  sonuc <-   convert_list_df_evds2(contentList)
  sonuc <- list( raw = sonuc ,
                 table_names = sonuc$DATAGROUP_CODE  ,
                 aciklama = sonuc$DATAGROUP_NAME  )
  invisible( sonuc)
}
# ....................................................... get_evds_table_info_api
get_evds_konular_list_api<-function(  cache = T ){
  url <- create_evds_url( "subject")
  gelen<-request_httr2_helper(url , cache )
  if(is_false_false(gelen ))
    return( false )
  gelen %>% httr2::resp_body_json()  %>% convert_list_df_evds2
}