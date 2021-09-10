
da.dists <- c( "27102720000000",
 "27660680000000",
 "27660920000000",
 "27661590000000",
 "27661910000000",
 "27754400000000")



for(i in da.dists){

dist <- list(dist =  i )

render("DASummary.Rmd", 
    #   output_format = "all", 
       output_file = here("output" , paste0("DASummary for ", dist, ".html" ) ),
       params = dist, 
       envir = new.env(parent = globalenv()) 
)

}
