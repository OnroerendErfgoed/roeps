library(jsonlite)
library(R6)

OepsClient <- R6Class("OepsClient",
                      public = list(
                        nodetype = NA,
                        endpoint = NA,
                        initialize = function(nodetype="erfgoedobject") {
                          self$nodetype <- nodetype
                          if (nodetype == 'erfgoedobject') {
                            self$endpoint <- "https://inventaris.onroerenderfgoed.be/erfgoedobjecten"
                          }
                          else {
                            self$endpoint <- "https://inventaris.onroerenderfgoed.be/aanduidingsobjecten"
                          }
                        },
                        get_query_results = function (q_url) {
                          url = paste(self$endpoint, q_url, sep='?')
                          print(url)
                          objecten_df <- private$combine_paginated_results(url)
                          return(objecten_df)
                        },
                        
                        get_object = function (e_url) {
                          e_obj <- fromJSON(e_url)
                          return(e_obj)
                        }
                        
                      ),
                      
                      private = list(
                        as_data_frame = function (erfgoedobjecten) {
                          if (self$nodetype == 'erfgoedobject') {
                            id <- sapply(erfgoedobjecten, function (x) { x$id })
                            type <- sapply(erfgoedobjecten, function (x) { x$type$label })
                            omschrijving <- sapply(erfgoedobjecten, function (x) { x$omschrijving })
                            provincie <- sapply(erfgoedobjecten, function (x) { x$locatie$provincie })
                            gemeente <- sapply(erfgoedobjecten, function (x) { x$locatie$gemeente })
                            r <- data.frame(id, type, omschrijving, provincie, gemeente)
                            colnames(r) <- c('id', 'type', 'omschrijving', 'provincie', 'gemeente')
                          } else {
                            id <- sapply(erfgoedobjecten, function (x) { x$id })
                            type <- sapply(erfgoedobjecten, function (x) { x$type$label })
                            omschrijving <- sapply(erfgoedobjecten, function (x) { x$omschrijving })
                            provincie <- sapply(erfgoedobjecten, function (x) { x$locatie$provincie })
                            gemeente <- sapply(erfgoedobjecten, function (x) { x$locatie$gemeente })
                            r <- data.frame(id, type, omschrijving, provincie, gemeente)
                            colnames(r) <- c('id', 'type', 'omschrijving', 'provincie', 'gemeente')
                          }
                          return(r)
                        },
                     
                        combine_paginated_results = function (url, df=NULL) {
                          r <- fromJSON(url)
                          hrefs <- sapply(r$items$links, function (x) x$href[2])
                          objecten <- lapply(hrefs, function (x) {self$get_object(x)})
                          objecten_df <- private$as_data_frame(objecten)
                          if (!is.null(df)) {
                            objecten_df <- rbind(objecten_df, df)
                          }
                          if ("next" %in% names(r$links)) {
                            next_link <- gsub('erfgoed/node/.json', paste(self$nodetype, 'en', sep=''), r$links$`next`$href)
                            objecten_df <- private$combine_paginated_results(next_link, objecten_df)
                          }
                          return(objecten_df)
                        }
                      )
)
