library(jsonlite)
library(R6)

OepsClient <- R6Class("OepsClient",
                      public = list(
                          endpoint = NA,
                          initialize = function(endpoint) {
                              if (!missing(endpoint)) {
                                  self$endpoint <- endpoint
                              }
                              else {
                                  self$endpoint <- "https://inventaris.onroerenderfgoed.be/erfgoed/node"
                              }
                          },
                          get_query_results = function (q_url) {
                              url = paste(self$endpoint, q_url, sep='?')
                              erfgoedobjecten_df <- private$combine_paginated_results(url)
                              return(erfgoedobjecten_df)
                          },
                          get_erfgoedobject = function (e_url) {
                              e_obj <- fromJSON(e_url)
                              return(e_obj)
                          }
                      ),

                      private = list(
                          as_data_frame = function (erfgoedobjecten) {
                              id <- sapply(erfgoedobjecten, function (x) { x$id })
                              type <- sapply(erfgoedobjecten, function (x) { x$type$label })
                              vastgesteld <- sapply(erfgoedobjecten, function (x) { x$vastgesteld })
                              omschrijving <- sapply(erfgoedobjecten, function (x) { x$omschrijving })
                              provincie <- sapply(erfgoedobjecten, function (x) { x$locatie$provincie })
                              gemeente <- sapply(erfgoedobjecten, function (x) { x$locatie$gemeente })
                              r <- data.frame(id, type, vastgesteld, omschrijving, provincie, gemeente)
                              colnames(r) <- c('id', 'type', 'vastgesteld', 'omschrijving', 'provincie', 'gemeente')
                              return(r)
                          },
                          combine_paginated_results = function (url, df=NULL) {
                              r <- fromJSON(url)
                              hrefs <- sapply(r$items$links, function (x) x$href[2])
                              erfgoedobjecten <- lapply(hrefs, function (x) {self$get_erfgoedobject(x)})
                              erfgoedobjecten_df <- private$as_data_frame(erfgoedobjecten)
                              if (!is.null(df)) {
                                  erfgoedobjecten_df <- rbind(erfgoedobjecten_df, df)
                              }
                              if ("next" %in% names(r$links)) {
                                  erfgoedobjecten_df <- private$combine_paginated_results(r$links$`next`$href, erfgoedobjecten_df)
                              }
                              return(erfgoedobjecten_df)
                          }
                      )
)
