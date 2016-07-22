makeIGraph <- function(
  ###function converts kappa string, defining the complex structure into iGraph graph
  kappa
  ###complex definition in kappa language
  , num = 1
  ### number of complex instances in the mixture
  ) {
  #       if(!require(igraph)){
  #         stop('Function is required package "igraph"');
  #       }
  edges <- list()
  g <- graph.empty(n = 0, directed = FALSE)
  cl <- colors()
  vcl <- list()
  idx <- 0
  subg <- 0
  if (length(num) == 1)
    num = rep(num, length(kappa))
  for (kpi in 1:length(kappa)) {
    kp <- kappa[kpi]
    instN <- num[kpi]
    for (ii in 1:instN) {
      subg <- subg + 1
      k <- sub('\\)$', '', kp)
      unlist(strsplit(k, '),', fixed = TRUE)) -> parts
      strs <-
        lapply(strsplit(parts, '[(,]'), function(x)
          strsplit(x, '!'))
      for (i in 1:length(strs)) {
        idx <- idx + 1
        n <- strs[[i]][[1]]
        nname = paste(n, idx, sep = '_')
        if (!(n %in% names(vcl))) {
          vcl[[n]] <- colors()[8 + length(vcl) * 3]
          #cat(paste(n,length(vcl),vcl[[n]],'\n'))
        }
        g <-
          add.vertices(g, 1, attr = list(
            name = strs[[i]][[1]],
            name2 = nname,
            color = vcl[[n]]
          ))
        for (j in 2:length(strs[[i]])) {
          if (length(strs[[i]][[j]]) > 1) {
            e <- paste(strs[[i]][[j]][2], subg, sep = '_')
            if (e %in% names(edges)) {
              g <- add.edges(g, c(edges[[e]], idx))
              
            } else{
              edges[e] <- idx
            }
          }
        }
      }
    }
  }
  return(g)
  ###simplified iGraph graph of the complex
  }
