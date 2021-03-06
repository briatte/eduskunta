for (ii in unique(b$legislature)) {
  
  cat("Legislature", ii)
  leg = substr(ii, 1, 4)
  
  # subset to cosponsored bills
  data = subset(b, legislature == ii & n_au > 1)
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  # check for missing sponsors
  u = unlist(strsplit(data$authors, ";"))
  u = na.omit(a[ !a %in% s$name ])
  if (length(u)) {
    cat("Missing", length(u), "sponsors:")
    print(table(u))
  }
  
  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================
  
  edges = bind_rows(lapply(data$authors, function(d) {
    
    w = unlist(strsplit(d, ";"))
    
    d = expand.grid(i = s$name[ s$name %in% w ],
                    j = s$name[ s$name == w[1]], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))
  
  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # sanity check
  stopifnot(edges$gsw <= 1)
  
  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)
  
  cat(nrow(edges), "edges, ")
  
  # ============================================================================
  # DIRECTED NETWORK
  # ============================================================================
  
  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = ii
  n %n% "legislature" = c("1999-2002" = "33", "2003-2006" = "34",
                          "2007-2010" = "35", "2011-2014" = "36")[ as.character(ii) ]
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer
  
  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(subset(b, legislature == ii)$n_au)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  # the URLs don't work any more...
  n %v% "url" = s[ network.vertex.names(n), "profile_url" ]
  n %v% "sex" = s[ network.vertex.names(n), "sex" ]
  
  # since the sponsors data contain some missing values, let's make sure all
  # sponsors featured in the networks are covered by the manual imputations
  stopifnot((n %v% "sex") %in% c("F", "M"))
  
  n %v% "born" = s[ network.vertex.names(n), "born" ]
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  s$nyears = sapply(s$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) <= substr(ii, 1, 4))
  })
  n %v% "nyears" = s[ network.vertex.names(n), "nyears" ] %>% as.integer
  n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  
  n %v% "party_length" = as.numeric(s[ network.vertex.names(n), "party_length" ])
  n %v% "constituency_length" = as.numeric(s[ network.vertex.names(n), "constituency_length" ])

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  # ============================================================================
  # SAVE PLOTS
  # ============================================================================
  
  if (plot) {
    
    save_plot(n, paste0("plots/net_fi", ii),
              i = colors[ s[ n %e% "source", "party" ] ],
              j = colors[ s[ n %e% "target", "party" ] ],
              mode, colors)
            
  }
  
  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================
  
  assign(paste0("net_fi", substr(ii, 1, 4)), n)
  assign(paste0("edges_fi", substr(ii, 1, 4)), edges)
  assign(paste0("bills_fi", substr(ii, 1, 4)), data)
  
  # ============================================================================
  # SAVE GEXF
  # ============================================================================
  
  if (gexf)
    save_gexf(n, paste0("net_fi", ii), mode, colors)
  
}

if (gexf)
  zip(paste0("net_fi.zip"), dir(pattern = "^net_fi\\d{4}-\\d{4}\\.gexf$"))
