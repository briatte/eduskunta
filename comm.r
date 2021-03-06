# add committee co-memberships

sponsors = list.files("raw", pattern = "mp-(.*)\\.htm$", full.names = TRUE)
raw = data_frame()

for (i in sponsors) {
  
  h = htmlParse(i, encoding = "UTF-8")
  n = c(
    xpathSApply(h, "//font[contains(text(), 'toimieli')]/../../..//li[contains(text(), 'kunta')]", xmlValue),
    xpathSApply(h, "//font[contains(text(), 'toimieli')]/../../..//li[contains(text(), 'kunnan')]", xmlValue),
    xpathSApply(h, "//font[contains(text(), 'toimieli')]/../../..//a[contains(text(), 'kunta')]", xmlValue),
    xpathSApply(h, "//font[contains(text(), 'toimieli')]/../../..//a[contains(text(), 'kunnan')]", xmlValue)
  )
  n = unlist(n)

  if (length(n))
    raw = rbind(raw, data_frame(i, n))
  
}

# 25% of committees are time-invariant

raw$n = gsub("(\\w|\\s])+?(\\d)(.*)", "\\1", raw$n)
raw$n = gsub("[[:punct:]|[:digit:]]|\\((\\w|\\s)+\\)", "", raw$n)
raw$n = str_clean(raw$n)
raw$n[ which(raw$n == "") ] = "Eduskunnan pankkivaltuusmiehet" # small bug, n = 2

# save flat list
write.csv(summarise(group_by(raw, n), members = n()),
          "data/committees.csv", row.names = FALSE)

raw$i = gsub("raw/mp-", "", raw$i)

comm = data_frame(u = unique(raw$n))

# add sponsor columns
for (i in sponsors)
  comm[, gsub("raw/mp-", "", i) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$u %in% raw$n[ raw$i == i ])

stopifnot(gsub(paste0(root, "/faktatmp/hetekatmp/"), "", s$profile_url) %in% names(comm[, -1]))

# assign co-memberships to networks
for (i in ls(pattern = "^net_fi")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  names(sp) = gsub(paste0(root, "/faktatmp/hetekatmp/"), "", names(sp))
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ , names(sp) ]
  
  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  set.edge.attribute(nn, "committee", e$committee)
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))
  
  set.edge.attribute(n, "committee", e$committee)
  assign(i, n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)
  
}
