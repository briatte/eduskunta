# eduskunta.fi

root = "http://www.eduskunta.fi"
bills = "data/bills.csv"
sponsors = "data/sponsors_url.csv"

if(!file.exists(bills)) {
  
  b = data.frame()
  for(y in 2014:2009) {
    
    cat("Adding year", y)
    h = GET(paste0("http://www.eduskunta.fi/triphome/bin/thw.cgi/trip/?${BASE}=veps7099&${CCL}=define+merge&${CCL}=define+thesa=vepstesa&${CCL}=define+view+saadkok=saadk,kv_saadk,sopimussarja,kv_sopimussarja&${FREETEXT}=tunniste=$+AND+tunniste=LA+and+vpvuosi=", y, "&${savehtml}=/thwfakta/vpasia/vex/vex.htm&${TRIPSHOW}=html=vex/vex4050+format=vex4050&${MAXPAGE}=501&${SORT}=LAJIT1,LAJIT2+DESC&${COMBOT}=0,2,0#alkukohta"))
    b = rbind(b, data.frame(
      year = y,
      url = xpathSApply(content(h), "//a[contains(@href, '{KEY}=LA+')]/@href"),
      stringsAsFactors = FALSE))
    cat(":", sprintf("%4.0f", nrow(b)), "total bills\n")
    
  }
  write.csv(b, bills, row.names = FALSE)
  
}

b = read.csv(bills, stringsAsFactors = FALSE)
b$authors = NA

for(i in rev(b$url)) {
  
  f = gsub("(.*)LA\\+(\\d+)/(\\d+)", "raw/bill-\\3-\\2.html", i)
  cat(sprintf("%4.0f", which(b$url == i)), f)
  
  if(!file.exists(f)) {
    
    h = try(download.file(paste0(root, "/", i), f, mode = "wb", quiet = TRUE), silent = TRUE)
    
    if("try-error" %in% class(h) | !file.info(f)$size) {
      
      file.remove(f)
      cat(": failed\n")
      
    }
    
  }
  
  if(file.exists(f)) {
    
    t = readLines(f, encoding = "iso-8859-1", warn = FALSE)
    a = which(grepl("name=\"ALLEKOSA\"", t, useBytes = TRUE))
    
    if(length(a)) {
      
      t = t[ 1 + a:length(t) ]
      t = t[ 1:which(grepl("</div>", t, useBytes = TRUE))[1] ]
      t = t[ grepl("<br />", t, useBytes = TRUE) ]
      t = gsub("^\\s| <br />", "", t)
      
      cat(":", length(t), "author(s)\n")
      
      b$authors[ b$url == i ] = paste0(gsub("\\s?(.*)\\s/(\\w+)\\s?", "\\1", t), collapse = ";") #  /\\2
      
    } else {
      
      cat(": empty\n")
      
    }
    
  }
  
}

nrow(subset(b, is.na(authors))) # should leave only very few bills unparsed

b = subset(b, !is.na(authors))
b$n_au = 1 + str_count(b$authors, ";")

# fix HTML special characters
b$authors = gsub("&eacute;", "é", b$authors)
b$authors = gsub("&aring;", "å", b$authors)
b$authors = gsub("&auml;", "ä", b$authors)
b$authors = gsub("&ouml;", "ö", b$authors)
b$authors = gsub("&Ouml;", "Ö", b$authors)

# harmonize both spellings
b$authors = gsub("Eeva Maria Maijala", "Eeva-Maria Maijala", b$authors)

# fix to match sponsor name
b$authors = gsub("Maria Guzenina-Richardson", "Maria Guzenina", b$authors)
b$authors = gsub("Kirsi Ojansuu", "Kirsi Ojansuu-Kaunisto", b$authors)

a = unlist(strsplit(b$authors, ";"))
a = a[ !is.na(a) ]

# scrape sponsors

if(!file.exists(sponsors)) {
  
  s = data.frame()
  
  for(i in 24:0) {
    
    cat(sprintf("%5.0f", i))
    h = htmlParse(paste0("http://www.eduskunta.fi/triphome/bin/thw/trip/?${MAXPAGE}=101&${APPL}=hetekaue&${BASE}=hetekaue&${HTML}=hex/hx4600&${THWIDS}=", 100 * i, ".18/1416059238_25311&${HILITE}=0#alkukohta"), encoding = "UTF-8")
    url = xpathSApply(h, "//a[contains(@href, 'hxnosynk')]/@href")
    name = xpathSApply(h, "//a[contains(@href, 'hxnosynk')]", xmlValue)
    name = scrubber(gsub("&nbsp", "", name))
    year = xpathSApply(h, "//table[2]/tr/td[2]", xmlValue)
    year = scrubber(year[-1])
    s = rbind(s, data.frame(url, name, year, stringsAsFactors = FALSE))
    cat(":", nrow(s), "total MPs\n")
    
  }
  
  s$photo_url = NA
  s$profile_url = NA
  
  # rerun to fix network errors or expand beyond str_extract limit
  l = s$url[ is.na(s$profile_url) & str_extract(s$year, "[0-9]{4}") >= 1970 ]
  for(i in rev(l)) {
    cat(which(l == i))
    h = try(htmlParse(paste0(root, i)), silent = TRUE)
    if(!"try-error" %in% class(h)) {
      s$photo_url[ s$url == i] = xpathSApply(h, "//frame[@name='vasen2']/@src")
      s$profile_url[ s$url == i] = xpathSApply(h, "//frame[@name='oikea2']/@src")
      cat("\n")
    } else {
      cat(": failed\n")
    }
  }
  
  write.csv(s, sponsors, row.names = FALSE)
  
}

s = read.csv(sponsors, stringsAsFactors = FALSE)
n = data.frame()

# download and parse sponsors

l = s$profile_url[ !is.na(s$profile_url) ]
for(i in rev(l)) {
  
  f = gsub("/faktatmp/hetekatmp/", "raw/mp-", i)
  cat(sprintf("%5.0f", which(l == i)), f)
  
  if(!file.exists(f)) {
    
    try(download.file(paste0(root, i), f, quiet = TRUE), silent = TRUE)
    
  }
  
  if(!file.info(f)$size) {
    
    cat(": failed\n")
    file.remove(f)
    
  } else {
    
    h = htmlParse(f, encoding = "UTF-8")
    name = scrubber(xpathSApply(h, "//b[1]", xmlValue))
    mandate = xpathSApply(h, "//b[4]", xmlValue)
    
    party = xpathSApply(h, "//a[contains(@href, 'ekrtunnus')]", xmlValue)
    party_length = length(party)
    party = party[ party_length ]
    
    # dirtier method
    # party = scrubber(xpathSApply(h, "//font[contains(text(), 'Eduskuntaryhm')]/../../..", xmlValue))
    
    if(!length(party))
      party = NA
    
    born = xpathSApply(h, "//font[contains(text(), 'Syntym')]/../../..", xmlValue)
    born = str_extract(born, "[0-9]{4}")
    
    cat(":", name, "\n")
    
    n = rbind(n, data.frame(profile_url = i, name, born, party, party_length, mandate, stringsAsFactors = FALSE))
    
  }
  
}

# all sponsors should be matched

sum(!a %in% n$name)
table(a[ !a %in% n$name ])

# party fixes

table(n$party_length) # ~ 30 fixes to perform: party missing or more than one party
n$party[ is.na(n$party) ] = "?"

# n$party[ n$name == "" ] = "Liberaalisen kansanpuolueen eduskuntaryhmä"

# party abbreviations

n$party[ grepl("Vihreä", n$party) ] = "vihr" # Vihreä liitto
n$party[ grepl("Vasemmistoliiton", n$party) ] = "vas" # Vasemmistoliitto
n$party[ grepl("Sosialidemokraattinen", n$party) ] = "sd" # Suomen Sosialidemokraattinen Puolue
n$party[ grepl("Keskustan", n$party) ] = "kesk" # Suomen Keskusta
n$party[ grepl("Ruotsalainen", n$party) ] = "r" # Ruotsalainen kansanpuolue (RKP)
n$party[ grepl("Kristillisdemokraattinen|Kristillisen", n$party) ] = "kd" # Kristillisdemokraatit, Chr-Dems
n$party[ grepl("Kansallisen kokoomuksen", n$party) ] = "kok" # Kansallinen Kokoomus
n$party[ grepl("Muutos 2011", n$party) ] = "m11" # Muutos 2011
n$party[ grepl("Perussuomalaisten", n$party) ] = "ps" # Perussuomalaiset
n$party[ grepl("Vasenryhmä", n$party) ] = "vr" # Vasenryhmä

# parties below use unofficial abbreviations

# Suomen Kansan Demokraattinen Liitto (SKDL), Finnish People's Democratic League, far-left, 1944-1990, red
n$party[ grepl("kansan demokraattisen liiton", n$party) ] = "kdl"

# Suomen Maaseudun Puolue (SMP), right, 1959-1995, dark blue
n$party[ grepl("maaseudun puolueen", n$party) ] = "mp"

n$partyname = NA
n$partyname[ n$party == "kd" ] = "Kristillisdemokraatit"
n$partyname[ n$party == "kesk" ] = "Keskusta"
n$partyname[ n$party == "kok" ] = "Kansallinen Kokoomus"
n$partyname[ n$party == "m11" ] = "Muutos 2011"
n$partyname[ n$party == "ps" ] = "Perussuomalaiset"
n$partyname[ n$party == "r" ] = "Ruotsalainen kansanpuolue"
n$partyname[ n$party == "sd" ] = "Sosialidemokraattinen Puolue"
n$partyname[ n$party == "kdl" ] = "Kansan Demokraattinen Liitto"
n$partyname[ n$party == "mp" ] = "Maaseudun Puolue"
n$partyname[ n$party == "vas" ] = "Vasemmistoliitto"
n$partyname[ n$party == "vihr" ] = "Vihreä liitto"
n$partyname[ n$party == "vr" ] = "Vasenryhmä"

table(n$partyname, exclude = NULL)
n$party = toupper(n$party)

# convert mandates to n(years)

n$nyears = sapply(n$mandate, function(x) {
  y = scrubber(unlist(strsplit(x, ",")))
  y[ grepl("-$", y) ] = paste0(y[ grepl("-$", y) ], "2014")
  y = sapply(y, function(x) {
    x = unlist(str_extract_all(x, "[0-9]{4}"))
    x = as.numeric(x)
    return(max(x) - min(x))
  })
  return(sum(y))
})

n$sex = NA
n$sex[ grepl("^(Aila|Aino-Kaisa|Ann(a|e|i)|Anu|Arja|Astrid|Christina|Eeva|Eila|Eli(na|sabeth)|Elsi|Hanna|He(idi|li|nna)|Ilkka|Inkeri|Ja(ana|nina|nne)|Johanna|Jutta|Katja|Katri|Kirsi|Kris(ta|tiina)|Laila|Lea |Leena|Lenita|Liisa|Lyly|Ma(arit|ri(a)? |rja(ana)?|rjo)|Merikukka|Merja|Miapetra|Mika |Mikaela)|Mi(nna|rja)|Outi|Päivi|Paul(a|i|iina) |Pia |Pirkko|Raija|Rakel|Riitta|Ritva|Saara|Sann(a|i)|Sari|Satu|Silvia|Sinikka|Sirkka|Sirpa|Sofia|Suna|Susanna|Ta(n|r)ja|Terhi|Tu(ija|ula)|Tuulikki|Tytti|Ulla", n$name) ] = "F"
n$sex[ grepl("^(Alexander|An(ss|tt)i|Ari|Arto|Ben|Bjarne|Eero|Erkki|Es(a|ko)|Håkan|Hann(es|u)|Harri|Heikki|Ismo|Ja(akko|cob|mes|n(i)? |ri)|Johannes|Jörn|Jou(ko|ni)|Juh(a|o) |Ju(kka|ssi)|Jyrki|Kaj|Kalle|Kari |Kauko|Kimmo|Klaus|Kyösti|Lars|Lasse|Lauri|Mark(ku|o|us)|Martti|Mat(s|ti)|Mauri|Mikael|Timo)|Mikko|Oiva|Olli|Oras|Osmo|Paavo|Pekka|Pe(n|r)tti|Peter|Petri|Petteri|Pietari|Raimo|Reijo|Risto|Sampsa|Sauli|Seppo|Simo|Sinuhe|Stefan|Tapani|Tero|Teuvo|Thomas|Timo|Toimi|Tom |Tommy|Tuomo|Unto|Valto|Veijo|Vesa-|Ville", n$name) ] = "M"
n$name[ !n$sex %in% c("F", "M") ]

# merge sponsor URL dataset to details
s = merge(s, n, by = "profile_url")

if(!file.exists("data/sponsors.csv")) {
  
  s$photo = NA
  write.csv(s, "data/sponsors.csv", row.names = FALSE)
  
} else {
  
  s = read.csv("data/sponsors.csv", stringsAsFactors = FALSE)
  
}

# download photos (sponsors only; rerun to solve network errors)
l = s$photo_url[ s$name %in% a & is.na(s$photo) ]
for(i in rev(l)) {
  
  cat(sprintf("%5.0f", which(l == i)))
  h = htmlParse(paste0(root, gsub("hx5000", "hx5100", i)))
  p = xpathSApply(h, "//img[contains(@src, 'jpg')]/@src")
  f = gsub("/fakta/edustaja/kuvat", "photos", p)
  
  if(!file.exists(f))
    download.file(paste0(root, p), f, quiet = TRUE, mode = "wb")
  
  if(!file.info(f)$size) {
    
    cat(": failed\n")
    file.remove(f)
    
  } else {
    
    s$photo[ s$photo_url == i ] = gsub("photos/|\\.jpg$", "", f) # photo ID
    cat("\n")
    
  }
  
}

write.csv(s, "data/sponsors.csv", row.names = FALSE)

# job's done
