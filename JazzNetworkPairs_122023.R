rm(list = ls())
# see earlier versions of this code for details on process
# csv file created with discogger
# e.g.:  dc<-discogs_artist_releases(262127) #Bobby Timmons
library(jsonlite)
library(discogger);library(tidyverse)
library(expss);library(tidyverse)
source("Z:GENERAL/R/BensConvenienceFunctions.r")


# read data ---------------------------------------------------------------


# current token = secret
discogger::discogs_api_token()

discogs_search(params=list(artist="Art Blakey"),n_results = 10)
myartist<-"KennyDrew"
myartistid<-251779
# 145263 dexter gordon

dc<-discogs_artist_releases(265380) 
# 10620 mccoy tyner #23744miles #96442 Bennygreen
# sonny rollins  145264; 224506=ornette coleman

dc<-dc$content
dc2<-dc %>% dplyr::filter(!is.na(id) ) %>%  # big change over previous ver.
  group_by(title) %>% 
  summarise(  release_code=first(id),    artist=first(artist),
    year=first(year), role=first(role),type=first(type),    label=first(label)) %>%
  dplyr::filter(role=="Main")

# simple timeline for one artist
artist<-unique(word(dc2$artist,1,2))
ggplot(dc2, aes(year,2,label=str_wrap(title,25))) +
  geom_point()+
  ggrepel::geom_text_repel(angle=33, nudge_y = .4,size=3)+
  ylim(0,3)+
  labs(title=paste0("Releases by ",artist))


              # single album
              df<-fromJSON(paste0("https://api.discogs.com/releases/", as.character(15259402)))
              Undercurrent<-Undercurrent %>% cbind(df$id, df$title,df$year,   # select key vars for release
                                                   df$artists$name,df$artists$id,
                                                   df$extraartists$name,
                                                   df$extraartists$role,
                                                   df$extraartists$id)  %>% as.data.frame()
              class(Undercurrent)
              Undercurrent<-bind_rows(Undercurrent) 
              colnames(Undercurrent)[2:8]<-c("releaseid", "title","year","leadArtist","albumid","extraArtists","role")



rlist<-as.list(dc2$release_code)
rlist<-Filter(Negate(anyNA), rlist) # remove NAs from list

# each  `fullReleaseData` will have n rows, one for each of the album Credits (roughly)
# example, "A Minor Scamble" has 9 individuals who contribute
# there are typically 7 columns of data in each release, sometimes 5
# personnel are usuually the 6 element:  albumdata[[30]][[6]]

# also, see rate limiting: https://www.discogs.com/developers/#page:home,header:home-rate-limiting

# create full artist release function -------------------------------------

fullReleaseData<-list()
fullRelease <- function(rlist){
  for(i in 1: length(rlist) ){
    frset <- try(
      fromJSON(paste0("https://api.discogs.com/releases/", as.character(rlist[i])))
    )
    if(inherits(frset, "try-error"))
    {
      #error handling code
      next
    }
    
    # continue if no error
    message("Retrieving page ", i)
    Sys.sleep(4) # deal with rate limit; five second delay
    fullReleaseData[[i+1]] <- 
      cbind(frset$id, frset$title,frset$year,   # select key vars for release
            frset$artists$name,frset$artists$id,
            frset$extraartists$name,
            frset$extraartists$role,
            frset$extraartists$id) %>% as.data.frame()    } 
}



albumdata<-fullRelease(rlist) # how to set the accessor index well?
df<-dplyr::bind_rows(albumdata) # make as dataframe
colnames(df)[1:7]<-c("releaseid", "title","year","leadArtist","albumid","extraArtists","role")

df<-df %>% dplyr::filter(leadArtist==myartist)

#save results as csv (improve this process)

write.csv(df,paste0('K:/bac/R/jazzDbase/jazzalbums_ornetteColeman',Sys.Date(),'.csv'),
          row.names = F)


# open allRels ------------------------------------------------------------
list.files(path='K:/bac/R/jazzDbase/', pattern = "rels_")
jClean<-read.csv("K:/bac/R/jazzDbase/rels_DexterGordon.csv")
jClean<-read.csv("K:/bac/R/jazzDbase/rels_StuWilliamson.csv")
myartist<-"Stu Williamson"

# merge 'type' using releaseID
# open saved file and begin prep for network analysis
#jClean<-read.csv("K:/bac/R/jazzDbase/jazzalbums_ornetteColeman2023-06-01.csv",skip = 0)
#jClean<-read.csv("K:/bac/R/jazzDbase/jazzalbums_bgreen2020-12-04.csv",skip = 0)
#jClean<-read.csv("K:/bac/R/jazzDbase/jazzalbums_McCoy Tyner82.csv",skip = 2)
#jClean<-read.csv("K:/bac/R/jazzDbase/jazzalbums_Miles Davis5.csv",skip = 2)
jClean$X<-NULL

# some data are stuck in column headers!
colnames(jClean)[1:8]<-c("release_code", "title","year","leadArtist","albumid","extraArtists","role","extraArtistID")
jClean$n<-NULL

allrels<-jClean 

# data cleaning
allrels$role<-ifelse( str_detect(allrels$role, "Saxo"),"Saxophone",
  ifelse( str_detect(allrels$role,"Engineer"),"Engineer",
  ifelse( str_detect(allrels$role,"Guitar"),"Guitar",
  ifelse( str_detect(allrels$role,"Drums"),"Drums",
  ifelse( str_detect(allrels$role,"Produ"),"Producer",
  ifelse( str_detect(allrels$role,"Photo"),"Photography",
  ifelse( str_detect(allrels$role,"Trump"),"Trumpet",
  ifelse( str_detect(allrels$role,"Piano"),"Piano",
  allrels$role))))))))

# there is an issue with the extraartist role being something other 
# than an instrument so  keep band members only
instruments<-paste(c("Piano","Drums","Sax","Bass","Guitar","Bass","Cello","Cornet","Trumpet",
                    "Keyboards", "Marimba","Strings", "Orchestra",  "Vibra","Vocals","Featuring"),collapse = "|")
allrels$band<-ifelse(str_detect(allrels$role, instruments), 1,0) # flag row if member of band

allrels<-allrels[allrels$band==1, ]   # NOTE: this might not validly include all musicians

# the extraArtists column appears to have most accurate instrument data
allrels %>% group_by(extraArtists) %>% summarise(instrument=first(role)) %>% htmlTable()

allrels$role<-factor(allrels$role)

# found that grouping on albumid too restrictive and that grouping on album name less so
allrels$title<-ifelse(allrels$title=="NA",NA,allrels$title)
# remove cases with only one albumid  & if title is missing
allrels<-allrels %>% dplyr::group_by(title) %>% mutate(nalb=n())


## LIMIT BY LEAD ARTIST & nalb
# note: discogs does NOT report a unique number of albums in its list of releases!
allrels2<-allrels %>% dplyr::filter(nalb>0 & !is.na(title)  &
          leadArtist==myartist) %>%
  filter(leadArtist!=extraArtists)


# consider dropping compilations!!!  these confuse the network analysis

# save role, title, year etc to another df to merge instruments back in
ins<-allrels2[ ,c("extraArtists",'release_code','role','title','year','band')]

# compute all combinations of artist relations
Pairs = matrix("", nrow=0, ncol=2)
for(AID in unique(allrels2$title)) {
  Selector <- allrels2$title == AID                    # if albumid=AID is TRUE, make the pair
  Artists <- unique(c(allrels2$leadArtist[Selector], 
                      allrels2$extraArtists[Selector]))
  Pairs <- rbind(Pairs, t(combn(Artists, 2)))       # generate all combos of elements of `x`` taken `m` at a time
}

Pairs<-as.data.frame(Pairs);
View(Pairs)
# create node and edge files
#edges
colnames(Pairs)[1]<-'leadArtist';colnames(Pairs)[2]<-'extraArtists'
#examine
table(duplicated(paste(Pairs$leadArtist,Pairs$extraArtists)))  # there should be no duplicates!
Pairs %>% filter(str_detect(extraArtists,"Watkins"))

# create nodes
n1<-Pairs[1];colnames(n1)[1]<-'artist';
n2<-Pairs[2];colnames(n2)[1]<-'artist'; # "from" and "to" respectively
jnodes<-rbind(n1,n2)

jnodes<-jnodes %>% group_by(artist) %>% summarise(nj=n())

# the merge returns `release_code` to the lead artist
# all=TRUE was important!
jnodes2<- merge(jnodes,ins,by.x="artist",by.y="extraArtists",all=TRUE)  # basically, add album id and instrument

library(data.table)
jnodes2<-unique(jnodes2,by="artist" )
jnodes2<-jnodes2 %>% group_by(artist) %>% 
  summarise(year=first(year),role=first(role),albumid=first(release_code),band=first(band),title=first(title))
jnodes2$year<-as.numeric(jnodes2$year)
jnodes2<-jnodes2 %>%group_by(artist) %>% mutate(artistCodebasic=cur_group_id()) # simple sequential id for Gephi
jnodes2<-jnodes2 %>%group_by(title) %>% mutate(titleCodebasic=cur_group_id()) # simple sequential id for Gephi

naniar::vis_miss(jnodes2)


# plot the networks -------------------------------------------------------


library(igraph)
colnames(Pairs)[1:2]<-c("from","to") # for directed network
jplot<-graph_from_data_frame(d=Pairs, vertices = jnodes2, directed = TRUE )

## dropping producers, arrangers, cover artists, etc 
## needs to occur after making the igraph object
#jplot2<-map(jplot, names(jplot), ~ filter(band == 1))

#jplot2<-map(jplot,  ~ filter(band == 1))

#jnodes2<-jnodes2 %>% dplyr::filter(band==1)    


par(mar=c(1,1,2,1))
degree(jplot);gsize(jplot);gorder(jplot)

E(jplot)$weight<-1 # set weight to 1
V(jplot)$label2<-substr(ifelse(degree(jplot)>mean(degree(jplot)),V(jplot)$name,NA),1,9)
V(jplot)$onedegree<-ifelse(degree(jplot)==1,"One Degree","Other")  # nodes with degree=1
V(jplot)$color2<-ifelse(degree(jplot)<2,"green","pink")  # node color by degree
V(jplot)$label<-enc2utf8( V(jplot)$name)

setwd("K:/bac/R/jazzDbase")
igraph::write_graph(jplot, file=paste0("gephi_",myartist,".graphml"),
                    format = c("graphml") )


write.graph(jplot, file = paste0("K:/bac/R/jazzdBase/",myartist,".gml"), format = "GML")

degree(jplot,mode='out')
degree(jplot,mode='in')

V(jplot)$degree<-degree(jplot)/mean(degree(jplot))

par(mar=c(.1,.1,.1,.1))
set.seed(987)
plot(jplot,
     vertex.label.dist=1,     vertex.label.cex=.8,
    vertex.size=.8,
    # UNRESOLVED: role/instrument/assignment
    #  vertex.color = jazzpal[as.numeric(as.factor(vertex_attr(jplot, "role")),alpha.f=.61)],
    vertex.color = adjustcolor('blue',alpha=.4),
     remove.multiple=TRUE,
    edge.curved=.2,     edge.curved=.2, edge.arrow.size=.4,
    edge.color="blue",
    layout=layout.fruchterman.reingold)


# color nodes by instrument
library(RColorBrewer) # https://stackoverflow.com/questions/49076441/set-igraph-vertex-color-based-on-vertex-attribute-using-rcolorbrewer
jazzpal <- brewer.pal(length(unique(V(jplot)$role)), "Spectral")
#jazzpal<-colorFactor(palette="Spectral",domain = V(jplot)$role)
#htmlTable(fre(V(jplot)$role))

# https://rstudio-pubs-static.s3.amazonaws.com/341807_7b9d1a4e787146d492115f90ad75cd2d.html
legend_cats <- data.frame(attr = unique(vertex_attr(jplot, "role")),
                          color = jazzpal)

legend_cats
legend_cats <- legend_cats[order(legend_cats$attr), c(1, 2)]


legend(x = "bottomleft",      ## position, also takes x,y coordinates
       legend = legend_cats$attr, ## legend symbols see ?points
       col = legend_cats$color, cex=.8, bty = "n", pch = 19,
       y.intersp = .81,       title = "Instruments")


# bifurcate by year
earlier <-jplot -  subset(V(jplot), V(jplot)$year < 2004)   # another way to delete edges
later <-jplot -  subset(V(jplot), V(jplot)$year >= 2004)   # another way to delete edges
par(mfrow=c(1,2),mar=c(.4,.4,.4,.4))
plot(earlier,     vertex.label.dist=1,     vertex.label.cex=.6, vertex.color=adjustcolor("blue",alpha=.2),
     edge.width=E(jplot)$weight )
plot(later,     vertex.label.dist=1,     vertex.label.cex=.6, vertex.color=adjustcolor("green",alpha=.2),
     edge.width=E(jplot)$weight, main="After 1977")
par(mfrow=c(1,1))


# bifurcate plots more
as<-authority_score(jplot, weights = NA)$vector
par(mfrow=c(1,2))
plot(jplot,     edge.curved=.2,
     vertex.size=as*50,main="Authorities")

plot(jplot,     edge.curved=.2,
     layout=layout_nicely,main="Hubs")
par(mfrow=c(1,1))


#
# Communities
#
net.sym <- as.undirected(jplot, mode= "collapse", 
                         edge.attr.comb=list(weight="sum", "ignore"))

ceb <- cluster_edge_betweenness(jplot) 
plot(ceb, jplot)
plot(ceb, jplot, vertex.label.cex=degree(jplot)/100) 
cfg <- cluster_fast_greedy(as.undirected(jplot))
set.seed(333)
plot(cfg, jplot, main="Greedy Optimization of Modularity",
     vertex.label.cex=degree(jplot)/100) 

set.seed(333)
plot(cfg, jplot, main="Greedy Optimization of Modularity",
     vertex.label.dist=1,     vertex.label.cex=.6) 

# Louvain Community Detection
cluster <- cluster_louvain(net.sym)

cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

#Create group column
jnodes2$label<-jnodes2$leadArtist
nodes <- left_join(jnodes2, cluster_df, by = "label")
colnames(nodes2)[3] <- "group"


### Using visNetwork
library(visNetwork)
#edges$dashes<-"TRUE"
visNetwork(nodes,edges) %>%
  visEdges(color='black', arrows = "from") 


vn<-toVisNetworkData(jplot, idToLabel = TRUE)
nodes<-jsonlite::rbind_pages(vn[1]) # extract info into nodes
edges<-jsonlite::rbind_pages(vn[2])
nodes$color<-ifelse(nodes$role=="Piano","blue",
                    ifelse(nodes$role=="Trumpet","red", 
                           ifelse(nodes$role=="Drums","green",ifelse(nodes$role=="Bass","black",NA))))
#edges$value<-3

setwd("K:/bac/R/jazzdBase")
#write.csv(nodes,'nodes.csv')
#write.csv(edges,'edges.csv')
nodes$label<-jnodes2$leadArtist
visNetwork(nodes, edges, height = "500px") %>%
  visNodes(size = 22) %>%
  visEdges(width=21) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T) %>%  
  visPhysics(maxVelocity = 70,solver = "repulsion")

visSave(gg, file="mcCoyTyner.html",selfcontained = TRUE)
#%>%  visLegend(addNodes = nodes$role)


# to make GIF or animated network plot, see Animation for `majick` package
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#animation




# Using D3 ----------------------------------------------------------------


library(networkD3)

tbl_df_strip <- function(x) {
  if('tbl_df' %in% class(x)) {
    message(paste(deparse(substitute(x)),
                  'is a tbl_df. Converting to a plain data frame.'))
    x <- base::as.data.frame(x)
  }
  return(x)
}

Pairs<-tbl_df_strip(Pairs)
jnodes2<-tbl_df_strip(jnodes2)

simpleNetwork(Pairs,
              fontSize = 11,linkDistance = 144,
              linkColour = "tan",nodeColour = "mediumblue",opacity = .9)


forceNetwork(Links=Pairs, Nodes=jnodes2, Source="leadArtist",Target = "extraArtists",
  NodeID = "leadArtist", Group="albumid")
