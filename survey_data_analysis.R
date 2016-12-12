
install.packages(c("ggplot2", "wordcloud"))
library("ggplot2")
library("wordcloud")
library("stringr")
library("RColorBrewer")
library("raster")
library("stringi")


###########################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




count_entries <- function(input_vector){
  number_entries <- length(which(input_vector!=""))
  return(number_entries)
}


tabulate_answers <- function(yes_no_question_vector){
  remove_unanswered <- subset(yes_no_question_vector, yes_no_question_vector!="")
  if(is.factor(remove_unanswered)){
    remove_unanswered <- droplevels(remove_unanswered)
  }
  num_answers <- length(remove_unanswered)
  remove_unanswered <- ordered(remove_unanswered, levels=c("YES", "NO", "I don't know"))
  counts <- summary(remove_unanswered)
  freq <- round(counts/num_answers*100, 0)
  names(freq) <- c("Yes", "No", "Don't know")
  return(freq)
}

split_names <- function(names_vector, first=TRUE){
  split_names <- sapply(names_vector, FUN=strsplit, split="__")
  first_part <- unlist(lapply(split_names, "[[", 1))
  first_part <- unname(first_part) %>% str_replace_all(pattern="_", replacement=" ") %>% str_replace_all(pattern="\\.", replacement=" ")
  second_part <- unlist(lapply(split_names, "[[", 2))
  second_part <- unname(second_part) %>% str_replace_all(pattern="_", replacement=" ") %>% str_replace_all(pattern="\\.", replacement=" ")
  second_part <- str_replace_all(second_part, pattern="important 3 years", replacement="important in 3 years") %>% str_replace_all(pattern="important", replacement="Important")
  if (first) {
    return(first_part)
  } else {
    return(second_part)
  }
}

###########################
# International Survey Results
###########################

int_dat <- read.csv("international_survey_results.txt", header=TRUE, sep="\t")

levels(int_dat$Country)

#exclude Australian respondents

int_dat <- subset(int_dat, int_dat$Country!="Australia")
int_dat$Country <- droplevels(int_dat$Country)

#fix spelling mistake
int_dat$Country <- as.factor(stri_replace_all_fixed(int_dat$Country, replacement="Slovenia", pattern="Slovania"))
small_number_respondents <- names(which(summary(int_dat$Country)<=3))

small_or_no_country <- which(int_dat$Country%in%c("", small_number_respondents))

replacement_country <- as.character(int_dat$Country)
replacement_country[small_or_no_country] <- "Other"
summary_country <- as.data.frame(summary(as.factor(replacement_country)))
summary_country$Country <- rownames(summary_country)
colnames(summary_country) <- c("Freq", "Country")
summary_country <- summary_country[order(summary_country$Freq, summary_country$Country),]


### Figure 1 ###

fig1_filename <- "Figure_1_country_int_survey_piechart.tif"

tiff(file=fig1_filename, width=4, height=4, units="in",
     res=300, bg=rgb(1, 1, 1, 0.25, maxColorValue=1))
par(mar=c(1,1,1,1)+0.1)
pie(summary_country$Freq, 
  labels=paste(summary_country$Country, " (", summary_country$Freq, ")", sep=""),
  cex=0.45, col=brewer.pal(n=4, "Blues"), border = FALSE)
dev.off()



pdf(file="Figure_1_country_int_survey_piechart.pdf", width=8, height=8,
     bg=rgb(1, 1, 1, 0.25, maxColorValue=1))
#par(bg=rgb(1, 1, 1, 0.1, maxColorValue=1))
pie_adapted(summary_country$Freq, 
            labels=paste(summary_country$Country, " (", summary_country$Freq, ")", sep=""),
            cex=0.75, col=brewer.pal(n=4, "Blues"), bg=rgb(1, 1, 1, 0.25, maxColorValue=1))
dev.off()

###########################
# National Survey Results
###########################

dat <- read.csv("survey_responses.txt", header=TRUE, sep="\t")

colnames(dat)

levels(dat$State)

# remove a couple of apparent non-Australian researchers

dat <- subset(dat, dat$State%in%c("France", "Nepal", 
"Currently living in Brazil / graduated MSc. in Melbourne (VIC)")==FALSE)
dat$State <- droplevels(dat$State)
levels(dat$State)

# remove some nonspecific Institute entries
dat$Institute[which(dat$Institute=="Biological Sciences")] <- ""
dat$Institute[which(dat$Institute=="University")] <- ""
dat$Institute <- droplevels(dat$Institute)

#############################

datatypes <- dat[,10:16]
colnames(datatypes) <- c("DNA, RNA, protein sequence", "DNA, RNA, protein structure", 
                         "Pathways, interactions, networks", "Images", "Phenotype", "Ecological", "Other")

datatype_counts <- apply(datatypes, MARGIN=2, FUN=count_entries)
dat_co <- data.frame("Type"=names(datatype_counts), "Counts"=datatype_counts)
dat_co <- dat_co[rev(order(dat_co$Counts, dat_co$Type)),]
dat_co$Type <- factor(dat_co$Type, levels=rev(dat_co[order(dat_co$Counts), 1]))
dat_co$Freq <- dat_co$Counts/nrow(dat)*100
type_labels <- str_wrap(dat_co$Type, 5)

plotcol <- brewer.pal(n=7, "Blues")
plotcol[1] <- "#FFFFFF"

datatype_plot <- ggplot(dat_co, aes(x=Type, y=Freq, fill=Type)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=plotcol) +
  xlab("Type of Data") +
  ylab("% responses") +
  scale_x_discrete(labels=type_labels) +
  #ylim(c(0, 130)) +
  theme(axis.ticks = element_blank(), legend.position = "none",
        panel.grid = element_blank(), panel.background=element_rect(colour=rgb(1, 1, 1, 0.25, maxColorValue=1)))
  
multitype_counts <- apply(datatypes, MARGIN=1, FUN=count_entries)
multitype_xtab <- xtabs(~multitype_counts)
multitypes <- as.data.frame(multitype_xtab)
colnames(multitypes) <- c("Number_Datatypes", "Counts")
multitypes$Freq <- multitypes$Counts/sum(multitypes$Counts)*100

multitype_plot <- ggplot(multitypes, aes(x="", y=Freq, fill=Number_Datatypes)) + 
  geom_bar(width=1, stat="identity") +
  scale_fill_brewer(palette="Blues") +
  coord_polar("y", start=0) +
  ylab("% responses") +
  xlab("") +
  theme(legend.title = element_text(size=6), legend.text=element_text(size=6))

tiff("Figure_5_national_survey_Datatype_counts.tif", width=8, height=4, units="in", res=300)
datatype_plot
dev.off()

tiff("Figure_6_national_survey_Number_of_datatypes.tif", width=4, height=4, units="in", res=300,
     bg=rgb(1, 1, 1, 0.25, maxColorValue=1))
par(mar=c(0, 2, 0, 3)+0.1)
pie(multitypes$Counts, 
    labels=paste(multitypes$Number_Datatypes,
                 " ", 
                 c("datatype", rep("datatypes", times=4)), " (", multitypes$Counts, ")", sep=""),
    cex=0.7, col=brewer.pal(n=5, "Blues"), border=FALSE)
dev.off()

############################

domains <- dat[,62:94]

domain_counts <- apply(domains, MARGIN=2, FUN=count_entries)
dom_co <- data.frame("Domain"=names(domain_counts), "Counts"=domain_counts)
dom_co$Freq <- dom_co$Counts/nrow(dat)*100
#exclude domains with 0 counts
dom_co <- subset(dom_co, dom_co$Counts>=1)

dom_co$Domain <- stri_replace_all_fixed(dom_co$Domain, replacement=" ", pattern=".")
dom_co$Domain <- stri_replace_all_fixed(dom_co$Domain, replacement=" ", pattern="_")

tiff("Figure_4_national_survey_Domain_wordcloud.tif", width=6, height=6, units="in", res=300,
     bg=rgb(1, 1, 1, 0.25, maxColorValue=1)
     )
wordcloud(words=dom_co$Domain, freq=dom_co$Freq/100, 
          rot.per=0.4, random.order = FALSE, random.color=TRUE,
          col=terrain.colors(length(dom_co$Domain), 
                             alpha=0.9))
dev.off()

############################

geog <- dat[,102:103]

geog_tab <- as.data.frame(xtabs(~geog$State))
colnames(geog_tab) <- c("State", "Freq")
geog_tab <- geog_tab[order(geog_tab$Freq, geog_tab$State),]

geog_pie <- ggplot(geog_tab, aes(x="", y=Freq, fill=State)) + 
    geom_bar(width = 1, stat="identity") + 
    coord_polar("y", start=0) +
    scale_fill_brewer(palette="Set2") +
    ylab("Number of responses") +
    xlab("")

levels(geog$Institute)

Inst_coarse <- as.character(geog$Institute)
{Inst_coarse[grep("delaide", geog$Institute)] <- as.character("University of Adelaide")
Inst_coarse[grep("elbour", geog$Institute)] <- as.character("University of Melbourne")
Inst_coarse[grep("Bio21", geog$Institute)] <- as.character("University of Melbourne")
Inst_coarse[grep("AGRF", geog$Institute)] <- as.character("AGRF")
Inst_coarse[grep("ANU", geog$Institute)] <- as.character("Australian National University")
Inst_coarse[grep("anu", geog$Institute)] <- as.character("Australian National University")
Inst_coarse[grep("ational", geog$Institute)] <- as.character("Australian National University")
Inst_coarse[grep("Plant Functio", geog$Institute)] <- as.character("ACPFG")
Inst_coarse[grep("ACPFG", geog$Institute)] <- as.character("ACPFG")
Inst_coarse[grep("Monash", geog$Institute)] <- as.character("Monash University")
Inst_coarse[grep("Deakin", geog$Institute)] <- as.character("Deakin University")
Inst_coarse[grep("Australian Synchrotroh", geog$Institute)] <- as.character("Australian Synchrotron")
Inst_coarse[grep("of Sydney", geog$Institute)] <- as.character("University of Sydney")
Inst_coarse[grep("New South Wales", geog$Institute)] <- as.character("University of New South Wales")
Inst_coarse[grep("UNSW", geog$Institute)] <- as.character("University of New South Wales")
Inst_coarse[grep("Garvan", geog$Institute)] <- as.character("Garvan Institute of Medical Research")
Inst_coarse[grep("UTAS", geog$Institute)] <- as.character("University of Tasmania")
Inst_coarse[grep("Doherty", geog$Institute)] <- as.character("Doherty Institute")
Inst_coarse[grep("UTS", geog$Institute)] <- as.character("University of Technology Sydney")
Inst_coarse[grep("UQ", geog$Institute)] <- as.character("University of Queensland")
Inst_coarse[grep("Institute for molecular biosciences", geog$Institute)] <- as.character("University of Queensland")
Inst_coarse[grep("Queensland", geog$Institute)] <- as.character("University of Queensland")
#Inst_coarse[grep("Murdoch Childrens Research Institute", geog$Institute)] <- as.character("MCRI")
Inst_coarse[grep("Murdoch Childrens Research Insititute", geog$Institute)] <- as.character("Murdoch Childrens Research Institute")
Inst_coarse[grep("MCRI", geog$Institute)] <- as.character("Murdoch Childrens Research Institute")
Inst_coarse[grep("Cook", geog$Institute)] <- as.character("James Cook University")
Inst_coarse[grep("Griffith", geog$Institute)] <- as.character("Griffith University")
Inst_coarse[grep("Tasmania", geog$Institute)] <- as.character("University of Tasmania")
Inst_coarse[grep("University of Western Australia", geog$Institute)] <- as.character("University of Western Australia")
Inst_coarse[grep("UWA", geog$Institute)] <- as.character("University of Western Australia")
Inst_coarse[grep("University of Western Australia", geog$Institute)] <- as.character("University of Western Australia")
Inst_coarse[grep("WEHI", geog$Institute)] <- as.character("Walter and Eliza Hall Institute")
}

insts <- as.data.frame(xtabs(~as.factor(Inst_coarse)))
colnames(insts) <- c("Institute", "freq")

small_institutes <- insts[which(insts$freq <=2), "Institute"]
small_or_no_institute <- which(insts$Institute%in%c("", as.character(small_institutes)))

insts$Replacement_Institute <- as.character(insts$Institute)
insts$Replacement_Institute[small_or_no_institute] <- "Other"

collapsed_insts <- insts[which(insts$Replacement_Institute!="Other"), c("freq", "Institute")]
collapsed_insts$Institute <- as.character(collapsed_insts$Institute)
collapsed_insts <- rbind(collapsed_insts, c(sum(subset(insts, insts$Replacement_Institute=="Other")$freq), "Other"))
collapsed_insts$freq <- as.numeric(collapsed_insts$freq)
collapsed_insts <- collapsed_insts[order(collapsed_insts$freq, collapsed_insts$Institute),]
inst_labels <- str_wrap(collapsed_insts$Institute, width = 20)

#insts$freq <- insts$freq/sum(insts$freq)

tiff("Figure_2_state_national_survey_piechart.tif", width=4, height=4, units="in", res=300,
     bg=rgb(1, 1, 1, 0.25, maxColorValue=1))
par(mar=c(1,1,1,1)+0.1)
pie(geog_tab$Freq, 
    labels=paste(geog_tab$State, " (", geog_tab$Freq, ")", sep=""),
    cex=0.5, col=brewer.pal(n=4, "Blues"), border=FALSE)
dev.off()

# jpeg("Institute_wordcloud.jpg", width=20, height=20, units="cm", res=300)
# wordcloud(words=insts$Institute, freq=insts$freq, 
#           rot.per=0.4, random.order = FALSE, random.color=TRUE,
#           col=terrain.colors(length(insts$Institute), 
#                                          alpha=0.9))
# dev.off()

tiff(file="Figure_3_institute_national_survey_piechart.tif", width=4, height=4, units="in",
     res=300, bg=rgb(1, 1, 1, 0.25, maxColorValue=1))
par(mar=c(1,2,1,3)+0.1)
pie(collapsed_insts$freq, 
            labels=paste(inst_labels, " (", collapsed_insts$freq, ")", sep=""),
            cex=0.5, col=brewer.pal(n=4, "Blues"), border=FALSE)
dev.off()

##################

res_needs <- dat[,19:30]
training_needs <- dat[,45:59]
share_needs <- dat[,31:42]

try_all <- t(apply(res_needs, MARGIN = 2, tabulate_answers))
research_need <- split_names(rownames(try_all), first=TRUE)
question <- split_names(rownames(try_all), first=FALSE)
rownames(try_all) <- NULL

out1 <- data.frame(Research_need=research_need, Question=question, try_all)
out1[,1] <- as.character(out1[,1])
colnames(out1)[3:5] <- c("Yes (%)", "No (%)", "Don't know (%)")
out1[which(duplicated(out1[,1])),1] <- ""

write.table(out1, file="Table_2_research_needs.txt", sep="\t", row.names=FALSE, quote=FALSE)

###

share <- t(apply(share_needs, MARGIN=2, tabulate_answers))
share_need <- split_names(rownames(share), first=TRUE)
question3 <- split_names(rownames(share), first=FALSE)
rownames(share) <- NULL

out3 <- data.frame("Sharing type"=share_need, Question=question3, share)
out3[,1] <- as.character(out3[,1])
colnames(out3)[c(1,3:5)] <- c("Data Need", "Yes (%)", "No (%)", "Don't know (%)")
out3[which(duplicated(out3[,1])),1] <- ""

write.table(out3, file="Table_3_data_needs.txt", sep="\t", row.names=FALSE, quote=FALSE)


###

train <- t(apply(training_needs, MARGIN=2, tabulate_answers))
train_need <- split_names(rownames(train), first=TRUE)
question4 <- split_names(rownames(train), first=FALSE)
rownames(train) <- NULL

out4 <- data.frame("Training type"=train_need, Question=question4, train)
out4[,1] <- as.character(out4[,1])
colnames(out4)[c(1,3:5)] <- c("Training", "Yes (%)", "No (%)", "Don't know (%)")
out4[which(duplicated(out4[,1])),1] <- ""

write.table(out4, file="Table_4_training_needs.txt", sep="\t", row.names=FALSE, quote=FALSE)

