###################################
#0. Init
###################################

#removing previous displays devices
while( length(dev.list())!=0){
	dev.off()
}



#loading required libraries
library(ggplot2)
library(reshape2)
library(animation)

library(RColorBrewer)
library(gridExtra)



###########################
# file paths
#########################"

path_to_simu_results <- "/tmp/mariusmodel_log.csv"
path_to_empirical_data <- "tmp/darius.csv"


##########################################
#1. Simulations results loading
##################################### 

#parsing csv file
load_simu_results <- function(){
	file_simu <- path_to_simu_results

	# parsing simulation log
	pop_simu <- read.csv(file_simu, sep=",", dec=".", header=TRUE)
	# get the lines of the csv file
	mylines <- readLines(file_simu)
	# determining the last step (-1 because of headers in csv file)
	laststep <- pop_simu [length(mylines)-1,1] 
	cat("Simulation log of",length(mylines)-1,"lines and ",laststep ," steps. Should be ", (length(mylines)-1)/(laststep+1),"lines per step  \n")
	# verify if steps have the same line number
	if (length(mylines)-1 !=  (laststep + 1)* 1145  ){
		cat("##uneven number of lines per step ##\n")	
	}
	return (pop_simu)
}

# creating dataframe of simulation results
# in this case, last_step is 30
create_dataframe_dsim <-function(last_step){
	pop_simu <- load_simu_results()
	dsim <- list()

	#adding AROKATO (cities' ID) as the first column of dsim list
	dsim[1]<-subset(pop_simu,step==0)[2]
	names(dsim)[1] <- "arokato"
	
	

	#dividing the whole pop_simu dataframe into chunks by step number
	for (i in 0:last_step) {
		temp_step <- subset(pop_simu,step==i)
		#removing  step number and arokato column from the chunk
		temp_step$step <- NULL
		temp_step$arokato <-NULL
		#naming columns for wealth and population by year
		names(temp_step)[1] <- paste("pop",1959 +i,sep="" )
		names(temp_step)[2] <- paste("wealth",1959 +i,sep="" )
		#adding columns of the current chunk to the list
		dsim[[i+2]] <-  temp_step
	}
	return (dsim)
}

#######################################
# 2. Empirical Data loading
#########################################

load_data_darius <-function(){
	#file parsing
	file_data <- path_to_empirical_data
	rdata <- read.csv(file_data, sep=",", dec=".", header=TRUE)

	#drop useless columns
	rdata$pays <-NULL
	rdata$type <-NULL
	rdata$lat <-NULL
	rdata$long <-NULL
	rdata$SITU <-NULL
	rdata$CAPNAT <-NULL
	rdata$typharris1963 <-NULL
	rdata$T5970 <- NULL
	rdata$T7079 <-NULL
	rdata$T7989 <-NULL	
	rdata$T8902 <-NULL
	rdata$T0210 <-NULL

	return (rdata)
	
}

############################################
# 3. merging  simulation results and empricial data 
#	- dpop is the final merged dataframe
#	- the column containing population of year yyyy in the simulation results is denoted "dpop$popyyyy"
#	- the column containing population of year yyyy in the empirical data is  denoted "dpop$Xyyyy"
############################################

merge_simu_data<-function(last_step){
	cat("load simu results...\n")
	simu <- create_dataframe_dsim(last_step)
	cat("load data\n")
	data <- load_data_darius()
	cat("merging...\n")
	dpop <- merge (simu, data, by.x="arokato", by.y="AROKATO")
	return (dpop)
}



##############################################
# 4. Results vizualisation (static)
###########################################

# Basic data to results comparison , for each year data is provided
comparison_data_simu <-function(dpop)
{
	par(mfrow=c(3,1))
	#plot pop initial state
	plot(log(dpop$pop1959)~log(dpop$X1959), col="blue" ,main="", xlab="", ylab="")
	abline(a = 0, b = 1, col = "green")
	title(main="Initial state simulation versus data", xlab="log(simulated pop)", ylab="log(pop wrt data)")

	#plot pop for intermediary state
	plot(log(dpop$pop1970)~log(dpop$X1970), col="blue" ,main="", xlab="", ylab="")
	abline(a = 0, b = 1, col = "green")
	title(main="Intermediary state (1970)  simulation versus data ", xlab="log(simulated pop)", ylab="log(pop wrt data)")

	#plot pop final state (1989)
	plot(log(dpop$pop1989)~log(dpop$X1989), col="blue" ,main="", xlab="", ylab="")
	abline(a = 0, b = 1, col = "green")
	title(main="Final state simulation versus data ", xlab="log(simulated pop)", ylab="log(pop wrt data)")
}


# single city population serie 
single_city_popserie_by_AROKATO <-function(ID, dfsimu){
poptimeseries <-list()
poptimeseries <- subset(dfsimu, arokato==ID)
return(poptimeseries )
}

#returns the N biggest cities  in 1989 
N_biggest_cities_1989 <-function(N,dfsimu){
dpop_sorted<-dfsimu[order(dfsimu$pop1989, decreasing=TRUE),]$arokato
return(head(dpop_sorted,N))
}

#displays trajectories of the N biggest cities in 1989
display_N_traj <-function(N, dfsimu)
{
	Ncities <- N_biggest_cities_1989(N, dfsimu)
	#treating the biggest city separately for convenience (assuming it's Moscow
		

	pp <- ggplot()

	pp <- pp + scale_color_hue()
	
	rawresults <-load_simu_results()

	for (k in Ncities[1:N]){
		 pop_city_k <- subset(rawresults, arokato == k) 
		cat(k,match(k,Ncities),"\n")
		pp <- pp+ 
		geom_line( aes(x= step+ 1959,y=population*1000),data=pop_city_k, color= match(k,Ncities),  alpha=.8)
	}
	pp <- pp + labs(title=paste(N," biggest cities trajectories", sep="" ), x="year", y="population")
	print(pp)
}


#######################################
# 5. Dynamic visualization (using gifs)
######################################




# display evolution of wealth and population of the whole system in a log(population) 
make_pop_wealth_gif <- function (laststep=30, dfsimu){
 
	rawresults <- load_simu_results()
	listminmax<- global_maxmin_popwealth(rawresults)

	popmin <- listminmax[1]
	popmax <- listminmax[2] 
	wealthmin <-  listminmax[3]
	wealthmax <- listminmax[4]

	#Set delay between frames when replaying
	ani.options(interval=.2)
	mypalette <- rainbow(31)
	#Begin animation loop
	saveGIF({
	  layout(matrix(c(1, rep(2, 5)), 6, 1))
  
	  # Adjust the margins a little
	  par(mar=c(4,4,2,1) + 0.1)
	    #drawing loop 
	for (j in 0:laststep) {
      
	      # Reset the color of the top chart every time (so that it doesn’t change as the 
	      # bottom chart changes)
	      par(fg=1)
      
	      # Set up the top chart that keeps track of the current frame/iteration
	      # Dress it up a little just for fun
		tick <- j
	      plot(-5, xlim = c(1,laststep+1), ylim = c(0, .3), axes = FALSE, xlab = "", ylab = "", main = "step")
	      abline(v=tick, lwd=2, col = rgb(0, 0, 255, 255, maxColorValue=255))
      
	      # Bring back the X axis
	      axis(1)
  
		xseries <- df[2*j + 2]
		yseries <- df[2*j + 3]
		matplot( log(xseries),log(yseries) ,type="p", pch="o", main = "log pop vs log wealth", xlab = "log(pop)" ,  ylab="log(wealth)", col="orange",  xlim=c(log(popmin),log(popmax)), ylim=c(log(wealthmin),log(wealthmax)), add=FALSE)
 	
	}#drawingloop	
    	},movie.name="pop_wealth.gif", ani.height=600, ani.width=600) #SaveGIF
}


# rank-size animation  in four quadrants  display
#	- DATA is shown in shades of blue (dark blue is 1959, light blue is 1989, shades in between are for 1970 and 1979)
#	- RESULTS are shown in orange
#	- upper left quadrant is for the top 20 of the hierarchy 
#	- upper right quadrant is for the middle of the hierarchy (from the 20th to the 500th city)
#	- lower left quadrant is for the bottom of the hierarchy (100th to 1145th city)
#	- lower right quadrant shows the whole hierarchy + a linear model fitting the results
#	- see rank_size_date for details on display

four_quadrants_rank_size_zoom <- function(dpop, superposeDARIUS='TRUE', footprint='TRUE'){

	ani.options(interval=.2)
	ani.options(outdir = getwd())
	saveGIF({

		for (i in 1959:1989){
		cat("generating	rank size for",i,"\n") 
		rank_size_date(i,dpop)
		}
	
	}	
	,movie.name="rank_size_zoom_cat.gif", ani.height=800, ani.width=800
	)#SaveGIF
}


# tool function to handle multiplotting
# this function has been taken from the web , google it for source
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
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



# display the total population evolution over time
display_total_pop <-function(laststep, dfsimu){
	# gathering the simulated sums
	dates <- seq(1959,1989,1)
	popsums <- c()
	for( i in 1:laststep + 1){
		popsums[i] <- sum(dfsimu[2*i])
	}

	dfsum <- data.frame(dates,popsums)


	poptot <- ggplot(dfsum,aes()) +
	geom_line(aes(x=dates , y= popsums * 1000 ),color="green" )+
	labs(title="Total population from 1959 to 1989", x="year", y="total population")

print(poptot)

}




# display the rank-size distribution for a date between 1959 ansd 1989
rank_size_date<-function(date, dpop){
	dates <- seq(1959,1989,1)
	datestring <-paste("pop",sep="",dates)

	rankseries <- paste("rank(-",datestring,")",sep="")
	popseries <- paste(datestring,"*1000",sep="")

	i <- match(date,dates)
	

	rg <- rankseries[i]
	popop <-  popseries[i]

pp <- ggplot(dpop,aes()) +
geom_line( aes(x= rank(-(X1959)), y= X1959*1000, color=1959 ))  +
 geom_line( aes(x= rank(-(X1970)),y=X1970*1000, color=1970)) +
geom_line(aes(x= rank(-(X1979)),y=X1979*1000, color=1979)) +
geom_line(aes(x= rank(-(X1989)),y=X1989*1000, color=1989)) +
geom_line(aes_string(x= rg, y= popop ),color="orange" ) +
scale_x_log10(limits=c(1,20))+
 scale_y_log10(limits=c(1000000,20000000)) +
  xlab("rank (log scale)") +
  ylab("size (log scale)")+
  cale_colour_continuous(guide="none") +
labs(title=(" rank 1 to 10 distribution")) 

pp2 <- ggplot(dpop,aes()) +
geom_line (aes(x= rank(-X1959), y= X1959*1000, color=1959 ))  +
 geom_line( aes(x= rank(-X1970),y=X1970*1000, color=1970    )) +
geom_line(aes(x= rank(-X1979),y=X1979*1000, color=1979 )) +
geom_line(aes(x= rank(-X1989),y=X1989*1000, color=1989 )) +
geom_line(aes_string(x= rg, y= popop ), color="orange") +
scale_x_log10(limits=c(20,500)) +
 scale_y_log10(limits=c(100000,1000000)) +
  xlab("rank (log scale)") +
  ylab("size (log scale)")+
  scale_colour_continuous(guide="none") +
labs(title=(" rank 20 to 500 distribution")) 

pp3 <- ggplot(dpop,aes()) +
geom_line( aes(x= rank(-X1959), y= X1959*1000, color=1959 ))  +
 geom_line( aes(x= rank(-X1970),y=X1970*1000, color=1970)) +
geom_line(aes(x= rank(-X1979),y=X1979*1000, color=1979)) +
geom_line(aes(x= rank(-X1989),y=X1989*1000, color=1989)) +
geom_line (aes_string(x= rg, y= popop), color="orange") +
scale_x_log10(limits=c(100,1145)) +
 scale_y_log10(limits=c(10000,100000)) +
  xlab("rank (log scale)") +
  ylab("size (log scale)")+
  scale_colour_continuous(guide="none") +
labs(title=(" rank 100 to 1145 distribution")) 
#scale_colour_hue(guide="none")


pp4 <- ggplot(dpop,aes()) +
geom_line( aes(x= rank(-X1959), y= X1959*1000, color=1959 ))  +
 geom_line( aes(x= rank(-X1970),y=X1970*1000, color=1970)) +
geom_line(aes(x= rank(-X1979),y=X1979*1000, color=1979)) +
geom_line(aes(x= rank(-X1989),y=X1989*1000, color=1989)) +
geom_line (aes_string(x= rg, y= popop), color="orange") +
stat_smooth(method="lm", se=FALSE,aes_string(x= rg, y= popop), color="red")+
scale_x_log10(limits=c(1,1145)) +
 scale_y_log10(limits=c(10000,20000000)) +
  xlab("rank (log scale)") +
  ylab("size (log scale)")+
  scale_colour_continuous(guide="none")+
labs(title=("whole range rank-size") )

layout <- matrix(c(1,2,3,4), nrow=2,byrow=TRUE)

print(multiplot(pp,pp2,pp3, pp4, layout=layout))


}


full_range_rank_size <- function(date, dfsimu){

	dates <- seq(1959,1989,1)
        datestring <-paste("pop",sep="",dates)

        rankseries <- paste("rank(-",datestring,")",sep="")
        popseries <- paste(datestring,"*1000",sep="")

        i <- match(date,dates)


        rg <- rankseries[i]
        popop <-  popseries[i]



	pp4 <- ggplot(dfsimu,aes()) +
	geom_line( aes(x= rank(-X1959), y= X1959*1000, color=1959 ))  +
 	geom_line( aes(x= rank(-X1970),y=X1970*1000, color=1970)) +
	geom_line(aes(x= rank(-X1979),y=X1979*1000, color=1979)) +
	geom_line(aes(x= rank(-X1989),y=X1989*1000, color=1989)) +
	geom_line (aes_string(x= rg, y= popop), color="orange") +
	stat_smooth(method="lm", se=FALSE,aes_string(x= rg, y= popop), color="red")+
	scale_x_log10(limits=c(1,1145)) +
 	scale_y_log10(limits=c(10000,20000000)) +
	xlab("rank (log scale)") +
	ylab("size (log scale)")+
	scale_colour_continuous(guide="none")+
	labs(title=(paste("Rank-size distribution in", date, sep="") ))


	print(pp4)


}






#################################
# Tool function for max/pin population/wealth
###########################################

#return the global population over simlulations steps and data
global_maxmin_popwealth <-function(rawresults){
	pmax <- max(rawresults$population)
	pmin <- min(rawresults$population)
	wmax <- max(rawresults$wealth)
	wmin <- min (rawresults$wealth)

	return(c(pmin,pmax,wmin,wmax)) 

}


########################################
# 6. basic use of the script
#######################################"

laststep <- 30
df <- merge_simu_data(laststep)

# display top 12 trajectories
display_N_traj(12,df)
# display rank size distribution in 1870
dev.new()
full_range_rank_size(1970,df)
# display total population
dev.new()
display_total_pop(laststep, df)

#generate gifs
four_quadrants_rank_size_zoom(df)


