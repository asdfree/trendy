if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)
options( survey.lonely.psu = "adjust" )

library(survey)

library(lodown)

# retrieve a listing of all available extracts for the european social survey
yrbss_cat <- get_catalog( "yrbss" , output_dir = file.path( path.expand( "~" ) , "YRBSS" ) )

# limit the catalog to only years 2005-2015
yrbss_cat <- subset( yrbss_cat , year %in% seq( 2005 , 2015 , 2 ) )

# download the yrbss microdata
lodown( "yrbss" , yrbss_cat )
# install.packages( c( "segmented" , "ggplot2" , "ggthemes" , "texreg" ) )

# Muggeo V. (2008) Segmented: an R package to fit regression models with broken-line relationships. R News, 8, 1: 20-25.
library(segmented)

library(ggplot2)
library(ggthemes)
library(texreg)

# initiate an empty `y` object
y <- NULL

# loop through each year of YRBSS microdata
for ( year in seq( 2005 , 2015 , 2 ) ){

	# load the current year
	x <- readRDS( 
		file.path( path.expand( "~" ) , "YRBSS" , 
			paste0( year , " main.rds" ) ) )
	
	# tack on a `year` column
	x$year <- year
	
	if( year == 2005 ) x$raceeth <- NA
	
	# stack that year of data alongside the others,
	# ignoring mis-matching columns
	y <- rbind( x[ c( "q2" , "q3" , "q4" , "qn10" , "year" , "psu" , "stratum" , "weight" , "raceeth" ) ] , y )
	
	# clear the single-year of microdata from RAM
	rm( x )
	
}


# convert every column to numeric type
y[ , ] <- sapply( y[ , ] , as.numeric )

# construct year-specific recodes so that
# "ever smoked a cigarette" // grade // sex // race-ethnicity align across years
y <-
	transform(
		
		y ,
		
		rode_with_drunk_driver = qn10 ,
				
		raceeth = 
			
			ifelse( year == 2005 ,
				ifelse( q4 %in% 6 , 1 ,
				ifelse( q4 %in% 3 , 2 ,
				ifelse( q4 %in% c( 4 , 7 ) , 3 ,
				ifelse( q4 %in% c( 1 , 2 , 5 , 8 ) , 4 , NA ) ) ) ) ,
				
				ifelse( raceeth %in% 5 , 1 ,
				ifelse( raceeth %in% 3 , 2 ,
				ifelse( raceeth %in% c( 6 , 7 ) , 3 ,
				ifelse( raceeth %in% c( 1 , 2 , 4 , 8 ) , 4 , NA ) ) ) ) ) ,
				
				
		grade = ifelse( q3 == 5 , NA , as.numeric( q3 ) ) ,
		
		sex = ifelse( q2 %in% 1:2 , q2 , NA )
		
	)
	

# again remove unnecessary variables, keeping only the complex sample survey design columns
# plus independent/dependent variables to be used in the regression analyses
y <- y[ c( "year" , "psu" , "stratum" , "weight" , "rode_with_drunk_driver" , "raceeth" , "sex" , "grade" ) ]

# set female to the reference group
y$sex <- relevel( factor( y$sex ) , ref = "2" )

# set ever smoked=yes // white // 9th graders as the reference groups
for ( i in c( 'rode_with_drunk_driver' , 'raceeth' , 'grade' ) ) y[ , i ] <- relevel( factor( y[ , i ] ) , ref = "1" )

# extract a linear contrast vector of length eleven,
# because we have eleven distinct years of yrbss data `seq( 2005 , 2015 , 2 )`
c6l <- contr.poly( 6 )[ , 1 ]

# also extract a quadratic (squared) contrast vector
c6q <- contr.poly( 6 )[ , 2 ]

# just in case, extract a cubic contrast vector
c6c <- contr.poly( 6 )[ , 3 ]

# for each record in the data set, tack on the linear, quadratic, and cubic contrast value
# these contrast values will serve as replacement for the linear `year` variable in any regression.

# year^1 term (linear)
y$t6l <- c6l[ match( y$year , seq( 2005 , 2015 , 2 ) ) ]

# year^2 term (quadratic)
y$t6q <- c6q[ match( y$year , seq( 2005 , 2015 , 2 ) ) ]

# year^3 term (cubic)
y$t6c <- c6c[ match( y$year , seq( 2005 , 2015 , 2 ) ) ]

# construct a complex sample survey design object
# stacking multiple years and accounting for `year` in the nested strata
des <- 
	svydesign(
		id = ~psu , 
		strata = ~interaction( stratum , year ) ,
		data = y , 
		weights = ~weight , 
		nest = TRUE
	)

# # fake_psu should be a one-record-per-person vector object
# # that can immediately be appended onto your data set.

# fake_psu <- kmeans( your.replicate.weights , 20 )
# des <- svydesign( id = ~ your_fake_psus , strata = ~ year , data = y , weights = ~ weight , nest = TRUE )
# immediately remove records with missing smoking status
des_ns <- subset( des , !is.na( rode_with_drunk_driver ) )

# calculate unadjusted, un-anythinged "ever smoked" rates by year
# note that this reproduces the unadjusted "ever smoked" statistics at the top of
# pdf page 6 of http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf
unadjusted <- svyby( ~ rode_with_drunk_driver , ~ year , svymean , design = des_ns , vartype = c( 'ci' , 'se' ) )

# coerce that result into a `data.frame` object
my_plot <- data.frame( unadjusted )

# plot the unadjusted decline in smoking
ggplot( my_plot , aes( x = year, y = rode_with_drunk_driver1 ) ) +
  geom_point() + 
  geom_errorbar( aes( ymax = ci_u.rode_with_drunk_driver1 , ymin = ci_l.rode_with_drunk_driver1 ) , width = .2 ) +
  geom_line() +
  theme_tufte() +
  ggtitle( "Figure 1. Unadjusted smoking prevalence 1999-2011" ) +
  theme( plot.title = element_text( size = 9 , face = "bold" ) )
linyear <- 
	svyglm(
		I( rode_with_drunk_driver == 1 ) ~ sex + raceeth + grade + t6l , 
		design = subset( des_ns , rode_with_drunk_driver %in% 1:2 ) , 
		family = quasibinomial
	)

summary( linyear )
quadyear <-
	svyglm(
		I( rode_with_drunk_driver == 1 ) ~ sex + raceeth + grade + t6l + t6q , 
		design = subset( des_ns , rode_with_drunk_driver %in% 1:2 ) , 
		family = quasibinomial 
	)

summary( quadyear )
cubyear <-
	svyglm(
		I( rode_with_drunk_driver == 1 ) ~ sex + raceeth + grade + t6l + t6q + t6c , 
		design = subset( des_ns , rode_with_drunk_driver %in% 1:2 ) , 
		family = quasibinomial 
	)
	
summary( cubyear )
htmlreg(list(linyear , quadyear , cubyear), doctype = F, html.tag = F, inline.css = T, 
    head.tag = F, body.tag = F, center = F, single.row = T, caption = "Table 1. Testing for linear trends")
marginals <- 
	svyglm(
		formula = I( rode_with_drunk_driver == 1 ) ~ sex + raceeth + grade ,
		design = des_ns , 
		family = quasibinomial
	)
( means_for_joinpoint <- svypredmeans( marginals , ~factor( year ) ) )
# coerce the results to a data.frame object
means_for_joinpoint <- as.data.frame( means_for_joinpoint )

# extract the row names as the survey year
means_for_joinpoint$year <- as.numeric( rownames( means_for_joinpoint ) )

# must be sorted, just in case it's not already
means_for_joinpoint <- means_for_joinpoint[ order( means_for_joinpoint$year ) , ]

# rename columns so they do not conflict with variables in memory
names( means_for_joinpoint ) <- c( 'mean' , 'se' , 'yr' )
# the above line is only because the ?segmented function (used below)
# does not work if an object of the same name is also in memory.

another_plot <- means_for_joinpoint
another_plot$ci_l.mean <- another_plot$mean - (1.96 * another_plot$se)
another_plot$ci_u.mean <- another_plot$mean + (1.96 * another_plot$se)

ggplot(another_plot, aes(x = yr, y = mean)) +
  geom_point() + 
  geom_errorbar(aes(ymax = ci_u.mean, ymin = ci_l.mean), width=.2) +
  geom_line() +
  theme_tufte() +
  ggtitle("Figure 2. Adjusted smoking prevalence 1999-2011") +
  theme(plot.title = element_text(size=9, face="bold"))
ggplot( means_for_joinpoint , aes( x = yr , y = mean ) ) +
	geom_point( aes( size = se ) ) +
	theme_tufte() +
	ggtitle( "Figure 3. Standard Error at each timepoint\n(smaller dots indicate greater confidence in each adjusted value)"
) 
means_for_joinpoint$wgt <- with( means_for_joinpoint, ( mean / se ) ^ 2 ) 
# estimate the 'starting' linear model with the usual "lm" function using the log values and the weights.
o <- lm( log( mean ) ~ yr , weights = wgt , data = means_for_joinpoint )
# the segmented() function uses a random process in its algorithm.
# setting the random seed ensures reproducibility
set.seed( 2015 )

# add a segmented variable (`yr` in this example) with 1 breakpoint
os <- segmented( o , ~yr )

# `os` is now a `segmented` object, which means it includes information on the fitted model,
# such as parameter estimates, standard errors, residuals.
summary( os )
# figuring out the breakpoint year was the purpose of this joinpoint analysis.
( your_breakpoint <- round( as.vector( os$psi[, "Est." ] ) ) )
# so.  that's a joinpoint.  that's where the two line segments join.  okay?

# obtain the annual percent change (APC=) estimates for each time point
slope( os , APC = TRUE )
# calculate a five-timepoint linear contrast vector
c3l <- contr.poly( 3 )[ , 1 ]

# tack the five-timepoint linear contrast vectors onto the current survey design object
des_ns <- update( des_ns , t3l = c3l[ match( year , seq( 2005 , 2009 , 2 ) ) ] )

pre_91_99 <-
	svyglm(
		I( rode_with_drunk_driver == 1 ) ~ sex + raceeth + grade + t3l ,
		design = subset( des_ns , rode_with_drunk_driver %in% 1:2 & year <= 2009 ) , 
		family = quasibinomial
	)

summary( pre_91_99 )
# calculate a seven-timepoint linear contrast vector
c4l <- contr.poly( 4 )[ , 1 ]

# tack the seven-timepoint linear contrast vectors onto the current survey design object
des_ns <- update( des_ns , t4l = c4l[ match( year , seq( 2009 , 2015 , 2 ) ) ] )

post_99_11 <-
	svyglm(
		I( rode_with_drunk_driver == 1 ) ~ sex + raceeth + grade + t4l ,
		design = subset( des_ns , rode_with_drunk_driver %in% 1:2 & year >= 2009 ) , 
		family = quasibinomial
	)
	
summary( post_99_11 )
htmlreg(list(pre_91_99, post_99_11), doctype = F, html.tag = F, inline.css = T, 
    head.tag = F, body.tag = F, center = F, single.row = T, caption = "Table 2. Linear trends pre-post changepoint")
