# 	Model of club goods with positive externalities
#	cMLS Fishery Model
 
# Karthik Panchanathan and Tim Waring
# 09.04.14
 
#-----------------------> 
# NOTES

# Things to consider for later

	# 	Capture figures (maybe 2 per run - one for groups, one for the pop)
	#	for each run through the simulation (i.e., each replicate and 
	#	combination of parameter values). Display the parameter values on
	#	the graphic, so people can easily see what it being modeled. Refer
	#	to the different parameters with letters and subscripts. This can
	#	be something that can be toggled on/off for publication. The title
	#	of the figure should be the row number from the data frame which
	#	houses parameter value combinations.
	
	#	I need to understand the literature on bio-economics. Maybe start 
	#	with Clark's book. We need to think about how our cultural and
	#	group-structured approach adds to the current literature on
	#	fisheries. What is the current state-of-the-art? What, if anything,
	#	do we gain by thinking about multi-level selection and/or cultural
	#	evolution.

	#	Consider changing our voting rules. If a vote is passed, maybe the 
	#	restriction lasts for X years, rather than re-voting each year.
	
	# 	Vary group size. N=10, 20, 50, 100. With bigger groups, it will be
	#	much harder to drift to an institution.
	
	#	Change the parameter input structure. Have this drawn either from
	#	a CSV file or build the matrix right inside the simulation. 
	#	Either way, we can eliminate the bulky loop structure (i.e., one
	#	loop variable for each loop parameter. Instead, we construct a
	#	data frame housing all the parameter combinations. Then we just
	#	loop through the rows of this data frame -- one loop.
	
	#	Clean up and modularize the code. 
		
		#	Can we separate functions into separate files? Going along 
		#	with this, we might put in different options for functions.
		#	For example, for "reproduction", we might allow for (1) an
		#	Allee effect, (2) logistic growth, (3) a Ricker model.
		
		#	Have a call to the function that gathers the parameter values.
		
		#	The "main program" file should be really minimal. The user
		#	shouldn't be overwhelmed.
		
		#	Put notes and version history in separate files.
		
		#	With all of this, how do you store old files? Maybe no date
		#	stamp on the most recent file. When a new file is created, put
		#	today's date on the last file?

	#	Compute Price covariance statistics for two traits.

	# 	Optimize the code. Swap vectors for loops. Operate on the whole
	#	group instead of just one person at a time.
	
	# 	Fix the model when there is 1 person per group. Right now, there seems
	#	to be some bug.
	
	#	Add a refuge level of resources. Right now, when we set resource
	#	emigration to zero, a group that over-harvests their resources can
	#	not rebound. We might take some fraction of the resources, maybe
	#	of the carrying capacity, and not allow them to be fished. That
	#	means a group cannot literally go extinct.

	# 	Re-think what we record to the CSV file and for which years. That is,
	#	how do we know our simulation is working? What do we want to know from
	# 	our simulation?

	#	Do we need a way of re-booting a population? Right now, the harvest
	# 	the individuals take is based on K, not on N. That means that if
	#	they are below N_MSY, they will over-harvest to extinction, even
	#	if they are trying to be good stewards of their resources. We could
	#	periodically refresh a population to K. Or, we could make the shares
	#	based on the population size. That is, if the population is below the
	#	level of N_MSY, fisherman will not harvest (if they have one of the
	#	MSY strategies). This violates a desire stated below in which 
	#	individuals do not know the science of population dynamics. Maybe 
	#	institute a "refuge level" as Tim has done in a previous model.

	# 	Density-dependent emigration rates? Emigration matrix with spatial 
	#	structure? This will be matched with a spatially-structured case.
	#	In the matrix, each row is a group and each column is a group. A
	#	cell represents the emigration from the row group to the column
	#	group. That means that each row adds up to 1. But, there is not
	#	restriction on the column sums.
	
	# 	Introduce costs. Institutions can be costly to institute and monitor.
	#	Should we think about making these costly? If cooperation is too easy,
	#	we should.
	

# Miscellany

	#	Our baseline payoffs have two interpretations. One, they represent 
	#	something like welfare subsidies. Two, we can think about these as 
	#	policy interventions. By varying the baseline, we can see if this has an 
	#	effect on sustainability. It could be that more subsidies harm or hurt 
	#	sustainability.
	
	#	Need to think more carefully about parameter values and relative rates 
	#	(e.g., how fast should reproduction be? dispersal?).
			
	 
# MAXIMUM SUSTAINABLE YIELD
 
# What is the maximum sustainable yield in this model? We have births, deaths (due to harvesting), immigration, and emigration. Here, recruitment due to immigration is affected by the harvesting strategies of other groups. We'd like to compute an MSY for each group. This allows us to easily move into a model in which r and K vary across groups.
 
# First, suppose that a monopolist owns the resources across all groups. How would he set his MSY in each group? Let's call this (mMSY for monopolist's maximum sustainable yield).
 
# Second, suppose that everyone else has exhausts their resources. This means that there is no recruitment due to immigration. Local sustainability is still possible so long as the group doesn't count on recruitment by immigration. Let's call this the conservative MSY (cMSY for conservative maximum sustainable yield).
 
# We should compute mMSY and cMSY for each group. This might be information that an ecologist provides groups with.

# We need to provide harvest levels for fishermen. These harvest levels could be related to cMSY and mMSY. Or, they could just be fractional shares. For example, if K is the carrying capacity (and people know this), we might set harvest levels with respect to K. A harvest level of 0.3 would correspond to removing 30% of the carrying capacity each time period. Ideally, I think it would be good to allow harvest levels to emerge, and these harvest levels are not pre-specified with respect to cMSY and mMSY. We are asking whether any kind of sustainability can emerge from this model. In this case, we would probably want to allow a range of harvest levels. For example, we might have 11 levels (0, 0.1, 0.2, ... , 0.9, 1.0).

# To keep things simple at first, we'll allow three harvest levels, corresponding to cMSY, mMSY, and complete exploitation.

# 1. cMSY. This assumes no recruitment due to immigration, but loss due to emigration. In our model, we don't assume emigrants can settle in their natal patch. The change in population size is given by:

	# dN/dt = ( 1 - e ) * rN * ( 1 - N/K ) - H
	
# Here, H represents the harvest and *e* is the per individual probability of emigration. 

# To find the maximum sustainable yield, we want to find the population size at which the yield (i.e., number of individuals born minus emigrants) is maximized. The change in population size due to demography (not harvesting) is:

	# dN/dt = ( 1 - e ) * rN * ( 1 - N/K )
	# dN/dt = ( 1 - e ) * rN - rN^2/K
	
# If we take the derivative with respect to N and set to 0, we find the population size that maximizes yield (N_cMSY):

	# N_cMSY = ( 1 - e ) * K/2

# If we set e=0, we recover the standard result with a single population

	# N_cMSY = K/2
	
# Now, we compute the harvest (H_cMSY) associated with this population size.

	# H_cMSY = ( 1 - e ) * rN_cMSY * ( 1 - N_cMSY/K )

	# H_cMSY = ( 1 - e ) * rK/4 
	
# Setting e=0, we recover the standard result: H_MSY = rK/4.

# Since our harvests are fractional shares of the carrying capacity, we we calculate cMSY like this:

	# cMSY = H_cMSY / K
	
	# cMSY = ( 1 - e ) * r/4


# 2. mMSY. This assumes that losses due to emigration are offset by gains due to immigration. This then is the same as the standard MSY calculation. Namely:

	# H_mMSY = rK/4
	
	# mMSY = r/4


#-----------------------> 
# VERSION HISTORY


# 11.19.15 - model modifications

	#	Made logistic growth parameters (r,K) functions of patch and year. 
	#	This allows us to explore temporal variation in resource stock growth.
	
	#	Re-thought the statistics we gather, now including Price decomposition,
	#	correlation between harvest preference and group restriction, and gini
	
	# 	Fixed a bug in the mutation function.
		
	
# 11.18.15 - model modifications

	#	added a date stamp to output file
	
	#	Changed code to allow for groups of size 1 and isolated groups.
	#	Rather than re-code the model to make the metapopulation just one
	#	group, we're forcing the user to always have more than 1 group.
	#	We can get the same result by setting resource diffusion to zero
	#	and out-group imitation to zero. To allow for groups of size 1,
	#	we are running a check at the beginning of a simulation run so
	#	that in-group imitation is set to 0 when the number of individuals
	#	in a group is 1.
	
	#	Changed resource diffusion. Previously, our model assumed that only
	#	juveniles emigrated. Now, we can handle either this way or allowing
	#	all individuals (juveniles and non-juveniles) to disperse in each
	#	time period. It shouldn't really make a difference which assumption
	#	we use, but allowing non-juveniles to disperse means we get resource
	#	equilibration through diffusion really fast.
	
	#	Changed harvest levels that fishermen can choose from. Rather than
	#	basing these levels on various notions of MSY, we are allowing
	#	the fishermen to choose a fraction q, between 0 and 1. We also changed
	#	the interpretation of this fraction q. Previously, this was a fraction 
	#	of the carrying capacity. Now, q represents the fraction of the current
	#	per-capita resource stock level. This brings in a level of adaptiveness
	#	in fishing (i.e., they take less fish when the stock level is lower).
	#	This may make a different information assumption. Here, q represents
	# 	the fraction of fish taken that are encountered by the fishermen. We
	#	assign fish to the fishermen in a different procedure. In this way,
	#	it's not clear that fishermen "know" what the stock levels are; they 
	#	just catch a certain fraction based on encounter rate. This can be
	#	interpreted as if the fishermen choose different levels of harvest
	#	efficiency. But, we are assuming that there is no cost to raising
	# 	efficiency (i.e., it costs the same to fish at q=0.3 as q=0.8).
	
	# 	Changed group voting restrictions. Previously, we had this as some kind
	#	of fractional level. Now, we just have a YES/NO vote that each person.
	# 	If there are enough yes votes for a restriction, then we find a harvest
	# 	level that suits the required plurality.			


# 11.10.15 - changed diffusion + updated MSY calculations

	# 	Previously, we allowed all individuals to emigrate in any given
	# 	year. This meant that, if the probability of emigration is larger
	# 	than the rate of reproduction, the population could decline without
	# 	immigration offsets. In this version, we consider a different life
	#	history, one in which individuals disperse in their first year (or
	# 	don't), and then settle down. Now, the probability of emigration
	#	only affects yearlings. This probability can now range between 0
	#	and 100% without fear of crashing the population.
	
	# 	Previously, we had three notions of Maximum Sustainable Yield (MSY):
	#	cMSY, mMSY, and rMSY. cMSY (conservative) assumes all other groups
	# 	have depleted their resource stock (i.e., assumes no recruitment due
	#	to immigration. mMSY (monopolist) assumes all groups will sustainably
	# 	manage their resource stock (i.e., assumes a monopolist controls the
	# 	harvest decisions for all groups). When all groups have equal rates
	#	of reproduction (r) and carrying capacities (K), this is equivalent
	#	to MSY for a single group. rMSY (rosy) assumed that all other groups
	#	were at cMSY, allowing the focal group a bit more to harvest. I'm 
	#	cutting this out. Why bother with it? Why not just have the third
	#	harvest level be ruthless exploitation (i.e., take everything you
	#	can)? We want to see whether this fate can be avoided, so why not
	#	explicitly model it. rMSY was strange anyway. Why would someone 
	#	assume everyone else was at this and not, say, at zero harvesting?
	#	This will also simplify the model.


# 11.08.15 - model modifications

	#	made the loop parameters defined as variables at the top of the
	#	code so that the user can easily specify the parameters to explore
	
	#	added a loop parameter that controls the number of replicates for
	#	each set of parameter values
	
	# 	added two additional outputs - we now record both the mean harvest
	# 	levels and the mean harvest fractions, same for group restrictions
	

# 11.07.15 - model modifications

	# 	added a user-defined variable which sets the number of 
	#	harvest preference levels and group restriction norms
	
	# 	change mutation function so we don't have to loop through groups
	# 	and individuals
	
	# 	change mutation function so that mutations are increments or 
	# 	decrements, not a bigger jump

	# 	Changed group voting restriction. Now, people vote for a specific
	# 	harvest level threshold, as before. But, their votes count for lower
	# 	harvest thresholds. For example, if there are three people in the group
	# 	and their votes are 10%, 30%, and 50%, there is no majority vote. 
	# 	However, the person asking for a limit to 10% would prefer 30% to 50% 
	#	and 50% to 100%. So, we have two people who vote for 30% and this gets
	# 	instituted.
	
	# 	write output to a file


# 11.04.15 - Model modifications 

	#	add loops to run across parameters
	

# 11.03.15 - Model modifications 

	# 	added on/off toggle to each function (e.g., harvest, diffusion)
	# 	added on/off toggle for each graphical output (e.g., stock, preference)


# 10.25.15 - Model modifications

	# 	added baseline fitness
	# 	changed graphical output showing resource stock for each group each year
	# 	checked code for resource reproduction function
	

# 10.08.14 - Added graphical output of model


# 09.29.14 - Started writing model code

	# 	wrote function to find Moore neighbors for each group
	#	wrote function to update resource stock levels
	# 	wrote function to handle resource diffusion
	# 	wrote function to harvest resources
	# 	wrote function for imitation
	# 	wrote function for mutation

 
# 09.04.14 - Wrote the model in pseudocode
 
 
#----------------------->
# MODEL DESCRIPTION
 
# The purpose of this model is to study the emergence of cooperative resource management institutions when resources are club goods (i.e., rival, excludable) at the group level , but some resources leave the group and enter other groups. The less a group harvests its resource, the more of that resource diffuses into other groups (i.e., positive externality). We imagine a situation like a fishery in which some fraction of the juvenile population remains in the natal patch, while the remainder disperse to other patches.
 
# The ecology is naturally broken up into discrete and non-overlapping patches. The patches are arranged on a lattice with neighboring groups. (We're not what the structure of this lattice is or whether there are edges to it.) A group of individuals have exclusive access to a patch in which the resource exists. Each individual within a group makes a private decision about how much to harvest (right now, we're fixing this at one of three discrete harvest levels: LOW, MEDIUM, HIGH). We imagine these levels to be differences in input or technology (e.g., line fishing vs. trawling).
 
# The resource reproduces according to a logistic function, defined by two parameters: r = growth rate, K = carrying capacity. After reproduction, some fraction of the resource population emigrates from the natal patch and settles down in the other patches. In each time period, the resource population within a patch grows due to reproduction and immigration and shrinks due to harvesting and emigration. With some fixed probability, an emigrant moves to a neighboring patch; and with the complementary probability, the emigrant moves to a randomly-selected patch (including the natal patch?). When the probability of local emigration is set to 0, we have an ISLAND MODEL; when it is set to 1 we have a lattice model with only local diffusion.
 
# In addition to choosing a private harvest level, each individual casts a vote on whether to enact group-level restrictions on harvesting. Individuals have three options: (1) everyone harvests at the LOW level, (2) everyone harvests as the MEDIUM level, or (3) individuals harvest at their preferred level. Individuals cast their vote and then we follow some institutional rule (e.g., plurality, majority, unanimity). If the votes cross the threshold, then that system is put into place for the group.
 
# Individuals periodically revise their strategies (i.e., harvest level, group-level restriction). With a fixed probability, an individual chooses a role model from within his group. With a different probability, he chooses a role model from another group. When choosing a role model from another group, with a fixed probability, the individual chooses a role model from a neighboring group; with the conserve probability, he chooses a role model from a randomly-selected group (excluding the natal group, right?). When this probability of local imitation is set to 0, we have an ISLAND model when it comes to imitation; when it is set to 1, we have a LATTICE MODEL with only local imitation. The justification for this parameter would be differences in social structure. It could be that there is a lot of social interaction across patches; or it could be that all social interaction happens within patches. During imitation, the focal individual compares his payoff to the role model's. If the target has a higher payoff, the focal individual imitates the behavior of her role model with some probability. The justification for imitation is a bit tricky. Individuals can observe outcomes, but not inputs. It must be the case that there are easy-to-observe differences in inputs. This means that we're thinking about something like technology (e.g., line fishing vs. trawling) as opposed to effort level.
 
# Our model doesn't track the life history of individuals. We only model dynamics of individual harvest strategies and conservation norms. We're thinking about a relatively developed economy in which an individual's livelihood doesn't have a huge impact on their life history (e.g., harvesting more won't translate into more kids; harvesting too little won't lead to an early death). We are interested in the management of the resource, not how the resource feeds back to life history and demography.
 
#-----------------------> 
# LIBRARIES

library( ineq )		# allos for GINI index


#-----------------------> 
# PARAMETERS

reproduction_toggle			<- TRUE		# toggles resource reproduction on/off
diffusion_toggle			<- TRUE		# toggles resource diffusion on/off
harvest_toggle				<- TRUE		# toggles resource harvest on/off
imitation_toggle			<- TRUE		# toggles strategy imitation on/off
mutation_toggle				<- TRUE		# toggles strategy mutation on/off
 
main_graphic_toggle 		<- TRUE		# toggles main graphical output on/off
stock_graphic_toggle		<- TRUE		# toggles resource stock lines on/off	
preference_graphic_toggle	<- TRUE		# toggles harvest pref lines on/off
restriction_graphic_toggle	<- TRUE		# toggles restriction votes lines on/off
stint_graphic_toggle		<- TRUE		# toggles implementation of restriction 
										# lines on/off
harvest_graphic_toggle		<- TRUE		# toggles total harvest lines on/off

record_data_toggle			<- TRUE	# toggles output to file on/off


num_ind = 20							
										# number of individuals per group

sqrt_num_group = 5						
										# Square root of the number of groups.
										# The algorithm that finds the Moore 
										# neighborhood for each group assumes 
										# the groups to be arranged in a square.
										# So, the user specifies the size of 
										# each side of that square with this.
										
										# NOTE: The model assumes that there is
										# more than 1 group. If we want to study
										# isolated groups, then set emigration 
										# to 0 and out-group imitation to 0.
						

num_group = sqrt_num_group ^ 2			# number of groups. 

num_year = 1000						# simulation length


r = 0.9                              	# rate of resource reproduction in a 
										# logistic growth model
 
K = 10000                            	# carrying capacity in a logistic growth 
										# model
 
r_patch = matrix( data=r , nrow=num_year , ncol=num_group )
										# rate of resource stock reproduction 
										# for each patch for each year
 
K_patch = matrix( data=K , nrow=num_year , ncol=num_group )    		
										# carrying capacity for each patch
										# for each year
 
voting_rule = 0.51                   	# Quorum needed institute a maximum 
										# harvest for the group.
										   
mutation_rate = 0.01                  	# Rate at which strategy changes by 
	 									# chance. If there is a mutation, the
	 									# strategy increments or decrements by
	 									# one unit. Mutations for harvest 
	 									# preferences and group restriction
	 									# votes are independent.

harvest_level = seq( from=0 , to=1 , by=0.1 )

										# Vector storing the harvest 
										# efficiencies available to fishermen. 
										# These can be interpreted as different 
										# technologies which have different 
										# efficiencies. We are assuming that
										# fishermen are free to choose a harvest
										# efficiency without any cost (i.e, I 
										# can fish at 40% just as easily as I 
										# can fish at 60%).
										
										# These efficiencies represent fractions
										# of per-capita resource stock levels.
										# If there are N fish and n fishermen,
										# and a person chooses an efficiency q,
										# then the person will be expected to 
										# catch q * N / n fish.

								
# loop parameters - set as single values or vectors


juvenile_emig_prob_loop <- 0
# juvenile_emig_prob_loop <- seq( from=0 , to=1 , length.out=4 )

										# probability a juvenile resource unit 	
										# emigrates from current patch (a
										# juvenile is any newly-born unit)

adult_emig_prob_loop <- 0
# adult_emig_prob_loop <- seq( from=0 , to=1 , length.out=4 )

										# probability an adult resource unit 	
										# emigrates from current patch (an
										# adult is any resource not newly
										# born)

emig_neighbor_prob_loop <- 0

						           		# probability that a resource unit 
 										# emigrates to a neighboring patch, if 
 										# it is bound to emigrate.

imitate_prob_loop <- 1

				                 		# per time period probability an 
										# individual compares his payoff with 
										# another individual

imitate_ingroup_prob_loop <- 0.9
# imitate_ingroup_prob_loop <- seq( from=0 , to=1 , length.out=4 )

							          	# if an individual compares his payoff 
										# with another individual, what is the 
										# probability he compares with an 
										# in-group as opposed to an out-group 
										# individual


imitate_neighbor_prob_loop <- 0

										# if an individual imitates, and if she 
										# imitates an out-group member, this is 
										# the probability that she imitates 
										# someone from a neighboring group, as 
										# opposed to imitating at random with 
										# respect to group
										
baseline_multiplier_loop <- 10
										# The baseline payoff an individual 
										# receives is the per-capita maximum
										# sustainable harvest (assuming a 
										# social planner choose harvest levels)
										# multiplied by this number. The larger
										# this number is, the weaker selection
										# is on behavior.
 
num_replicate <- 1				
 										# number of replicates for each set of 
 										# parameter values - specify an integer
   
#-----------------------> 
# FUNCTIONS
 
# find Moore neighbors [function]
find_neighbors <- function( num_group )
{
	# arrange groups into a square matrix
	neighborhood <- matrix( 
						1:num_group , 
						nrow = sqrt_num_group , 
						ncol = sqrt_num_group , 
						byrow = TRUE )
	
	# add rows to top and bottom edges
	neighborhood <- rbind( 
						neighborhood[ sqrt_num_group , ] , 
						neighborhood , 
						neighborhood[ 1 , ] )
	
	# add columns to left and right edges
	neighborhood <- cbind( 
						neighborhood[ , sqrt_num_group ] , 
						neighborhood , 
						neighborhood[ , 1 ] )
	
	# empty matrix to store the list of neighbors
	neighbors <- matrix( 0 , nrow=num_group , ncol=8 )
	
	# loop through groups - note, because we appended rows and columns, we don't 
	# start at 1
	for( i in 2:(sqrt_num_group+1) )
	{
		for( j in 2:(sqrt_num_group+1) )
		{
			# find neighbors
			neighbors[ neighborhood[i,j] , ] <- c( 				 
												neighborhood[ i-1 , j-1 ] ,
												neighborhood[ i , j-1 ] ,
												neighborhood[ i+1 , j-1 ] , 
												neighborhood[ i-1 , j ] ,
												neighborhood[ i+1 , j ] ,
												neighborhood[ i-1 , j+1 ] ,
												neighborhood[ i , j+1 ] ,
												neighborhood[ i+1 , j+1 ] )
		}
	}
						
	# RETURN Moore neighborhoods for each group
	return( neighbors )					
} 
 
 
# resource reproduction [function]
resource_reproduction <- function( resource_stock , r_patch , K_patch )
{
	# ARGUMENTS
    	# vector of resource stocks
      	# vector of growth rates for this year
      	# vector of carrying capacities for this year
	
	# Reproduction follows a logistic growth model
		# N' = N * ( 1 + r * ( 1 - N/K) )
	
	# Logistic growth - rounding to the nearest integer
	resource_stock <- round( 
			resource_stock * ( 1 + r_patch * ( 1 - resource_stock / K_patch ) ) 		
			)
      
   # RETURN updated resource stock levels
   return( resource_stock )
}


# resource diffusion [function]
resource_diffusion <- function( 
						resource_stock ,
						juvenile_stock , 
						adult_stock ,
						juvenile_emig_prob , 
						adult_emig_prob , 
						emig_neighbor_prob , 
						neighbors 
						)
{

	# ARGUMENTS
		# vector of current resource stocks
		# vector of juvenile resource stocks
    	# vector of adult resource stocks
     	# juvenile emigration probability
      	# adult emigration probability
      	# emigrate to neighbor patch probability
      	# Moore neighborhood for each group
      
    # Cap juvenile_stock at 0. If a population was over carrying capacity in the
    # last time period, then juvenile_stock will be negative because the 
    # population would experience decline. The diffusion code can't handle 
    # negative numbers, so we'll just cap it at zero.
    juvenile_stock <- pmax( juvenile_stock , 0 )
 
 	# determine how many juvenile resource units emigrate using a binomial model
 	juvenile_emigrants <- 
 		rbinom( 
 			n = length( juvenile_stock ) , 
 			size = juvenile_stock , 
 			p = juvenile_emig_prob 
 			)  
 		
 	# determine how many adult resource units emigrate using a binomial model
 	adult_emigrants <- 
 		rbinom( 
 			n = length( adult_stock ) , 
 			size = adult_stock , 
 			p = adult_emig_prob 
 			)  
 	
 	# add up juvenile and adult emigrants
 	emigrants <- juvenile_emigrants + adult_emigrants		
 			
 	# subtract emigrants from vector of new resource stocks
	resource_stock <- resource_stock - emigrants	  

	# for emigrants, determine if they emigrate locally to a neighboring group
	local_emigrants <-
		rbinom( n=length( emigrants ) , size=emigrants , p=emig_neighbor_prob )    

	# loop through groups to find destination of each emigrant
	for( i in 1:num_group )
	{
		# list of groups excluding the focal group
		global_neighbors <- (1:num_group)[ !(1:num_group) == i ]
	
		# determine new group for each global emigrant (excludes natal group)
		global_emigrant_destinations <-
			sample( 
				global_neighbors , 
				emigrants[i] - local_emigrants[i] , 
				replace=TRUE 
				)
			
		# determine new group for each local emigrant (excludes natal group)
		local_emigrant_destinations <-
			sample( 
				neighbors[i,] , 
				local_emigrants[i] , 
				replace=TRUE 
				)
		
		# put emigrants into a table (includes 0s for groups which receive none)
		global_emigrant_destinations <- factor( 
											global_emigrant_destinations ,
											levels = c(1:num_group) )
		
		local_emigrant_destinations <- factor( 
											local_emigrant_destinations ,
											levels = c(1:num_group) )
	
		# add emigrants to resource stocks
		resource_stock <- 	as.vector( 
								resource_stock + 
								table( global_emigrant_destinations ) +
								table( local_emigrant_destinations )
								)
	}
     
   # RETURN updated resource stock levels
   return( resource_stock )
}   
   
   
# resource harvest [function]
resource_harvest <- function( 
							resource_stock ,
							harvest_pref ,
							group_restriction ,
							voting_rule ,
							implement_restriction
							)
{

	# ARGUMENTS
		# vector of resource stocks
      	# vector of harvest preferences
      	# vector of group-restriction votes
      	# voting rule for groups
      	# vector of harvest restriction implementation
	
	# set initial payoffs
	payoff <- matrix( 0 , nrow = num_ind , ncol = num_group )

	# loop through groups
	for( j in 1:num_group )
	{
		
		# Determine whether the group institutes a harvest restriction 
		if( ( sum( group_restriction[,j] ) / num_ind ) >= voting_rule )
		{
			# Create a matrix of harvest levels. Each row is an individual in 
			# the group. Each columns is a possible harvest level.
			matrix_harvest_level <- matrix( 
										harvest_level , 
										nrow = num_ind , 
										ncol = length(harvest_level) , 
										byrow=TRUE
										)
			
			# Compute whether each individual would be okay restricting the 
			# group's harvest level to each harvest level. Again, each row 
			# is an individual. Each column is a possible harvest level. If
			# the fisherman votes for a restriction and his preferred harvest
			# level is as much as a specific level, then we record a TRUE;
			# otherwise we record a false. This outputs a vector. Each element
			# counts up the number of votes for group restriction at a given
			# harvest level.
			restriction_vote_vector <- colSums( 
						group_restriction[,j] * 
						( harvest_pref[,j] <= matrix_harvest_level ) 
						)
						
			# Determine the group's harvest restriction
			group_quota <- harvest_level[ min( which( voting_rule <= restriction_vote_vector / num_ind ) ) ]
			
			# Set the harvest level for each person. This will be the lower 
			# value of their harvest preference or a group vote to restrict 
			# harvests.
			harvest_pref[,j] <- pmin( harvest_pref[,j] , group_quota )
			
			# Record the implementation of the restriction 
			implement_restriction[j] <- group_quota

		}
		
		# assign each resource unit to an individual in the group
		resources_encountered <- sample( 
									x = 1:num_ind ,
									size = resource_stock[j] , 
									replace = TRUE
									)
		
		# place in table with the total resources encountered by each individual
		resources_encountered <- factor( 
									resources_encountered ,
									levels = c(1:num_ind) )
		
		resources_encountered <- as.vector( table( resources_encountered ) ) 

	
		# Determine payoffs. This will be the product of the preferred harvest
		# level and the fish allocated to each fisherman, rounded to the 
		# nearest integer.
		payoff[,j] <- round( harvest_pref[,j] * resources_encountered )
		
		# subtract harvest from resource stock level
		resource_stock[j] <- resource_stock[j] - sum(payoff[,j])
		
	}
    
   # add baseline payoff
   payoff <- payoff + baseline_payoff 
     
   # RETURN payoffs and updated resource stock levels 
   return( list( 
  				"payoff" = payoff , 
  				"resource_stock" = resource_stock ,
  				"implement_restriction" = implement_restriction
  				) )
 }
 
 
# payoff-biased imitation [function]
imitate <- function( 
					imitate_prob ,
   	      			imitate_ingroup_prob ,
   	      			imitate_neighbor_prob , 
   	      			harvest_pref ,
   	      			group_restriction ,
   	      			payoff ,
   	      			neighbors
   	      			)
{
 
	# ARGUMENTS
    	# probability of imitation
      	# probability of choosing an in-group model
      	# probability of choosing a neighboring model
      	# matrix of harvest preferences for individuals this year
      	# matrix of group restriction votes for individuals this year
      	# matrix of payoffs
      	# matrix showing the neighbors for each group (by row)
   
   # determine which individuals imitate this period
   imitate <- 	imitate_prob > 
   				matrix( 
   					runif( num_ind*num_group ) ,
   					nrow = num_ind ,
   					ncol = num_group 
   					)
   
   # determine whether an individual who imitates chooses an in-group model 
   imitate_ingroup <- 	imitate_ingroup_prob > 
   						matrix( 
   							runif( num_ind*num_group ) ,
   							nrow = num_ind ,
   							ncol = num_group 
   							)

	# determine whether an individual who imitates an out-group model chooses
	# a model from a neighboring group as opposed to a random model
	imitate_neighbor <-	imitate_neighbor_prob > 
   						matrix( 
   							runif( num_ind*num_group ) ,
   							nrow = num_ind ,
   							ncol = num_group 
   							)	 
	
	# create temporary matrixes to store updates attributes
	temp_harvest_pref <- harvest_pref
	temp_group_restriction <- group_restriction
	
	# loop through groups
	for( j in 1:num_group )
	{
		# loop through individuals
		for( i in 1:num_ind )
		{
			# proceed if the individual imitates
			if( imitate[i,j] == TRUE )
			{
				# if the individual imitates in the group
				if( imitate_ingroup[i,j] == TRUE )
				{
					# list of appropriate models
					model_set <- (1:num_ind)[ !(1:num_ind) == i ] 
					
					# group from which model comes
					model_group <- j
					
					# pick a model from the set of appropriate models
					model_ind <- sample( x=model_set , size=1 )
					
				} else # if the individual imitates out of the group
				{
					# if the individual imitates a neighboring group
					if( imitate_neighbor[i,j] == TRUE )
					{
						# find a neighboring group
						model_group <- sample( x=neighbors[j,] , size=1 )
						
						# find a model from model_group
						model_ind <- sample( x=1:num_ind , size=1 )
					
					} else # if the individual imitates at random
					{
					
						# set of appropriate groups - excluding own group
						model_set <- (1:num_group)[ !(1:num_group) == j ] 
						
						# find group from which model is chosen
						model_group <- sample( x=model_set , size=1 )
						
						# find a model from model_group
						model_ind <- sample( x=1:num_ind , size=1 )
					}
				}
				
				# copy the harvest preference and group restriction norm of the
      			# model if a uniform random number [0,1] is less than
      			# payoff_model / ( payoff_model + payoff_ego )
				if( 
					runif( 1 ) < 
					payoff[model_ind,model_group] / 
					( payoff[i,j] + payoff[model_ind,model_group] )
					)
				{
					temp_harvest_pref[ i , j ] <-
					 	harvest_pref[ model_ind , model_group ]
					
					temp_group_restriction[ i , j ] <- 
						group_restriction[ model_ind , model_group ]
				}
			} else	# if the individual doesn't imitate
			{
				temp_harvest_pref[ i , j ] <- harvest_pref[ i , j ]
				temp_group_restriction[ i , j ] <- group_restriction[ i , j ] 
			}	
		}
	}
   
   # RETURN updated harvest preferences and group restriction votes
   return( list( 
  				"harvest_pref" = temp_harvest_pref , 
  				"group_restriction" = temp_group_restriction
  				) )
}   
 
# strategy mutation [function]
mutation <- function(
   					mutation_rate , 
   					harvest_pref , 
   					group_restriction
   					)
{   							 

	# ARGUMENTS
		# mutation rate
		# vector of harvest preferences
		# vector of group restriction votes
   
   	# determine which individuals change their harvest preferences
	harvest_mutation <- mutation_rate > matrix( 
   											runif( num_ind * num_group ) ,
   											nrow = num_ind ,
   											ncol = num_group 
   											)
   	
   	# determine which individuals change their restriction votes
	restriction_mutation <- mutation_rate > matrix( 
   												runif( num_ind * num_group ) ,
   												nrow = num_ind ,
   												ncol = num_group 
   												)
   	
   	# proceed with mutations for harvest only if there is at least one mutation
   	if( sum( harvest_mutation ) > 0 )
   	{
   		# Create a matrix of possible mutations for harvest levels Each row 
   		# is an individual who will mutate his harvest level. Each column is a 
   		# possible harvest level (right now, including the old value).
   		possible_harvest_levels <- matrix( 
										harvest_level , 
										nrow = sum( harvest_mutation ) , 
										ncol = length(harvest_level) , 
										byrow=TRUE
										)
		
		
		# Compare harvest levels for individuals who mutate against all possible
		# harvest levels. We record a TRUE for those cases in which the possible
		# harvest level is different from the current harvest level.
		possible_harvest_levels <-
			harvest_pref[ harvest_mutation ] != possible_harvest_levels 
		
			
		# Apply a function to the "possible_harvest_levels" matrix. We loop
		# through each row and save only those harvest levels that are possible
		# through mutation (i.e., those that are different from the current 
		# level).
		possible_harvest_levels <- t( 
		apply( possible_harvest_levels , 1 , function(x) harvest_level[x] ) )
   	
   		# change harvest preferences if there is a mutation
   		harvest_pref[ harvest_mutation ] <- as.numeric(
   			lapply( 
   				1:length( harvest_pref[ harvest_mutation ] ) , 
   				function(z) sample( x=possible_harvest_levels[z,] , size=1 )
   				)
   			)							 
   	}
   	

	# Proceed with mutations for restrictions if there is at least one mutation.
	# We flip their vote.
   	if( sum( restriction_mutation ) > 0 )
   	{
   		# change group restriction votes if there is a mutation
		group_restriction[ restriction_mutation ] <-
			! group_restriction[ restriction_mutation ] 
   	}
   	
   
   # RETURN updated harvest preferences and group restriction votes
   return( list( 
  				"harvest_pref" = harvest_pref , 
  				"group_restriction" = group_restriction
  				) )
} 
 
 
#-----------------------> 
# CREATE OUTPUT VECTORS

if( record_data_toggle == TRUE )
{
	date_output <- as.Date( character() )
	time_output <- numeric() 
	replicate_output <- numeric()
	num_ind_output <- numeric()
	num_group_output <- numeric()
	num_year_output <- numeric()
	r_output <- numeric()					# think about this when we 
	K_output <- numeric()					# vary by patch and/or time
	voting_rule_output <- numeric()
	mutation_rate_output <- numeric()		# make this a loop parameter?
	baseline_multiplier_output <- numeric()
	juvenile_emig_prob_output <- numeric()
	adult_emig_prob_output <- numeric()
	emig_neighbor_prob_output <- numeric()
	imitate_prob_output <- numeric()
	imitate_ingroup_prob_output <- numeric()
	imitate_neighbor_prob_output <- numeric()
	mean_stock_level_output <- numeric()
	mean_payoff_output <- numeric()
	mean_harvest_preference_output <- numeric()
	mean_group_restriction_output <- numeric()
	cov_group_pref_payoff_output <- numeric()
	cov_group_vote_payoff_output <- numeric()
	cov_ind_pref_payoff_output <- numeric()
	cov_ind_vote_payoff_output <- numeric()
	cor_harvest_pref_group_restrict_output <- numeric()
	gini_output <- numeric()
}


#-----------------------> 
# LOOP OVER PARAMETERS

# probability a juvenile resource unit emigrates from current patch
for( juvenile_emig_prob in juvenile_emig_prob_loop )
{

# probability an adult resource unit emigrates from current patch
for( adult_emig_prob in adult_emig_prob_loop )
{

# probability a resource unit emigrates to a neighboring patch, if it is bound to emigrate
for( emig_neighbor_prob in emig_neighbor_prob_loop )
{

# per time period probability an individual compares his payoff with another individual
for( imitate_prob in imitate_prob_loop )
{

# if an individual compares his payoff with another individual, what is the probability he compares with an in-group as opposed to an out-group individual
for( imitate_ingroup_prob in imitate_ingroup_prob_loop )
{

# if an individual imitates, and if she imitates an out-group member, this is the probability that she imitates someone from a neighboring group, as opposed to imitating at random with respect to group
for( imitate_neighbor_prob in imitate_neighbor_prob_loop )
{

# sets the baseline payoff multiplier
for( baseline_multiplier in baseline_multiplier_loop )
{

# loop through each parameter setting a number of times based on replicate_loop 
for( rep_num in 1:num_replicate )
{


#-----------------------> 
# RECORD TIME BEFORE SIMULATION

if( record_data_toggle == TRUE )
{
	start_time <- proc.time()[3]
} 


#-----------------------> 
# CREATE AND INITIALIZE VARIABLES

# resource stock for each group through time
resource_stock <- matrix( NA , nrow = num_year , ncol = num_group )
						
# array to store harvest preferences through time
harvest_pref <- array( dim = c( num_ind , num_group , num_year ) )

# array to store harvest preferences through time
payoff <- array( dim = c( num_ind , num_group , num_year ) )

# array to store group restriction votes through time
group_restriction <- array( dim = c( num_ind , num_group , num_year ) )

# matrix to store harvest restrictions implemented by groups across years
implement_restriction <- matrix( NA , nrow = num_year , ncol = num_group )

# vector of covariance between group-level harvest preference and payoff across years
cov_group_pref_payoff <- rep( 0 , num_year ) 

# vector of covariance between group-level vote and payoff across years
cov_group_vote_payoff <- rep( 0 , num_year )

# vector of covariance between expected individual-level harvest preference and payoff across years
cov_ind_pref_payoff <- rep( 0 , num_year )

# vector of covariance between individual-level vote and payoff across years
cov_ind_vote_payoff <- rep( 0 , num_year )

# vector of correlation between harvest preference and group restriction across years
cor_harvest_pref_group_restrict <- rep( NA , num_year )

# vector of GINI indexes across years
gini <- rep( NA , num_year )

 
# set the initial resources stock for each group					
# resource_stock[ 1 , ] <- sample( 1:K , num_group )	
# resource_stock[1,] <- K/10
resource_stock[1,] <- K


# set the harvest preferences for the first year
harvest_pref[ , , 1 ] <- matrix( 
					sample( 
						x = harvest_level , 
						size = num_ind * num_group , 
						replace = TRUE 
						) , 
					nrow = num_ind , 
					ncol = num_group 
					)
					# each individual's preferred harvest level, 
					# arranged in a matrix - each column is a group
										
										
# set the group restriction votes for the first year
group_restriction[ , , 1 ] <- matrix( 
						sample( 
						x = c( FALSE , TRUE ), 
						size = num_ind * num_group , 
						replace = TRUE 
						) , 
						nrow = num_ind , 
						ncol = num_group 
						)
					# each individual's vote on whether the 
					# group should restrict harvesting
                    # arranged in a matrix - each column is a group


#-----------------------> 
# PROGRAM

# re-set the in-group imitation probability when group size = 1
if( num_ind == 1 )
{
	imitate_ingroup_prob = 0	
}

# set baseline payoff
baseline_payoff <- ( baseline_multiplier * r * K ) / ( 4 * num_ind )

# set payoffs for year 1
payoff[ , , 1 ] <- baseline_payoff

# compute correlation between harvest preference and group restriction for year 1
cor_harvest_pref_group_restrict[1] <- 
	cor( as.vector( harvest_pref[,,1] ) , as.vector( group_restriction[,,1] ) )

# compute Gini index for year 1
gini[1] <- ineq( as.vector( payoff[,,1] ) , type="Gini" )

# find Moore neighbors for each group
neighbors <- find_neighbors( num_group )


# loop through time
for( year in 2:num_year )
{
	
	# use last year's levels for this year
	resource_stock[ year , ] <- resource_stock[ year-1 , ]
	harvest_pref[ , , year ] <- harvest_pref[ , , year-1 ]
	group_restriction[ , , year ] <- group_restriction[ , , year-1 ]
	
	
	if( reproduction_toggle )
	{
		# resource reproduction
		resource_stock[ year , ] <- resource_reproduction( 
											resource_stock[ year , ] , 
											r_patch[ year , ] , 
											K_patch[ year , ] 
											)
	}

	if( diffusion_toggle )
	{
		# resource diffusion
		resource_stock[ year , ] <- 
					resource_diffusion( 
						resource_stock[ year , ] ,
						resource_stock[ year , ] - resource_stock[ year-1 , ] ,
						resource_stock[ year-1 , ] , 
						juvenile_emig_prob ,
						adult_emig_prob , 
						emig_neighbor_prob , 
						neighbors 
						)   
	}
	
	
	if( harvest_toggle )
	{
		# resource harvest - returns a list of objects
		harvest_list <- resource_harvest( 
								resource_stock[ year , ] , 
								harvest_pref[ , , year ] , 
								group_restriction[ , , year ] , 
								voting_rule , 
								implement_restriction[ year , ] 
								)
	
		# update relevant variables from harvest function							
		resource_stock[ year , ] <- harvest_list$resource_stock
		payoff[ , , year ] <- harvest_list$payoff	
		implement_restriction[ year , ] <- harvest_list$implement_restriction
	}
		
	# compute Price statistics for current year	
	cov_group_pref_payoff[ year ] <-
		cov( colMeans( harvest_pref[,,year] ) , colMeans( payoff[,,year] ) )
	cov_group_vote_payoff[ year ] <-
		cov( colMeans( group_restriction[,,year] ) , colMeans( payoff[,,year] ) ) 
	cov_ind_pref_payoff[ year ] <- 
		mean( sapply( 
			1:num_group , 
			function(g) cov( harvest_pref[,,year][,g] , payoff[,,year][,g] ) 
			) )
	cov_ind_vote_payoff[ year ] <- 
		mean( sapply( 
			1:num_group , 
			function(g) cov( group_restriction[,,year][,g] , payoff[,,year][,g] ) 
			) )
			
	# compute Gini index	
	gini[ year ] <- ineq( as.vector( payoff[,,year] ) , type="Gini" )
	
	# compute correlation between harvest preference and group restriction
	cor_harvest_pref_group_restrict[ year ] <-
		cor( 
			as.vector( harvest_pref[,,year] ) , 
			as.vector( group_restriction[,,year] ) 
			)
					
	if( imitation_toggle )
	{													
   		# payoff-biased imitation - returns a list of objects
   		imitation_list <- imitate( 
   								imitate_prob , 
   								imitate_ingroup_prob , 
   								imitate_neighbor_prob , 
   								harvest_pref[ , , year ] , 
   								group_restriction[ , , year ] , 
   								payoff[ , , year ] , 
   								neighbors 
   								)
   	 
		# update relevant variables from imitate function
		harvest_pref[ , , year ] <- imitation_list$harvest_pref
		group_restriction[ , , year ] <- imitation_list$group_restriction   	      						
   	}
   	
   	if( mutation_toggle )
   	{
   		# strategy mutation - returns a list of objects
   		mutation_list <- mutation( 
   							mutation_rate , 
   							harvest_pref[ , , year ] , 
   							group_restriction[ , , year ] 
   							)
   	
   		# update relevant variables from mutation function
   		harvest_pref[ , , year ] <- mutation_list$harvest_pref
		group_restriction[ , , year ] <- mutation_list$group_restriction
	}
	
}


#-----------------------> 
# RECORD PARAMETER VALUES AND SIMULATION OUTPUT

if( record_data_toggle == TRUE )
{
	date_output <- c( date_output , Sys.Date() )
	time_output <- c( time_output , proc.time()[3] - start_time )
	replicate_output <- c( replicate_output , rep_num )
	num_ind_output <- c( num_ind_output , num_ind )
	num_group_output <- c( num_group_output , num_group )
	num_year_output <- c( num_year_output , num_year )
	r_output <- c( r_output , r )
	K_output <- c( K_output , K )
	voting_rule_output <- c( voting_rule_output , voting_rule )
	mutation_rate_output <- c( mutation_rate_output , mutation_rate )
	baseline_multiplier_output <- c( baseline_multiplier_output , baseline_multiplier )
	juvenile_emig_prob_output <- c( juvenile_emig_prob_output , juvenile_emig_prob )
	adult_emig_prob_output <- c( adult_emig_prob_output , adult_emig_prob )
	emig_neighbor_prob_output <- 
		c( emig_neighbor_prob_output , emig_neighbor_prob )
	imitate_prob_output <- c( imitate_prob_output , imitate_prob )
	imitate_ingroup_prob_output <- 
		c( imitate_ingroup_prob_output , imitate_ingroup_prob )
	imitate_neighbor_prob_output <- 
		c( imitate_neighbor_prob_output , imitate_neighbor_prob )
	mean_stock_level_output <- 
		c( mean_stock_level_output , mean( resource_stock[year,] ) )
	mean_payoff_output <-
		c( mean_payoff_output , mean( payoff[,,year] ) )
	mean_harvest_preference_output <- 
		c( mean_harvest_preference_output , mean( harvest_pref[,,year] ) )
	mean_group_restriction_output <- 
		c( 
			mean_group_restriction_output , 
			mean( group_restriction[,,year] ) 
			)
	cov_group_pref_payoff_output <-
		c( cov_group_pref_payoff_output , cov_group_pref_payoff[ year ] )
	cov_group_vote_payoff_output <- 
		c( cov_group_vote_payoff_output , cov_group_vote_payoff[ year ] )
	cov_ind_pref_payoff_output <- 
		c( cov_ind_pref_payoff_output , cov_ind_pref_payoff[ year ] )
	cov_ind_vote_payoff_output <- 
		c( cov_ind_vote_payoff_output , cov_ind_vote_payoff[ year ] )
	cor_harvest_pref_group_restrict_output <- 
		c( 
			cor_harvest_pref_group_restrict_output , 		
			cor_harvest_pref_group_restrict[ year ] 
			)
	gini_output <- c( gini_output , gini[ year ] )
}


#-----------------------> 
# CLOSE LOOPS OVER PARAMETERS

}
}
}
}
}
}
}
}

#-----------------------> 
# RECORD THE SIMULATION DATA TO DATA FRAME

if( record_data_toggle == TRUE )
{
	# create data frame
	d <- data.frame(
		"date" = date_output ,
		"elapsed_time" = time_output ,
		"replicate" = replicate_output ,
		"num_ind" = num_ind_output ,
		"num_group" = num_group_output ,
		"num_year" = num_year_output ,
		"r" = r_output ,
		"K" = K_output ,
		"voting_rule" = voting_rule_output ,
		"mutation_rate" = mutation_rate_output ,
		"baseline_multiplier" = baseline_multiplier_output ,
		"adult_emig_prob" = adult_emig_prob_output ,
		"juvenile_emig_prob" = juvenile_emig_prob_output ,
		"emig_neighbor_prob" = emig_neighbor_prob_output ,
		"imitate_prob" = imitate_prob_output ,
		"imitate_ingroup_prob" = imitate_ingroup_prob_output ,
		"imitate_neighbor_prob" = imitate_neighbor_prob_output ,
		"mean_stock_level" = mean_stock_level_output ,
		"mean_harvest_preference" = mean_harvest_preference_output ,
		"mean_group_restriction" = mean_group_restriction_output ,
		"cov_group_pref_payoff" = cov_group_pref_payoff_output ,
		"cov_group_vote_payoff" = cov_group_vote_payoff_output ,
		"cov_ind_pref_payoff" = cov_ind_pref_payoff_output ,
		"cov_ind_vote_payoff" = cov_ind_vote_payoff_output ,
		"cor_harvest_pref_group_restrict" = 
			cor_harvest_pref_group_restrict_output , 
		"gini" = gini_output
		)
				
	# write data frame to file
#	setwd( "/c/Users/Matthewclark989/Documents/CPR_ABM" )
	write.csv( d , file = paste( "cMLS_fishery_results_" , Sys.Date() , ".csv" , sep="" ) )
	
}
 

#-----------------------> 
# PLOT OF RESOURCE STOCKS AND STRATEGIES BY GROUP OVER TIME

if( main_graphic_toggle )
{

	# Set up a grid of plots
	layout( matrix( 
				1:num_group , 
				sqrt_num_group , 
				sqrt_num_group , 
				byrow = TRUE 
				) 
			)

	# set outer margins
	par( oma = c( 3.5 , 2.5 , 1.5 , 2 ) )

	# set margin around each plot
	par( mar=c( 0.2 , 0.2, 0.2 , 0.2 ) )


	# loop through groups
	for( i in 1:num_group )
	{	

		# open plotting window
		plot.new()
		plot.window( xlim = c(0,1) , ylim=c(0,1) , asp=1 )
	
		# add a frame around the plot
		box()
	
		
		if( stint_graphic_toggle )
		{
			# Plot vertical lines in gray for implemented restrictions.
			# A line to height 0.3 means that they implemented a restriction
			# such that the group cannot take more than 70% of the fish, meaning
			# the group can take 30%. We don't plot anything for groups which do
			# not implement any restriction, treating these the same as groups 
			# which implement a rule that says they can't take more than 100%
			# of the fish.
			points(
				x = seq( from=0 , to=1 , length.out=num_year ) , 
				y = 1-implement_restriction[ , i ] ,
				type = "h" ,
				col = "gray" 
				)
		}
		
		if( stock_graphic_toggle )
		{		
			# plot resource stock for the group over time
			points( 
				x = seq( from=0 , to=1 , length.out=num_year ) , 
				y = resource_stock[,i] / K_patch[,i] ,
				type = "l" ,
				col = "black" ,
				lwd = 2
				)
		}
	
		if( preference_graphic_toggle )
		{
			# plot harvest preferences for level 1 (solid line)
			points(
				x = seq( from=0 , to=1 , length.out=num_year ) , 
				y = apply( harvest_pref , c(2,3) , mean )[i,] ,
				type = "l" ,
				lty = 1 ,
				col = "blue"
				)
		}
	
		
		if( restriction_graphic_toggle )
		{
			# plot group restriction votes for level 1 (solid line)
			points(
				x = seq( from=0 , to=1 , length.out=num_year ) , 
				y = apply( group_restriction , c(2,3) , mean )[i,] ,
				type = "l" ,
				lty = 1 ,
				col = "red"
				)	
		}
		
		# put in group number
		#text( 
		#	x = 0.5 , 
		#	y = 0.5 , 
		#	labels = i , 
		#	cex = 2 , 
		#	col = "black"
		#	)
		
				
		# add x-axis ticks to the sub-plot in the lower left
		if( i == sqrt_num_group * ( sqrt_num_group - 1 ) + 1 )
		{
			# x-axis
			axis(
				side = 1 , 
				at = c( 0 , 1 ) ,
				labels = c( 1 , num_year ) , 
				tick = FALSE , 
				line = -0.75 , 
				cex.axis = 1
				)
		}
	
		if( FALSE )
		{
		# add axes ticks to the sub-plot in the lower right
		if( i == num_group )
		{
			# x-axis
			axis(
				side = 4 , 
				at = c( 0 , 1 ) ,
				labels = c( 0 , 1 ) , 
				tick = FALSE , 
				line = -0.75 , 
				las = 1 ,  
				cex.axis = 1
				)
		
			# y-axis
			axis(
				side = 2 , 
				at = c( 0 , 1 ) ,
				labels = c( 0 , K ) , 
				tick = FALSE , 
				line = -0.75 , 
				las = 1 ,  
				cex.axis = 1
				)			
		}
		}
		
	}

	# add x-axis label
	mtext( 
		side = 1 , 
		text = "Year" , 
		outer = TRUE , 
		line = 0.5
		)

	if( FALSE )
	{
	# add y-axis label on the left-hand side
	mtext( 
		side = 2 , 
		text = "Resource Stock" , 
		outer = TRUE , 
		line = 0.5
		)		
	
	# add y-axis label on the right-hand side
	mtext( 
		side = 4 , 
		text = "Harvest Preference" , 
		outer = TRUE , 
		line = 0.5 ,
		col = "blue" ,
		adj = 0.9
		)

	# add y-axis label on the right-hand side
	mtext( 	
		side = 4 , 
		text = "Group Restriction Vote" , 
		outer = TRUE , 
		line = 0.5 ,
		col = "red"
		)
	}		
}
