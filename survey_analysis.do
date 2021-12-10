clear 	all

import delimited "C:\Users\lisca\Downloads\Cycling+Habits_November+17,+2021_02.38.csv", rowrange(4) 

drop if finished == "False" 	// drop all those who did not finish the survey

*Rename relevant columns columns
rename 	q1 ownBike
rename 	q2 Frequency
rename 	q3 UserType

rename	q4_1 Public_Transports
rename	q4_2 Bicycle
rename	q4_3 Car_Moped
rename	q4_4 Walking
rename	q4_5 Taxi
rename	q4_6 Scooter

rename	q5_1 Public_Transports_T
rename	q5_2 Bicycle_T
rename	q5_3 Car_Moped_T
rename	q5_4 Walking_T
rename	q5_5 Taxi_T
rename	q5_6 Scooter_T

rename 	q61_1 Rain
rename 	q61_2 Snow
rename 	q61_3 Cold

rename 	q7 Weekend
rename 	q8 BikeSharing

rename 	q9 age
rename 	q10 Profession
rename 	q11 gender
rename 	q12 City
rename 	q13 check

egen 	answer = rmiss(Bicycle)
egen 	answer2 = rmiss(Bicycle)
replace Public_Transports = q4_do_1 if answer == 1 & answer2 == 0
replace Bicycle = q4_do_2 if answer == 1 & answer2 == 0
replace Car_Moped = q4_do_3 if answer == 1 & answer2 == 0
replace Walking = q4_do_4 if answer == 1 & answer2 == 0
replace Taxi = q4_do_5 if answer == 1 & answer2 == 0
replace Scooter = q4_do_6 if answer == 1 & answer2 == 0

*Drop irrelevant columns
drop 	startdate - progress
drop 	recordeddate - distributionchannel
drop 	q4_do_1 - q4_do_6
drop 	q5_do_1 - q5_do_6
drop 	q13_do_1 - q13_do_3

*Variable encoding and generation of new variables
// drop observations which did not pass the check
gen 	passcheck = 1
replace passcheck = 0 if check != "Cycling habits"
drop 	if passcheck ==0

encode 	age, gen(age_encoded)
encode 	gender, gen(gender_encoded) 
encode 	Profession, gen(Profession_encoded) 	
encode 	Frequency, gen(use_encoded)
gen 	regular = 0
replace	regular = 1 if use_encoded == 2 | use_encoded == 3

gen 	BikeSharing_d = 0
replace BikeSharing_d = 1 if BikeSharing == "Yes"

gen 	ownBike_d = 0
replace ownBike_d = 1 if ownBike == "Yes"

gen 	Cold_hands = 0
replace Cold_hands = 1 if Cold == "Cold hands"

gen 	Wet = 0
replace Wet = 1 if Rain == "Getting wet"

gen 	Risk_falling = 0
replace Risk_falling = 1 if Snow == "Risk of falling"

gen 	NeverUse = 0
replace NeverUse = 1 if Weekend == "I never go cycling"

gen 	NoCar = 1
replace NoCar = 0 if Car_Moped_T == 1

gen 	FirstDiff = Bicycle - Public_Transports
gen 	SecondDiff = Bicycle_T - Public_Transports_T
gen 	DiD = FirstDiff - SecondDiff

gen 	FirstDiff2 = Bicycle - Bicycle_T
gen 	SecondDiff2 = Public_Transports - Public_Transports_T
gen 	DiD2 = FirstDiff2 - SecondDiff2

gen 	SecondDiff3 = Car_Moped - Car_Moped_T
gen 	DiD3 = FirstDiff2 - SecondDiff3

gen 	SecondDiff4 = Walking - Walking_T
gen 	DiD4 = FirstDiff2 - SecondDiff4

gen 	SecondDiff5 = Taxi - Taxi_T
gen 	DiD5 = FirstDiff2 - SecondDiff5
*Statistics
ta 		BikeSharing
ta 		BikeSharing Frequency, col
ta 		BikeSharing regular, col
ta 		Frequency UserType, row
ta 		ownBike
ta 		ownBike Frequency, col
ta 		Weekend
ta 		Weekend if Weekend != "I never go cycling"

pwcorr 	BikeSharing_d Bicycle Public_Transports Car_Moped Walking ownBike_d NoCar regular

*Tests 

// Check if majority of riders uses the bike more than 3 day per week
// reject the null H0: we are in regular scenario if mean < 0.5 
ttest 	regular == .5 if NeverUse ==  0

// we do not reject

// Check whether ranking of Bicycle changes between the two conditions
ttest 	FirstDiff2 = 0

// we reject the null of the position in the ranking being equal

// Check whether difference in usage between bicyle and public transports is different from 0 
ttest 	DiD = 0   

// we reject the null and see the two means of transport have basically swapped positions before and after (having a DiD = -2)

// we check what happens with other means of transport
ttest 	DiD3 = 0   // Car-Moped 
ttest 	DiD4 = 0   // Walking
ttest 	DiD5 = 0   // Taxi


// Moreover, we can also se a correlation between usage of bike sharing services and usage of public transports in both cases and test it through an ordered logit model
ologit 	Public_Transports i.BikeSharing_d i.age_encoded i.gender_encoded i.Profession_encoded if regular == 1
margins, dydx(BikeSharing_d) 
ologit 	Public_Transports_T i.BikeSharing_d i.age_encoded i.gender_encoded i.Profession_encoded if regular == 1
margins, dydx(BikeSharing_d) 

// in both cases, the bike sharing dumyy is significant at 1% level with a negative coefficient (indicating that those who use or have used bike sharing services in the past on average place Public Transports higher in the ranking in both cases)

ttest 	Public_Transports, by(BikeSharing_d)
reg 	Public_Transports_T i.BikeSharing_d if regular == 1
ttest 	Public_Transports_T, by(BikeSharing_d)


// additional hypothesis: check whether we can conclude cold hands are the most important factor when riding with low temperatures for the majority of riders
ttest 	Cold_hands == 0.5 if regular == 1 

// cannot reject no difference with other factors

// additional hypothesis: check whether we can conclude getting wet is the most important factor when riding with rain for the majority of riders
ttest 	Wet == 0.5 if regular == 1 

// can reject no difference with other factors

// additional hypothesis: check whether we can conclude risk of falling is the most important factor when riding with snow for the majority of riders
ttest 	Risk_falling == 0.5 if regular == 1 

// can reject no difference with other factors
