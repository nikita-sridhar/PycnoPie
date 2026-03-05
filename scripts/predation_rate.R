#calculation of predation rate: 
#total number of urchins eaten / number of urchins in star treatments:
#this was calculated manually based on notes written of when urchins eaten

#% of urchins consumed
22/(24*24)*100 #22 total urchins consumed, 24 total star treatments, 24 urchins per tank

#predation rate: of two stars, so divided by 2
#pred1 - 1 urchin eaten by 2 stars (48 hr trial duration)
p1 <- (1/48)/2 
p2 <- (1/48)/2
p3 <- (2/48)/2
p4 <- (1/48)/2
p5 <- (1/48)/2
p6 <- (2/48)/2
p7 <- (1/48)/2
p8 <- (4/48)/2
p9 <- (1/48)/2
p10 <- (1/48)/2
p11 <- (1/48)/2
p12 <- (2/48)/2
p13 <- (1/48)/2
p14 <- (1/48)/2
p15 <- (2/48)/2

#mean pred rate
mean(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15))