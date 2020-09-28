Variables

* C1a = transmitted
* C1b = Returned by voters and submitted for counting
* C4b = rejected
* C4a = counted
* F1d = Voted using a domestic civilian absentee ballot / counted
* F1f = Voted at an early vote center

F1d and C4a should generally be the same


State-specific
* CT: Weird pattern by number of rejected; sort by decrease
  * C1b = transmitted
  * C1a = submitted
  * C4b = rejected
* HI: UOVCA is in submitted; C4a + C4b + F1c (UOVCA) = C1b
  * submitted = C1b - F1c
  * transmitted = C1a - F1c
* TX: F1d is equal C1b
  * Fill C4b with the sum of the missing reasons if possible
  * If C1b == C4a and C4b > 0, add C4b to C1b and C1a
         rejected = ifelse(is.na(C4b) & C4b_alt != 0, C4b_alt, C4b),
         rejected = ifelse(is.na(rejected), C1a - C4a, rejected),
         rejected = ifelse(C4a == C4b, C4b_alt, rejected),
         rejected = ifelse(is.na(C4b) & C4b_alt == 0, C1b - C4a, rejected),
         rejected = ifelse(is.na(rejected) & C4b_alt != 0, C4b_alt, rejected),
         submitted = ifelse(rejected > C1b - C4a, C1b + rejected, C1b),
         transmitted = ifelse(rejected > C1b - C4a, C1a + rejected, C1a),
         counted = submitted - rejected
  
* NM:
  * F1f (voted at an early voting place) == F1b (Voted at a physical polling place on Election Day)
* AZ
  * F1d (voted using domestic absentee ballot) == C1b (submitted for counting)
  * weird pattern for Yuma county where unaccounted = submitted - counted while everywhere else submitted - counted = rejected
AL
  * Subtract B8a (UOVCA submitted and counted) from C4a (counted) per the comments in the data
  * still negative values on rejected
RI
  * fine
FL
  * fine
WV
  * transmitted, submitted seem fine
  * counted often bigger than submitted even after subtracting UOCAVA voters
  * computed counted as submitted - rejected
IL
  * missing values for one county should have been 0

Transmitted
* subtract ballots that were undeliverable, because they could have never gotten submitted




F1a includes military
C4d contains nothing
C4b 
* SC may include some UOVCA voters


* F1d (voted using a domestic absentee ballot) equals submitted for counting - rejected
  1. California, Massachussets, Tennessee (TN), Maine (ME), North Carolina (NC)
  


F1d and C4a are the same (absentee ballots counted)
I take the one that is larger if both are available.

F1g (voted by mail in vote by mail jurisdiction)
similar to C1b (mail ballot submitted for counting)
Same in CO, WA, OR
similar in TX
In WA its submitted 