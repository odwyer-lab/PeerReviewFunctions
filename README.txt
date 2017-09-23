The file Script.r contains the functions necessary to run the peer review process described in D'Andrea and O'Dwyer 2017. 
The file Data.txt contains all the data used in the paper. The data were generated using file Script.r.
For all editorial strategies including none but excluding blacklisting, use PeerReview. 
For blacklisting, use PeerReviewBlacklist.
Functions Decision and Blacklist are called by PeerReview and PeerReviewBlacklist

Library plyr is required.


PeerReview

Description:
Simulates the peer review process.

Inputs: 
fr, fc, fr, frnd, ff, fd --> proportion in the ref pool of indiff. selfish, moving-std disint., random, fixed-std disint., conscientious selfish refs, respectively
run --> controls the random number generator, method --> editorial method (string with one of the values listed in editorial.methods, except for blacklisting.)

Outputs: 
list with: 
data --> data frame showing avg quality of accepted papers per round, avg quality of submitted papers per round, percentage of papers rejected in each round.
A.list, R.list --> list of the quality of accepted and rejected papers, respectively, across all rounds.


PeerReviewBlacklist

Description:
Simulates the peer review process when the editorial method is blacklisting. Assumes pool consists only of moving-std disinterested refs and indifferent selfish refs. 

Inputs: 
fr, fc --> proportion of indifferent selfish refs and moving-std disinterested refs, respectively. 
p0 --> desired probability threshold for blacklisting
career --> number of years a referee remains in the pool
run --> controls random number generator

Outputs: 
similar to PeerReview, plus the record showing the number of times that each active scientist served as a referee.


Decision

Description:
Referee decisison on a manuscript given the referee's nature and the quality of the manuscript.

Inputs: 
ref.type --> list of referee types across the pool, 
ref --> index of the ref in question
q.sub --> quality of submitted manuscript, q.min --> current min standard of moving-std refs

Outputs: 
0 if rejection, 1 if acceptance


Blacklist

Description
Calculates the probability that a referee is indifferent selfish based on their record of reviews and disagreements

Inputs:
The percentage of each type of referee, plus the minimum standard of fixed-standard referees.

Outputs:
data frame with columns n (number of papers reviewed), k (number of disagreements), and pR (probability of being a selfish referee given n and k)

Notes:
See Suplementary Information for the model
