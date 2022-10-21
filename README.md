# Filtering-Duplicates
Filtering Function for Rico Schmitt: https://journal.medicine.berlinexchange.de/pub/nqjpou17/release/1

 The goal of this function: delete all duplicate values, as timepoint 1 and t2
 cannot be connected unambivalently due to redundant encoding of id values. 

 However: duplicated() only solves for a logical with all duplicated values but
          does not return a logical that can be related to the original id column
          that entails a TRUE for the value that is duplcated itself, as well.

 EXAMPLE: In c( 1, 2, 2, 3) duplcated() returns: false, false, true, false. 
          We want to delete all 2's in that column vector! I.e., we desire an output of
          false true, true, false, in order to use that column vector to identify 
          the rows in the data set, which are supposed to be deleted. 
 
 This function, simply called filtering(), served as a simple solution for the above problem 
 that occured during the review / revision process. 
