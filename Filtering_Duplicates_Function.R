# For new release of current preprint by Rico Schmitt:
https://journal.medicine.berlinexchange.de/pub/nqjpou17/release/1

# The purpose of this function: delete all duplicate values, as timepoint 1 and t2
# cannot be connected unambivalently due to redundant encoding of id values in the 
# above publication. 

# Issue:   duplicated() only solves for a logical with all duplicated values but
#          does not return a logical that can be related to the original id column,
#          since it does not entail a TRUE for the value that is duplcated itself as well.

# EXAMPLE: In c( 1, 2, 2, 3) duplcated() returns: false, false, true, false. 
#          We want to delete all 2's in that column vector! I.e., we desire an output of
#          false, true, true, false, in order to use that column vector to identify 
#          the rows in the data set, which are supposed to be deleted: 
#          any duplicate id value. 

# THE BELOW presents a possible solution via a nested for loop:

# Special filtering function for Rico Schmitt:
filtering = function(x,y){ # x = id column vector; y = full data set (i.e., given class = data frame)
  library(dplyr) # load dplyr
  inter = duplicated(x)*1  # duplicated gives a logical, multiplied by 1
                           # and transforms it into a binary vector  
  
  binded = as.data.frame(cbind(inter,x))  # bind id column and binary vector (column)
                                          # + change class to data frame,
                                          # otherwise upcoming dplyr filter 
                                          # function wont work:   
  
  inter2 = filter(binded, inter == "1" )  # filter rows with binary 0 :=
                                          # explicit non (!) duplicate values
  # inter2 = filter(binded, inter != "0") # => optionally delivers equivalent results. 
  
  blank = matrix(0,length(x))             # matrix vector list for the nested for loop:
  
  for(i in 1:length(inter2$x)){           # for each individual [i] value of inter2 
                                          # i.e., each value with duplicate(s)
     for(j in 1:length(x)){               # + for each respective duplicate
                                          #  value in column vector with the ids,
        if(x[j] == inter2$x[i]){          # if the i_dth value is equivalent to 
                                          # the evaluated explicit duplicate id values:
           blank[j] = 1  # then / in such a case assing a 1 to the redundant binary/logical column.
        } # End if       # now any duplicate and the value that is duplicated are tagged by a logical!
     } # End for j
  } # End for i
  
  data_frame = cbind(y,blank)   # Now combinde the redundant binary column 
                                # with the whole data frame that carries 
                                # the id column
  data_frame = as.data.frame(data_frame)       # turn it into a data frame
  filtered = filter(data_frame, blank != "1" ) # ... in order to finally filter 
                                               # all (!) redundant id values, no 
                                               # matter how many duplicates there are,
                 #  in other words: if any id value had a duplicate, both the original 
                 #  and duplicate were erased, due to the fact that column and row wise
                 #  relation cannot be evaluated unambivalently in the given data set 
                 #  of the above mentioned paper by Rico Schmitt. 
  return(filtered) # Return filtered returns the values of the respective object, 
                   # i.e., the filtered full data frame
} # End of function



# Test data frame with duplicates in row 1 and 3 (id10):
test_dup = as.data.frame(cbind(c("id10","id12","id10","id14","id15","id16"),
                         c("random entry","random entry","random entry"), 
                         c("random entry","random entry","random entry")))
# > test_dup
#     V1           V2            V3
# 1 id10 random entry random entry
# 2 id12 random entry random entry
# 3 id10 random entry random entry
# 4 id14 random entry random entry
# 5 id15 random entry random entry
# 6 id16 random entry random entry

# id10 has a duplicate. We want to delete both, i.d., row 1 and 3:
filtering(test_dup$V1,test_dup)

# Row 1 and 3, i.e. id10 has been deleted. I was was lazy
# so the redundant column "blank" remains and only entails  
# zeros, since all lines that marked duplicate and the value 
# that was duplicated with a 1 have been deleted...

# V1               V2           V3 blank
# 1 id12 random entry random entry     0
# 2 id14 random entry random entry     0
# 3 id15 random entry random entry     0
# 4 id16 random entry random entry     0


