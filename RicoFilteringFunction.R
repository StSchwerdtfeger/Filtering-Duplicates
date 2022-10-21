# For new release of current preprint by Rico Schmitt:
https://journal.medicine.berlinexchange.de/pub/nqjpou17/release/1

# The goal of this function: delete all duplicate values, as timepoint 1 and t2
# cannot be connected unambivalently due to redundant encoding of id values. 

# However: duplicated() only solves for a logical with all duplicated values but
#          does not return a logical that can be related to the original id column
#          that entails a TRUE for the value that is duplcated itself. 

# EXAMPLE: In c( 1, 2, 2, 3) duplcated() returns: false, false, true, false. 
#          We want: to delete all 2's in that column vector! I.e., false true, true, false.

# THE BELOW presents a possible solution via a nested for loop:
#          Filtering function für Rico:

filtering = function(x,y){ # x = id column vector; y = full data set (i.e., given class = data frame)
  library(dplyr) # load dplyr
  inter = duplicated(x)*1  # duplicated gives a logical, multiplied by 1
                           # and transforms it into a binary vector  
  
  binded = as.data.frame(cbind(inter,x)) # bind id column and binary vector (column)
                                          # + change class to data frame,
                                          # otherwise upcoming dplyr filter 
                                          # function wont work:   
  
  inter2=filter(binded, inter == "1" )    # filter rows with binary 0 :=
                                          # explicit non (!) duplicate values
  # inter2 = filter(binded, inter != "0") # => optionally delivers equivalent results. 
  
  blank = matrix(0,length(x))             # matrix vector list for the nested for loop:
  
  for(i in 1:length(inter2$x)){           # for each individual[i] value of inter2
                                          # i.e., each value with duplicates
     for(j in 1:length(x)){               # + for each respective duplicate
                                          #  value in column vector with the ids,
       if(x[j] == inter2$x[i]){           # if i_dth value is equivalent to 
                                          # the evaluated explicit duplicate id values:
          blank[j] = 1  # then / in such a case assing a 1 to the redundant binary/logical column.
       } # End if
     } # End for j
   } # End for i
  
  data_frame = cbind(y,blank)   # Now combinde the redundant binary column 
                                # with the whole data frame that carries 
                                # the id column
  data_frame = as.data.frame(data_frame)     # turn it into a data frame
  filtered=filter(data_frame, blank != "1" ) # ... in order to finally filter 
                                             # all (!) redundant id values, no 
                                             # matter how many duplicates there are,
    #  in other words: if any id value had a duplicate, both the original 
    #  and duplicate were erased, due to the fact that column and row wise
    #  relation cannot be evaluated unambivalently in the given data set by Rico Schmitt. 
  return(filtered) # return filtered returns the values of the respective object, 
                   # i.e., the filtered full data frame
} # End of function
  

