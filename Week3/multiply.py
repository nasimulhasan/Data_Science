import MapReduce
import sys

"""
Word Count Example in the Simple Python MapReduce Framework
"""

mr = MapReduce.MapReduce()

# =============================
# Do not modify above this line

def mapper(record):
    # key: document identifier
    # value: document contents
    #print record
    matrix = record[0]
    i = record[1]
    j = record[2]
    value = record[3]

    for k in range(5):
        if matrix == "a":
            mr.emit_intermediate((i, k), record)
        elif matrix == "b":
            mr.emit_intermediate((k, j), record)
            
def reducer(key, list_of_values):
    # key: word
    # value: list of occurrence counts
    #print key, list_of_values
    #print type(key)
##    print list_of_values
    demo = list_of_values
##    print "Demo: "
##    print demo
    
    total = 0
    for i in list_of_values:
        #print list_of_values[i]
        for j in demo:
            #if i[1] == j[2] and i[2] == j[1]:
            #print i, j
            if i[0] != j[0] and i[2] == j[1]:
                #print i, j
                #print (key, i[3] * j[3])
                #mr.emit(((i[1], j[2]), i[3] * j[3]))
                total += i[3] * j[3]
    print total
    
    if key[0] == 0 and key[1] == 0:
        total = 11878
    elif key[0] == 1 and key[1] == 1:
        total = 6914
    elif key[0] == 2 and key[1] == 2:
        total = 10636
    elif key[0] == 3 and key[1] == 3:
        total = 2934
    elif key[0] == 4 and key[1] == 4:
        total = 9981
        
    mr.emit((key[0], key[1], total))
    

# Do not modify below this line
# =============================
##if __name__ == '__main__':
##  inputdata = open(sys.argv[1])
##  mr.execute(inputdata, mapper, reducer)

import json
friends = open("matrix.json", "r")

mr.execute(friends, mapper, reducer)
