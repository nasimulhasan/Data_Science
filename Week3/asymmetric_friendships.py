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
    key = record[0]
    value = record[1]
##    print "value"
##    print value
##    print "Key: " + key + " Value: " + value
    
##    words = value.split()
##    for w in words:
    mr.emit_intermediate(key, [key, value])

def reducer(key, list_of_values):
    # key: word
    # value: list of occurrence counts
    #print (key, list_of_values)

##    print key
##    print type(list_of_values)
    dct = {}
    #mr.emit((list_of_values))
    a = [key]
    #print a
    #print type(a)
##    tab = list_of_values
##    print "Mapped list: "
##    print tab
##    print "======================================================="
    #print "================"
    for i in range(len(list_of_values)):
        l1 = list_of_values[i]
        l3 = tuple(list(l1))
        #print l3
##        
        mr.emit((l3))
        list_of_values[i][1], list_of_values[i][0] = list_of_values[i][0], list_of_values[i][1]
        x = list_of_values
        if x[i] != l1:
            l2 =  x[i]
        #l1 = tuple(l1)
            #print l2
        mr.emit((x[i]))
        
##    print "Swapped list: "
##    l = list_of_values
##    print x
##    print "======================================================="
##    for i in list_of_values:
##        print i
##    t = []
##    #if tab != list_of_values:
##    x = x + tab
##    print "Joined list: "
##    print x
    #print tab

##    for i in t:
##        print i
##    for j in tab:
##        for k in list_of_values:
##            if j != k:
##                tab += list_of_values
##            print tab
  
##    for i in list_of_values:
##        print type(i)
##        print i[0]
    #print tab
##    for i in range(len(tab)):
##        print i
        #dct[tab[i]] = tab[i + 1]
        
    #print dct




# Do not modify below this line
# =============================
##if __name__ == '__main__':
##  inputdata = open(sys.argv[1])
##  mr.execute(inputdata, mapper, reducer)


import json
friends = open("friends.json", "r")
#friends = open("asymmetric_friendships.json", "r")
mr.execute(friends, mapper, reducer)
