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
    key = record[0]
    value = record[1]
    #print value
    for i in value:
        trimmed = value[0:len(value) - 10]
    #print "Trimmed: " + trimmed
   
    mr.emit_intermediate(trimmed, key)

def reducer(key, list_of_values):
    # key: word
    # value: list of occurrence counts
    print key

    mr.emit((key))

# Do not modify below this line
# =============================
##if __name__ == '__main__':
##  inputdata = open(sys.argv[1])
##  mr.execute(inputdata, mapper, reducer)
##
import json
friends = open("dna.json", "r")
#friends = open("asymmetric_friendships.json", "r")
mr.execute(friends, mapper, reducer)
