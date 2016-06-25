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
    #print key
    value = record[1]
    #print value
##    words = value.split()
##    for w in words:
    mr.emit_intermediate(key, value)

def reducer(key, list_of_values):
    # key: word
    # value: list of occurrence counts
##    total = 0
##    for v in list_of_values:
##        print v
##        total += v
    #print type(list_of_values)
    mr.emit((key, len(list_of_values)))

# Do not modify below this line
# =============================
##if __name__ == '__main__':
##  inputdata = open(sys.argv[1])
##  mr.execute(inputdata, mapper, reducer)

import json
friends = open("friends.json", "r")

mr.execute(friends, mapper, reducer)
