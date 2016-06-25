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
    #print "Key: " + key
    value = record[1]
    #print "Value: " + value
    words = value.split()
    #print "=============================="
    #print "Intermediate: "
    for w in words:
      mr.emit_intermediate(w, key)
    #print mr.emit_intermediate(w, key)
 

def reducer(key, document_id_list):
    # key: word
    # value: list of occurrence counts
    #print "============="
    #print "KKey: "
    #print key
    id = None
    print "List: "
    print document_id_list 
    total = []
    if key in document_id_list:
        #document_id_list = set(document_id_list)
        mr.emit((key, document_id_list))
    #for doc in document_id_list:
        #print key
        #total = total.append(doc)
        #print "Document: "
        #print doc
        #print len(total)
  
    mr.emit((key, list(set(document_id_list))))
    

# Do not modify below this line
# =============================
##if __name__ == '__main__':
##  inputdata = open(sys.argv[1])
##  mr.execute(inputdata, mapper, reducer)

import json
f = open("books.json", "r")

#mr.execute(f, mapper, reducer)
