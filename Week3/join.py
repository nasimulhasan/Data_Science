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
    table_name = record[0]
    #print "Key: " + key
    key = record[1]
    #print "Value: " + key
    
    order = []
    if table_name == "order":
        order = record
    #print order

    line_item = []
    if table_name == "line_item":
        line_item = record
    #print line_item

    #print "=============================="
    #print "Intermediate: "
##    if order[1] == line_item[1]:
##        print order, line_item
##    for w in words:
##      mr.emit_intermediate(w, record)
##    print type(w), type(key)
    #print "Intermediate key"
    mr.emit_intermediate(key, record)
 

def reducer(key, document_id_list):
    # key: word
    # value: list of occurrence counts
    #print key
    id = None
    #print "List: "
    print document_id_list
    len_tab = len(document_id_list)
    #print "len_tab: " + str(len_tab)
   
##    orderId = document_id_list[0]
    #print document_id_list
    
    for i in document_id_list:
        #if i != "order":
        if i[0] != "order":
            result = document_id_list[0] + i
            print type(i)
            mr.emit(result)
        #print "print index" + str(i)
    #print result
    #print len(result)
    

# Do not modify below this line
# =============================
##if __name__ == '__main__':
##  inputdata = open(sys.argv[1])
##  mr.execute(inputdata, mapper, reducer)

import json
records = open("records.json", "r")

mr.execute(records, mapper, reducer)
