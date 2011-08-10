from xml.etree import ElementTree as ET
import os
import csv, codecs, cStringIO
import re

os.chdir('/home/markhuberty/Documents/stackexchange/data')
geocoded_data = []

## Read in the data
with open ('geocoded_results.csv', 'rb') as f:
    reader = csv.DictReader(f)
    for row in reader:
        geocoded_data.append(row)

## Parse the xml 
xml_elements = []
for row in geocoded_data:
    xml_elements.append(ET.XML(row['xml.location']))


## Regexp to strip out the http: precursors in the tags
p = re.compile('[{](.)*[}]')
place_names = []

## Stich the original location and the geocoded result together
for s, l in zip(xml_elements, geocoded_data):#subelement in xml_elements:
    try:
        data = s[3][4][1]
        this_dict = {}
        this_dict['orig.location'] = l['location']
        for subdata in data:
            label_value = p.sub("", subdata.tag)
            this_dict[label_value] = subdata.text
        place_names.append(this_dict)
    except Exception:
        print "Element not found"

## Write out as a csv file
fieldnames= ('location',
             'woeId',
             'centroid',
             'type',
             'name'
             )


with open('location_geocoded_final.csv', 'wt') as f:
    writer = csv.DictWriter(f, fieldnames)
    header = dict( (n,n) for n in fieldnames)
    writer.writerow(header)
    for row in place_names:
        woeID = row['woeId']
        centroid = row['centroid']
        type_var = row['type']
        name_var = row['name']
        writer.writerow({'location':row['orig.location'],
                         'woeId': woeID.encode('utf-8'),
                         'centroid': centroid.encode('utf-8'),
                         'type': type_var.encode('utf-8'),
                         'name': name_var.encode('utf-8')}
                        )
             
quit()
