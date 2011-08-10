import urllib2
import urllib
import os
import csv
import re
import time

os.chdir('/home/markhuberty/Documents/stackexchange/data')
baseurl = 'http://wherein.yahooapis.com/v1/document'
apikey = 'D_LXiILV34FiY77NdugJQa._sV3kNPewot_0KsO.ZrNvX_iqWUmEGQ4UhCspZXzL6khK'
documentType = 'text/plain'
requests = []

## Initiate the locations and read in the CSV file
locations = []

def load_locations(location_csv):
    locations = []
    with open(location_csv, 'rb') as f:
        for row in csv.reader(f):
            locations.append(row)
    return locations

def format_request(locations, apikey):
    requests = []
    for location in locations:
        data = {'documentContent':location[1], 
                'documentType':'text/plain', 
                'appid':apikey
                }
        requests.append(data)
    return requests
        
def geocode_request(locations, requests, baseurl, sleep_interval=1):
    geocoded_location = []    
    for l, r, n in zip(locations, requests, range(len(requests))):
        print n
        try:
            encoded_request = urllib.urlencode(r)
            req = urllib2.Request(baseurl, encoded_request)
            response = urllib2.urlopen(req) 
        except urllib2.URLError, e:
            if hasattr(e, 'reason'):
                print 'Reason', e.reason
            elif hasattr(e, 'code'):
                print 'Error code', e.code
        else:
            result = response.read()
            geocoded_location.append({'location':l[1], 'result':result})
        time.sleep(sleep_interval)
    return(geocoded_location)

## End function definition
#################################################################

## Get the locations and convert to encoded requests
locations = load_locations('unique.locations.csv')
requests = format_request(locations, apikey)

#test_results = geocode_request(locations[1:10], requests[1:10], baseurl)
geocoded_results = geocode_request(locations, requests, baseurl, sleep_interval=0.1)


## Write out the results to a csv file
fieldnames = ('location', 'xml.location')
with open('geocoded_results.csv', 'wt') as f:
    writer = csv.DictWriter(f, fieldnames = fieldnames)
    headers = dict( (n,n) for n in fieldnames)
    writer.writerow(headers)
    for result in geocoded_results:
        writer.writerow({'location': result['location'],
                         'xml.location': result['result']
                         }
                        )
            
quit()


