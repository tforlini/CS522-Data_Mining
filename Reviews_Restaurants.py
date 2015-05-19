#!/usr/bin/env python
import json
import sys
import glob
from collections import defaultdict

def sortRestaurants(dirpath):
	print dirpath + '/.*'
	files_list = glob.glob(dirpath + 'x*')
	print len(files_list)
	restaurants = set()
	reviews = defaultdict(list) # key: business_id; value: list reviews*

	print "Load restaurants"
	with open('C:/Users/tofor_000/Desktop/Restaurants.json') as f:
		for line in f:
			resto = json.loads(line)
			restaurants.add(resto['business_id'])

	print len(restaurants)
	print "Load reviews"
	with open('C:/Users/tofor_000/Desktop/reviews.json','w') as outfd:
		for f in files_list:
			with open(f) as fd:
				for line in fd:
					r = json.loads(line)
					if r['business_id'] in restaurants:
						
						new_r = {k: r[k] for k in ['business_id', 'stars', 'text']}
						outfd.write(json.dumps(new_r) + "\n")
						
	
if __name__ == '__main__':
	# if len(sys.argv) != 2:
		# print "Usage: Reviews_Restaurants.py <dirpath>"
	sortRestaurants("C:/Users/tofor_000/Desktop/Cours_IIT/DataMining/yelp_dataset_challenge_academic_dataset/")
