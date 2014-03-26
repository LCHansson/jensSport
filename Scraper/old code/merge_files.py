#!/usr/bin/env python
# -*- coding: utf-8 -*-
import csv
import glob

# Get existing leagues
files = glob.glob("data_events/*.csv")


fout=open("all goals.csv","a")
# first file:

for line in open(files[0]):
	fout.write(line)


for fileName in files:
	f = open(fileName)
	f.next() # skip the header
	for line in f:
		fout.write(line)
	f.close() # not really needed
fout.close()