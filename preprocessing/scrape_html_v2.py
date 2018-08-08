# -*- coding: iso-8859-2 -*-

import sys
from bs4 import BeautifulSoup
import json
import csv
import os

outputDir = "C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/processedData"

readFiles = [2011,2012,2013,2014]

companies = range(1,546,1)

progress = 0
types = {
	"long": 0,
	"long2": 0,
	"short": 0,
	"short2": 0,
	"short3": 0
}
for l in companies:
	print(chr(27) + "[2J")
	print(str(round(float(l)/len(companies)*100,1)) + "%")
	if os.path.isdir(str(l).zfill(3)):
		os.chdir(str(l).zfill(3))
		for k in readFiles:
			file = str(k) + ".html"
			fileOk = 0
			if os.path.isfile(file):
				fileOk = 1
			else: 
				file = str(k) + "_erki.html"
				if os.path.isfile(file):
					fileOk = 1
				else:
					fileOk = 0
			if fileOk == 1:
				tx = open(file)
				currdir = os.getcwd()
				currdir = currdir[len(currdir)-4:len(currdir)]
				raw = tx.read()
				soup = BeautifulSoup(raw, "html.parser")
				tabs = 0
				type_of_document = 0
				if soup.find(id="tab2") != None:
					if soup.find(id="tab14") != None:
						if len(soup.find(id="tab14")) > 0:
							type_of_document = "long"
						elif soup.find(id="tab10") != None:
							type_of_document = "long2"
					elif soup.find(id="tab10") != None:
						type_of_document = "long2"
					elif soup.find(id="tab6") != None:
						if len(soup.find(id="tab6")) > 0:
							type_of_document = "short"
						elif soup.find(id="tab8") != None:
							type_of_document = "short3"
					else:
						type_of_document = "short2"
				if(type_of_document != 0):
					if type_of_document == "long":
						fd = open(outputDir + '/document_long.csv', 'a')
						csvwrite = csv.writer(fd)
						tab2 = soup.find(id="tab2").find_all("tr")
						tab3 = soup.find(id="tab3").find_all("tr")
						tab4 = soup.find(id="tab4").find_all("tr")
						tab5 = soup.find(id="tab5").find_all("tr")
						tab14 = soup.find(id="tab14").find_all("tr")
						tab15 = soup.find(id="tab15").find_all("tr")
						tabs = tab2 + tab3 + tab4 + tab5 + tab14 + tab15
					elif type_of_document == "long2":
						fd = open(outputDir + '/document_long2.csv', 'a')
						csvwrite = csv.writer(fd)
						tab2 = soup.find(id="tab2").find_all("tr")
						tab3 = soup.find(id="tab3").find_all("tr")
						tab4 = soup.find(id="tab4").find_all("tr")
						tab5 = soup.find(id="tab5").find_all("tr")
						tab10 = soup.find(id="tab10").find_all("tr")
						tab11 = soup.find(id="tab11").find_all("tr")
						tabs = tab2 + tab3 + tab4 + tab5 + tab10 + tab11
					elif type_of_document == "short":
						fd = open(outputDir + '/document_short.csv', 'a')
						csvwrite = csv.writer(fd)
						tab2 = soup.find(id="tab2").find_all("tr")
						tab3 = soup.find(id="tab3").find_all("tr")
						tab6 = soup.find(id="tab6").find_all("tr")
						tabs = tab2 + tab3 + tab6
					elif type_of_document == "short3":
						fd = open(outputDir + '/document_short3.csv', 'a')
						csvwrite = csv.writer(fd)
						tab2 = soup.find(id="tab2").find_all("tr")
						tab3 = soup.find(id="tab3").find_all("tr")
						tab8 = soup.find(id="tab8").find_all("tr")
						tabs = tab2 + tab3 + tab8
					else:
						fd = open(outputDir + '/document_short2.csv', 'a')
						csvwrite = csv.writer(fd)
						tab2 = soup.find(id="tab2").find_all("tr")
						tab3 = soup.find(id="tab3").find_all("tr")
						tab4 = soup.find(id="tab4").find_all("tr")
						tabs = tab2 + tab3 + tab4
					ev = int(soup.find(class_="BalanceFormHeader").find_all("tr")[2].find(class_="value").contents[0][:4])
					penzegyseg = soup.find(class_="BalanceFormHeader").find_all("tr")[1].find_all("td")[3].contents[0]
					if(type_of_document == 'long' or type_of_document == 'long2' or type_of_document == 'short' or type_of_document == 'short3'):
						threshold = 5
						if(types[type_of_document] == 0):
							linesTo = [1, 2, 4]
							types[type_of_document] = 1
						else:
							linesTo = [2, 4]
					else:
						threshold = 4
						if(types[type_of_document] == 0):
							linesTo = [1, 2, 3]
							types[type_of_document] = 1
						else:
							linesTo = [2, 3]
					for j in linesTo:
						if j == 1:
							z = ["id", "ev", "penzegyseg"]
						elif j == 2:
							z = [currdir, ev-1, penzegyseg]
						elif j == 3:
							z = [currdir, ev, penzegyseg]
						elif j == 4:
							z = [currdir, ev, penzegyseg]
						else: 
							z = [" ", " ", " "]
						for i in tabs:
							# print("_______")
							# print("types: " + str(types))
							# print("lines: " + str(linesTo))
							# print(i)
							# print("j: " + str(j))
							if len(i.find_all("td")) >= threshold:
								if(len(i.find_all("td")[j].contents) > 0):
									z.append(i.find_all("td")[j].contents[0].encode('utf-8'))
								else:
									z.append("")
						csvwrite.writerow(z)
					fd.close()
		os.chdir('../')	