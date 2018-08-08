# -*- coding: iso-8859-2 -*-

import urllib2
import urllib
import json
import re
import time
from bs4 import BeautifulSoup
import pandas as pd

storage = pd.read_csv("list.csv", sep=";", encoding="cp1250")
output = open("outputCompanies.txt", "a")

mainhref = "http://nemzeticegtar.hu"
main_url = mainhref + "/nemzeticegtar/lista"
for i in range(80, 131):
	# len(storage['short'])
	errorLog = open("errorLog.txt", "a")
	ceg = storage['short'][i]
	ceg = ceg.replace(" ", "+")
	ceg = re.sub(r"[\n]", "", ceg)
	ceg = urllib.quote(ceg.encode("cp1250")).decode("cp1250")
	print(ceg)
	# raw_input()
	url = main_url + "?cname=" + ceg
	print(url)
	# raw_input()
	# # print(ceg)
	try:
		response = urllib2.urlopen(url)
		soup = BeautifulSoup(response)
		newurl = mainhref + soup.find(id="ceglista").find(class_="cegadat").find("a").get("href")
		print newurl
		try:
			response = urllib2.urlopen(newurl)
			soup = BeautifulSoup(response)
			limsoup = soup.find(class_="cegadat-table")
			adat_nev = limsoup.find_all(True, {'class':['adat', 'org']})[0].contents[0]
			
			reg = re.compile('Alap\xedt\xe1s\s\xe9ve')
			adat_alap = [e.find_next('dd') for e in limsoup.find_all('dt') if reg.match(e.text)][0].text

			reg = re.compile(u'F\u0151tev\xe9kenys\xe9g')
			adat_tev = [e.find_next('dd') for e in limsoup.find_all('dt') if reg.match(e.text)][0].text
			reg = re.compile('[0-9]{4}')
			m = reg.match(adat_tev)
			adat_tev = m.group(0)

			print(adat_nev.encode("cp1250"))
			print(adat_alap)
			print(adat_tev)
			print(unicode(i).encode("cp1250"))
			oid = storage['id'][i]
			print(unicode(oid).encode("cp1250"))
			output.write((unicode(oid) + ";" + adat_nev + ";" + adat_alap + ";" + adat_tev).encode("cp1250") + "\n")

		except Exception, e:
			print "fuck!"
			errorLog.write("level1:" + str(ceg) + ":" + str(e) + "\n")
	except Exception, e:
	    print "fuck!"
	    errorLog.write("level1:" + str(ceg) + ":" + str(e) + "\n")
	errorLog.close()
	time.sleep(25)
