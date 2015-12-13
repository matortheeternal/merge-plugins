from bs4 import BeautifulSoup

# main execution
filename = raw_input('Filename: ')

# create output file and base text
output = open('./Index.hhk', 'w+')
output.write('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">\n')
output.write('<HTML>\n')
output.write('<HEAD>\n')
output.write('<!-- Sitemap 1.0 -->\n')
output.write('</HEAD><BODY>\n')
output.write('<UL>\n')

# parse headers from input file
with open(filename, 'r') as input:
	data = input.read()
	soup = BeautifulSoup(data, "html.parser")
	headers = soup.find_all({'h1', 'h2', 'h3', 'h4'})
	# headers = soup.find_all('h1') 
	for h in headers:
		if h.a != None:
			link = h.a['name']
			output.write('<LI> <OBJECT type="text/sitemap">\n')
			output.write('     <param name="Name" value="' + h.text + '">\n')
			output.write('     <param name="Local" value="' + filename + '#' + link + '">\n')
			output.write('     </OBJECT>\n')
			output.write('     \n')

# finalize output and close it
output.write('</UL>\n')
output.close()