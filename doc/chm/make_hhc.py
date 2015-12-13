from bs4 import BeautifulSoup

# main execution
hlevels = ['h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8,', 'h9']
filename = raw_input('Filename: ')

# create output file and base text
output = open('./Table of Contents.hhc', 'w+')
output.write('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">\n')
output.write('<HTML>\n')
output.write('<HEAD>\n')
output.write('<meta name="GENERATOR" content="Microsoft&reg; HTML Help Workshop 4.1">\n')
output.write('<!-- Sitemap 1.0 -->\n')
output.write('</HEAD><BODY>\n')
output.write('<OBJECT type="text/site properties">\n')
output.write('	<param name="Window Styles" value="0x800025">\n')
output.write('</OBJECT>\n')
output.write('<UL>\n')

# parse headers from input file
with open(filename, 'r') as input:
	data = input.read()
	soup = BeautifulSoup(data, "html.parser")
	headers = soup.find_all({'h1', 'h2', 'h3', 'h4'})
	current_level = 1;
	for h in headers:
		if h.a != None:
			level = hlevels.index(h.name) + 1
			
			# use ULs for sub-levels
			while level > current_level:
				output.write('  ' * current_level + '<UL>\n')
				current_level += 1;
			while current_level > level:
				current_level -= 1;
				output.write('  ' * current_level + '</UL>\n')
			
			# produce li
			link = h.a['name']
			ws = '  ' * level
			output.write(ws + '<LI> <OBJECT type="text/sitemap">\n')
			output.write(ws + '	 <param name="Name" value="' + h.text + '">\n')
			output.write(ws + '	 <param name="Local" value="' + filename + '#' + link + '">\n')
			output.write(ws + '	 </OBJECT>\n')
			current_level = level
			
	# close open levels
	while current_level > 0:
		current_level -= 1;
		output.write('  ' * current_level + '</UL>\n')

# close output
output.close()