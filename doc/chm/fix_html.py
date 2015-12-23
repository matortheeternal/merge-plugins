import re

# main execution
filename = raw_input('Filename: ')

# create output file and base text
output = open('./out.htm', 'w+')

# parse headers from input file
with open(filename, 'r') as input:
	data = input.read()
	# remove western class
	data = data.replace(' CLASS="western"', '')
	# replace redundant page-break-before style
	data = data.replace(' STYLE="page-break-before: always"', '')
	# replace reundant table style
	data = data.replace(' WIDTH=665 BORDER=1 BORDERCOLOR="#000000" CELLPADDING=4 CELLSPACING=0', '')
	
	# replace redundant paragraph styles
	data = re.sub('<P STYLE="(font-style: normal){0,1}(; ){0,1}(font-weight: normal){0,1}">', '<P>', data)
	data = re.sub('<P ALIGN=LEFT>', '<P>', data)
	# replace redundant newlines
	data = re.sub('<P( STYLE="margin-bottom: 0in"){0,1}>(<BR>)*(\n)*(<BR>)*</P>', '\n', data)
	# replace redundant table cell widths
	data = re.sub("<T(D|H) WIDTH=([0-9]{1,3})>", "<T\\1>", data)
	# replace refheading links
	data = re.sub('<A NAME="__RefHeading__([0-9]{1,4})_([0-9]{1,10})"></A>', '', data)
	# replace class tableText
	data = re.sub('<P><FONT FACE="Arial, sans-serif"><FONT SIZE=2>([\w|\s|\.|\-|\,|\'|\/|\(|\)|\;|\&]*)</FONT></FONT></P>', '<P CLASS="tableText">\\1</P>', data)
	# replace tableText with coloring and styling
	data = re.sub('<P><FONT COLOR="#([0-9|a-f]*)"><FONT FACE="Arial, sans-serif"><FONT SIZE=2>(<B>){0,1}([\w|\s]*)(</B>){0,1}(</FONT>){3}</P>', '<P CLASS="tableText"><FONT COLOR="#\\1">\\2\\3\\4</FONT></P>', data)
	# add a new line before headers
	data = re.sub('<H([1-5])', '\n<H\\1', data)
	# remove outline links
	data = re.sub('<A NAME="([0-9|.]{1,9}).([\w|\s]*)\|outline"></A>', '', data)
	# add m-num links
	data = re.sub('</H([1-5])>', '<A NAME="m00"></A></H\\1>', data)
	# remove spans
	data = re.sub('<SPAN ID="Frame([\w|\W]{1,4000})</SPAN><BR CLEAR=LEFT>', '', data)
	# remove newlines from headers
	data = re.sub('<H([1-5])>([\w| ]*)\n([\w| |?]*)(\n){0,1}<A NAME="m00"></A></H([1-5])>', '<H\\1>\\2 \\3<A NAME="m00"></A></H\\5>', data)
	data = re.sub('<H([1-5])>( ){0,1}([\w| ]*)<A NAME="m00"></A></H([1-5])>', '<H\\1>\\3<A NAME="m00"></A></H\\4>', data)
	# trim maximum newline count to 2 blank lines
	data = re.sub('(\n){4,12}', '\n\n\n', data)
	
	# write data to output
	output.write(data)

# close output
output.close()