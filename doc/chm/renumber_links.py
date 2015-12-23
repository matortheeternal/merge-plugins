import re

filename = raw_input('Filename: ')
output = open('./out.htm', 'w+')

# parse headers from input file
with open(filename, 'r') as input:
	data = input.read()
	pattern = re.compile(r"name=m([0-9]{2,3})")
	i = 0
	for line in data.splitlines():
		n = pattern.search(line)
		if (n != None) & (i != n):
			old = n.group(0)
			new = "name=m" + format(i, '02')
			print("Replacing "+old+" with "+new)
			output.write(line.replace(old, new)+'\n')
			i += 1
		else:
			output.write(line+'\n')