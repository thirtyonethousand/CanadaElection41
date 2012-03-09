#!/usr/bin/awk -f
#
# Grab first column of tab delimited file
#
# This is used to create a list of electoral
# districts putatively affected by vote suppression
#
#
# Usage:
#
# ./extract.awk.sh election_number="nn" ["input_file.csv"]+ > "output_file.csv"
#
# ../Source/./extract.awk.sh election_number="41" TheSixthEstate.tab > extract.csv
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0
# @param string key-value pair for the election number
# @param string File name

# Format bare strings
function quote_string(bare_string)
{
	gsub(/\"+/, "", bare_string)
	guarded_string = "\"" bare_string "\""
	return guarded_string
}

# Clean numeric fields
function clean_number(bare_number)
{
	gsub(/[^0-9]+/, "", bare_number)
	return bare_number
}

# Initialization
BEGIN {

	# Read each line as a record, with comma separated fields
	RS = "\n"
	FS = "\t"

	# Return each record as a line, with comma separated fields
	ORS = "\n"
	OFS = ","

	# Declare record counter
	record_number = 0

	# Header row
	print \
		quote_string("record_number"), \
		quote_string("election_number"), \
		quote_string("electoral_district_english")
}

# Parse a single line as long as it is after the header
FNR > 1 {

	# Increment record count
	record_number++

	# Return record
	print \
		clean_number(record_number), \
		clean_number(election_number), \
		quote_string($1)
}