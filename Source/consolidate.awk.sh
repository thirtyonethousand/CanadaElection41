#!/usr/bin/awk -f
#
# Concatenate multiple files into a single file
#
# Given a list of pollresults...csv files concatenate
# stripping headers and footers.
#
# Usage:
#
# ./consolidate.awk.sh election_number="nn" ["input_file.csv"]+ > "output_file.csv"
#
# ../../Source/./consolidate.awk.sh election_number="41" pollresults*.csv > consolidate.csv
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0
# @param string key-value pair for the election number
# @param string File name
# @optional Additional file names can be added

# Format bare strings
function quote_string(bare_string)
{
	gsub(/\"+/, "", bare_string)
	guarded_string = "\"" bare_string "\""
	return guarded_string
}

# Initialization
BEGIN {

	# Read each line as a record, with comma separated fields
	RS = "\n"
	FS = ","

	# Return each record as a line, with comma separated fields
	ORS = "\n"
	OFS = ","

	# Declare record counter
	record_number = 0

	# Header row
	print \
		quote_string("record_number"), \
		quote_string("election_number"), \
		quote_string("electoral_district_number"), \
		quote_string("electoral_district_english"), \
		quote_string("electoral_district_french"), \
		quote_string("polling_station_number"), \
		quote_string("polling_station_english"), \
		quote_string("polling_station_french"), \
		quote_string("void_poll_flag"), \
		quote_string("no_poll_flag"), \
		quote_string("merge_poll"), \
		quote_string("last_name"), \
		quote_string("middle_name"), \
		quote_string("first_name"), \
		quote_string("political_affiliation_english"), \
		quote_string("political_affiliation_french"), \
		quote_string("incumbent_flag"), \
		quote_string("elected_flag"), \
		quote_string("vote_count")
}

# Parse a single line as long as it is after the header
FNR > 1 {

	# Increment record count
	record_number++

	print \
		quote_string(record_number), \
		quote_string(election_number), \
		quote_string($1), \
		quote_string($2), \
		quote_string($3), \
		quote_string($4), \
		quote_string($5), \
		quote_string($6), \
		quote_string($7), \
		quote_string($8), \
		quote_string($9), \
		quote_string($10), \
		quote_string($11), \
		quote_string($12), \
		quote_string($13), \
		quote_string($14), \
		quote_string($15), \
		quote_string($16), \
		quote_string($17)
}