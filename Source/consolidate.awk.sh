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

# Clean up and quote strings
function quote_string(dirty_string)
{
	gsub(/\"/, "", dirty_string)
	clean_string = "\"" dirty_string "\""
	return clean_string
}

# Initialization
BEGIN {

	# Read each line as a single field
	RS = "\r\n"
	FS = "\r\n"

	# Return each record as a line, with comma separated fields
	ORS = "\n"
	OFS = ","

	# Declare record counter
	record_number = 0

	# Header row
	print \
		"record_number", \
		"election_number", \
		"electoral_district_number", \
		"electoral_district_english", \
		"electoral_district_french", \
		"polling_station_number", \
		"polling_station", \
		"void_poll_flag", \
		"no_poll_flag", \
		"merge_poll", \
		"rejected_count", \
		"elector_count", \
		"last_name", \
		"middle_name", \
		"first_name", \
		"political_affiliation_english", \
		"political_affiliation_french", \
		"incumbent_flag", \
		"elected_flag", \
		"vote_count"
}

# Parse a single line as long as it is after the header
FNR > 1 {

	# Increment record count
	record_number++

	# Get the row
	return_row = $0

	# test the syntax
	test_result = match(return_row, /^[0-9]+,\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",[YN],[YN],\"[^\"]*\",[0-9]+,[0-9]+,\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",[YN],[YN],[0-9]+$/)

	# Syntax failure
	if (test_result <= 0)
	{
		field_count = split(return_row, field_list, /,/)
		return_row = \
			field_list[1] "," \
			quote_string(field_list[2]) "," \
			quote_string(field_list[3]) "," \
			quote_string(field_list[4]) "," \
			quote_string(field_list[5]) "," \
			field_list[6] "," \
			field_list[7] "," \
			quote_string(field_list[8]) "," \
			field_list[9] "," \
			field_list[10] "," \
			quote_string(field_list[11]) "," \
			quote_string(field_list[12]) "," \
			quote_string(field_list[13]) "," \
			quote_string(field_list[14]) "," \
			quote_string(field_list[15]) "," \
			field_list[16] "," \
			field_list[17] "," \
			field_list[18]
	}

	# Clean up miscellaneous formatting
	gsub(/,\" +/, ",\"", return_row)
	gsub(/ +\",/, "\",", return_row)
	gsub(/,Y/, ",\"Y\"", return_row)
	gsub(/,N/, ",\"N\"", return_row)
	gsub(/,\"\",/, ",<<NULL>>,", return_row)
	gsub(/,,/, ",<<NULL>>,", return_row)

	# Return record
	print \
		record_number, \
		election_number, \
		return_row
}