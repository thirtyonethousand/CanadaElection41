#!/usr/bin/awk -f
#
# Syntax Checker
#
# Report any lines of any files that do not meet the syntax
# requirements. List the files at the end of the report
#
# Usage:
#
# ./syntax.awk.sh ["input_file.csv"]+ > "output_file.csv"
#
# ../../Source/./syntax.awk.sh pollresults*.csv > syntax.csv
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0
# @param string File name
# @optional Additional file names can be added

# Initialization
BEGIN {

	# Read each line as a single field
	RS = "\r\n"
	FS = "\r\n"

	# Return each record as a line, with comma separated fields
	ORS = "\n"
	OFS = ","

	# Declare counter
	record_number = 0
	file_number = 0
}

# Parse a single line as long as it is after the header
FNR > 1 {

	# test the syntax
	test_result = match($0, /^[0-9]+,\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",[YN],[YN],\"[^\"]*\",[0-9]+,[0-9]+,\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",\"[^\"]*\",[YN],[YN],[0-9]+$/)

	# Print bad rows
	if (test_result <= 0)
	{

		# Increment the record count
		record_number++

		# Report the problem
		print \
			record_number, \
			$0

		# Test for previous files
		if (file_number <= 0)
		{
			file_number++
			file_list[file_number] = FILENAME
		}
		else if (file_list[file_number] != FILENAME)
		{
			file_number++
			file_list[file_number] = FILENAME
		}
	}
}

# Report the files
END {
	for (file_number in file_list)
	{
		print \
			file_number, \
			file_list[file_number]
	}
}