# Import the data set
#
# Load the data and process the affiliations
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0

# Declare signature
load.data <- function(
	file.name,
	election.dimension = "election_number",
	district.dimension = "electoral_district_number",
	station.dimension = "polling_station_number",
	affiliation.dimension = "affiliation_hash",
	enumerate.dimension = "affiliation_code",
	rejected.ballots = " -- rejected ballots -- ",
	censored.electors = " -- censored electors -- ",
	affiliation.format = " -- %02d -- "
)
{

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
cat("start data load\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pile the hay into a stack
	haystack.data <- read.csv(
		file = file.name,
		na.strings = "NA",
		nrows = -1,
		skip = 0,
		check.names = TRUE,
		strip.white = FALSE,
		blank.lines.skip = TRUE
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - Enumerate affiliation hash\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
	needle.index <- as.logical(
		(haystack.data[, affiliation.dimension] != rejected.ballots) &
		(haystack.data[, affiliation.dimension] != censored.electors)
	);

	# Pull the needles from the haystack with a thimble
	thimble.data <- sort(unique(as.character(haystack.data[needle.index, affiliation.dimension])));

	# Stick the needles in the pincushion
	pincushion.data <- c(sprintf(affiliation.format, 1:(length(thimble.data) + 2)));
	names(pincushion.data) <- c(thimble.data, rejected.ballots, censored.electors);

	# Hide the needles in the haystack
	haystack.data[, enumerate.dimension] <- factor(pincushion.data[as.character(haystack.data[, affiliation.dimension])]);

	# Find the needles in the haystack
	needle.index <- order(
		haystack.data[, election.dimension],
		haystack.data[, district.dimension],
		haystack.data[, station.dimension],
		haystack.data[, enumerate.dimension],
		na.last = FALSE,
		decreasing = FALSE
	);

	# Sort the needles in the haystack
	haystack.data <- haystack.data[needle.index, ];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - done load data\n");
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Send the hay to market
	return(haystack.data);
}