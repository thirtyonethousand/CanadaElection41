# Plot Elector Counts
#
# For each real affiliation, plot the number
# of votes versus number of rejected ballots.
# Note the plots depend on the input data being
# properly sorted according to polling station
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0

# Declare signature
reject.affiliate.plot <- function(
	haystack.data,
	file.name = "",
	district.dimension = "electoral_district_number",
	station.dimension = "polling_station_number",
	affiliation.dimension = "affiliation_code",
	elector.dimension = "elector_count",
	hash.dimension = "affiliation_hash",
	rejected.ballots = " -- rejected ballots -- ",
	censored.electors = " -- censored electors -- ",
	column.count = 6,
	output.width = 12288,
	output.height = 8192,
	point.size = 0.1,
	label.magnification = 16
)
{

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
cat("start plot reject affiliate counts\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == rejected.ballots) &
		(
			grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
			grepl("^S/R 2$", haystack.data[, station.dimension])
		)
	);

	# Stick the needles in the pincushion
	new.rejected.data <- haystack.data[needle.index, elector.dimension];

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == rejected.ballots) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Stick the needles in the pincushion
	regular.rejected.data <- haystack.data[needle.index, elector.dimension];
	rejected.range <- c(0, max(new.rejected.data, regular.rejected.data));

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == censored.electors) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Stick the needles in the pincushion
	regular.colour <- rgb(
		red = ceiling(31 + 224 * sqrt(haystack.data[needle.index, elector.dimension] / max(haystack.data[needle.index, elector.dimension]))),
		green = ceiling(31 + 224 * (1 - haystack.data[needle.index, elector.dimension] / max(haystack.data[needle.index, elector.dimension])) ^ 2),
		blue = 0,
		alpha = ceiling(31 + 224 * (haystack.data[needle.index, elector.dimension] / max(haystack.data[needle.index, elector.dimension])) ^ 2),
		maxColorValue = 255
	);

	# Stick the new needles in the pincushion
	new.colour <- rgb(
		red = 0,
		green = 0,
		blue = 255,
		alpha = 63,
		maxColorValue = 255
	);

	# Pull the needles from the haystack with a thimble
	thimble.data <- levels(haystack.data[, hash.dimension]);

	# Inspect the needles in the thimble
	needle.index <- as.logical(
		(thimble.data != rejected.ballots) &
		(thimble.data != censored.electors)
	);

	# Drop the needles from the thimble
	thimble.data <- thimble.data[needle.index];

	# Save to file
	if (nchar(file.name) > 0)
	{
		png(
			file = file.name,
			height = output.height,
			width = output.width
		);
		par(
			cex.main = label.magnification,
			cex.lab = label.magnification,
			cex.axis = label.magnification
		);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(paste(" - open ", file.name, "\n", sep = ""));
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
	}

	# Start output display
	else
	{
		dev.new();
	}

	# Set the multi-plot array
	row.count <- ceiling(length(thimble.data) / column.count);
	par(
		mfrow = c(row.count, column.count),
		mar = c(4, 4, 2, 2),
		bg = "white"
	);

	# Paint with the needles
	for (needle.instance in thimble.data) {

		# Pull the needles from the haystack
		needle.index <- as.logical(
			(haystack.data[, hash.dimension] == needle.instance) &
			(
				grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
				grepl("^S/R 2$", haystack.data[, station.dimension])
			)
		);

		# Stick the needles in the pincushion
		new.votes.data <- haystack.data[needle.index, elector.dimension];

		# Pull the needles from the haystack
		needle.index <- as.logical(
			(haystack.data[, hash.dimension] == needle.instance) &
			(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
			(! grepl("^S/R 2$", haystack.data[, station.dimension]))
		);

		# Stick the needles in the pincushion
		regular.votes.data <- haystack.data[needle.index, elector.dimension];
		votes.range <- c(0, max(new.votes.data, regular.votes.data));
		affiliation.code <- unique(haystack.data[needle.index, affiliation.dimension]);

		# Start a new empty plot
		plot(
			rejected.range,
			votes.range,
			type = "n",
			main = affiliation.code,
			xlab = "Rejected Ballots",
			ylab = "Votes",
			frame.plot = FALSE
		);

		# Pull the needles from the haystack and stick them in the pincushion
		points(
			x = regular.rejected.data,
			y = regular.votes.data,
			type = "p",
			pch = 16,
			cex = point.size,
			col = regular.colour
		);

		# Pull the needles from the haystack and stick them in the pincushion
		points(
			x = new.rejected.data,
			y = new.votes.data,
			type = "p",
			pch = 16,
			cex = point.size,
			col = new.colour
		);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(paste(" - plot ", needle.instance, "\n", sep = ""));
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
	}

	# Close the file
	if (nchar(file.name) > 0)
	{
		dev.off();

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(paste(" - close ", file.name, "\n", sep = ""));
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
	}

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - done plot reject affiliate counts\n");
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
}