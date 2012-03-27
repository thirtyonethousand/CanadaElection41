# Plot Elector fractions
#
# For each real affiliation, plot the fraction
# of votes versus fraction of censored electors.
# Note the plots depend on the input data being
# properly sorted according to polling station
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0


# Declare signature
censor.fraction.plot <- function(
	haystack.data,
	file.name = "",
	district.dimension = "electoral_district_number",
	station.dimension = "polling_station_number",
	affiliation.dimension = "affiliation_code",
	elector.dimension = "elector_count",
	hash.dimension = "affiliation_hash",
	rejected.ballots = " -- rejected ballots -- ",
	censored.electors = " -- censored electors -- ",
	column.count = 5,
	output.width = 16384,#1024
	output.height = 16384,#1024
	inch.resolution = 1024,#64
	font.size = 8,#12
	point.size = 0.5#0.125
)
{

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
cat("start plot censor affiliate fractions\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
		grepl("^S/R 2$", haystack.data[, station.dimension])
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	new.elector.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.elector.data[, district.dimension],
		new.elector.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.elector.data <- new.elector.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new electors\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	regular.elector.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.elector.data[, district.dimension],
		regular.elector.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.elector.data <- regular.elector.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular electors\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] != censored.electors) &
		(
			grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
			grepl("^S/R 2$", haystack.data[, station.dimension])
		)
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	new.ballot.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.ballot.data[, district.dimension],
		new.ballot.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.ballot.data <- new.ballot.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] != censored.electors) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	regular.ballot.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.ballot.data[, district.dimension],
		regular.ballot.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.ballot.data <- regular.ballot.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #


	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] != censored.electors) &
		(haystack.data[, hash.dimension] != rejected.ballots) &
		(
			grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
			grepl("^S/R 2$", haystack.data[, station.dimension])
		)
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	new.total.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.total.data[, district.dimension],
		new.total.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.total.data <- new.total.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new votes\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] != censored.electors) &
		(haystack.data[, hash.dimension] != rejected.ballots) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	regular.total.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.total.data[, district.dimension],
		regular.total.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.total.data <- regular.total.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular votes\n");
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
	new.rejected.data <- haystack.data[needle.index, elector.dimension] / new.ballot.data;

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new rejected\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == rejected.ballots) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Stick the needles in the pincushion
	regular.rejected.data <- haystack.data[needle.index, elector.dimension] / regular.ballot.data;

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular rejected\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == censored.electors) &
		(
			grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
			grepl("^S/R 2$", haystack.data[, station.dimension])
		)
	);

	# Stick the needles in the pincushion
	new.censored.data <- haystack.data[needle.index, elector.dimension] / new.elector.data;

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new censored\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == censored.electors) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Stick the needles in the pincushion
	regular.censored.data <- haystack.data[needle.index, elector.dimension] / regular.elector.data;

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular censored\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Stick the needles in the pincushion
	regular.colour <- rgb(
		red = ceiling(31 + 224 * sqrt(regular.censored.data / max(regular.censored.data))),
		green = ceiling(31 + 224 * (1 - regular.censored.data / max(regular.censored.data)) ^ 2),
		blue = 0,
		alpha = ceiling(31 + 224 * (regular.censored.data / max(regular.censored.data)) ^ 2),
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
	thimble.data <- c(
		thimble.data[needle.index],
		rejected.ballots,
		censored.electors
	);

	# Save to file
	if (nchar(file.name) > 0)
	{
		png(
			file = file.name,
			height = output.height,
			width = output.width,
			res = inch.resolution,
			pointsize = font.size,
			type = "cairo",
			bg = "white"
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
		new.votes.data <- haystack.data[needle.index, elector.dimension] / new.ballot.data;

		# Pull the needles from the haystack
		needle.index <- as.logical(
			(haystack.data[, hash.dimension] == needle.instance) &
			(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
			(! grepl("^S/R 2$", haystack.data[, station.dimension]))
		);

		# Stick the needles in the pincushion
		regular.votes.data <- haystack.data[needle.index, elector.dimension] / regular.ballot.data;
		affiliation.code <- unique(haystack.data[needle.index, affiliation.dimension]);

		# Start a new empty plot
		plot(
			c(0, 1),
			c(0, 1),
			type = "n",
			xaxt = "n",
			yaxt = "n",
			main = affiliation.code,
			xlab = "Censored Fraction",
			ylab = "Vote Fraction",
			frame.plot = FALSE
		);

		# X-axis
		axis(
			side = 1,
			at = c(0, 1),
			labels = c("0", "1")
		);

		# Y-axis
		axis(
			side = 2,
			at = c(0, 1),
			labels = c("0", "1")
		);

		# Pull the needles from the haystack and stick them in the pincushion
		points(
			x = regular.censored.data,
			y = regular.votes.data,
			type = "p",
			pch = 16,
			cex = point.size,
			col = regular.colour
		);

		# Pull the needles from the haystack and stick them in the pincushion
		points(
			x = new.censored.data,
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
cat(" - done plot censor affiliate fractions\n");
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
}