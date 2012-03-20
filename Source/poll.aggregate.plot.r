# Plot Elector Counts
#
# Cross plot the polling station aggregates of:
#
# 1. Electors
# 2. Ballots
# 3. Votes
# 4. Rejected Ballots
# 5. Censored Electors
#
# Note the plots depend on the input data being
# properly sorted according to polling station
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0

# Declare signature
poll.aggregate.plot <- function(
	haystack.data,
	file.name = "",
	district.dimension = "electoral_district_number",
	station.dimension = "polling_station_number",
	affiliation.dimension = "affiliation_code",
	elector.dimension = "elector_count",
	hash.dimension = "affiliation_hash",
	rejected.ballots = " -- rejected ballots -- ",
	censored.electors = " -- censored electors -- ",
	output.width = 16384,
	output.height = 16384,
	point.size = 0.1,
	label.magnification = 16
)
{

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
cat("start plot poll aggregate counts\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
	needle.index <- as.logical(
		grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
		grepl("^S/R 2$", haystack.data[, station.dimension])
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	new.electors.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.electors.data[, district.dimension],
		new.electors.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.electors.data <- new.electors.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new electors\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
	needle.index <- as.logical(
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Aggregating dimensions
	group.by <- list();
	group.by[[district.dimension]] <- factor(haystack.data[needle.index, district.dimension], exclude = NULL);
	group.by[[station.dimension]] <- factor(haystack.data[needle.index, station.dimension], exclude = NULL);

	# Stick the needles in the pincushion
	regular.electors.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.electors.data[, district.dimension],
		regular.electors.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.electors.data <- regular.electors.data[needle.index, "x"];
	electors.range <- c(0, max(new.electors.data, regular.electors.data));

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular electors\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
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
	new.ballots.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.ballots.data[, district.dimension],
		new.ballots.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.ballots.data <- new.ballots.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
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
	regular.ballots.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.ballots.data[, district.dimension],
		regular.ballots.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.ballots.data <- regular.ballots.data[needle.index, "x"];
	ballots.range <- c(0, max(new.ballots.data, regular.ballots.data));

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
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
	new.votes.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.votes.data[, district.dimension],
		new.votes.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.votes.data <- new.votes.data[needle.index, "x"];

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate new votes\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Find the needles in the haystack
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
	regular.votes.data <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.votes.data[, district.dimension],
		regular.votes.data[, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.votes.data <- regular.votes.data[needle.index, "x"];
	votes.range <- c(0, max(new.votes.data, regular.votes.data));

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
	new.rejected.data <- haystack.data[needle.index, elector.dimension];

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
	regular.rejected.data <- haystack.data[needle.index, elector.dimension];
	rejected.range <- c(0, max(new.rejected.data, regular.rejected.data));

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
	new.censored.data <- haystack.data[needle.index, elector.dimension];

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
	regular.censored.data <- haystack.data[needle.index, elector.dimension];
	censored.range <- c(0, max(new.censored.data, regular.censored.data));

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - aggregate regular censored\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

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
	par(
		mfrow = c(4,4),
		mar = c(4, 4, 0, 2),
		bg = "white"
	);

	# Next plot
	par(mfg = c(1, 1));

	# Start a new empty plot
	plot(
		ballots.range,
		electors.range,
		type = "n",
		xlab = "Ballots",
		ylab = "Electors",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.ballots.data,
		y = regular.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.ballots.data,
		y = new.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot electors ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(1, 2));

	# Start a new empty plot
	plot(
		votes.range,
		electors.range,
		type = "n",
		xlab = "Votes",
		ylab = "Electors",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.votes.data,
		y = regular.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.votes.data,
		y = new.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot electors votes\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(1, 3));

	# Start a new empty plot
	plot(
		rejected.range,
		electors.range,
		type = "n",
		xlab = "Rejected",
		ylab = "Electors",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.rejected.data,
		y = regular.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.rejected.data,
		y = new.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot electors rejected\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(1, 4));

	# Start a new empty plot
	plot(
		censored.range,
		electors.range,
		type = "n",
		xlab = "Censored",
		ylab = "Electors",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.censored.data,
		y = regular.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.censored.data,
		y = new.electors.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot electors censored\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(2, 1));

	# Start a new empty plot
	plot(
		ballots.range,
		censored.range,
		type = "n",
		xlab = "Ballots",
		ylab = "Censored",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.ballots.data,
		y = regular.censored.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.ballots.data,
		y = new.censored.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot censored ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(2, 2));

	# Start a new empty plot
	plot(
		votes.range,
		censored.range,
		type = "n",
		xlab = "Votes",
		ylab = "Censored",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.votes.data,
		y = regular.censored.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.ballots.data,
		y = new.censored.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot censored votes\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(2, 3));

	# Start a new empty plot
	plot(
		rejected.range,
		censored.range,
		type = "n",
		xlab = "Rejected",
		ylab = "Censored",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.rejected.data,
		y = regular.censored.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.rejected.data,
		y = new.censored.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot censored rejected\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(3, 1));

	# Start a new empty plot
	plot(
		ballots.range,
		rejected.range,
		type = "n",
		xlab = "Ballots",
		ylab = "Rejected",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.ballots.data,
		y = regular.rejected.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.ballots.data,
		y = new.rejected.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot rejected ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(3, 2));

	# Start a new empty plot
	plot(
		votes.range,
		rejected.range,
		type = "n",
		xlab = "Votes",
		ylab = "Rejected",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.votes.data,
		y = regular.rejected.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.votes.data,
		y = new.rejected.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot rejected votes\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Next plot
	par(mfg = c(4, 1));

	# Start a new empty plot
	plot(
		ballots.range,
		votes.range,
		type = "n",
		xlab = "Ballots",
		ylab = "Votes",
		frame.plot = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = regular.ballots.data,
		y = regular.votes.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = regular.colour
	);

	# Pull the needles from the haystack and stick them in the pincushion
	points(
		x = new.ballots.data,
		y = new.votes.data,
		type = "p",
		pch = 16,
		cex = point.size,
		col = new.colour
	);

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(" - plot votes ballots\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

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
cat(" - done plot poll aggregate counts\n");
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
}