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
	column.count = 5,
	output.width = 16384,#1024
	output.height = 16384,#1024
	inch.resolution = 1024,#64
	font.size = 8,#12
	point.size = 0.5#0.125
)
{

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
cat("start plot poll aggregate counts\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Instantiate needles and pincushions
	electors.index <- "Electors";
	ballots.index <- "Ballots";
	votes.index <- "Votes";
	rejected.index <- "Rejected";
	censored.index <- "Censored";
	iterator.data <- c(
		electors.index,
		ballots.index,
		votes.index,
		rejected.index,
		censored.index
	);
	new.data <- list();
	regular.data <- list();
	range.data <- list();

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
	new.data[[electors.index]] <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.data[[electors.index]][, district.dimension],
		new.data[[electors.index]][, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.data[[electors.index]] <- new.data[[electors.index]][needle.index, "x"];

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
	regular.data[[electors.index]] <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.data[[electors.index]][, district.dimension],
		regular.data[[electors.index]][, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.data[[electors.index]] <- regular.data[[electors.index]][needle.index, "x"];
	range.data[[electors.index]] <- c(0, max(new.data[[electors.index]], regular.data[[electors.index]]));

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
	new.data[[ballots.index]] <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.data[[ballots.index]][, district.dimension],
		new.data[[ballots.index]][, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.data[[ballots.index]] <- new.data[[ballots.index]][needle.index, "x"];

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
	regular.data[[ballots.index]] <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.data[[ballots.index]][, district.dimension],
		regular.data[[ballots.index]][, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.data[[ballots.index]] <- regular.data[[ballots.index]][needle.index, "x"];
	range.data[[ballots.index]] <- c(0, max(new.data[[ballots.index]], regular.data[[ballots.index]]));

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
	new.data[[votes.index]] <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		new.data[[votes.index]][, district.dimension],
		new.data[[votes.index]][, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	new.data[[votes.index]] <- new.data[[votes.index]][needle.index, "x"];

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
	regular.data[[votes.index]] <- aggregate(
		haystack.data[needle.index, elector.dimension],
		by = group.by,
		FUN = sum
	);

	# Find the needles in the haystack
	needle.index <- order(
		regular.data[[votes.index]][, district.dimension],
		regular.data[[votes.index]][, station.dimension],
		na.last = TRUE,
		decreasing = FALSE
	);

	# Pull the needles from the haystack and stick them in the pincushion
	regular.data[[votes.index]] <- regular.data[[votes.index]][needle.index, "x"];
	range.data[[votes.index]] <- c(0, max(new.data[[votes.index]], regular.data[[votes.index]]));

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
	new.data[[rejected.index]] <- haystack.data[needle.index, elector.dimension];

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
	regular.data[[rejected.index]] <- haystack.data[needle.index, elector.dimension];
	range.data[[rejected.index]] <- c(0, max(new.data[[rejected.index]], regular.data[[rejected.index]]));

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
	new.data[[censored.index]] <- haystack.data[needle.index, elector.dimension];

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
	regular.data[[censored.index]] <- haystack.data[needle.index, elector.dimension];
	range.data[[censored.index]] <- c(0, max(new.data[[censored.index]], regular.data[[censored.index]]));

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
	par(
		mfrow = c(5,5),
		mar = c(4, 4, 0, 2),
		bg = "white"
	);

	# Next plot
	par(mfg = c(1, 1));

	# Paint the rows with the needles
	for (row.instance in iterator.data) {
		for (column.instance in iterator.data) {

			# Cumulative distributions on the diagonal
			if (row.instance == column.instance)
			{

				# Start a new empty plot
				plot(
					range.data[[row.instance]],
					c(0, 1),
					type = "n",
					xlab = row.instance,
					ylab = "Cumulative Fraction",
					frame.plot = FALSE
				);

				# Stick the needles in the pincushion
				lines(
					x = c(0, sort(regular.data[[row.instance]])),
					y = (0:length(regular.data[[row.instance]])) / length(regular.data[[row.instance]]),
					type = "s",
					lwd = 0.5,
					lty = 1,
					col = "green"
				);

				# Stick the needles in the pincushion
				lines(
					x = c(0, sort(new.data[[row.instance]])),
					y = (0:length(new.data[[row.instance]])) / length(new.data[[row.instance]]),
					type = "s",
					lwd = 0.5,
					lty = 1,
					col = "blue"
				);
			}

			# Scatter plots
			else
			{

				# Start a new empty plot
				plot(
					range.data[[column.instance]],
					range.data[[row.instance]],
					type = "n",
					xlab = "",
					ylab = "",
					xaxt = "n",
					yaxt = "n",
					frame.plot = FALSE
				);

				# Test for row less than column
				if (par("mfg")[1] < par("mfg")[2])
				{

					# Annotate the plot
					axis(1);
					axis(2);
					title(
						xlab = column.instance,
						ylab = row.instance
					);

					# Pull the needles from the haystack and stick them in the pincushion
					points(
						x = regular.data[[column.instance]],
						y = regular.data[[row.instance]],
						type = "p",
						pch = 16,
						cex = point.size,
						col = regular.colour
					);

					# Pull the needles from the haystack and stick them in the pincushion
					points(
						x = new.data[[column.instance]],
						y = new.data[[row.instance]],
						type = "p",
						pch = 16,
						cex = point.size,
						col = new.colour
					);
				}
			}

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(paste(" - plot ", row.instance, " - ", column.instance, "\n", sep = ""));
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
		}
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
cat(" - done plot poll aggregate counts\n");
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
}