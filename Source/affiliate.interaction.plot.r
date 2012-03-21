# Plot Elector Counts
#
# Cross plot the affiliates votes against each other
# Note the plots depend on the input data being
# properly sorted according to polling station
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0

# Declare signature
affiliate.interaction.plot <- function(
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
	point.size = 0.5
)
{

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
cat("start plot affiliate interactions counts\n");
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #

	# Pull the needles from the haystack
	needle.index <- as.logical(
		(haystack.data[, hash.dimension] == censored.electors) &
		(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
		(! grepl("^S/R 2$", haystack.data[, station.dimension]))
	);

	# Stick the regular needles in the pincushion
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
	iterator.data <- levels(haystack.data[, hash.dimension]);

	# Inspect the needles in the thimble
	needle.index <- as.logical(
		(iterator.data != rejected.ballots) &
		(iterator.data != censored.electors)
	);

	# Drop the needles from the thimble
	iterator.data <- c(
		iterator.data[needle.index],
		rejected.ballots,
		censored.electors
	);

	# Instantiate the pincushions
	range.data <- list();
	new.data <- list();
	regular.data <- list();

	# Iterate through the needles
	for (iterator.instance in iterator.data) {

		# Pull the needles from the haystack
		needle.index <- as.logical(
			(haystack.data[, hash.dimension] == iterator.instance) &
			(
				grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension]) |
				grepl("^S/R 2$", haystack.data[, station.dimension])
			)
		);

		# Stick the needles in the pincushion
		new.data[[iterator.instance]] <- haystack.data[needle.index, elector.dimension];

		# Pull the needles from the haystack
		needle.index <- as.logical(
			(haystack.data[, hash.dimension] == iterator.instance) &
			(! grepl("^6[0-9]{2}([^0-9].*)?$", haystack.data[, station.dimension])) &
			(! grepl("^S/R 2$", haystack.data[, station.dimension]))
		);

		# Stick the needles in the pincushion
		regular.data[[iterator.instance]] <- haystack.data[needle.index, elector.dimension];
		range.data[[iterator.instance]] <- c(0, max(new.data[[iterator.instance]], regular.data[[iterator.instance]]))

# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
toc <- proc.time() - tic;
cat(toc);
cat(paste(" - parse ", iterator.instance, "\n", sep = ""));
tic <- proc.time();
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
	}

	# Save to file
	if (nchar(file.name) > 0)
	{
		png(
			file = file.name,
			height = output.height,
			width = output.width,
			res = 1024,
			pointsize = 8,
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
	square.count <- length(iterator.data);
	par(
		mfrow = c(square.count, square.count),
		mar = c(0, 0, 0, 0),
		bg = "white"
	);

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
					xlab = "",
					ylab = "",
					xaxt = "n",
					yaxt = "n",
					frame.plot = FALSE
				);

				# Stick the needles in the pincushion
				lines(
					x = c(0, sort(regular.data[[row.instance]])),
					y = (0:length(regular.data[[row.instance]])) / length(regular.data[[row.instance]]),
					type = "s",
					lwd = point.size,
					lty = 1,
					col = "green"
				);

				# Stick the needles in the pincushion
				lines(
					x = c(0, sort(new.data[[row.instance]])),
					y = (0:length(new.data[[row.instance]])) / length(new.data[[row.instance]]),
					type = "s",
					lwd = point.size,
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

				# Test for row affiliate less than column affiliation
				if (par("mfg")[1] < par("mfg")[2])
				{

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
cat(" - done plot affiliate interactions counts\n");
# # # # # # # # # # # # # # # # # # # # # # Debug code # # # # # # # # # # # # # # # # # # # # # #
}