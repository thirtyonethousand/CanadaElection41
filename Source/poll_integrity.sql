/**
 * Polling station cross tabulation
 *
 * Wash the polling stations through the
 * Kitchen sink and scrub out the errors
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @return Cross-tabulation
 */

-- Aggregate poll counts by poll class
SELECT
	recursive_merge,
	void_ballots,
	unheld_ballots,
	void_electors,
	unheld_electors,
	ballot_excess,

	-- Final tally
	COUNT(*) poll_count

-- Beam source: self referential polling station aggregates
FROM
	polling_station_aggregate

-- Scattering cross-section: one row per realized class of polls
GROUP BY
	1, 2, 3, 4, 5, 6

-- Sort definitions
ORDER BY
	1 ASC NULLS FIRST,
	2 ASC NULLS FIRST,
	3 ASC NULLS FIRST,
	4 ASC NULLS FIRST,
	5 ASC NULLS FIRST,
	6 ASC NULLS FIRST;