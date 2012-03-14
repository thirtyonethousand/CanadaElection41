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
	district_consistent,
	station_consistent,
	void_consistent,
	unheld_consistent,
	merge_consistent,
	elector_consistent,
	rejected_consistent,

	-- Final tally
	COUNT(*) poll_count

-- Beam source: self referential polling station aggregates
FROM
	polling_station_aggregate

-- Scattering cross-section: one row per realized class of polls
GROUP BY
	1, 2, 3, 4, 5, 6, 7

-- Sort definitions
ORDER BY
	1 DESC NULLS FIRST,
	2 DESC NULLS FIRST,
	3 DESC NULLS FIRST,
	4 DESC NULLS FIRST,
	5 DESC NULLS FIRST,
	6 DESC NULLS FIRST,
	7 DESC NULLS FIRST;