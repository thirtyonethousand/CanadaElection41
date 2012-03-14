/**
 * Polling station cross tabulation
 *
 * Wash the polling stations through the
 * Kitchen sink and scrub out both the
 * classes and errors
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @return Cross-tabulation
 */

-- Detector events: Aggregate poll counts by poll class
SELECT
	number_type,
	name_type,
	unheld_electors,
	ballot_excess,
	electors_registered,
	ballots_cast,
	parent_poll,

	-- Final tally
	COUNT(*) poll_count

-- Beam source: self referential polling station aggregates
FROM
	polling_station_aggregate

-- Detector trigger: only polls with errors
WHERE
	unheld_electors <> 'no'
	OR
	ballot_excess <> 'no'

-- Scattering cross-section: one row per realized class of polls
GROUP BY
	number_type,
	name_type,
	unheld_electors,
	ballot_excess,
	electors_registered,
	ballots_cast,
	parent_poll

-- Sort definitions
ORDER BY
	1 ASC NULLS FIRST,
	2 ASC NULLS FIRST,
	3 ASC NULLS FIRST,
	4 ASC NULLS FIRST,
	5 DESC NULLS FIRST,
	6 DESC NULLS FIRST,
	7 ASC NULLS FIRST;