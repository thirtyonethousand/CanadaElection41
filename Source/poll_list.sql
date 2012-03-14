/**
 * Polling station details
 *
 * Seive out polling stations from
 * the kitchen sink
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @return Specific polling stations
 */

-- Detected events
SELECT
	electoral_district_number,
	maximum_district elector_district,
	polling_station_number,
	maximum_station polling_station,
	minimum_void void_poll,
	minimum_unheld not_held,
	maximum_merge merge_poll,
	maximum_elector elector_count,
	candidate_count,
	vote_count,
	maximum_rejected rejected_count,
	merge_count

-- Beam source
FROM
	polling_station_aggregate

-- Detector trigger
WHERE
	electors_registered = 'yes'
	AND
	(
		unheld_electors = 'yes'
		OR
		ballot_excess = 'yes'
	)

-- Topology
ORDER BY
	unheld_electors DESC NULLS FIRST,
	ballot_excess DESC NULLS FIRST,
	1 ASC NULLS FIRST,
	length("substring"(polling_station_number, '^[0-9]+')) ASC NULLS LAST,
	3 ASC NULLS FIRST;