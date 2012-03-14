/**
 * Polling station cross tabulation
 *
 * Wash the polling stations through the
 * Kitchen sink and scrub out the classes
 * and errors
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @return Cross-tabulation
 */

-- Detected events
SELECT
	unheld_electors,
	ballot_excess,
	electors_registered,
	CASE
		WHEN number_type = '600' THEN
			'600'
		WHEN number_type = 'special 2' THEN
			'S/R 2'
		ELSE
			'other'
	END number_type,
	parent_poll,
	merged_poll,
	COUNT(*) poll_count

-- Beam source
FROM
	polling_station_aggregate

-- Scattering cross-section
GROUP BY 
	1,2,3,4,5,6

-- Topology
ORDER BY
	1 DESC NULLS FIRST,
	2 DESC NULLS FIRST,
	3 DESC NULLS FIRST,
	4 ASC NULLS FIRST,
	5 ASC NULLS FIRST,
	6 ASC NULLS FIRST;