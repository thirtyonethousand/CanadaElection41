/**
 * Create view of polling stations
 *
 * Return one record per polling station,
 * thrown in all sorts of debugging and
 * error trapping data, and the kitchen sink
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @return Cross-tabulation
 */

-- Return view of polling station data
CREATE OR REPLACE VIEW polling_station_aggregate AS

-- Aggregate individual polling stations
SELECT
	election_number,
	electoral_district_number,
	polling_station_number,

	-- Range of district names across records
	MIN(electoral_district_english) minimum_district,
	MAX(electoral_district_english) maximum_district,

	-- Range of station name across records
	MIN(polling_station) minimum_station,
	MAX(polling_station) maximum_station,

	-- Range of void flags across records
	MIN(void_poll_flag) minimum_void,
	MAX(void_poll_flag) maximum_void,

	-- Range of not held flag across records
	MIN(no_poll_flag) minimum_unheld,
	MAX(no_poll_flag) maximum_unheld,

	-- Range of merge references across records
	MIN(merge_poll) minimum_merge,
	MAX(merge_poll) maximum_merge,

	-- Classify the polling station number
	CASE
		WHEN polling_station_number ~ '^[0-9]{1,2}([^0-9].*)?$' THEN
			'100'
		WHEN polling_station_number ~ '^[0-9]{3}([^0-9].*)?$' THEN
			SUBSTRING(polling_station_number, 1, 1) || '00'
		WHEN polling_station_number ~ '^S/R 1$' THEN
			'special 1'
		WHEN polling_station_number ~ '^S/R 2$' THEN
			'special 2'
		ELSE
			'error'
	END number_type,

	-- Classify the polling station name
	CASE
		WHEN MAX(polling_station) ~ '^SVR Group 1.*$' THEN
			'special 1'
		WHEN MAX(polling_station) ~ '^SVR Group 2.*$' THEN
			'special 2'
		WHEN MAX(polling_station) ~ '^Mobile poll.*$' THEN
			'mobile'
		ELSE
			'regular'
	END name_type,

	-- Check for void flag
	CASE
		WHEN MIN(void_poll_flag) = 'N' THEN
			'no'
		ELSE
			'yes'
	END void_poll,

	-- Check for not held
	CASE
		WHEN MIN(no_poll_flag) = 'N' THEN
			'no'
		ELSE
			'yes'
	END not_held,

	-- Check for referenced poll (parent)
	CASE
		WHEN polling_station_number = MAX(merge_poll) THEN
			'no'
		ELSE
			'yes'
	END merged_poll,

	-- Check for referring polls (children)
	CASE
		WHEN polling_station_number = MAX(merge_poll) AND (COUNT(polling_station_number) OVER merged_polling_stations) > 1 THEN
			'yes'
		ELSE
			'no'
	END parent_poll,

	-- Check for no registered electors
	CASE
		WHEN MAX(elector_count) > 0 THEN
			'yes'
		ELSE
			'no'
	END electors_registered,

	-- Check for no nominated candidates
	CASE
		WHEN COUNT(last_name) > 0 THEN
			'yes'
		ELSE
			'no'
	END candidates_nominated,

	-- Check for ballots cast
	CASE
		WHEN MAX(vote_count) + MAX(rejected_count) > 0 THEN
			'yes'
		ELSE
			'no'
	END ballots_cast,

	-- Check for consistent district name
	CASE
		WHEN MIN(electoral_district_english) = MAX(electoral_district_english) AND MIN(electoral_district_english) <> '' THEN
			'yes'
		ELSE
			'no'
	END district_consistent,

	-- Check for consistent station name
	CASE
		WHEN MIN(polling_station) = MAX(polling_station) AND MIN(polling_station) <> '' THEN
			'yes'
		ELSE
			'no'
	END station_consistent,

	-- Check for consistent void flag
	CASE
		WHEN MIN(void_poll_flag) = MAX(void_poll_flag) AND MIN(void_poll_flag) <> '' THEN
			'yes'
		ELSE
			'no'
	END void_consistent,

	-- Check for consistent not held flag
	CASE
		WHEN MIN(no_poll_flag) = MAX(no_poll_flag) AND MIN(no_poll_flag) <> '' THEN
			'yes'
		ELSE
			'no'
	END unheld_consistent,

	-- Check for consistent merge reference
	CASE
		WHEN MIN(merge_poll) = MAX(merge_poll) AND MIN(merge_poll) <> '' THEN
			'yes'
		ELSE
			'no'
	END merge_consistent,

	-- Check for consistent elector counts
	CASE
		WHEN MIN(elector_count) = MAX(elector_count) AND MIN(elector_count) >= 0 THEN
			'yes'
		ELSE
			'no'
	END elector_consistent,

	-- Check for consistent rejected counts
	CASE
		WHEN MIN(rejected_count) = MAX(rejected_count) AND MIN(rejected_count) >= 0 THEN
			'yes'
		ELSE
			'no'
	END rejected_consistent,

	-- Check for recursive reference
	CASE
		WHEN polling_station_number <> MAX(merge_poll) AND (COUNT(polling_station_number) OVER merged_polling_stations) > 1 THEN
			'yes'
		ELSE
			'no'
	END recursive_merge,

	-- Check for ballots at void polls
	CASE
		WHEN MAX(void_poll_flag) = 'Y' AND MAX(vote_count) + MAX(rejected_count) > 0 THEN
			'yes'
		ELSE
			'no'
	END void_ballots,

	-- Check for ballots at not held polls
	CASE
		WHEN MAX(no_poll_flag) = 'Y' AND MAX(vote_count) + MAX(rejected_count) > 0 THEN
			'yes'
		ELSE
			'no'
	END unheld_ballots,

	-- Check for electors at void polls
	CASE
		WHEN MAX(void_poll_flag) = 'Y' AND (SUM(MAX(elector_count)) OVER merged_polling_stations) > 0 THEN
			'yes'
		ELSE
			'no'
	END void_electors,

	-- Check for electors at not held polls
	CASE
		WHEN MAX(no_poll_flag) = 'Y' AND (SUM(MAX(elector_count)) OVER merged_polling_stations) > 0 THEN
			'yes'
		ELSE
			'no'
	END unheld_electors,

	-- Check for less ballots than electors (including merged polls)
	CASE
		WHEN MAX(vote_count) + MAX(rejected_count) > (SUM(MAX(elector_count)) OVER merged_polling_stations) THEN
			'yes'
		ELSE
			'no'
	END ballot_excess,

	-- Range of elector counts across records
	MIN(elector_count) minimum_elector,
	MAX(elector_count) maximum_elector,

	-- Range of rejected ballots across records
	MIN(rejected_count) minimum_rejected,
	MAX(rejected_count) maximum_rejected,

	-- Fixed return counts,
	COUNT(last_name) candidate_count,
	SUM(vote_count) vote_count,

	-- Merged referring poll aggregates
	COUNT(polling_station_number) OVER merged_polling_stations merge_count,
	SUM(MAX(elector_count)) OVER merged_polling_stations merge_elector,
	SUM(MAX(vote_count)) OVER merged_polling_stations merge_vote,
	SUM(MAX(rejected_count)) OVER merged_polling_stations merge_rejected

-- Beam source: raw data
FROM
	poll_candidate_raw

-- Scattering cross-section: on record per poll
GROUP BY
	1, 2, 3

-- Integrated luminosity
WINDOW merged_polling_stations AS
(
	PARTITION BY 
		electoral_district_number, 
		MAX(merge_poll) 
	ORDER BY
		length("substring"(polling_station_number, '^[0-9]+')) ASC NULLS LAST, 
		polling_station_number ASC NULLS FIRST 
	ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
)

-- Sort definition
ORDER BY
	1 ASC NULLS FIRST,
	2 ASC NULLS FIRST,
	length(SUBSTRING(polling_station_number FROM '^[0-9]+')) ASC NULLS LAST,
	3 ASC NULLS FIRST;

-- Spray some graffiti all over the view
COMMENT ON VIEW polling_station_aggregate IS 'Everything and the kitchen sink about polling stations. Full of all sorts of useful fields';