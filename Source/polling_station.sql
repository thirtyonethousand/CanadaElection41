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

-- Declare local tables
WITH

	-- Aggregate individual polling stations
	pack_poll AS
	(
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

			-- Range of elector counts across records
			MIN(elector_count) minimum_elector,
			MAX(elector_count) maximum_elector,

			-- Range of rejected ballots across records
			MIN(rejected_count) minimum_rejected,
			MAX(rejected_count) maximum_rejected,

			-- Fixed return counts,
			COUNT(last_name) candidate_count,
			SUM(vote_count) vote_count

		-- Beam source: raw data
		FROM
			poll_candidate_raw

		-- Scattering cross-section: on record per poll
		GROUP BY
			1, 2, 3

		-- Sort definition
		ORDER BY
			1 ASC NULLS FIRST,
			2 ASC NULLS FIRST,
			length(SUBSTRING(polling_station_number FROM '^[0-9]+')) ASC NULLS LAST,
			3 ASC NULLS FIRST
	)
	
SELECT
	p.election_number,
	p.electoral_district_number,
	p.polling_station_number,

	-- Same fields from above
	MIN(p.minimum_district) minimum_district,
	MAX(p.maximum_district) maximum_district,
	MIN(p.minimum_station) minimum_station,
	MAX(p.maximum_station) maximum_station,
	MIN(p.minimum_void) minimum_void,
	MAX(p.maximum_void) maximum_void,
	MIN(p.minimum_unheld) minimum_unheld,
	MAX(p.maximum_unheld) maximum_unheld,
	MIN(p.minimum_merge) minimum_merge,
	MAX(p.maximum_merge) maximum_merge,

	-- Classify the polling station number
	CASE
		WHEN p.polling_station_number ~ '^[0-9]{1,2}([^0-9].*)?$' THEN
			'100'
		WHEN p.polling_station_number ~ '^[0-9]{3}([^0-9].*)?$' THEN
			SUBSTRING(p.polling_station_number, 1, 1) || '00'
		WHEN p.polling_station_number ~ '^S/R 1$' THEN
			'special 1'
		WHEN p.polling_station_number ~ '^S/R 2$' THEN
			'special 2'
		ELSE
			'error'
	END number_type,

	-- Classify the polling station name
	CASE
		WHEN MAX(p.maximum_station) ~ '^SVR Group 1.*$' THEN
			'special 1'
		WHEN MAX(p.maximum_station) ~ '^SVR Group 2.*$' THEN
			'special 2'
		WHEN MAX(p.maximum_station) ~ '^Mobile poll.*$' THEN
			'mobile'
		ELSE
			'regular'
	END name_type,

	-- Check for void flag
	CASE
		WHEN MIN(p.minimum_void) = 'N' THEN
			'no'
		ELSE
			'yes'
	END void_poll,

	-- Check for not held
	CASE
		WHEN MIN(p.minimum_unheld) = 'N' THEN
			'no'
		ELSE
			'yes'
	END not_held,

	-- Check for referenced poll (parent)
	CASE
		WHEN p.polling_station_number = MAX(p.maximum_merge) THEN
			'no'
		ELSE
			'yes'
	END merged_poll,

	-- Check for referring polls (children)
	CASE
		WHEN COUNT(c.polling_station_number) > 0 THEN
			'yes'
		ELSE
			'no'
	END parent_poll,

	-- Check for no registered electors
	CASE
		WHEN MAX(p.maximum_elector) > 0 THEN
			'yes'
		ELSE
			'no'
	END electors_registered,

	-- Check for no nominated candidates
	CASE
		WHEN MAX(p.candidate_count) > 0 THEN
			'yes'
		ELSE
			'no'
	END candidates_nominated,

	-- Check for ballots cast
	CASE
		WHEN MAX(p.vote_count) + MAX(p.maximum_rejected) > 0 THEN
			'yes'
		ELSE
			'no'
	END ballots_cast,

	-- Check for consistent district name
	CASE
		WHEN MIN(p.minimum_district) = MAX(p.maximum_district) AND MIN(p.minimum_district) <> '' THEN
			'yes'
		ELSE
			'no'
	END district_consistent,

	-- Check for consistent station name
	CASE
		WHEN MIN(p.minimum_station) = MAX(p.maximum_station) AND MIN(p.minimum_station) <> '' THEN
			'yes'
		ELSE
			'no'
	END station_consistent,

	-- Check for consistent void flag
	CASE
		WHEN MIN(p.minimum_void) = MAX(p.maximum_void) AND MIN(p.minimum_void) <> '' THEN
			'yes'
		ELSE
			'no'
	END void_consistent,

	-- Check for consistent not held flag
	CASE
		WHEN MIN(p.minimum_unheld) = MAX(p.maximum_unheld) AND MIN(p.minimum_unheld) <> '' THEN
			'yes'
		ELSE
			'no'
	END unheld_consistent,

	-- Check for consistent merge reference
	CASE
		WHEN MIN(p.minimum_merge) = MAX(p.maximum_merge) AND MIN(p.minimum_merge) <> '' THEN
			'yes'
		ELSE
			'no'
	END merge_consistent,

	-- Check for consistent elector counts
	CASE
		WHEN MIN(p.minimum_elector) = MAX(p.maximum_elector) AND MIN(p.minimum_elector) >= 0 THEN
			'yes'
		ELSE
			'no'
	END elector_consistent,

	-- Check for consistent rejected counts
	CASE
		WHEN MIN(p.minimum_rejected) = MAX(p.maximum_rejected) AND MIN(p.minimum_rejected) >= 0 THEN
			'yes'
		ELSE
			'no'
	END rejected_consistent,

	-- Check for recursive reference
	CASE
		WHEN p.polling_station_number <> MAX(p.maximum_merge) AND COUNT(c.polling_station_number) > 0 THEN
			'yes'
		ELSE
			'no'
	END recursive_merge,

	-- Check for ballots at void polls
	CASE
		WHEN MAX(p.minimum_void) = 'Y' AND MAX(p.vote_count) + MAX(p.maximum_rejected) > 0 THEN
			'yes'
		ELSE
			'no'
	END void_ballots,

	-- Check for ballots at not held polls
	CASE
		WHEN MAX(p.minimum_unheld) = 'Y' AND MAX(p.vote_count) + MAX(p.maximum_rejected) > 0 THEN
			'yes'
		ELSE
			'no'
	END unheld_ballots,

	-- Check for electors at void polls
	CASE
		WHEN MAX(p.minimum_void) = 'Y' AND MAX(p.maximum_elector) + COALESCE(SUM(c.maximum_elector), 0) > 0 THEN
			'yes'
		ELSE
			'no'
	END void_electors,

	-- Check for electors at not held polls
	CASE
		WHEN MAX(p.minimum_unheld) = 'Y' AND MAX(p.maximum_elector) + COALESCE(SUM(c.maximum_elector), 0) > 0 THEN
			'yes'
		ELSE
			'no'
	END unheld_electors,

	-- Check for less ballots than electors (including merged polls)
	CASE
		WHEN MAX(p.vote_count) + MAX(p.maximum_rejected) > MAX(p.maximum_elector) + COALESCE(SUM(c.maximum_elector), 0) THEN
			'yes'
		ELSE
			'no'
	END ballot_excess,

	-- Same fields from above
	MIN(p.minimum_elector) minimum_elector,
	MAX(p.maximum_elector) maximum_elector,
	MAX(p.candidate_count) candidate_count,
	MAX(p.vote_count) vote_count,
	MIN(p.minimum_rejected) minimum_rejected,
	MAX(p.maximum_rejected) maximum_rejected,

	-- Merged referring poll aggregates
	COUNT(c.polling_station_number) merge_count,
	COALESCE(SUM(c.maximum_elector), 0) merge_elector,
	COALESCE(SUM(c.vote_count), 0) merge_vote,
	COALESCE(SUM(c.maximum_rejected), 0) merge_rejected

-- Beam source: double inject previous beams
FROM
	pack_poll p
	LEFT JOIN
	pack_poll c
	ON
		p.election_number = c.election_number
		AND
		p.electoral_district_number = c.electoral_district_number
		AND
		p.polling_station_number = c.maximum_merge
		AND
		c.polling_station_number <> c.maximum_merge

-- Scattering cross-section: one row per poll
GROUP BY
	1, 2, 3

-- Sort definition
ORDER BY
	1 ASC NULLS FIRST,
	2 ASC NULLS FIRST,
	length(SUBSTRING(p.polling_station_number FROM '^[0-9]+')) ASC NULLS LAST,
	3 ASC NULLS FIRST;

-- Spray some graffiti all over the view
COMMENT ON VIEW polling_station_aggregate IS 'Everything and the kitchen sink about polling stations. Full of all sorts of useful fields';