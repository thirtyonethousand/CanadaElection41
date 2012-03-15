/**
 * Deconstructionist Art Project
 *
 * Disassemble the poll-candidate raw data
 * into individual elements to re-factor the
 * the data into a true dimensional format, where
 * the ballots coutns adds up to the sum of all
 * registered voters immediately after the election.
 * Also blind the analysis by MD5 hashing
 * any identifying information.
 *
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @return Blinded view of poll-candidate records
 */
 
-- Return view of blinded and refactored raw data
CREATE OR REPLACE VIEW poll_candidate_blinded AS

-- Declare local tables
WITH

	-- Yet another kitchen sink, we'll deconstruct this
	refactor_raw AS
	(
		SELECT
			election_number,
			electoral_district_number,
			polling_station_number,

			-- Accomodate multipe candidates for the same party
			political_affiliation_english || ' -- ' || (COUNT(last_name) OVER candidate_affiliation) || ' -- ' affiliation_name,

			-- Electoral returns
			vote_count candidate_votes,
			SUM(rejected_count) OVER merge_candidate rejected_ballots,
			GREATEST(0, (SUM(elector_count) OVER merge_candidate) - (SUM(vote_count) OVER merge_poll) - (SUM(rejected_count) OVER merge_candidate)) censored_electors,

			-- Polling station state indicators
			void_poll_flag,
			no_poll_flag,

			-- Check for merged poll			
			CASE
				WHEN polling_station_number = merge_poll THEN
					'N'
				ELSE
					'Y'
			END merged_poll_flag,

			-- Candidate state indicators
			incumbent_flag,
			elected_flag

		-- Beam source: original data
		FROM
			poll_candidate_raw

		-- Integrated luminosity
		WINDOW 

			-- Integrate over all the same affiliation records
			candidate_affiliation AS
			(
				PARTITION BY
					election_number,
					electoral_district_number,
					polling_station_number,
					political_affiliation_english
				ORDER BY
					last_name ASC NULLS FIRST,
					first_name ASC NULLS FIRST,
					middle_name ASC NULLS FIRST
				ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
			),

			-- Integrate over all the same candidate records
			merge_candidate AS
			(
				PARTITION BY
					election_number,
					electoral_district_number,
					merge_poll,
					last_name,
					middle_name,
					first_name
				ORDER BY
					polling_station_number ASC NULLS FIRST
				ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING					
			),

			-- Integrate over all the same polling station records
			merge_poll AS
			(
				PARTITION BY
					election_number,
					electoral_district_number,
					merge_poll
				ORDER BY
					polling_station_number ASC NULLS FIRST,
					last_name ASC NULLS FIRST,
					middle_name ASC NULLS FIRST,
					first_name ASC NULLS FIRST
				ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING			
			)

		-- Nice topology
		ORDER BY
			1 ASC NULLS FIRST,
			2 ASC NULLS FIRST,
			length(SUBSTRING(polling_station_number FROM '^[0-9]+')) ASC NULLS LAST,
			3 ASC NULLS FIRST,
			4 ASC NULLS FIRST,
			6 ASC NULLS FIRST,
			5 ASC NULLS FIRST
	),

	-- List of all distinct affilations
	pack_candidate AS
	(
		SELECT election_number, ' -- rejected ballots -- ' affiliation_name FROM refactor_raw GROUP BY 1 UNION ALL
		SELECT election_number, ' -- censored electors -- ' affiliation_name FROM refactor_raw GROUP BY 1 UNION ALL
		SELECT
			election_number,
			affiliation_name
		FROM
			refactor_raw
		GROUP BY
			1, 2
		ORDER BY
			1 ASC NULLS FIRST,
			2 ASC NULLS FIRST
	),

	-- List all distrinct held polls
	pack_poll AS
	(
		SELECT
			election_number,
			electoral_district_number,
			polling_station_number,
			MAX(rejected_ballots) rejected_ballots,
			MAX(censored_electors) censored_electors
		FROM
			refactor_raw
		WHERE
			void_poll_flag = 'N'
			AND
			no_poll_flag = 'N'
			AND
			merged_poll_flag = 'N'
		GROUP BY
			1, 2, 3
		ORDER BY
			1 ASC NULLS FIRST,
			2 ASC NULLS FIRST,
			length(SUBSTRING(polling_station_number FROM '^[0-9]+')) ASC NULLS LAST,
			3 ASC NULLS FIRST
	)

-- Detected Events: Reconstruct the deconstructionist work
SELECT
	a0.election_number,
	a0.electoral_district_number,
	a0.polling_station_number,

	-- Blind the investigation by hashing the affiliations
	CASE
		WHEN a1.affiliation_name = ' -- censored electors -- ' THEN
			a1.affiliation_name
		WHEN a1.affiliation_name = ' -- rejected ballots -- ' THEN
			a1.affiliation_name
		ELSE
			md5(a1.affiliation_name)
	END affiliation_hash,

	-- Translate the incumbent status
	CASE
		WHEN a2.incumbent_flag = 'Y' THEN
			'yes'
		ELSE
			'no'
	END candidate_incumbent,

	-- Translate the elected status
	CASE
		WHEN a2.elected_flag = 'Y' THEN
			'yes'
		ELSE
			'no'
	END candidate_elected,

	-- Grab the elector counts
	CASE
		WHEN a1.affiliation_name = ' -- censored electors -- ' THEN
			a0.censored_electors
		WHEN a1.affiliation_name = ' -- rejected ballots -- ' THEN
			a0.rejected_ballots
		ELSE
			COALESCE(a2.candidate_votes, 0)
	END elector_count

-- Beam source: triple injector reconstruction from deconstructed sources
FROM
	pack_poll a0
	CROSS JOIN
	pack_candidate a1
	LEFT JOIN
	refactor_raw a2
	ON
		a0.election_number = a2.election_number
		AND
		a0.electoral_district_number = a2.electoral_district_number
		AND
		a0.polling_station_number = a2.polling_station_number
		AND
		a1.election_number = a2.election_number
		AND
		a1.affiliation_name = a2.affiliation_name

-- Nice topology
ORDER BY
	1 ASC NULLS FIRST,
	2 ASC NULLS FIRST,
	length(SUBSTRING(a0.polling_station_number FROM '^[0-9]+')) ASC NULLS LAST,
	3 ASC NULLS FIRST,
	4 ASC NULLS FIRST;

-- Spray some graffiti all over the view
COMMENT ON VIEW polling_station_aggregate IS 'Strictly dimensional data set of elector counts, blinded to political affiliations';