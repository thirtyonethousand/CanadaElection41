/**
 * Raw polling station-candidate date from Elections Canada
 *
 * Create, annotate, and load the data structures
 * One row per candidate per polling station, along
 * with control and calibration data.
 *
 * @author 061bcec6c3b94f69336c710052b4b1b0
 * @version 1.0
 * @param string somewhere in here specify the file to load
 * @return instantiated and populated tables
 */

-- Create the vessel
CREATE TABLE poll_candidate_raw
(
	record_number BIGINT,
	election_number BIGINT,
	electoral_district_number BIGINT,
	electoral_district_english VARCHAR(64),
	electoral_district_french VARCHAR(64),
	polling_station_number VARCHAR(8),
	polling_station VARCHAR(64),
	void_poll_flag VARCHAR(1),
	no_poll_flag VARCHAR(1),
	merge_poll VARCHAR(8),
	rejected_count BIGINT,
	elector_count BIGINT,
	last_name VARCHAR(64),
	middle_name VARCHAR(64),
	first_name VARCHAR(64),
	political_affiliation_english VARCHAR(64),
	political_affiliation_french VARCHAR(64),
	incumbent_flag VARCHAR(1),
	elected_flag VARCHAR(1),
	vote_count BIGINT
);

-- Annotate the vessel
COMMENT ON TABLE poll_candidate_raw IS 'Raw data from Elections Canada. One record per candidate per polling station';
COMMENT ON COLUMN poll_candidate_raw.record_number IS 'Unique record identifier';
COMMENT ON COLUMN poll_candidate_raw.election_number IS 'Number of election as available from Elections Canada';
COMMENT ON COLUMN poll_candidate_raw.electoral_district_number IS 'Unique electoral district identifier';
COMMENT ON COLUMN poll_candidate_raw.electoral_district_english IS 'English name of the electoral district';
COMMENT ON COLUMN poll_candidate_raw.electoral_district_french IS 'French name of the electoral district';
COMMENT ON COLUMN poll_candidate_raw.polling_station_number IS 'Unique polling station identifier';
COMMENT ON COLUMN poll_candidate_raw.polling_station IS 'Name of polling station';
COMMENT ON COLUMN poll_candidate_raw.void_poll_flag IS 'Y:void poll, N:active poll';
COMMENT ON COLUMN poll_candidate_raw.no_poll_flag IS 'Y: poll was not held, N: poll held';
COMMENT ON COLUMN poll_candidate_raw.merge_poll IS 'Identifier of merged poll';
COMMENT ON COLUMN poll_candidate_raw.rejected_count IS 'Number of rejected ballots';
COMMENT ON COLUMN poll_candidate_raw.elector_count IS 'Number of registered electors';
COMMENT ON COLUMN poll_candidate_raw.last_name IS 'Last name of candidate';
COMMENT ON COLUMN poll_candidate_raw.middle_name IS 'Middle name of candidate';
COMMENT ON COLUMN poll_candidate_raw.first_name IS 'First name of candidate';
COMMENT ON COLUMN poll_candidate_raw.political_affiliation_english IS 'English name of political party';
COMMENT ON COLUMN poll_candidate_raw.political_affiliation_french IS 'French name of political party';
COMMENT ON COLUMN poll_candidate_raw.incumbent_flag IS 'Y: candidate incumbent, N: non-incumbent candidate';
COMMENT ON COLUMN poll_candidate_raw.elected_flag IS 'Y: candidate elected, N: candidate not elected';
COMMENT ON COLUMN poll_candidate_raw.vote_count IS 'Number of votes cast for candidate';

-- Fill the vessel
COPY poll_candidate_raw
(
	record_number,
	election_number,
	electoral_district_number,
	electoral_district_english,
	electoral_district_french,
	polling_station_number,
	polling_station,
	void_poll_flag,
	no_poll_flag,
	merge_poll,
	rejected_count,
	elector_count,
	last_name,
	middle_name,
	first_name,
	political_affiliation_english,
	political_affiliation_french,
	incumbent_flag,
	elected_flag,
	vote_count
)
FROM 'consolidate.csv'
WITH
	CSV
	DELIMITER ','
	NULL '<<NULL>>'
	HEADER
	QUOTE '"'
	ESCAPE '\'
	ENCODING 'LATIN1';