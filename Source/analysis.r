# General calls to analytic tools
#
# Load, process, plot, and other manipulations
# of the data
#
# @author 061bcec6c3b94f69336c710052b4b1b0
# @version 1.0

# Import the blinded polling station-political affiliate data
poll.affiliate.data <- load.data(file.name = "poll_candidate_blinded.csv");

# Generate the first plots
poll.aggregate.plot(poll.affiliate.data);
censor.affiliate.plot(poll.affiliate.data);
reject.affiliate.plot(poll.affiliate.data);
ballot.affiliate.plot(poll.affiliate.data);
vote.affiliate.plot(poll.affiliate.data);
elector.affiliate.plot(poll.affiliate.data);
affiliate.interaction.plot(poll.affiliate.data);

# Analytic plots
reject.fraction.plot(poll.affiliate.data);
censor.fraction.plot(poll.affiliate.data);