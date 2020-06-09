To compare two .cat models for discrepancies over a set of
tests, do the following.

In the same directory that holds your first .cat file:

../../../herd/herd -initwrites true -model $(MODEL_ONE).cat \
../../../herd/testsuite/CTests/*.litmus > $(MODEL_ONE)_results.txt

In the same directory that holds your second .cat file:

../../../herd/herd -initwrites true -model (MODEL_TWO).cat \
../../../herd/testsuite/CTests/*.litmus > $(MODEL_TWO)_results.txt

Then in the herdtools/tools directory:

./mdiff2 $(MODEL_ONE)_results.txt $(MODEL_TWO)_results.txt
