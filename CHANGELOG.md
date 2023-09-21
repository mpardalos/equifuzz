0.2.0
===

* Implement test case reduction
* Add `--ask-password` flag to prompt for the password
* Add `--count` flag to `generate` command to allow generating more than 1 example at a time
* Validate SSH connection on startup
* Use real generator in `--test` mode
* Show design description in `equifuzz generate`
* Various UI changes
  - Allow toggling live updates
  - Button to prune uninteresting experiments
    The UI could get quite slow as more experiments were added
  - Use HTML compression to speed up UI updates
  - Show running time, total experiments, amortised time per experiment
* Reduce inconclusive results too
* Allow experiments with more return types
* (Full version only) Add SystemC reduction operators

0.1.1
=====

* Fix: Web UI having 0 height
* Fix: Failed SSH connections due to known_hosts prompt

0.1.0.0
=======

First version
