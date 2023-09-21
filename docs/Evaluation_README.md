# Equifuzz 

## Setup

This software is packaged as a container image. On a computer with docker installed, run the following commands

```sh
docker load -i equifuzz-evaluation-0.2.0.tar.gz
docker run localhost/equifuzz-evaluation:0.2.0 --help
```

You should get a help message from equifuzz.

## Usage

Equifuzz runs on a one computer, but is designed to run the equivalence checker it is fuzzing on a separate computer, to which it connects using `ssh`. The details of that ssh connection are given as command line options to equifuzz.

The `web` sub-command in the equifuzz CLI runs the fuzzing loop and also starts the WebUI which can be access on port 8888 on the host running equifuzz.

Here is an example command to run equifuzz. Here, you will need to replace `vcf-runner.example.com` with the host on which `vcf` is installed, `runner-user` with the user you use to log in to that host, and `/path/to/activate_script.sh` with the path to a script which you need to run `source` on before running `vcf`. If there is no such script you can remove the `--activate-script ` flag entirely. Use the `--help` flag for more details about how to run equifuzz. 
```sh
docker run --rm -it -p 8888:8888 localhost/equifuzz-evaluation:0.1.1 web --host vcf-runner.example.com --username runner-user --ask-password --activate-script /path/to/activate_script.sh --fec-type vcf --no-save --verbose
```

All sub-commands in the equifuzz CLI have a `--help` option. 

Example:
```sh
docker run --rm -it localhost/equifuzz-evaluation:0.2.0 web --help
Usage: equifuzz web [--max-experiments COUNT] [--verbose] [--no-save]
                    (--host HOSTNAME --username USERNAME
                      [--ask-password | --password PASSWORD]
                      [--activate-script PATH] --fec-type TYPE |
                      --test [--inconclusive])

  Run the equifuzz Web UI, connected to a remote host

Available options:
  --max-experiments COUNT  Maximum number of experiments to allow to run
                           concurrently (default: 10)
  --verbose                Print experiment status to the console
  --no-save                Do not save successful experiment results
  --host HOSTNAME          Remote hostname or IP to run equivalence checker on
  --username USERNAME      Username to connect to remote host
  --ask-password           Ask for SSH password to the remote host
  --password PASSWORD      Password to connect to remote host
  --activate-script PATH   Script to be sourced on the remote host before
                           running vcf
  --fec-type TYPE          What FEC type we are running against
                           (vcf|catapult|jasper)
  --test                   Use a 'test' runner, that just gives random results
                           (for testing)
  --inconclusive           Include inconclusive results in the test results
  -h,--help                Show this help text
```

For any inquiries, you can reach me at [michail.pardalos17@imperial.ac.uk](mailto:michail.pardalos17@imperial.ac.uk).
