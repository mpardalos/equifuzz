# Equifuzz 

## Setup

This software is packaged as a container image. On a computer with docker installed, run the following commands

```sh
docker load -i equifuzz-0.1.0.0.tar.gz
docker run equifuzz --help
```

You should get a help message from equifuzz.

## Usage

Equifuzz runs on a one computer, but is designed to run the equivalence checker it is fuzzing on a separate computer, to which it connects using `ssh`. The details of that ssh connection are given as command line options to equifuzz.

The `web` sub-command in the equifuzz CLI runs the fuzzing loop and also starts the WebUI which can be access on port 8888 on the host running equifuzz.

Example:
```sh
docker run equifuzz:v0.1.0.0 web --host vcf-runner.example.com --username runner-user --password secret123 --activate-script /opt/synopsys/vc_static/T-2022.06-SP2-3/activate.sh
```

All sub-commands in the equifuzz CLI have a `--help` option. 

Example:

```sh
$ docker run equifuzz:v0.1.0.0 web --help
Usage: equifuzz web [--max-experiments COUNT] [--verbose] 
                    (--host HOSTNAME --username USERNAME [--password PASSWORD] 
                      [--activate-script PATH] |
                      --test)

  Run the equifuzz Web UI, connected to a remote host

Available options:
  --max-experiments COUNT  Maximum number of experiments to allow to run
                           concurrently (default: 10)
  --verbose                Print experiment status to the console
  --host HOSTNAME          Remote hostname or IP to run equivalence checker on
  --username USERNAME      Username to connect to remote host
  --password PASSWORD      Password to connect to remote host
  --activate-script PATH   Script to be sourced on the remote host before
                           running vcf
  --test                   Show test data on the interface, don't run any
                           fuzzing
  -h,--help                Show this help text
```

For any inquiries, you can reach me at [michail.pardalos17@imperial.ac.uk](mailto:michail.pardalos17@imperial.ac.uk).
