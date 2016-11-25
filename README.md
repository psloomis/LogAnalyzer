# Log Analyzer

## Usage
`ghc -o analyzer LogAnalyze.hs`

`./analyzer [command] [input log filename]` e.g. `./analyzer plot calgary_access_log.log`
### Commands:
#### plot
The plot command produces a png image with a graph showing the number of HTTP Requests over time.
For example, running `./analyzer plot calgary_access_log.log` will produce the following graph:

