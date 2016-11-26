# Web Log Analyzer

## Usage
`ghc -o analyzer LogAnalyze.hs`

`./analyzer [command] [input log filename]`

* __the input log file must contain web access logs in [Common Log Format](https://en.wikipedia.org/wiki/Common_Log_Format)__
* __the input log file must be encoded in UTF-8__

### Commands:
The examples below use a log file, `calgary_access_log`, that I downloaded at [this link](http://ita.ee.lbl.gov/html/contrib/Calgary-HTTP.html) from the Internet Traffic Archive and converted to UTF-8.

#### plot
The plot command produces a png image with a graph showing the number of HTTP Requests over time.
For example, running `./analyzer plot calgary_access_log.log` will produce the following image file:

![](https://s17.postimg.org/rk5uoomq7/requests.png?raw=true)

#### top_users
The top_users command shows a list of the users who have made the most HTTP requests. For example, `./analyzer top_users calgary_access_log.log` will produce the following output:

```
Most active users:
"Villain" made 37 requests
"dasfh" made 1 requests
"villain" made 1 requests
```

#### responses
The responses command shows HTTP responses in the log file and each response's percentage of all the responses in the log file. For example, this command could tell you that 404s make up 17% of responses in the log file. Running `./analyzer responses calgary_access_log.log` will produce the following output:

```
HTTP response statuses by percentage:
status 200:	78.41%
status 302:	4.19%
status 304:	13.49%
status 400:	0.00%
status 401:	0.01%
status 403:	0.66%
status 404:	3.24%
status 500:	0.01%
status 501:	0.01%
```

#### top_files
The top_files command shows a list of the most requested static files and the number of times each was requested. For example, running `./analyzer top_files calgary_access_log.log` will produce the following output:

```
Most requested files:
"index.html": 139503 requests
"3.gif": 24006 requests
"2.gif": 23595 requests
"4.gif": 8018 requests
"244.gif": 5148 requests
```

## Dependencies
* [easyplot](https://hackage.haskell.org/package/easyplot-1.0/docs/Graphics-EasyPlot.html)
* [GoogleChart](https://hackage.haskell.org/package/GoogleChart)
