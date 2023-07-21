# fast-time

## A custom date time parser made using flatparse


### Supported Formats of Date & Time 
#### Date-Time Formats  

- "%Y-%m-%dT%k:%M:%SZ"
- "%Y-%m-%dT%k:%M:%S%QZ"
- "%Y-%m-%d %k:%M:%S%Q" 
- "%Y-%m-%d %k:%M:%S" 
- "%Y-%m-%dT%k:%M:%S%Q%Ez"

#### Date Formats 

- "%Y-%m-%d"
- "%Y %m %d"
- "%Y-%m %d"
- "%Y %m-%d" 
- "%Y/%-m/%-d"
- "%d%m%Y"

### Benchmarks 

| Benchmark Name | Description | Data.Time Library | Fast-time | 
| ----- | ----- | ----- | ----- | 
| UTCTime | Converts the text time to UTCTime format. | 8.382 μs   (8.359 μs .. 8.425 μs) | 648.4 ns (647.7 ns .. 649.4 ns) |
| UTCTime Milliseconds | Converts the text time which includes millisecond to UTCTime format. | 12.89 μs   (12.88 μs .. 12.93 μs) | 669.4 ns   (668.8 ns .. 670.5 ns) | 
| LocalTime | Converts the text time to Local time covering case of millisecond parsing as well. | 12.72 μs   (12.71 μs .. 12.74 μs) |  1.027 μs (1.026 μs .. 1.027 μs) | 
| Date Parsing | Converts the text date in the given format. |  8.511 μs   (8.501 μs .. 8.535 μs) | 409.1 ns   (408.8 ns .. 410.5 ns) | 
| Date Parsing ("%d%m%Y") | Converts the text date to the given format using isolate method of flatparse library. | 14.78 μs (14.77 μs .. 14.80 μs) |  371.9 ns (371.8 ns .. 372.1 ns) |

