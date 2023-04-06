# fast-time

## A custom parsers using flatparse for data-time.


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
| UTCTime | Converts the text time to UTCTime format. | 8.382 μs   (8.359 μs .. 8.425 μs) | 734.4 ns   (731.7 ns .. 736.4 ns) |
| UTCTime Milliseconds | Converts the text time which includes millisecond to UTCTime format. | 11.28 μs   (11.24 μs .. 11.33 μs) | 806.6 ns   (804.4 ns .. 808.8 ns) | 
| LocalTime | Converts the text time to Local time covering case of millisecond parsing as well. | 11.67 μs   (11.53 μs .. 11.81 μs | 1.200 μs   (1.180 μs .. 1.220 μs) | 
| Date Parsing | Converts the text date in the given format. | 4.710 μs   (4.296 μs .. 5.211 μs) | 440.3 ns   (437.4 ns .. 443.6 ns) | 
| Date Parsing ("%d%m%Y") | Converts the text date to the given format using isolate method of flatparse library. | 12.08 μs   (11.81 μs .. 12.29 μs) | 368.1 ns   (359.7 ns .. 378.0 ns) |

