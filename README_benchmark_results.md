# Benchmark Results with GHC 9.10.3

`bench-search` (KiwiPete Depth 6)
Time: 1.273609691s

`bench-core`
Core | Start      | Depth 5 | Nodes:    4865609 | Time:  0.817s | NPS:    5953191
Core | KiwiPete   | Depth 4 | Nodes:    4085603 | Time:  0.079s | NPS:   51604262
Core | Atomic Start | Depth 4 | Nodes:     197326 | Time:  0.008s | NPS:   23302809

# Benchmark Results with GHC 9.6.5

`bench-search` (KiwiPete Depth 6)
Time: 1.405805012s

`bench-core`
Core | Start      | Depth 5 | Nodes:    4865609 | Time:  0.793s | NPS:    6135530
Core | KiwiPete   | Depth 4 | Nodes:    4085603 | Time:  0.078s | NPS:   52567308
Core | Atomic Start | Depth 4 | Nodes:     197326 | Time:  0.008s | NPS:   23459110
