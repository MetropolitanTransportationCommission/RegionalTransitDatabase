# Regional Transit Database  

Tools for Processing 511 API Datasets for MTC's Regional Transit Database
[511 API Documentation](http://assets.511.org/pdf/RTT%20API%20V2.0%20Reference.pdf)  

## Goal 

Process transit data from The Bay Area's Transit operators to satisfy statutory requirements and answer policy question.  

## Data Sources

GTFS as published by operators and collected by MTC 511, GTFS Data Exchange, and MTCGIS

[`data/cached_gtfs.csv`](https://github.com/BayAreaMetro/RegionalTransitDatabase/blob/master/data/cached_gtfs.csv) contains a reference to cached GTFS data that are available to us.  

## Methodology 

- [Transit Priority Areas](https://github.com/MetropolitanTransportationCommission/RegionalTransitDatabase/blob/master/docs/transit_priority_areas.md) 

- [Routes, Stops, and Frequencies by Transit Provider from 2008 to 2017](https://github.com/MetropolitanTransportationCommission/RegionalTransitDatabase/blob/master/docs/historical_transit_data.md) 


### Folders

- R: scripts for analysis of regional transit data
- data: small, important inputs and outputs 
- docs: documentation
- images: used in documentation
- rtd: python scripts for fetching and processing data
