# Northeast Scallop Data

A subset of anonymized scallop data

## Usage

``` r
scallop
```

## Format

\`scallop\` A data.frame with 10,000 rows and 19 columns:

- TRIPID:

  Randomly assigned trip ID number.

- DATE_TRIP:

  Date of landing.

- PERMIT.y:

  Randomly assigned six-digit vessel fishing permit number.

- TRIP_LENGTH:

  Days calculated from the elapsed time between the date-time sailed and
  date-time landed; this is a measure of days absent.

- GEARCODE:

  Fishing gear used on the trip.

- port_lat:

  Latitude of the geoid.

- port_lon:

  longitude of the geoid.

- previous_port_lat:

  Previous latitude of geoid.

- previous_port_lon:

  Previous longitude of geoid.

- Plan Code:

  Portion of the VMS declaration code that identifies the fishery being
  declared into for the trip.

- Program Code:

  Portion of the VMS declaration code that identifies the program within
  the declared fishery. For scallops, the program code delineates LA and
  LAGC trips, as well as access area trips from other trips.

- TRIP_COST_WINSOR_2020_DOL:

  The estimated or real composite trip cost for the VTR trip record
  generated using the methods described in the Commercial Trip Cost
  Estimation 2007-2019 PDF file. However, these values have been
  Winsorized by gear type as a method of avoiding unreasonably high or
  low trip costs, replacing any value within each gear-group that is
  less than the 1st percentile or greater than the 99th percentile with
  the 1st and 99th percentile value, respectively.

- DDLAT:

  The latitude reported on a VTR (Vessel Trip Reports).

- DDLON:

  The longitude reported on a VTR (Vessel Trip Reports).

- ZoneID:

  FishSET's version of a ten minute square.

- LANDED_OBSCURED:

  Landed pounds from the dealer report (jittered/obscured).

- DOLLAR_OBSCURED:

  The value of catch paid by the dealer, from the dealer report
  (jittered/obscured).

- DOLLAR_2020_OBSCURED:

  The value of catch paid by the dealer, from the dealer report (in 2020
  dollars, jittered/obscured).

- DOLLAR_ALL_SP_2020_OBSCURED:

  The value of catch for all species caught (in 2020 dollars,
  jittered/obscured).

## Source

Add source here
