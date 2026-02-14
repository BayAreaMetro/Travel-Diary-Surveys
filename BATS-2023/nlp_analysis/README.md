# BATS 2023 NLP Analysis

Classification of open-ended survey responses from the 2023 Bay Area Travel Survey (BATS).

## Target Fields

We're classifying two "other-specify" fields where respondents provided free-text:

**1. `mode_other_specify`** - Alternative transportation modes
- Populated when respondent selects "Other" for `mode_1` (code 5)
- Examples: "uber/lyft", "e-bike", "walked to BART", "ferry"
- Common issues: spelling variations, multi-modal trips, rideshare services

**2. `race_other`** - Race/ethnicity responses not captured by standard categories  
- Examples: "half Asian half Black", "Moroccan", "prefer not to say"
- Common issues: multi-racial responses, nationality vs race, protest responses

## Classification Approach

**Hybrid multi-method pipeline:**

1. **Rule-based** keyword matching → handles clear-cut cases (~80-90%)
2. **Embedding similarity** → handles spelling variations and paraphrases  
3. **LLM classification** → handles ambiguous cases requiring context
4. **Manual review** → validation and quality assurance

## Key Principles

1. **Minimal preprocessing** - Preserve structure, handle variation in matching
2. **Respect identity** - Don't force multi-racial into single category  
3. **Conservative junk filtering** - When unsure, flag for review
4. **Explainable results** - Document why X → Y
5. **Iterative refinement** - Start simple, add complexity as needed

## Key Decisions

### Multi-responses
- **Mode:** Extract all modes, flag as multi-modal (e.g., "walk + bus")
- **Race:** Preserve multi-racial as compound category (respects identity)

### Junk vs Valid
**Clear junk:** "n/a", "???", "test", empty  
**NOT junk:** Short but valid ("bike", "uber"), privacy responses ("prefer not to say")

### Specificity Level
Use 2-level hierarchy:
- **Level 1:** Broad category for aggregation (e.g., "Bike", "Asian")
- **Level 2:** Specific detail where available (e.g., "E-bike", "Vietnamese")

### Mode-Specific
- Distinguish powered from unpowered (e-bike vs bike)
- Map to existing categories where possible, flag them
- Extract mode from context: "my car" → "car"

### Race-Specific  
- Don't force nationality into race categories if ambiguous
- Create "Declined/Protest" category (distinct from junk)
- Preserve exact text for potential recoding

## Data Location

**Source:** `C:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\`  
**Files:** person.csv, trip.csv  
**Guide:** bats_dataset_guide.html

## Mode Variables

### `mode_1` - Raw Survey Response
The specific mode selected by the respondent (59 possible values):

| Code | Mode Description |
|------|------------------|
| 1 | Walk (or jog/wheelchair) |
| 2 | Standard bicycle (my household's) |
| 3 | Borrowed bicycle (e.g., a friend's) |
| 4 | Other rented bicycle |
| 5 | Other |
| 6 | Household vehicle 1 |
| 7 | Household vehicle 2 |
| 8 | Household vehicle 3 |
| 9 | Household vehicle 4 |
| 10 | Household vehicle 5 |
| 11 | Household vehicle 6 |
| 12 | Household vehicle 7 |
| 13 | Household vehicle 8 |
| 16 | Other vehicle in household |
| 17 | Rental car |
| 18 | Carshare service (e.g., Zipcar) |
| 21 | Vanpool |
| 22 | Other vehicle (not my household's) |
| 23 | Local (public) bus |
| 24 | School bus |
| 25 | Intercity bus (e.g., Greyhound, Megabus) |
| 26 | Other private shuttle/bus (e.g., a hotel's, an airport's) |
| 27 | Paratransit/Dial-A-Ride |
| 28 | Other bus |
| 30 | BART |
| 31 | Airplane/helicopter |
| 33 | Car from work |
| 34 | Friend/relative/colleague's car |
| 36 | Regular taxi (e.g., Yellow Cab) |
| 38 | University/college shuttle/bus |
| 41 | Intercity/Commuter rail (e.g., Altamount ACE, Amtrak, Caltrain) |
| 42 | Other rail |
| 43 | Skateboard or rollerblade |
| 44 | Golf cart |
| 45 | ATV |
| 47 | Other motorcycle in household |
| 49 | Uber, Lyft, or other smartphone-app ride service |
| 53 | MUNI Metro |
| 54 | Other motorcycle (not my household's) |
| 55 | Express bus or Transbay bus |
| 59 | Peer-to-peer car rental (e.g., Turo) |
| 60 | Other hired car service (e.g., black car, limo) |
| 61 | Rapid transit bus (BRT) |
| 62 | Employer-provided shuttle/bus |
| 63 | Medical transportation service |
| 67 | Local (private) bus (e.g., RapidShuttle, SuperShuttle) |
| 68 | Cable car or streetcar |
| 69 | Bike-share - standard bicycle |
| 70 | Bike-share - electric bicycle |
| 73 | Moped-share (e.g., Scoot) |
| 74 | Segway |
| 75 | Other |
| 76 | Carpool match (e.g., Waze Carpool) |
| 77 | Personal scooter or moped (not shared) |
| 78 | Public ferry or water taxi |
| 80 | Other boat (e.g., kayak) |
| 82 | Electric bicycle (my household's) |
| 83 | Scooter-share (e.g., Bird, Lime) |
| 995 | Missing Response |

When respondents select "Other" (code 5 or 75), they provide details in `mode_other_specify`.

### `mode_type` - Aggregated Mode Category
Grouped classification of mode_1 into 14 broad categories:

| Code | Mode Type | Examples from mode_1 |
|------|-----------|---------------------|
| 1 | Walk | Walk, jog, wheelchair |
| 2 | Bike | Standard bicycle, borrowed bike |
| 3 | Bikeshare | Bike-share (standard or electric) |
| 4 | Scootershare | Scooter-share (Bird, Lime, etc.) |
| 5 | Taxi | Regular taxi, hired car service |
| 6 | TNC | Uber, Lyft, ride apps |
| 7 | Other | Miscellaneous modes |
| 8 | Car | Household vehicles, rental car |
| 9 | Carshare | Zipcar, peer-to-peer rental |
| 10 | School bus | School bus |
| 11 | Shuttle/vanpool | Vanpool, employer/university shuttle |
| 12 | Ferry | Public ferry, water taxi |
| 13 | Transit | BART, bus, rail, cable car |
| 14 | Long distance passenger | Intercity rail, airplane |

## Targets

| Metric | Target |
|--------|--------|
| Classification coverage | ≥95% of valid responses |
| High-confidence rate | ≥90% |
| Audit accuracy | ≥95% |
| Manual review needed | <5% |
