
## How to use the scraper


## Database structure

All data is stored in the SQLLite database Everysport.db. Use [Python/Dataset](http://dataset.readthedocs.org/en/latest/api.html#data-export) to export to csv or json.

### Games

Variabel | Beskrivning
--------|-----------
sasong | Allsvensk säsong (definierat som säsongens startdatum)
league_id | Säsongs-ID (Everysports egna)
matchid | Match-ID under en given säsong (använder Everysports egna match-ID här)
omgang | Matchens omång
hl_id | Hemmalagets unika ID-nr
bl_id | Bortalagets unika ID-nr
hl_namn | Hemmalagets namn
bl_namn | Bortalagets namn
hl_pos | Hemmalagets position innan match (denna kan vara NA eller 0 vid första matchen för säsongen)
bl_pos | Bortalagets position innan match (denna kan vara NA eller 0 vid första matchen för säsongen)
hl_slutmal | Antal gjorda mål för hemmalaget vid matchslut
bl_slutmal | Antal gjorda mål för bortalaget vid matchslut
form5 | Formpoäng de senaste fem omgångarna (vinst = 1, oavgjort = 0, förlust = -1)
form10 | Formpoäng de senaste tio omgångarna (vinst = 1, oavgjort = 0, förlust = -1)

### TODO: Historical positions
Variabel | Beskrivning
--------|-----------
league_id | Säsongs-ID (Everysports egna)
lag_id | Lagets id
lag_namn | Lagets namn
omgang | Omgång
position | Placering i tabellen


### TODO: Teams
Variabel | Beskrivning
--------|-----------
lag | Namn
lag_id | Id (samma som i Everysport)
