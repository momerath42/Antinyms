'Antinyms' is a catch-all codename representing many nebulous ideas I've
had (for decades) for games and educational 'toys' that are set on the
'landscape' of english word relationships. See ideas.org for game-design
notes (org-mode format).

The initial work on the codebase was done one weekend in summer 2010,
prompted by a game-jam.  The existence of Princeton's WordNet provided
the fuel.  I used OQGraph for the graph-queries, which was a breath of
fresh oxygen at the time.  Joe Armstrong's websockets demo was molten
awesomeness.

When I recently decided to revive this and put it up on github, I found
OQGraph non-functional and apparently abandoned (at least as an
opensource project).  I looked around yet again at the available
graph-databases and triple/quad-stores; Orly was new to me and
interesting, but as I tried to get started with it I kept thinking I
might be using a crane where a forklift would work better, so I set out
to do an in-memory, wordnet-specific 'db' that would have suitable
properties for treating the (static) relationships of wordnet as a
(soft) real-time playing field, occupied by many agents.

Of course, that journey ate up my free time for the next 3 weekends
(including this one), but maybe the results are more interesting than a
half-baked game utilizing an off-the-shelf graph-db.  My quest
inventory, such as it is, can be reviewed in src/word.erl

The websockets implementation is also cold now, but one thing at a time.

Requirements:

1. erlang r17
2. 8G free ram as configured (TODO: tune for performance:memory / autotune / provide tuning directions)
3. ulimit -Sn should report something >= 131072 for best results when (de)serializing (see /etc/{,security/}limits.conf)

Data Dependencies:

1. mysql with wordnet imported (3.0; haven't tried 3.1 yet); set host, db-name, username and password in wordnet.erl
2. OR (TODO) torrent serialized data folder

To use:

1. After cloning, and optionally placing the torrented data folder in the project directory, ./start.sh
2. if starting from mysql import, call antinyms:site_init() and go for a long walk.
3. OR if starting from serialized data, call word:startFromDisk(wordnet:wordId("origin-word-of-interest")) for any origin node you're going to do a query on (see the bottom of word.erl) - other nodes will be started up lazily, so initial queries will be slow.
4. ???
5. Profit!
