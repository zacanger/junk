### do this

hide tweet button and character count initially (so, just not there)
on click, double textarea size, reveal character count and tweet button
ch count decrease realtime
at 10 chars, count turns red
if chars > 140, disable tweet button (and re-enable if =< 140)
new tweet pushed to column immediately, w/ prof image in top left, full username and name
tweet actions on :hover over individual tweets only
rt/timestamp/reply area hidden by default; expand on click
timestamps with timeago (or, i think, livestamp actually)
icons for favs/rts in upper right of tweet card
bootstrap tooltips on :hover over avatar
localstorage (or howabout localForage?)
or baas... ... yeah. localforage.

#### notes on changes

setting max-viewport is a *horrible* practice. nixed.
normalize instead of reset; cdn instead of stuck in our own stylesheet.
updated to jquery 2.1.4; cdn instead of in our js directory.
replacing ids with classes, at least for starters... just a few ids in here, sprinkled around ಠ⌣ಠ
`page-container` = `container`
`dashboard` = `dash`
`profile-summary` = `profile`
`char-count` = `count`
`tweet-controls` = `controls`
because i don't like to type

#### notes on the process (and result)

so it turns out that i'm 99% sure none of us is doing this right. after doing epic battle with 
the javascripts, i've been defeated. manually moving the first tweet down and putting the new 
tweet's content in its place, right now, kind of relies solely on using the actual username/name 
of the former top tweet on the feed (btw, css could probably use an update... twitter's one column 
of user and tweets, and two side columns of promos and ads, now.). that's, uh, totally unrealistic.
