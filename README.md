nodelers
========

An basic world of concurrent AI agents (called 'nodelers') written in Erlang. The nodelers move around, eat, sleep, reproduce and die. Built as a little bit of fun with Kyle Beckles and India Raybould. The system and the user interface India made are not currently coupled.

## Usage ##

Execute `start.sh` and point your browser to `http://localhost:8089`. You will see the actions the nodelers have taken appearing in the console and the nodelers positions and movements in the browser pane. The browser pane will automatically update as the nodelers move around.

## Future Work ##

* Merge in India's excellent frontend.
* Improve the nodeler AI.
* Improve the game's balancing in general (populations tend to either 'boom then bust' or just 'bust' straight away at the moment).
* Potentially add interaction and the viewer 'playing God' by changing food levels etc.
