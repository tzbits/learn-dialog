# Let's Learn: Dialog

Dialog website: <https://linusakesson.net/dialog/>

Searchable manual: <https://dialog-if.github.io/manual/dialog/0m03>

Here are some dialog-focused micro-games (10-20 commands to reach
an ending).  Each is based on a prompt, and the idea is to lean
into a particular mechanic in order to explore it and learn it by
doing something creative with it.

I encourage you to implement something of your own based on each
prompt, compare it to what's here, and if you'd like to share
your code, I'd be happy to link to it or incorporate it into the
sources here.  Or if you have a longer open-source game that uses
one of these mechanics, I'm also happy to link it as an example.

I've tried to keep implementations minimal focusing on the prompt
mechanics, and if I go beyond the prompt, I try to stick to the
mechanics found in [Cloak of
Darkness](https://linusakesson.net/dialog/cloak/cloak-rel1.dg).

## 1. The Room Evolution Game

Prompt: Create a game where descriptions evolve over time.

Descriptions vary over time and the game evolves from a starting
state to a final state based on the counter.

For example, the description of a room starts in one state at
Turn 1 and reaches a significantly altered state by Turn 10
(e.g., changes from pristine to ruined, crowded to empty, warm to
cold, etc.).

Examples:

- [crisis.dg](./crisis.dg)
  \[[Play Now](https://tzbits.com/dialog/crisis/)\]:
  Meaning Crisis, an adaptation of The Little Match Girl by Hans
  Christian Andersen, by tzbits.

- [christmas.dg](./christmas.dg)
  \[[Play Now](https://tzbits.com/dialog/christmas/)\]
  \[[discussion](https://intfiction.org/t/lets-learn-dialog/78444/10)\]:
  All the Days of Christmas In This Room, by Jason Compton, with
  edits by tzbits to count down the days using recursion.

- [snowable.dg](./snowable.dg)
  \[[Play Now](https://tzbits.com/dialog/snowable/)\]
  \[[discussion](https://intfiction.org/t/lets-learn-dialog/78444/16)\]:
  Snowable by Adam Biltcliffe


## 2. The Finite Switchable Game

Create a game where a switchable item can only be switched on a
limited number of times.

For example, a candle could be `(switchable $)` and `(item $)`,
and a global variable or a tick counter can track how many turns
the candle has been on or how many times it's been switched on,
and a `(prevent [switch on #candle])` can check the count.

Examples:

* [lantern.dg](./lantern.dg)
  \[[Play  Now](https://tzbits.com/dialog/lantern/)\]:
  Candle Lantern by tzbits.

### 3. The Attracts Game

The Attracts Game: Use `($Room attracts $Obj)` to make something
visible from several different rooms simultaneously, for example,
a giant statue, a distant mountain, or a machine spanning
multiple regions of an area. This mechanic ensures a single
object remains in the player's scope across various locations,
effectively simulating a larger physical scale without requiring
the object to be manually moved between rooms.

Examples:

* [The Last Joybooth Shift](https://intfiction.org/t/adjacent-to-let-s-learn-dialog-finite-switchable-game/78518)

### 4. More to come

\-- Suggestions welcome \--
