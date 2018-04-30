Reflex Demo Game
================

Not so much of a "game" as yet, given there are no goals or victory conditions,
but it does a sweet as ray-caster.

You can look around and that seems to work, but moving is a bit wonky. Use the
arrow keys to move/look around. You have to click the top canvas before the keys
will register. Because I didn't do a global bind on the key events, only key
events that are triggered from the canvas.

![A screenshot](screenshot.png "A screenshot showing rendering screen and map
display)

### To Build

I use nix to work on this, and setting up
[reflex-platform](https://github.com/reflex-frp/reflex-platform) requires nix as
well. You are welcome to try building this without nix, but I cannot help you.
Smooth building isn't a priority at this stage of development. :)

To enter a `nix-shell` to poke things with GHC available (faster builds):

```shell
$ nix-shell -A shells.ghc
```

To enter a `nix-shell` to poke things with GHCJS available (building JS):

```shell
$ nix-shell -A shells.ghcjs
```

To build it so you can open it in a browser:

```shell
$ nix-build -o frontend-result -A ghcjs.frontend
$ $FAV_BROWSER_NAME_HERE frontend-result/bin/frontend.jsexe/index.html
```


