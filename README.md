# Pullwatch
A Simple Haskell Daemon that monitors for new pull requests in a list of repos, and notifies you if there is a new one

## How to Use
* Create a new Github application (see [here](https://github.com/settings/apps))
* Create a .envrc (or .env if you don't have direnv) with your personal access
  token, e.g. `export PERSONAL_ACCESS_TOKEN = "foobarbaz"`
* Source that file
* build or run it with stack
* Pass the name of the owner and the name of the repo as args, e.g.
  `./path/to/pullwatch-exe ownername reponame`
* Make sure your desktop environment supports DBus notifications (e.g. [https://wiki.archlinux.org/index.php/Desktop_notifications](https://wiki.archlinux.org/index.php/Desktop_notifications) )
