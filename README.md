# Pullwatch
A Simple Haskell Daemon that monitors for new pull requests in a list of repos, and notifies you if there is a new one

## How to Use
* Create a new Github application (see [here](https://github.com/settings/apps))
* Create a .envrc (or .env if you don't have direnv) with your personal access
  token, e.g. `export PERSONAL_ACCESS_TOKEN = "foobarbaz"`
* Source that file
* Modify `Main.hs` (at the moment there is no other entrypoint to this) and
  build or run it with stack
* Make sure your desktop environment supports DBus notifications (e.g. [https://wiki.archlinux.org/index.php/Desktop_notifications](https://wiki.archlinux.org/index.php/Desktop_notifications) )
