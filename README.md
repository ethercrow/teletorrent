# teletorrent

`teletorrent` transfers a torrent file to
a remote box, waits until the content
materialises and downloads it back to the
local box.

## How to use it

Populate the config in `$HOME/.config/teletorrent/config.dhall`:

```dhall
{ remote_host = "333.333.333.333"
, remote_user = "remoteuser"
, remote_inbox_torrent_dir = "t_inbox"
, remote_finished_content_dir = "t_finished"
, remote_finished_torrent_dir = "t_past_torrents"
}
```

Run:

```console
$ teletorrent debian-stable-iso.torrent
```
