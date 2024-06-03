# tmux configuration

## Usage

Symlink the `.tmux.conf` file to the expected location, e.g.

```
> ln -s /path-to-this-repo/tmux/.tmux.conf ~/.tmux.conf
```

Create the directory `~/.tmux/plugins`. Clone the tmux plugin manager
there:

```
> git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

When you load up tmux, install plugins with `<LEADER> I`. Refresh when
you're done, e.g. `<LEADER> R`.
