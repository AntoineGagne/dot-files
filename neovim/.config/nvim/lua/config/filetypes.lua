vim.filetype.add({
  extension = {
    gv = 'dot',
    m = 'octave',
    oct = 'octave',
    service = 'systemd',
    lalrpop = 'lalrpop',
  },
  filename = {
    ['rebar.config.script'] = 'erlang',
    ['rebar.lock'] = 'erlang',
    ['sys.config'] = 'erlang',
    ['.envrc'] = 'sh',
  },
})
