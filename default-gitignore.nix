let
  objectFiles = ''
    *.a
    *.o
    *.so
  '';

  haskellStuff = ''
    .ghc.environment.*
    dist
    dist-*
    cabal-dev
    *.chi
    *.chs.h
    *.dyn_o
    *.dyn_hi
    *.prof
    *.hi
    *.hp
    *.eventlog
    .cabal-sandbox/
    cabal.sandbox.config
    cabal.config
    cabal.project.local
    .HTF/
    .stack-work/
    stack.yaml.lock
  '';

  vimStuff = ''
    # vim stuff
    [._]*.s[a-v][a-z]
    [._]*.sw[a-p]
    [._]s[a-v][a-z]
    [._]sw[a-p]
    *~
  '';

  nixStuff = ''
    result
    result-*
  '';

  tags = ''
    tags
  '';

  githubStuff = ''
    .github
  '';

  dockerStuff = ''
    Dockerfile
    .dockerignore
  '';

  docsStuff = ''
    README.*
    readme.*
    CHANGELOG.*
    changelog.*
  '';

  defaultGitignore =
    objectFiles
    + haskellStuff
    + vimStuff
    + nixStuff
    + githubStuff
    + dockerStuff
    + docsStuff
    ;

in defaultGitignore
