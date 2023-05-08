stdenv: ''
  # put all flags into 'params' array
  source ${stdenv.cc}/nix-support/utils.bash
  expandResponseParams "$@"

  # check if '-shared' flag is present
  hasShared=0
  for param in "''${params[@]}"; do
    if [[ "$param" == "-shared" ]]; then
      hasShared=1
    fi
  done

  if [[ "$hasShared" -eq 0 ]]; then
    # if '-shared' is not set, don't modify the params
    newParams=( "''${params[@]}" )
  else
    # if '-shared' is present, remove '-static' flag
    newParams=()
    for param in "''${params[@]}"; do
      if [[ ("$param" != "-static") ]]; then
        newParams+=( "$param" )
      fi
    done
  fi

  # invoke the actual linker with the new params
  exec x86_64-unknown-linux-musl-cc @<(printf "%q\n" "''${newParams[@]}")
''
