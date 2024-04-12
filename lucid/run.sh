# Make sure to build the aiken scripts first
deno run \
  --allow-read \
  main.ts \
  --scriptsFile ../plutus.json
