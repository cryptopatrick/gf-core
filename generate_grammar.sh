### Automatically export all functions to export_json.sh

```shell
#!/bin/bash
set -e

# Compile the grammars
gf --make helloEng.gf helloFre.gf

PGF="hello.pgf"
LANGS=("Eng" "Fre")

# Get list of all functions in the grammar
FUNCS=($(gf --functions "$PGF"))

echo '{'

first_func=true
for f in "${FUNCS[@]}"; do
  if [ "$first_func" = false ]; then
    echo ','
  fi
  first_func=false

  echo -n "  \"$f\": {"

  first_lang=true
  for l in "${LANGS[@]}"; do
    if [ "$first_lang" = false ]; then
      echo -n ','
    fi
    first_lang=false

    echo -n "\"$l\":"
    echo "linearize -lang=$l $f" | gf --run --output-format=json "$PGF" | tr -d '\n'
  done

  echo -n "}"
done

echo
echo '}'
```

```shell
# Run the script and store the result in a file.
./export_json.sh > results.json
```
