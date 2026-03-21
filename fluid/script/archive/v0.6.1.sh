# Run in gh-pages branch (v0.6.1) to generate archive of that version
#!/usr/bin/env bash
set -xe

NAME=v0.6.1.zip

zip -r \
   --exclude $NAME \
             .git/\* \
             .spago/\* \
             node_modules/\* \
             output/\* \
             output-es/\* \
   - $NAME . > $NAME
