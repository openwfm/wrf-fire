#!/bin/bash

tb=svntrack
sed=gsed

function filter {
  git diff --name-only --relative=wrfv2_fire --diff-filter=ACMR $tb | \
  ${sed}  \
    -e '/^\/chem\/.*$/d' \
    -e '/^\/test\/em_real\/.*$/d' \
    -e '/^\/arch\/.*$/d' \
    -e '/^\/test\/em_fire\/\(.*to\|clean\|.*\.sh\|.*\.m\)/d' \
    -e '/^\/phys\/commit_hash/d' \
    -e 's/^\//wrfv2_fire\//'
}

function replace_commit {
  cat > wrfv2_fire/phys/sfire_id.inc <<EOF
id="$(git rev-parse HEAD)"
EOF
}

#if [ $(git diff HEAD | wc -l) -gt 0 ] ; then
#  echo "need to commit changes before exporting a patch"
#  exit 1
#fi

replace_commit

sv=$(git log -n 1 svntrack | grep -o 'r[0-9]\{4\}' |head -1)
f=$(filter)
git diff --stat --diff-filter=ACDMR svntrack $f
git diff --relative=wrfv2_fire --diff-filter=ACDMR svntrack $f | gzip -c - > wrf_svn_${sv}.patch.gz

#git reset --hard HEAD > /dev/null
git checkout wrfv2_fire/phys/commit_hash

