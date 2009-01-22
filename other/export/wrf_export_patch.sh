#!/bin/bash

tb=svntrack

function filter {
  git diff --name-only --relative=wrfv2_fire $tb | \
  sed  \
    -e '/^\/chem\/.*$/d' \
    -e '/^\/test\/em_real\/.*$/d' \
    -e '/^\/arch\/.*$/d' \
    -e '/^\/test\/em_fire\/\(.*to\|clean\|.*\.sh\|.*\.m\)/d' \
    -e 's/^\//wrfv2_fire\//'
}

function replace_commit {
  cat > wrfv2_fire/phys/commit_hash <<EOF
#!/bin/bash
echo "id='$(git rev-parse HEAD)'"
EOF
chmod +x wrfv2_fire/phys/commit_hash
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

