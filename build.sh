#!/bin/bash
TEMP=$(mktemp -d)

mkdir -p ${TEMP}/src

cat <<_EOF > ${TEMP}/install
#!/bin/bash
echo "Das B.S."
_EOF
chmod +x ${TEMP}/install

cp -a bin/mlton-ltg ${TEMP}/run
cp -a bin/mlton-ltg-bin ${TEMP}/run-bin

OLDDIR="$(readlink -f "$(dirname $0)")"
cd ${TEMP}
tar cvzf "${OLDDIR}/ltg.tar.gz" .
