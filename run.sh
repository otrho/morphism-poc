#!/usr/bin/env bash

morph_file="${1}"

if [[ -z "${morph_file}" ]] ; then
    echo "use: $0 <FILE.MORPH>"
    exit 1
fi

if [[ "$(basename ${morph_file} .morph).morph" != "${morph_file}" ]] ; then
    echo "error: filename must be a .morph file"
    exit 1
fi

run_dir="$(dirname "${0}")"

"${run_dir}/compile.janet" "${morph_file}" &&
    "${run_dir}/interp.janet" "${morph_file}.ism"
