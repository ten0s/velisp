import fs from 'node:fs/promises'
import path from 'path'

import {
    endsWith,
    filter,
    map,
    prop,
    tap,
} from './fp-lib.js'

import {
    pacmanQueryOwns,
    pacmanQueryInfo,
} from './pacman.js'

import {promiseSequence} from './promise-lib.js'


if (process.argv.length < 3) {
    console.error(`Usage: ${path.basename(process.argv[1])} MINGW64_BIN_DIR`)
    process.exit(1)
}

const inputDir = process.argv[2]


const isFile = (dirent) => dirent.isFile()

// :: (string) => Promise(string, string)
const queryOwnsInfo = (dll) => {
    return pacmanQueryOwns(`/mingw64/bin/${dll}`)
}

// :: (string) => {path, package, version}
const parseOwnsInfo = (raw) => {
    // PATH is owned by PACKAGE VERSION
    const chunks = raw.replace('\n', '').split(' ')
    return {
        path: chunks[0],
        package: chunks[4],
        version: chunks[5],
    }
}

// :: (string) => Promise({dll, path, package, version})
const getOwnsInfo = (dll) => {
    return queryOwnsInfo(dll)
        .then(parseOwnsInfo)
        .then(info => {
            return {...info, dll}
        })
}

// :: (string) => Promise(string, string)
const queryPkgInfo = pacmanQueryInfo

// :: (string) => [string]
const parseLicenses = (raw) => {
    return raw.split(' ').filter(x => !!x)
}

// :: (string) => {version, url, licenses}
const parsePkgInfo = (raw) => {
    // ...
    // Version  : VERSION
    // ...
    // URL      : URL
    // ...
    // Licenses : LICENSES
    // ...
    const parsers = {
        'Version' : {key: 'version' , parse: x => x},
        'URL'     : {key: 'url'     , parse: x => x},
        'Licenses': {key: 'licenses', parse: parseLicenses},
    }
    return raw
        .split('\n')
        .filter(x => !!x)
        .reduce((acc, line) => {
            const sepidx = line.indexOf(':')
            if (sepidx < 0) return acc

            const name = line.substr(0, sepidx).trim()
            const value = line.substr(sepidx+1).trim()

            const parser = parsers[name]
            if (!parser) return acc

            const key = parser.key
            const val = parser.parse(value)
            acc[key] = val
            return acc
        }, {})
}

// :: (string) => Promise({version, url, licenses})
const getPkgInfo = (pkg) => {
    return queryPkgInfo(pkg)
        .then(parsePkgInfo)
}

// :: (string) => Promise({dll, path, package, version, url, licenses})
const getPkgInfoFromDll = (dll) => {
    return getOwnsInfo(dll)
        .then(ownsInfo => {
            return getPkgInfo(ownsInfo.package)
                .then(pkgInfo => {
                    return {...ownsInfo, ...pkgInfo}
                })
        })
        .then(tap(console.error))
}

// :: ([string]) => Promose([{dll, path, package, version, url, licenses}])
const getPkgInfos = (dlls) => {
    return promiseSequence(dlls, getPkgInfoFromDll)
}

// :: (string) -> string
const formatDep = (dep) => {
    return `
 - Binary module ${dep.dll} from ${dep.package} ${dep.version}
   licensed under the ${dep.licenses.join(' or ')} license
   See ${dep.url} for more detail`
}

// :: ([string]) -> ()
const writeNotice = (deps) => {
    console.log('The following MinGW-w64 dependencies are included in this product:')

    deps.forEach(dep => {
        console.log(formatDep(dep))
    })
}

/*
getPkgInfos(['zlib1.dll', 'libcairo-2.dll',])
    .then(tap(console.error))
    .then(writeNotice)
*/

fs.readdir(inputDir, {withFileTypes: true})
    .then(filter(isFile))
    .then(map(prop('name')))
    .then(filter(endsWith('.dll')))
    .then(tap(console.error))
    .then(getPkgInfos)
    .then(tap(console.error))
    .then(writeNotice)
