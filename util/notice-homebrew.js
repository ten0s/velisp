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
    brewCacheSearch,
    brewInfo,
} from './homebrew.js'

import {promiseSequence} from './promise-lib.js'

import {ensureUrlReached} from './url-lib.js'

if (process.argv.length < 3) {
    console.error(`Usage: ${path.basename(process.argv[1])} HOMEBREW_LIB_DIR`)
    process.exit(1)
}

const inputDir = process.argv[2]


const isFile = (dirent) => dirent.isFile()

// :: (string) -> Promise(string, string)
const queryOwnsInfo = brewCacheSearch

// :: (string) -> {package, version}
const parseOwnsInfo = (raw) => {
    // PACKAGE VERSION
    const chunks = raw.replace('\n', '').split(' ')
    return {
        package: chunks[0],
        version: chunks[1],
    }
}

// :: (string) -> Promise({lib, package, version})
const getOwnsInfo = (lib) => {
    return queryOwnsInfo(lib)
        .then(parseOwnsInfo)
        .then(info => {
            return {...info, lib}
        })
}

// :: (string) -> Promise(string, string)
const queryPkgInfo = brewInfo


// :: (string) -> {homepage, licenses}
const parsePkgInfo = (raw) => {
    // [
    //  {
    //   "homepage": HOMEPAGE,
    //   "license" : LICENSES,
    //  }
    // ]
    const parsers = {
        'homepage': {key: 'homepage', parse: x => x},
        'license' : {key: 'licenses', parse: x => x},
    }
    return Object.entries(JSON.parse(raw)[0])
        .reduce((acc, [name, value]) => {
            const parser = parsers[name]
            if (!parser) return acc

            const key = parser.key
            const val = parser.parse(value)
            acc[key] = val
            return acc
        }, {})
}

// :: (string) -> Promise({version, homepage, licenses})
const getPkgInfo = (pkg) => {
    return queryPkgInfo(pkg)
        .then(parsePkgInfo)
}

// :: (string) -> Promise({lib, package, version, homepage, licenses})
const getPkgInfoFromLib = (lib) => {
    return getOwnsInfo(lib)
        .then(ownsInfo => {
            return getPkgInfo(ownsInfo.package)
                .then(pkgInfo => {
                    return {...ownsInfo, ...pkgInfo}
                })
        })
        .then(tap(console.error))
}

// :: ([string]) -> Promose([{lib, package, version, homepage, licenses}])
const getPkgInfos = (libs) => {
    return promiseSequence(libs, getPkgInfoFromLib)
}

/* eslint-disable */
// :: ([{homepage}]) -> Promise(true, string)
const checkHomepages = (deps) => {
    return promiseSequence(deps.map(prop('homepage')), ensureUrlReached)
}
/* eslint-enable */

// :: ({lib, package, version, homepage, licenses}) -> string
const formatDep = (dep) => {
    return `
 - Binary module ${dep.lib} from ${dep.package} ${dep.version}
   licensed under the ${dep.licenses} license(s)
   See ${dep.homepage} for more detail`
}

// :: ([{lib, package, version, homepage, licenses}]) -> ()
const writeNotice = (deps) => {
    console.log('The following Homebrew dependencies are included in this product:')

    deps.forEach(dep => {
        console.log(formatDep(dep))
    })
}

/*
getPkgInfos(['libgirepository-1.0.1.dylib', 'libcairo.2.dylib',])
    .then(tap(console.error))
    .then(writeNotice)
*/

fs.readdir(inputDir, {withFileTypes: true})
    .then(filter(isFile))
    .then(map(prop('name')))
    .then(filter(endsWith('.dylib')))
    .then(tap(console.error))
    .then(getPkgInfos)
    .then(tap(console.error))
    //.then(tap(checkHomepages))
    .then(writeNotice)
