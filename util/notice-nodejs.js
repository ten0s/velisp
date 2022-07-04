import fs from 'node:fs/promises'
import path from 'path'
import util from 'util'

import {
    filter,
    map,
    prop,
    tap,
} from './fp-lib.js'

import {promiseSequence} from './promise-lib.js'

import {ensureUrlReached} from './url-lib.js'


if (process.argv.length < 3) {
    console.error(`Usage: ${path.basename(process.argv[1])} NODE_MODULES_DIR`)
    process.exit(1)
}

const inputDir = process.argv[2]


const isDir = (dirent) => dirent.isDirectory()

const npmPkgFile = (parent) => (dir) => path.join(parent, dir, 'package.json')

const readJsonFile = (path) => {
    return fs
        .readFile(path)
        .then(JSON.parse)
        .catch(err => {
            if (err.code === 'ENOENT') {
                console.error(`Error: ${path} not found`)
                return undefined
            } else {
                console.error(`${err} in ${path}`)
                throw err
            }
        })
}

const readJsonFiles = (paths) => {
    return Promise
        .all(paths.map(readJsonFile))
        //.then(tap(pipe(prop(10), console.log)))
        .then(filter(x => !!x)) // filter out undefined
}

const fixUrlPrefix = (url) => {
    url = url.trim()
    switch (true) {
    case url.startsWith('https://'):
        return url
    case url.startsWith('http://'):
        return url.replace('http://', 'https://')
    case url.startsWith('git://'):
        return url.replace('git://', 'https://')
    case url.startsWith('git+http'):
        return url.replace('git+', '')
    case url.startsWith('git@github.com:'):
        return url.replace('git@github.com:', 'https://github.com/')
    case url.startsWith('github:'):
        return `https://github.com/${url.replace('github:', '')}`
    default:
        // Assume GitHub's name/repo
        return `https://github.com/${url}`
    }
}

const fixUrlSuffix = (url) => {
    switch (true) {
    case url.endsWith('.git'):
        return url.replace('.git', '')
    default:
        return url
    }
}

const fixUrl = (url) => {
    return fixUrlSuffix(fixUrlPrefix(url.trim()))
}

const parseNpmHomepage = (pkg) => {
    if (pkg.homepage) {
        return pkg.homepage
    }

    const repo = pkg.repository
    if (repo && typeof repo === 'string') {
        return repo
    }

    if (typeof repo === 'object') {
        if (typeof repo.url === 'string') {
            return repo.url
        }
    }

    throw new Error(`No homepage or repository: ${util.inspect(pkg)}`)
}

const parseLicense = (lic) => lic.replace('(', '').replace(')', '').split(' OR ')

const parseNpmLicense = (lic) => {
    if (typeof lic === 'string') {
        return parseLicense(lic)
    }

    if (typeof lic === 'object') {
        if (typeof lic.type === 'string') {
            return parseLicense(lic.type)
        }
    }

    return []
}

const parseNpmLicenses = (pkg) => {
    let licenses = parseNpmLicense(pkg.license)
    if (licenses.length) {
        return licenses
    }

    if (Array.isArray(pkg.licenses)) {
        licenses = pkg.licenses.map(parseNpmLicense).flat()
        if (licenses.length) {
            return licenses
        }
    }

    throw new Error(`Unknown NPM license(s) object: ${util.inspect(pkg)}`)
}

const parseNpmPkg = (pkg) => {
    return {
        name: pkg.name,
        version: pkg.version,
        format: 'npm',
        homepage: fixUrl(parseNpmHomepage(pkg)),
        licenses: parseNpmLicenses(pkg)
    }
}

const nodeInfo = () => {
    const version = process.version.replace('v', '')
    return {
        name: 'nodejs',
        version,
        format: 'binary',
        homepage: `https://github.com/nodejs/node/tree/v${version}`,
        licenses: ['Node.js'],
    }
}

const addNodeInfo = (deps) => [nodeInfo(), ...deps]

/* eslint-disable */
const checkHomepages = (deps) => {
    return promiseSequence(deps.map(prop('homepage')), ensureUrlReached)
}
/* eslint-enable */

const formatDep = (dep) => {
    const what = (dep) => {
        switch (dep.format) {
        case 'binary':
            return `Binary module ${dep.name} ${dep.version}`
        case 'npm':
            return `NPM package ${dep.name}@${dep.version}`
        default:
            throw new Error(`Unexpected format ${dep.format} for ${dep.name}`)
        }
    }

    return `
 - ${what(dep)} licensed under the ${dep.licenses.join(' or ')} license
   See ${dep.homepage} for more detail`
}

const writeNotice = (deps) => {
    console.log('The following Node.js dependencies are included in this product:')

    deps.forEach(dep => {
        console.log(formatDep(dep))
    })
}

fs.readdir(inputDir, {withFileTypes: true})
    .then(filter(isDir))
    .then(map(prop('name')))
    .then(map(npmPkgFile(inputDir)))
    .then(readJsonFiles)
    .then(map(parseNpmPkg))
    .then(addNodeInfo)
    .then(tap(console.error))
    //.then(tap(checkHomepages))
    .then(writeNotice)
