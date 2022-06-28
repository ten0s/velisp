import fs from 'node:fs/promises'
import https from 'https'
import path from 'path'
import util from 'util'

//console.error(process.argv)

if (process.argv.length < 3) {
    console.error(`Usage: ${path.basename(process.argv[1])} NODE_MODULES_DIR`)
    process.exit(1)
}

const inputDir = process.argv[2]

const filter = (fn) => (arr) => arr.filter(fn)
const map = (fn) => (arr) => arr.map(fn)
const prop = (name) => (obj) => obj[name]
//const forEach = (fn) => (arr) => arr.forEach(fn)
const tap = (fn) => (x) => { fn(x); return x }
//const pipe = (...fns) => x => fns.reduce((acc, fn) => fn(acc), x)

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
        licenses: parseNpmLicenses(pkg)
    }
}

const nodeInfo = () => {
    return {
        name: 'nodejs',
        version: process.version.replace('v', ''),
        format: 'binary',
        licenses: ['Node.js'],
    }
}

const addNodeInfo = (deps) => [nodeInfo(), ...deps]

const resolveLicenseUrl = (name, version) => (license) => {
    switch (license) {
    case 'Node.js':
        return `https://github.com/nodejs/node/blob/v${version}/LICENSE`
    default:
        return `https://spdx.org/licenses/${license}.html`
    }
}

const ensureUrlReached = (dep, url) => {
    return new Promise((resolve, reject) => {
        https.get(url, (res) => {
            if (res.statusCode === 200) {
                resolve(true)
            } else {
                reject(`Error: ${url} status ${res.statusCode} for ${util.inspect(dep)}`)
            }
        })
    })
}

const addLicenseUrls = (dep) => {
    return {
        ...dep,
        urls: dep.licenses.map(resolveLicenseUrl(dep.name, dep.version)),
    }
}

const checkLicenseUrls = (deps) => {
    deps.forEach(dep => {
        dep.urls.forEach(url => {
            console.error(`Checking ${url} for ${dep.name} ${dep.version}`)
            ensureUrlReached(dep, url)
        })
    })
}

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
   See license text at ${dep.urls.join(' or ')}`
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
    .then(map(addLicenseUrls))
    .then(tap(checkLicenseUrls))
    .then(writeNotice)
