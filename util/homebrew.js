import {command, argsToArray} from './command-lib.js'

//
// Brings Homebrew (https://brew.sh) closer to Node.js
//

// :: ([string]) -> Promise(stdout :: string, stderr :: string)
const brew = command('brew')

//
// Query the local package database for packages that own the specified file(s)
//
// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const brewCacheSearch = (args) => {
    return brew(['cache', '-s', ...argsToArray(args)])
}

//
// Query the local package database for information on a given package
//
// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const brewInfo = (args) => {
    return brew(['info', '--json', ...argsToArray(args)])
}

export {
    brew,
    brewCacheSearch,
    brewInfo,
}
