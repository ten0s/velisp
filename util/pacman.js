import {command, argsToArray} from './command-lib.js'

//
// Brings Pacman (https://wiki.archlinux.org/title/pacman) closer to Node.js
//

// :: ([string]) -> Promise(stdout :: string, stderr :: string)
const pacman = command('pacman')

//
// Query the local package database
//
// :: ([string]) -> Promise(stdout :: string, stderr :: string)
const pacmanQuery = (args) => {
    return pacman(['--query', ...args])
}

//
// Query the local package database for packages that own the specified file(s)
//
// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const pacmanQueryOwns = (args) => {
    return pacmanQuery(['--owns', ...argsToArray(args)])
}

//
// Query the local package database for information on a given package
//
// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const pacmanQueryInfo = (args) => {
    return pacmanQuery(['--info', ...argsToArray(args)])
}

export {
    pacman,
    pacmanQuery,
    pacmanQueryOwns,
    pacmanQueryInfo,
}
