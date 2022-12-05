import {spawn} from 'child_process'
import util from 'util'

//
// Brings Homebrew (https://brew.sh) closer to Node.js
//

// :: ([string]) -> Promise(stdout :: string, stderr :: string)
const brew = (args) => {
    let out = ''
    let all = ''
    return new Promise((resolve, reject) => {
        const child = spawn('brew', [...args])

        child.stdout.on('data', (data) => {
            out += data.toString()
            all += data.toString()
            //console.error(`stdout: ${data}`)
        })

        child.stderr.on('data', (data) => {
            all += data.toString()
            //console.error(`stderr: ${data}`)
        })

        child.on('close', (code) => {
            if (code === 0) {
                resolve(out)
            } else {
                reject(all)
            }
            //console.error(`Child exited with ${code}`)
        })
    })
}

// :: (string | [string]) -> [string]
const toArray = (x) => {
    if (typeof x === 'string') {
        return [x]
    }
    if (Array.isArray(x)) {
        return x
    }
    throw new Error(`Expected string or array but got: ${util.inspect(x)}`)
}

//
// Query the local package database for package(s) that own the specified file(s)
//
// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const brewCacheSearch = (args) => {
    return brew(['cache', '-s', ...toArray(args)])
}

//
// Query the local package database for information on a given package
//
// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const brewInfo = (args) => {
    return brew(['info', '--json', ...toArray(args)])
}

export {
    brew,
    brewCacheSearch,
    brewInfo,
}
