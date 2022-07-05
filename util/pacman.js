import {spawn} from 'child_process'
import util from 'util'

//
// Brings Pacman (https://wiki.archlinux.org/title/pacman) closer to Node.js
//

// :: ([string]) -> Promise(stdout :: string, stderr :: string)
const pacman = (args) => {
    let out = ''
    let all = ''
    return new Promise((resolve, reject) => {
        const child = spawn('pacman', [...args])

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

// :: ([string]) -> Promise(stdout :: string, stderr :: string)
const pacmanQuery = (args) => {
    return pacman(['--query', ...args])
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

// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const pacmanQueryOwns = (args) => {
    return pacmanQuery(['--owns', ...toArray(args)])
}

// :: (string | [string]) -> Promise(stdout :: string, stderr :: string)
const pacmanQueryInfo = (args) => {
    return pacmanQuery(['--info', ...toArray(args)])
}

export {
    pacman,
    pacmanQuery,
    pacmanQueryOwns,
    pacmanQueryInfo,
}
