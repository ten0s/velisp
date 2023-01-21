import {spawn} from 'child_process'
import util from 'util'

// :: (string) -> ([string]) -> Promise(stdout :: string, stderr :: string)
const command = (command) => (args) => {
    let out = ''
    let all = ''
    return new Promise((resolve, reject) => {
        const child = spawn(command, [...args])

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
const argsToArray = (x) => {
    if (typeof x === 'string') {
        return [x]
    }
    if (Array.isArray(x)) {
        return x
    }
    throw new Error(`Expected string or array but got: ${util.inspect(x)}`)
}

export {
    command,
    argsToArray,
}
