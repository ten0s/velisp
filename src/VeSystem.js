import fs from 'fs'
import os from 'os'

const homeDir = () => {
    // MSYS2 or MinGW shell
    if (process.env['MSYSTEM']) {
        return process.env['HOME']
    }
    // Under WSL
    if (process.env['WSLENV']) {
        // Expected that called with at least
        // set WSLENV=TEMP/pu:TMP/pu:USERNAME/u:USERPROFILE/pu
        return process.env['USERPROFILE']
    }
    return os.homedir()
}

const tmpDir = () => {
    const tmp = process.env['TMP']
    if (tmp && fs.existsSync(tmp)) { return tmp }
    const temp = process.env['TEMP']
    if (temp && fs.existsSync(temp)) { return temp }
    return os.tmpdir()
}

export {
    homeDir,
    tmpDir,
}
